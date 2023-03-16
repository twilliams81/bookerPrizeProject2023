setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/Thesis/Code")

#These are the packages I need to use (I'm a bit package heavy and you'll see I mix styles too so please be patient with my english lit background.)
install.packages("reshape") 
install.packages("plotly")
library(data.table)
library(dplyr)
library(tidytext)
library(tidyverse)
library(quanteda)
library(stm)
library(reshape2)
library(plotly)



#some stop words have already been removed by hathi trust but I need to remove more so this loads my own list. 
df <- read_csv("booker stop words.csv", col_names = F, show_col_types = F) 
bookerStopWords <- tibble(tolower(df$X1)) #convert df to tibble and turns contents to lowercase
colnames(bookerStopWords)[1] <- "stopwords" #rename column to "stopwords"

#import title list. This is needed later when we rebind the titles
title_tb <- read_csv("imported booker filename and title.csv", show_col_types = FALSE)


#We're importing a ton of texts that were extracted from hathi so I created a function to do so. Importantly this also chunks up the texts in 1000 word bits. If the last bit is <500 words it gets tagged onto the previous bit. If it's >500 words it can be independent.

hathiCorpusChunker<-function(dir,type=".tsv"){
  curr_folder<-getwd()
  setwd(dir)
  corpusDF<-NULL
  files<-list.files(pattern = type)
  for(i in 1:length(files)){
    
    #This takes out unwanted text but leaves us with a single string
    text <- read_tsv(files[i], col_names = TRUE, col_select = "page_tokens", show_col_types = FALSE)
    text$page_tokens <- gsub("\\[","",as.character(text$page_tokens))
    text$page_tokens <- gsub("\\]","",as.character(text$page_tokens))
    text$page_tokens <- gsub("\\'","",as.character(text$page_tokens))
    text <- text[-which(text$page_tokens == ""), ] 
    text_v <- paste(text, sep = ",")
    
    clean_text_v <- unlist(strsplit(text_v, "\\W"))
    clean_text_v<-clean_text_v[which(clean_text_v!="")]
    
    #This chunks the text into 1000 words chunks
    chunkSize <- 1000
    x <- seq_along(clean_text_v)
    chunks_l <- split(clean_text_v, ceiling(x/chunkSize))
    
    #below is conditional code to stop the final chunk being less than 500 words
    
    if(length(chunks_l[[length(chunks_l)]]) <= chunkSize/2){ 
      chunks_l[[length(chunks_l)-1]] <- c(
        chunks_l[[length(chunks_l)-1]],
        chunks_l[[length(chunks_l)]]
      )
      chunks_l[[length(chunks_l)]] <- NULL }
    
    #this binds the chunk into a df
    
    chunk_strings_l <- lapply(chunks_l, paste, collapse=" ") 
    chunks_df <- do.call(rbind, chunk_strings_l) 
    
    #this grabs the file name
    
    textname_v <- gsub("\\.tsv","", files[i])
    chunk_ids_v <- 1:nrow(chunks_df)
    chunk_names_v <- paste(textname_v, chunk_ids_v, sep="_")
    fileNamesDF <- data.frame(
      id = chunk_names_v,
      text = chunks_df,
      stringsAsFactors = FALSE
    )
    corpusDF <- rbind(corpusDF,fileNamesDF)
  }
  setwd(curr_folder)
  return(corpusDF)
}





#Ok, having done all that...we can begin.

bookerCorpus <- hathiCorpusChunker("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/Thesis/Code/Booker Texts from Hathi")


#This brings in the title file so that we can see what the texts are called. 


colnames(bookerCorpus)[1] <- "filename"
bookerCorpusSep <- bookerCorpus %>%  separate_wider_delim("filename", delim = "_", names = c("filename", "number"))
bookerCorpusSepNames <- as_tibble(merge(x = bookerCorpusSep, y = title_tb, by = "filename", all.x = TRUE)) 
finalBookerCorpus <- bookerCorpusSepNames %>% unite("title_section", title, number, remove = FALSE) %>% 
  unite("filename_section", filename, number, remove = FALSE)

#This drops some of the metadata from hathi that I don't need anymore. 
finalBookerCorpus_tb <- finalBookerCorpus %>% 
  relocate(title_section) %>% 
  select(-filename) %>% 
  select(-filename_section) %>% 
  select(-number)



#convert to tidy model because I learnt how to topic model in the tidyverse and not in base R. This takes all the novel_sections and turns them into bags of words and then we remove stopwords. 

tidy_bookerCorpus <- finalBookerCorpus_tb %>%
  unnest_tokens(word, text) %>%
  anti_join(bookerStopWords, c(word = "stopwords")) 


#Converts the bags of words into a tabled count of the words in the the novel sections
booker_dfm <- tidy_bookerCorpus %>%
  count(title_section, word, sort = TRUE) %>%
  cast_dfm(title_section, word, n)

#Topic Model Time
#
#
#This runs the STM topic model over booker_dfm. 35 topics seems to work well. A note: it's set to verbose = TRUE so that I can track progress but you can set it to FALSE for it to silently process. It should take about 30 mins. There is a semantic coherence / exclusivity plots to sense check my choice of k.
##
#
#
#


bookerTopicModelBySection35<- stm(booker_dfm, K = 35, 
                                  verbose = TRUE, init.type = "Spectral")

topicQuality(
  bookerTopicModelBySection35,
  booker_dfm,
  xlab = "Semantic Coherence of 35 Topics",
  ylab = "Exclusivity",
  labels = 1:ncol(bookerTopicModelBySection35$theta),
  M = 100,
) 



#The thing I'm interested right now is the topic that a novel section is assigned too so this pulls that out of the Topic Model. I then add in some external data: the title, author, year and award.
bookerTD_gamma35 <- tidy(bookerTopicModelBySection35, matrix = "gamma",                   
                         document_names = rownames(booker_dfm))

bookerTD_gamma35$document <- tolower(bookerTD_gamma35$document)
colnames(bookerTD_gamma35)[1] <- "title"

bookerDatesAwardsAuthor <- read.csv("booker title author year award.csv") 
bookerDatesAwardsAuthor_tb <- tibble(bookerDatesAwardsAuthor)

# This pulls apart the novel section's number and puts it in another column.
cleanerBookerTD_gamma35 <- bookerTD_gamma35 %>%  separate_wider_delim("title", delim = "_", names = c("title", "number"))
cleanerBookerTD_gamma35 <- as_tibble(merge(x = cleanerBookerTD_gamma35, y = bookerDatesAwardsAuthor_tb, by = "title", all.x = TRUE)) 


#This is where I'm getting stuck. So here are two graphs. 
#
#
#This first is pretty cool. It shows the number of times each topic features in a text section. Now, technically, all topics feature in all texts so I've filtered out anything with a gamma <0.2 which is reasonably standard. A topic with a probability of being in a novel_section great that 0.2 is either medium present or strongly present. Importantly, note that Topic 2 *seems* to be the most common topic and it *seems* to get more popular over time.
cleanerBookerTD_gamma35 %>% 
  filter(gamma >= 0.2
         & award != "longlist") %>% 
  ggplot(aes(year,gamma))+
  geom_bar(position = "stack", stat = "identity")+
  facet_wrap(~topic)+
  theme_minimal()+
  labs (title = "topics over time")


#Lets look more closely at Topic 2. I couldn't get it to fut in the Plot viewer with the filter set to >= 0.2 so I increased it to 0.6 Now what becomes clear is that a small number of novels account for the heavy presence of topic 2 after 2000. In 2003 it is very present in FINGERSMITH and it's also very present in 2006 in THE NIGHT WATCH (they're by the same author). 
cleanerBookerTD_gamma35 %>% 
  filter(topic == "2" &
           gamma >= 0.6) %>% 
  ggplot(aes(year,gamma, colour = title))+
  geom_point()+
  theme_minimal()+
  labs (title = " topic 2 over time")


#Here are my core questions at this stage.
# 1. How do I account for the novel_sections? Obviously the longer the novel, the more novel sections there are and so a topic can appear to be strong when, in fact, it only appears strongly in one novel but in many, many sections?
# 
# 2. I'd like to demonstrate the change in topics over time. Ideally I'd have a line chart that plots the popularity of different topics between 1970 and 2010 but, firstly, I need to deal with problem 1.
# 
# 3. I've exported the data to excel (I'm more confident there) with the filter set to a gamma >= 0.2 and I can see that Topic 2 is really important (the file is attached) but I need to replicate the pivot table in R ideally. Then, assuming I have handled the issue 1, I'd use the pivot table to calculate the heterogenity of the novels i.e. which novels contain the most topics and which the fewest.