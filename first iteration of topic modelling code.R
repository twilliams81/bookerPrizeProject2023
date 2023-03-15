#install.packages("data.table") 
install.packages("reshape") # Install data.table
install.packages("plotly")
library(data.table)
library(dplyr)
library(tidytext)
library(tidyverse)
library(quanteda)
library(stm)
library(reshape2)
library(plotly)


setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/Thesis/Code")

#bookerStopWords is made up of publishers, author names and character names that appeared in some rounds of modelling. 
df <- read_csv("booker stop words.csv", col_names = F, show_col_types = F) 
bookerStopWords <- tibble(tolower(df$X1)) #convert df to tibble and turns contents to lowercase
colnames(bookerStopWords)[1] <- "stopwords" #rename column to "stopwords"


title_tb <- read_csv("imported booker filename and title.csv", show_col_types = FALSE) #import title list



#create a function to import all the texts

getHathiTablecorpus<-function(dir,type=".tsv"){
  curr_folder<-getwd()
  setwd(dir)
  corpus<-list()
  files<-list.files(pattern=type)
  for(i in 1:length(files)){
    text<-fread(files[i],sep="\t",header = TRUE, select = "page_tokens")
    text<-paste(text,collapse=" ")
    lowertext<-tolower(text)
    text_words<-unlist(strsplit(lowertext,"\\W"))
    text_words<-text_words[which(text_words!="")]
    corpus[[files[i]]]<-text_words
  }
  setwd(curr_folder)
  return(corpus)
}


bookerCorpus <- getHathiTablecorpus("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/Thesis/Code/Booker Texts from Hathi")






booker_v <- sapply(bookerCorpus, paste, collapse = "\n")
booker_tb <- as_tibble(cbind(booker_v=names(booker_v), text=booker_v))



colnames(booker_tb)[1] <- "filename"
finalBooker_tb <- as_tibble(merge(x = booker_tb, y = title_tb, by = "filename", all.x = TRUE)) #adds column of names

newFinalBooker_tb <- finalBooker_tb %>% relocate(title) %>% 
  select(-filename)

#class(titleList)

#View(finalBooker_tb)

#convert to tidy model

tidy_bookerCorpus <- newFinalBooker_tb %>%
  unnest_tokens(word, text) %>%
  anti_join(bookerStopWords, c(word = "stopwords")) 

#Remove numbers
#tidy_bookerCorpus <- tidy_bookerCorpus[-grep("\\b\\d+\\b", tidy_bookerCorpus$word),]

tidy_bookerCorpus %>%  count(word, sort = TRUE) #sense checking to see most popular words in our corpus






#Converts the bags of words into a tabled count of the words in the play.
booker_dfm <- tidy_bookerCorpus %>%
  count(title, word, sort = TRUE) %>%
  cast_dfm(title, word, n)

#Topic Model Time
#
#
#This runs the STM topic model over the renDrama dataframe. It's set at 15 topics (k = 15). This will take a few minutes to run. Some people seed this to ensure that it is reproducible but, for me, results have been consistent.




bookerTopicModel20<- stm(booker_dfm, K = 20, 
                         verbose = FALSE, init.type = "Spectral")

topicQuality(
  bookerTopicModel20,
  booker_dfm,
  xlab = "Semantic Coherence of 19 Topics",
  ylab = "Exclusivity",
  labels = 1:ncol(bookerTopicModel20$theta),
  M = 100,
) 


#Convert topic model into tidy format to examine the words in each topic and then grab the top 10 terms per topic
td_betaBooker <- tidy(bookerTopicModel20) %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

write_csv2(td_betaBooker, 'top 10 topic words at 20 topics.csv')





bookerNames <- rownames(booker_dfm)#pulls out the names of the novels so that they can be reattached to the novels by topic list.

booksByTopic <- findThoughts(
  bookerTopicModel20,
  texts = bookerNames,
  topics = NULL,
  n = 10,
  thresh = 0.6,
  where = NULL,
  meta = NULL)



#This lets you see probability of a novel belonging to a topic
bookerTD_gamma20 <- tidy(bookerTopicModel20, matrix = "gamma",                    
                   document_names = rownames(booker_dfm))



bookerTD_gamma20$document <- tolower(bookerTD_gamma20$document)
colnames(bookerTD_gamma20)[1] <- "title"


bookerDatesAwardsAuthor <- read.csv("booker title author year award.csv") 
bookerDatesAwardsAuthor_tb <- tibble(bookerDatesAwardsAuthor)




bookerTDDatesAwardsAuthor_gamma20 <- as_tibble(merge(x = bookerTD_gamma20, y = bookerDatesAwardsAuthor_tb, by = "title", all.x = TRUE)) #adds column of names, author and award


#create a scatter plot of topics over time
bookerTDDatesAwardsAuthor_gamma20 %>% 
  filter(gamma >= 0.6,
         award != "Longlist" ) %>% 
  ggplot(aes(x = year,
             y = topic,
             colour = award)) +
    geom_point()+
  geom_smooth(method = lm,
              se = F)+
  labs (x = "Year",
        y = "Topic") +
  theme_minimal()

x <- bookerTDDatesAwardsAuthor_gamma20 %>% 
  filter(gamma >= 0.6) %>% 
  write_csv2("booker dates awards authors with 20 topics.csv")


######## Some fiddling with other numbers of topics...

td_betaBooker <- tidy(bookerTopicModel22) %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)




bookerNames <- rownames(booker_dfm)#pulls out the names of the novels so that they can be reattached to the novels by topic list.

booksByTopic <- findThoughts(
  bookerTopicModel20,
  texts = bookerNames,
  topics = NULL,
  n = 10,
  thresh = 0.6,
  where = NULL,
  meta = NULL)



#This lets you see probability of a novel belonging to a topic
bookerTD_gamma22 <- tidy(bookerTopicModel22, matrix = "gamma",                    
                         document_names = rownames(booker_dfm))



bookerTD_gamma22$document <- tolower(bookerTD_gamma22$document)
colnames(bookerTD_gamma22)[1] <- "title"


bookerDatesAwardsAuthor <- read.csv("booker title author year award.csv") 
bookerDatesAwardsAuthor_tb <- tibble(bookerDatesAwardsAuthor)




bookerTDDatesAwardsAuthor_gamma22 <- as_tibble(merge(x = bookerTD_gamma22, y = bookerDatesAwardsAuthor_tb, by = "title", all.x = TRUE)) #adds column of names, author and award


#create a scatter plot of topics over time
bookerTDDatesAwardsAuthor_gamma22 %>% 
  filter(gamma >= 0.6,
         award != "Longlist" ) %>% 
  ggplot(aes(x = year,
             y = topic,
             colour = award)) +
  geom_point()+
  geom_smooth(method = lm,
              se = F)+
  labs (x = "Year",
        y = "Topic") +
  theme_minimal()
  




