setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/Thesis/Code")

library(data.table)
library(dplyr)
library(tidytext)
library(tidyverse)
library(quanteda)
library(stm)
library(reshape2)
library(plotly)

hathiCorpusChunker<-function(dir,type=".tsv"){
  curr_folder<-getwd()
  setwd(dir)
  corpusDF<-NULL
  files<-list.files(pattern = type)
for(i in 1:length(files)){
  text <- read_tsv(files[i], col_names = TRUE, col_select = "page_tokens", show_col_types = FALSE)
  text$page_tokens <- gsub("\\[","",as.character(text$page_tokens))
  text$page_tokens <- gsub("\\]","",as.character(text$page_tokens))
  text$page_tokens <- gsub("\\'","",as.character(text$page_tokens))
  text <- text[-which(text$page_tokens == ""), ] 
  text_v <- paste(text, sep = ",")
  
  #This takes out unwanted text but leaves us with a single string
  
  clean_text_v <- unlist(strsplit(text_v, "\\W"))
  clean_text_v<-clean_text_v[which(clean_text_v!="")]
  chunkSize <- 1000
  x <- seq_along(clean_text_v)
  chunks_l <- split(clean_text_v, ceiling(x/chunkSize))
  
#below is conditional code to stop the final chunk being too small
  
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


z <- hathiCorpusChunker("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/Thesis/Code/Booker Texts from Hathi")
