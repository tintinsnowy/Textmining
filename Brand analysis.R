library(tm)
library(xlsx)
library(rJava)
library(Rwordseg)
# SENTENCES.csv is the same with FOrd 2017....
data<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\SENTENCES.csv",
               header=TRUE,encoding='UTF-8',sep=",")
names(data)
dataset <- data$sentence
dataset <- gsub(pattern="[@|,|;|.|?|*|!]"," ",dataset)
dataset<- dataset[grep("nissan|Nissan|NISSAN",dataset)]
write.csv(dataset,fileEncoding='UTF-8',"D:\\ubuntu\\WZL-project\\Project-Ford\\nissan.csv")

