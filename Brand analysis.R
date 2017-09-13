#####################
#Author: Xiaoli Yang#
#Version: 1.0       #
#####################
library(tm)
library(xlsx)
library(rJava)
library(Rwordseg)
# SENTENCES.csv is the same with FOrd 2017....
data<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\SENTENCES.csv",
               header=TRUE,encoding='UTF-8',sep=",")
names(data)
dataset <- data$sentence
#delete the punctuations
dataset <- gsub(pattern="[@|,|;|.|?|*|!]"," ",dataset)
# fliter out the reviews with ford and nissan
dataset<- dataset[grep("nissan|Nissan|NISSAN &&(ford|fiesta|Ford)",dataset)]
write.csv(dataset,fileEncoding='UTF-8',"D:\\ubuntu\\WZL-project\\Project-Ford\\nissan.csv")

