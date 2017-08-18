library(tm)
library(xlsx)
library(rJava)
library(Rwordseg)
# SENTENCES.csv is the same with FOrd 2017....
data<-read.csv("D:\\ubuntu\\WZL-project\\Textmining\\SENTENCES.csv",
               header=TRUE,encoding='UTF-8',sep=",")
names(data)
dataset <- data$sentence
dataset <- gsub(pattern="[@|,|;|.|?|*|!]"," ",dataset)
dataset[complete.cases(dataset)]
##cleansing the dataset
content_source <- VectorSource(dataset)
corpus <- Corpus(content_source)
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removeWords,stopwords("german"))
corpus <- tm_map(corpus,removeWords,stopwords("en"))
corpus <- tm_map(corpus,removeNumbers)
text.dtm <- TermDocumentMatrix (corpus)
car <- read.csv("D:\\ubuntu\\WZL-project\\Textmining\\car.csv",
                header=TRUE,encoding='UTF-8',sep=",")
inspect(text.dtm)
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")
len <- length(dataset)
result <- ""
for(i in seq(1, len, 50)){
  result1<- dataset[grepl(paste(car[i:i+50,], collapse="|"),dataset)]
  result <- c(result,result1)
}
result<-unique(result)
write.csv(result,fileEncoding='UTF-8',"D:\\ubuntu\\WZL-project\\Textmining\\filtering-car-term.csv")

#-------------------------------LDA---------------------------
  ############################
### LDA topic clustering ###
############################

text.dtm<-DocumentTermMatrix(corpus)
inspect(text.dtm)
library("topicmodels")

k<-8
SEED <- 2010
jss_TM <- list(VEM = LDA(text.dtm, k = k, control = list(seed = SEED)),
               VEM_fixed = LDA(text.dtm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)),
               Gibbs = LDA(text.dtm, k = k, method = "Gibbs",
                           control = list(seed = SEED, burnin = 1000,
                                          thin = 100, iter = 1000)),
               CTM = CTM(text.dtm, k = k,
                         control = list(seed = SEED,
                                        var = list(tol = 10^-4), em = list(tol = 10^-3))))
inspect(text.dtm)
VEM = LDA(text.dtm, k = k, control = list(seed = SEED))
Terms<-terms(VEM,20)
Terms
write.csv(Terms,"D:\\ubuntu\\WZL-project\\Textmining\\DocsToTopics2.csv")



#------------------------------LDA------------------------------
burnin<-4000
iter<-2000
thin<-500
seed<-list(2003,5,63,100001,765)
nstart<-5
best<-TRUE

#Number of Topics
k<-8
