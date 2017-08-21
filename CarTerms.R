library(tm)
library(xlsx)
library(rJava)
library(Rwordseg)
# SENTENCES.csv is the same with FOrd 2017....
data<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\SENTENCES.csv",
               header=TRUE,encoding='UTF-8',sep=",")
names(data)
dataset <- data$forR_Noun_Adj
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
car <- read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\car.csv",
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
write.csv(result,fileEncoding='UTF-8',"D:\\ubuntu\\WZL-project\\Project-Ford\\filtering-car-term.csv")
# before you go further you have to delete the first row
#-------------------------------LDA---------------------------
############################
### LDA topic clustering ###
############################

text.dtm <- DocumentTermMatrix(corpus)
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
rowTotals <- apply(text.dtm , 1, sum) #Find the sum of words in each Document
text.dtm <- text.dtm[rowTotals> 0, ]  
VEM = LDA(text.dtm, k = k, control = list(seed = SEED))
#chapters_lda_td <- tidy(VEM)
Topic <- topics(VEM, 1)
Topic
Terms<-terms(VEM,30)
Terms
write.csv(Terms,"D:\\ubuntu\\WZL-project\\Project-Ford\\DocsToTopics2.csv")

