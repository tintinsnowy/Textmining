#####################
#Author: Xiaoli Yang#
#Version: 1.0       #
#####################
### initial process
# used to calculate the freqence of brands mentioned.
library(tm)
library(rJava)
library(Rwordseg)
library("wordcloud")

data<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\SENTENCES.csv",
               header=TRUE,encoding='UTF-8',sep=",")
names(data)  #to observe the structure of the table
dataset <- data$forR_Noun_Verbs  #abstract this column
dataset <- gsub(pattern="[@|,|;|.|?|*|!]"," ",dataset) # delete the punctuation
dataset[complete.cases(dataset)]
##cleansing the dataset
content_source<-VectorSource(dataset)
corpus<-Corpus(content_source)
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removeWords,stopwords("german"))
corpus<-tm_map(corpus,removeWords,stopwords("en"))
corpus<-tm_map(corpus,removeNumbers)

##################################
##   car relevent words process ##
##################################
carslot <-readLines("carwords.txt") #the raw material of words relevant to the cars 
res<-carslot[carslot!=" "]
res <-gsub(pattern="[@|,|;|.|?|~|*|//]"," ",res)
res[complete.cases(res)]
content_source<-VectorSource(res)
corpus<-Corpus(content_source)
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removeWords,stopwords("german"))
corpus<-tm_map(corpus,removeWords,stopwords("en"))
corpus<-tm_map(corpus,removeNumbers)
text.dtm<-TermDocumentMatrix (corpus)
#text.dtm <- removeSparseTerms(text.dtm, 0.9999)
inspect(text.dtm)
# in oder to reduce the sparse matrix
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")
fre<-findFreqTerms(text.dtm,0,Inf)
write.csv(fre,fileEncoding='UTF-8',"D:\\ubuntu\\WZL-project\\Textmining\\car.csv")
#----------------------------------------------------------
car<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\car.csv",
              header=TRUE,encoding='UTF-8',sep=",")

################################################
##  Frequency of relevant words in the review ##
################################################

##cleansing the dataset
text.dtm <- TermDocumentMatrix (corpus)
#text.dtm <- removeSparseTerms(text.dtm, 0.9999)
inspect(text.dtm)
# in oder to reduce the sparse matrix
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")
#fre<-findFreqTerms(text.dtm,1,Inf)  # you can only
freq <- rowSums(text.matrix)
orderf <- sort(freq, decreasing = T)# sort the freq tables
#orderf <- orderf[orderf>1] # filtering out those whose frequence is too low
#result<- result[result !='supermini']
#orderf<- orderf[orderf !='supermini']
write.csv(orderf,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Project-Ford\\order.csv")
# now we have to find the most frequently mentioned cars
orderf<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\order.csv",
                 header=TRUE,encoding='UTF-8',sep=",")
names(orderf)<-c("name","value")
spc<-"acura|audi|bmw|buick|cadillac|chevrolet|chrysler|dodge|fiat|ford|gmc|honda|hummer|hyundai|infiniti|jaguar|jeep|kia|land rover|lexus|lincoln|maserati|mazda|mercedes-benz|mercury|mini|mitsubishi|nissan|oldsmobile|pontiac|porsche|ram|saab|saturn|scion|smart|subaru|suzuki|toyota|volkswagen|volvo"
spc <- unlist(strsplit("acura|audi|bmw|buick|cadillac|chevrolet|chrysler|dodge|fiat|gmc|honda|hummer|hyundai|infiniti|jaguar|jeep|kia|land rover|lexus|lincoln|maserati|mazda|mercedes-benz|mercedes|benz|mercury|mini|mitsubishi|nissan|oldsmobile|pontiac|porsche|ram|saab|saturn|scion|smart|subaru|suzuki|toyota|volkswagen|volvo", "[|]"))
spc <- c(spc)
result<-orderf[grepl(paste(spc, collapse="|"),orderf$name),]
result <- result[grep("supermini|minim|parameter|insta|phone",result$name,invert = TRUE),]

write.csv(result,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Textmining\\brandsfre.csv")
mycolors <- brewer.pal(8,"Dark2")
wordcloud(result$name,result$value,random.order=FALSE,random.color=TRUE,colors=mycolors,family="myFont3")

#############################
###        cluster        ###
#############################
Encoding(dataset)  <- "UTF-8"
# 
text.dtm<-TermDocumentMatrix (corpus)
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")
findFreqTerms(text.dtm,15)
k<-5
kmeansRes <- kmeans(hlzj.matrix,k)
mode(kmeansRes)
names(kmeansRes)
head(kmeansRes$cluster,10)
kmeansRes$size
hlzj.kmeansRes <- list(content=dataset,type=kmeansRes$cluster)
write.csv(hlzj.kmeansRes,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Textmining\\hlzj_kmeansRes.csv")
fix(hlzj.kmeansRes)  # used for inspect the details


#################
### wordcloud ###
#################
library("wordcloud")
text.dtm<-TermDlocumentMatrix (corpus)
inspect(text.dtm)
text.dtm <- removeSparseTerms(text.dtm, 0.9999)
inspect(text.dtm)
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")

freq <- rowSums(text.matrix)
order <- sort(freq, decreasing = T)# sort the freq tables
write.csv(order,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Textmining\\freq.csv")
barplot(order[51:75])  # to see the distribution of frequence.but only a
v <- head(table(freq),50)
d = data.frame(word=names(freq), freq=freq);
d = subset(d, nchar(as.character(d$word))>1 & d$freq>=10& d$freq<40)
mycolors <- brewer.pal(8,"Dark2")
wordcloud(d[180:255,]$word,d[180:255,]$freq,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")

