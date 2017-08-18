##########################
### initial process    ###
##########################
library(tm)
library(xlsx)
library(rJava)
library(Rwordseg)
data<-read.csv("D:\\ubuntu\\WZL-project\\Textmining\\token.csv",
               header=TRUE,encoding='UTF-8',sep=",")
data<-read.csv("D:\\ubuntu\\WZL-project\\Textmining\\SENTENCES.csv",
header=TRUE,encoding='UTF-8',sep=",")

names(data)
dataset<- data$forR_Noun_Adj
dataset <- gsub(pattern="[@|,|;|.|?|*|!]"," ",dataset)
dataset[complete.cases(dataset)]
##cleansing the dataset
content_source<-VectorSource(dataset)
corpus<-Corpus(content_source)
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removeWords,stopwords("german"))
corpus<-tm_map(corpus,removeWords,stopwords("en"))
corpus<-tm_map(corpus,removeNumbers)

##########################
##   car relevent worsd ##
##########################
carslot <-readLines("carwords.txt")
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
fre<-findFreqTerms(text.dtm,0,Inf)  # you can only
write.csv(fre,fileEncoding='UTF-8',"D:\\ubuntu\\WZL-project\\Textmining\\car.csv")
#----------------------------------------------------------
car<-read.csv("D:\\ubuntu\\WZL-project\\Textmining\\car.csv",
               header=TRUE,encoding='UTF-8',sep=",")


##############################
##       frequency          ##
##############################
text.dtm <- TermDocumentMatrix (corpus)
text.dtm <- removeSparseTerms(text.dtm, 0.9999)
inspect(text.dtm)
# in oder to reduce the sparse matrix
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")
#fre<-findFreqTerms(text.dtm,1,Inf)  # you can only
freq <- rowSums(text.matrix)
orderf <- sort(freq, decreasing = T)# sort the freq tables
orderf <- orderf[orderf>1] # filtering
write.csv(orderf,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Textmining\\order.csv")
orderf<-read.csv("D:\\ubuntu\\WZL-project\\Textmining\\order.csv",
               header=TRUE,encoding='UTF-8',sep=",")
names(orderf)<-c("name","value")
# "acura|audi|bmw|buick|cadillac|chevrolet|chrysler|dodge|fiat|ford|gmc|honda|hummer|hyundai|infiniti|jaguar|jeep|kia|land rover|lexus|lincoln|maserati|mazda|mercedes-benz|mercury|mini|mitsubishi|nissan|oldsmobile|pontiac|porsche|ram|saab|saturn|scion|smart|subaru|suzuki|toyota|volkswagen|volvo"
spc<-unlist(strsplit("acura|audi|bmw|buick|cadillac|chevrolet|chrysler|dodge|fiat|gmc|honda|hummer|hyundai|infiniti|jaguar|jeep|kia|land rover|lexus|lincoln|maserati|mazda|mercedes-benz|mercury|mini|mitsubishi|nissan|oldsmobile|pontiac|porsche|ram|saab|saturn|scion|smart|subaru|suzuki|toyota|volkswagen|volvo", "[|]"))
spc<-c(spc)
result<-orderf[grepl(paste(spc, collapse="|"),orderf$name),]
write.csv(result,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Textmining\\brandsfre.csv")
mycolors <- brewer.pal(8,"Dark2")
wordcloud(result$name,result$value,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")

#----------------try to find the cars mentioned---------------
result<-dataset[grepl(paste(spc, collapse="|"),dataset)]
write.csv(result,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Textmining\\rest.csv")
content_source<-VectorSource(dataset)
corpus<-Corpus(content_source)
text.dtm<-TermDocumentMatrix (corpus)
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")
kre<-kmeans(text.matrix, 4)
head(kre$cluster)
kre$size
##????hlzj.kmeansRes <- list(content=dataset,type=kre$cluster)
write.csv(hlzj.kmeansRes,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Textmining\\hlzj_kmeansRes.csv")
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
text.dtm<-TermDocumentMatrix (corpus)
inspect(text.dtm)
text.dtm <- removeSparseTerms(text.dtm, 0.9999)
inspect(text.dtm)
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")

freq <- rowSums(text.matrix)
order <- sort(freq, decreasing = T)# sort the freq tables
write.csv(order,fileEncoding='UTF-8',
          "D:\\ubuntu\\WZL-project\\Textmining\\freq.csv")
barplot(order[51:75])  # to see the distribution of frequence.
v <- head(table(freq),50)
d = data.frame(word=names(freq), freq=freq);
d = subset(d, nchar(as.character(d$word))>1 & d$freq>=10& d$freq<40)
mycolors <- brewer.pal(8,"Dark2")
wordcloud(d[180:255,]$word,d[180:255,]$freq,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")


############################
### LDA topic clustering ###
############################
text.dtm<-DocumentTermMatrix (corpus)
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
