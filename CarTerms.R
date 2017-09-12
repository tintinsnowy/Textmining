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
car <- read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\car.csv",
                header=TRUE,encoding='UTF-8',sep=",")
inspect(text.dtm)
text.matrix <- as.matrix(text.dtm,encoding = "UTF-8")
len <- length(car$name)
result <- ""
for(i in seq(1, len, 50)){
  result1<- dataset[grepl(paste(car[i:i+50,], collapse="|"),dataset)]
  result <- c(result,result1)
}
result<-unique(result)
write.csv(result,fileEncoding='UTF-8',"D:\\ubuntu\\WZL-project\\Project-Ford\\filtering-car-term.csv")
# before you go further you have to delete the first row
#-------------------------------LDA---------------------------
data<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\filtering-car-term.csv",
               header=TRUE,encoding='UTF-8',sep=",")
names(data)
dataset <- data$x
dataset <- gsub(pattern="[@|,|;|.|?|*|!]"," ",dataset)
dataset[complete.cases(dataset)]
##cleansing the dataset
content_source <- VectorSource(dataset)
corpus <- Corpus(content_source)
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removeWords,stopwords("german"))
corpus <- tm_map(corpus,removeWords,stopwords("en"))
#corpus <- tm_map(corpus,removeNumbers)
############################
### LDA topic clustering ###
############################
# be sure to use the data from freq-term!
text.dtm <- DocumentTermMatrix(corpus)
inspect(text.dtm)
library("topicmodels")
library(tidyverse)
library(tidytext)
k<-10
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
rowTotals <- apply(text.dtm , 1, sum) #Find the sum of words in each Docx`x`ument
text.dtm <- text.dtm[rowTotals > 0, ]  
VE = LDA(text.dtm, k = k, control = list(seed = SEED))
#chapters_lda_td <- tidy(VEM)
Topic <- topics(VE, 1)
Topic
Terms<-terms(VE,20)
Terms
write.csv(Terms,"D:\\ubuntu\\WZL-project\\Project-Ford\\VEM10-2.csv")
ap_topics <- tidy(VE, matrix = "beta")
ap_topics
top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms
write.csv(top_terms,"D:\\ubuntu\\WZL-project\\Project-Ford\\topics.csv")
#################
### wordcloud ###
#################
library("wordcloud")
mycolors <- brewer.pal(8,"Dark2")
# topic1
topic1 <- top_terms[1:22,]
topic1 <- topic1[grep("include",topic1$term,invert = TRUE),]
topic1
wordcloud(topic1$term,topic1$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")
# topic2
topic2 <- top_terms[33:62,]
topic2<- topic2[grep("image",topic2$term,invert = TRUE),]
topic2
wordcloud(topic2$term,topic2$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")
#toic3
topic3 <- top_terms[63:99,]
topic3<- topic3[grep("way|car",topic3$term,invert = TRUE),]
topic3
wordcloud(topic3$term,topic3$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")
#toic4
topic4 <- top_terms[100:129,]
topic4<- topic4[grep("way|car",topic4$term,invert = TRUE),]
topic4
wordcloud(topic4$term,topic4$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")
#toic5
topic5 <- top_terms[130:159,]
topic5<- topic5[grep("ford|car|fiesta|much|new",topic5$term,invert = TRUE),]
topic5
wordcloud(topic5$term,topic5$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")
#toic 6
topic6 <- top_terms[160:189,]
topic6<- topic6[grep("car|bit|fiesta|ford|engine|hatchback|buyer",topic6$term,invert = TRUE),]
topic6
wordcloud(topic6$term,topic6$beta,random.order=FALSE,random.color=TRUE,colors=mycolors,family="myFont3")
#toic7
topic7 <- top_terms[124:143,]
topic7<- topic7[grep("fiesta|car",topic7$term,invert = TRUE),]
topic7
wordcloud(topic7$term,topic7$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")
#toic8
topic8 <- top_terms[226:255,]
topic8<- topic8[grep("ford|fiesta|option",topic8$term,invert = TRUE),]
topic8
wordcloud(topic8$term,topic8$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")
#toic9
topic9 <- top_terms[256:285,]
topic9<- topic9[grep("fiesta|car|many|space",topic9$term,invert = TRUE),]
topic9
wordcloud(topic9$term,topic9$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")
#toic10
topic10 <- top_terms[286:315,]
topic10<- topic10[grep("keeping|car|fiesta|ford|driver|drive",topic10$term,invert = TRUE),]
topic10
wordcloud(topic10$term,topic10$beta,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont3")


#--------------------Gibbs---------------------
rowTotals <- apply(text.dtm , 1, sum) #Find the sum of words in each Document
text.dtm <- text.dtm[rowTotals> 0, ]  
Gibbs = LDA(text.dtm, k = k, method = "Gibbs",
            control = list(seed = SEED, burnin = 1000,
                           thin = 100, iter = 1000))

Topic <- topics(Gibbs, 1)
Topic
Terms<-terms(Gibbs,40)
Terms
write.csv(Terms,"D:\\ubuntu\\WZL-project\\Project-Ford\\Gibss4.csv")

#-----------------to find specific topics’s details-------------------
data<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\VEM15.csv",
               header=TRUE,encoding='UTF-8',sep=",")
names(data)
fileter <- data$Topic.1
fileter <- fileter[grep("ford|driver|system|feature|seat|control|standard",fileter,invert = TRUE)]
result1<- unique(dataset[grepl(paste(fileter[1:8], collapse="|"),dataset)])
write.csv(result1,"D:\\ubuntu\\WZL-project\\Project-Ford\\safty.csv")

#-----------------to find out the features of material--------------
fileter <- topic7$term
fileter <- fileter[grep("ford",fileter,invert = TRUE)]
data<-read.csv("D:\\ubuntu\\WZL-project\\Project-Ford\\SENTENCES.csv",
               header=TRUE,encoding='UTF-8',sep=",")
dataset <- data$sentence
result1<- unique(dataset[grepl(paste(fileter, collapse="|"),dataset)])
write.csv(result1,"D:\\ubuntu\\WZL-project\\Project-Ford\\material.csv")

#-----------------to visulize the topics------------------------
library(LDAvis)
doc.list <- strsplit(dataset, "[[:space:]]+")
# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)
K <- 10
G <- 5000
alpha <- 0.03
eta <- 0.02

# Fit the model:
library(lda)
set.seed(2010)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
carReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

json <- createJSON(phi = carReviews$phi, 
                   theta = carReviews$theta, 
                   doc.length = carReviews$doc.length, 
                   vocab = carReviews$vocab, 
                   term.frequency = carReviews$term.frequency)
serVis(json, out.dir = 'vis10', open.browser = FALSE)
#serVis(json, out.dir = 'vis10', open.browser = interactive())

