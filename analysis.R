#########################
### token frequencies ###
#########################

library(xlsx)

data<-read.csv("D:/LenovoCloud/ubuntu/WZL-project/Textmining/token.csv",
               header=TRUE, 
               sep=";", row.names=NULL)

nouns<-data[grep("NN|NE",data$pos),]
nouns[1:10,]
nouns<-tolower(nouns$lemma)
nouns[1:10]

table(nouns)
nounsSort<-sort(table(nouns),decreasing=TRUE)
nounsSort

setwd("D:/LenovoCloud/ubuntu/WZL-project/Textmining/")
write.xlsx(nounsSort,"170307_nouns_frequencies.xlsx",row.names=FALSE)


###############
### barplot ###
###############

library(ggplot2)

token<-read.csv("W:/Projekte/MQ_MQ-PM_16_04_1/2017/1603-1702/EU_de/R/features/170317_features_categories.csv",
                header=FALSE,
                sep=";")

token<-as.matrix(token)
token<-matrix(token,ncol=2)
frequency<-as.numeric(token[,2])
words<-token[,1]

###decreasing###
words<-reorder(words,frequency)
###increasing###
#words<-factor(words,as.character(words))

df<-data.frame(term=words,freq=frequency)
###plot term occurences with axes flipped
ggplot(df,aes(x=term,y=freq))+
  geom_bar(stat="identity",fill="blue")+
  coord_flip()


####################
### associations ###
####################

library(tm)
library(corrplot)
library(ggplot2)

data<-read.csv("W:/Projekte/MQ_MQ-PM_16_04_1/2017/1603-1702/de/R/170308_1603-1703_sentences.csv",
               header=TRUE,
               sep=";")

comments<-data$content

content_source<-VectorSource(comments)
corpus<-Corpus(content_source)
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removeWords,stopwords("german"))
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,PlainTextDocument)

dtm<-DocumentTermMatrix(corpus)

corr<-findAssocs(dtm,"touchscreen",0.1)
corr

### plot correlation
word<-"touchscreen"
corlimit<-0.2
target<-data.frame(corr=findAssocs(dtm,word,corlimit)[[1]],
                  terms=names(findAssocs(dtm,word,corlimit)[[1]]))
target$terms<-factor(target$terms,levels=target$terms)
ggplot(target,aes(y=terms))+
  geom_point(aes(x=corr),data=target)+
  xlab("touchscreen")

####################
### collocations ###
####################

library(quanteda)

txt<-c("Ich mag den Ford Ranger!",
  "Ich finde, der Ranger von Ford ist ganz gut gelungen.",
  "Ich mag VW einfach lieber als Ford!",
  "Ford Ranger oder VW Amarok - ich weiß es nicht!",
  "Das Design des Ford Rangers gefällt mir gar nicht!")

collocations(txt,
  punctuation="ignore",
  removePunct=TRUE,
  method="all")    

removeFeatures(collocations(txt),stopwords("german"))


#################
### wordcloud ###
#################

library(tm)
library(wordcloud)

set.seed(2005)

token<-read.csv("W:/Projekte/MQ_MQ-PM_16_04_1/2017/1603-1702/EU_de/R/features/service/170317_service.csv",
                header=FALSE,
                sep=";")

token<-as.matrix(token)
token<-matrix(token,ncol=2)
token.sort<-token[order(token[,2]),decreasing=TRUE]
token.sort

frequency<-as.numeric(token.sort[,2])
words<-token.sort[,1]

wordcloud(words,frequency,
          random.order=FALSE,
          random.color=TRUE,
          rot.per=.0,
          colors=c("black","blue","red","darkgreen","orange","purple"),
          ordered.colors=FALSE,
          use.r.layout=FALSE)


###################
### word search ###
###################

library(xlsx)

data<-read.csv("W:/Projekte/MQ_MQ-PM_16_04_1/2017/1603-1702/EU/EU_en/R/170315_sentences_EU.csv",
               header=TRUE,
               sep=";")

### SERVICE EN ###
target<-unique(data[grep(" dealer| guarantee | incentive | insurance | maitenance | rabate | reliability | salesman | seller | service | shop | warranty | workshop",data$content,ignore.case = TRUE),])
newSubset<-c("content", "date", "day", "title", "comment","paragraph", "sentence", "id", "forum", "author")
target2<-target[newSubset]
setwd("D:/")
write.xlsx(target2,"170317_service_sentiment.xlsx",row.names=FALSE)

### ENGINE EN ###
target<-unique(data[grep("cylinder |diesel | ecoboost | ecothirst | engine | motor | tdci | throttle | v6 ",data$content,ignore.case = TRUE),])
newSubset<-c("content", "date", "day", "title", "comment","paragraph", "sentence", "id", "forum", "author")
target2<-target[newSubset]
setwd("D:/")
write.xlsx(target2,"170317_engine_sentiment.xlsx",row.names=FALSE)

### NEW FEAUTURES DE ###
target<-unique(data[grep(" abstands| acc| allrad| assistent| audio| auffahrwarn| bergabfahr| berganfahr| bodenfreiheit | bremsassistent | differenzialsperre| drehknopf| drehschalt| einparkhilfe | fahrleistung| fahrspur| front| geschwindigkeitsbegrenz| geschwindigkeitsregel|grill | haube | motorhaube | infotainment | innen|verbrauch | multimedia| notruf| parkassistent | parksensoren | rückfahrkamera| scheinwerfer| sensor| servolenkung | sprach| spurerkennung| spurführung| spurhalte| spurwechselwarn| sync| tempomat|touchscreen | verbrauch | verbrauchssenkung | verkehrsschild| verkehrszeichen| zuschalt| watttiefe | bodenfreiheit | 130| 96| 175| 118| 160| 200| 147",data$content,ignore.case=TRUE),])
setwd("D:/")
write.xlsx(target,"170315_new_features.xlsx",row.names=FALSE)


#####################
### sentimentplot ###
#####################

data<-read.csv("W:/Projekte/MQ_MQ-PM_16_04_1/2017/1603-1702/EU/R/170320_number_of_posts_per_author_EU.csv",
                 header=TRUE, 
                 sep=";")
data<-data$number.of.platforms
tab<-table(data)
tab<-prop.table(tab)
tab
#barplot(tab,ylim=c(0.0,1.0),col="red","grey","green")
barplot(tab,ylim=c(0.0,0.8),col="blue")


##########################
### comments over time ###
##########################

data<-read.csv("D:/en_EU/R/170315_comments_time_EU.csv",
               header=TRUE,
               sep=";")

month<-data$Monat
table(month)

plot(month,type="b",
     #main="Number of comments over time",
     #xlab="Year",ylab="Number of comments",
     ylim=c(0,550),
     col="blue")
plot

###############
### cluster ###
##############

library(tm)
library(ggplot2)

data<-read.csv("S:/Lehrstuhl/Abteilung_QM/Abteilung PM/01_PQD/SOCIALYOU/Industrie/Akquise/Beispiele/probleme_motorrad.csv",
               sep=";",
               header=TRUE)

comments<-data$lemma2
content_source<-VectorSource(comments)
corpus<-Corpus(content_source)
tdm<-TermDocumentMatrix(corpus)


distMatrix<-dist(scale(tdm))
fit<-hclust(distMatrix,method="mcquitty")
plot(fit)


###############
### network ###
###############

library(tm)

data<-file.path("S:/Lehrstuhl/Abteilung_QM/Abteilung PM/01_PQD/SOCIALYOU/Industrie/Akquise/Claas/R/leaduser")

data<-Corpus(DirSource(data))
### clean texts; define texts as plain text document ###
data<-tm_map(data,removePunctuation)
data<-tm_map(data,tolower)
#data<-tm_map(data,removeNumbers)
#data<-tm_map(data,removeWords,stopwords("english"))
#data<-tm_map(data,removeWords,c("ive","one"))
data<-tm_map(data,PlainTextDocument)
tdm<-TermDocumentMatrix(data)
#tdm<-removeSparseTerms(dtm,0.1)
tdm<-as.matrix(tdm)
#tdm<-termDocMatrix
#tdm[1:2,1:2]

# change it to a Boolean matrix
tdm[tdm>=1]<-1
# transform into a term-term adjacency matrix
tm<-tdm%*%t(tdm)
# inspect terms numbered 5 to 10
#tm[1:2,1:2]

library(igraph)

# build a graph from the above matrix
g<-graph.adjacency(tm,weighted=T,mode ="directed")
plot(g)
# remove loops
g<-simplify(g)
plot(g)
# set labels and degrees of vertices
V(g)$label<-V(g)$name
V(g)$degree<-degree(g)

# set seed to make the layout reproducible
set.seed(395)
layout1<-layout.fruchterman.reingold(g)
plot(g,layout=layout1)
