## sentiment analysis of twitter data..................
## @Keya Mondal /2021/06/19 .......................
options(repos = c(CRAN = "http://cran.rstudio.com")) # repo-path setup
# install packages..............................
install.packages("syuzhet")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("tm")
# libraries
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(syuzhet)
library(tidyr)
library(dplyr)
library(tm)
## data input...................
cardata <- "D:/textAnalysis/Data/Twitter-sentiment-self-drive-DFE.csv" #datapath
lempath <- 'D:/textAnalysis/Data/lemmatization-en.txt'# Lemmatization path
df<-read.csv(cardata,header=TRUE) # read data frame in csv format
df1 <- df[,-c(2:6,8,9,10),drop=FALSE] # drop data column
# Function................................
# read lemmatization data
lemword <- scan(lempath,what='',sep='\n')
lemword<- strsplit(lemword,"[[:space:]]+")
fst_word<- sapply(lemword, `[[`, 1)# first element in a list
sec_word<- sapply(lemword, `[[`, 2)# Extract 2nd-vector element as list
# lemmatization function
lemword<-function(x1,fst_word,sec_word){
  linew<-unlist(strsplit(x1," "))
  word<- which(sec_word %in% linew)
  if(length((word)>0)){
    for(i in length(word)){
      linew[which(sec_word[word[i]] == linew)] <- fst_word[[word[i]]]
    }
    line<-paste(linew,collapse = " ")
  }
  return(x1)
}
# analysis part....................
# wordcloud on text
wordcloud(words = df$text,max.words=500, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
# clean text in the tweets raw data
df1['cleantext']<-""
for( i in 1:nrow(df1)){
  x2 <- df1$text[i]
  x2 <- tolower(x2) # all lower cases
  x2 <- lemword(x2,fst_word,sec_word) # using lemmatization
  line1 <-unlist(strsplit(x2," ")) #using split
  line1 <-line1[lapply(line1,function(x) length(grep("@",x,value=FALSE))) == 0] # Remove "@" with
  words
  line1 <- line1[lapply(line1,function(x) length(grep("http",x,value=FALSE))) == 0]# Remove weblink
  line1 <- gsub(", "," ",toString(line1))
  line1 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", line1, perl=TRUE)
  cleanTweet <- gsub("[[:punct:]]", "", line1) # remove punctuation
  cleanTweet <- gsub("[[:digit:]]", "", cleanTweet) # remove numbers/Digits
  cleanTweet = lemword(cleanTweet,fst_word,sec_word)# call lemmmatization
  cleanTweet <- gsub("googleì???ââ???ãs", " ", cleanTweet) # # remove a special word
  cleanTweet <- gsub("ââå", "", cleanTweet) # remove a special word
  cleanTweet <- gsub("cars", "car", cleanTweet) # remove tabs
  cleanTweet <- gsub("[^[:graph:]]"," ",cleanTweet)
  cleanTweet <- gsub(" $", "", cleanTweet) # remove blank spaces at the end
  df1$cleantext[i] <- cleanTweet
}
# analysis on sentiment.confidence................
# create frequency table
df1["confidence_label"]<- ifelse(df1$sentiment.confidence<=0.5,50,
                                 ifelse(df1$sentiment.confidence<=0.75 & df1$sentiment.confidence>0.5,75,100))
x<-table(df1$confidence_label)
summary(df1$confidence_label)
# histogram plot on confidence_lables
plotData3 =gather(df1,"confidence","values",5)%>%
  group_by(df1$confidence_label) %>% summarise(Total=n())
names(plotData3)=c("confidence","Total")
# Plot
ggplot(data = plotData3, aes(x = plotData3$confidence, y = plotData3$Total),cex=1.5) +
  geom_bar(aes(fill = confidence), stat = "identity") +
  theme(legend.position = "none") +
  xlab("confidence") + ylab("Total") + ggtitle("confidence for Self Drive Cars")+
  geom_text(aes(label =plotData3$Total), position = position_dodge(width=30), vjust = -0.25,hjust=0.5)
# Pie chart on confidence_lables
pie_percent<- round(100*x/sum(x), 1)
labels <- c("Count of Users(842)", "Count of Users(2577)", "Count of Users(3737)")
pie(x,labels,main = "User Self-Confidence", col = rainbow(length(x)))
legend("topright",c("50% Confidence(11.8%)", "75% Confidence(36%)", "100%
Confidence(52.2%)"),cex=0.8,fill = rainbow(length(x)))
# The sentiment analysis algorithm used here is based on the Word-Emotion Association...............
text_sentiment<- get_nrc_sentiment(df1$cleantext)
df2 <- cbind(df1,text_sentiment)# column join
plotData1 =gather(df2,"sentiment","values",6:13)%>%
  group_by( sentiment) %>% summarise(Total = sum(values))# sentiment/emotion
# Plot
ggplot(data = plotData1, aes(x = plotData1$sentiment, y = plotData1$Total)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Emotions") + ylab("Total") + ggtitle("Emotion for Self Drive Cars")+
  geom_text(aes(label = plotData1$Total), position = position_dodge(width=0.75), vjust = -0.25)
# Positive vs negetive text
plotData2 =gather(df2,"Polarity","values",14:15) %>%
  group_by( Polarity) %>%
  summarise(Total = sum(values))
# plot
ggplot(data = plotData2, aes(x = plotData2$Polarity, y = plotData2$Total)) +
  geom_bar(aes(fill = plotData2$Polarity), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total") + ggtitle("Sentiment for Self-Drive Cars")+
  geom_text(aes(label = plotData2$Total), position = position_dodge(width=0.75), vjust = -0.25)
# word cloud on modified text
x <- VectorSource(df1$cleantext)# text to word vector
x <- VCorpus(x)# create a corpus object
dtm <- TermDocumentMatrix(x, control = list(removePunctuation = TRUE,removeNumbers = TRUE,
                                            stopwords = TRUE))# docoment matrix on text
m <- as.matrix(dtm) # store a matrix
v <- sort(rowSums(m),decreasing=TRUE) # decreasing order sorting
d <- data.frame(word = names(v),freq=v)# create adata frame
d1 <- d[0:2000,]# consider ony first 2000 text
# plotting the word colud
par(bg='grey30')
png(file='WordCloud.png',width=1280,height=720, bg='grey30')
wordcloud(d1$word, d1$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = 'Self Driveing Cars Most Used in the Tweets', font.main = 1, col.main = 'cornsilk3', cex.main = 1.5)
dev.off()