rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c('httr', 'XML', 'rvest', 'xml2', 'RCurl', 'tidyverse', 'tidytext', 'glue', 'stringr',
            'purrr', 'tm', 'lubridate', 'Scale', 'reshape2', 'wordcloud', 'RColorBrewer', 'syuzhet', 'dbplyr')      
installIfAbsentAndLoad(needed)

url_base <- ("https://<url>/?page=%d")
map_df(1:39, function(i) {
  cat("scraped!")
  url_content <- getURLContent("https://<url>/?page=%d", userpwd="user:password", httpauth = 1L)
  pg <- read_html(url_content, i)
  
  data.frame(Reviews=html_text(html_nodes(pg, ".entry-content")),
             stringsAsFactors=FALSE)
  
}) -> df

write.table(df, file = "newreviews.csv")

dataset <- read.csv('newreviews.csv', na.strings = c("", "NA"), stringsAsFactors = FALSE)
dataset <- data.frame(dataset[-grep('exercise' ,dataset$Reviews, perl=T),])
colnames(dataset) [1] <- c("Reviews")
dataset <- data.frame(dataset[-grep('web' ,dataset$Reviews, perl=T),])
colnames(dataset) [1] <- c("Reviews")
corpus <- VCorpus(VectorSource(dataset$Reviews))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords(),"hpb"))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)
mat <- as.matrix(dtm)
v <- sort(colSums(mat),decreasing=TRUE)
data <- data.frame(word = names(v),freq=v)

set.seed(1056)
wordcloud(words = data$word, freq = data$freq, min.freq = 1,
          max.words=3000, random.order=FALSE, rot.per=0.55,
          colors=brewer.pal(10, "Dark2"))
dataset <- data.frame(lapply(dataset, as.character), stringsAsFactors=FALSE)
Sentiment <- get_nrc_sentiment(dataset$Reviews)
text <- cbind(dataset$Reviews,Sentiment)

TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  scale_fill_manual(values = c("#FF3333", "#78B67E", "#CCFF33", "#000000", "#FF9933", "#333300", "#FFCC33",
                               "#3333FF", "#FFFF00", "#FFFFFF"))+
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score")
