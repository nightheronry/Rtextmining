library(XML)
library(RCurl)


data <- list()

for( i in 1118:1118){
  tmp <- paste(i, '.html', sep='')
  url <- paste('www.ptt.cc/bbs/StupidClown/index', tmp, sep='')
  html <- htmlParse(getURL(url, ssl.verifypeer = FALSE))
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  data <- rbind(data, paste('www.ptt.cc', url.list, sep=''))
}

data <- unlist(data)



getdoc <- function(line){
  start <- regexpr('www', line)[1]
  end <- regexpr('html', line)[1]

  if(start != -1 & end != -1){
    url <- substr(line, start, end+3)
    html <- htmlParse(getURL(url, ssl.verifypeer = FALSE), encoding='UTF-8')
    doc <- xpathSApply(html, "//div[@id='main-content']", xmlValue)
    name <- strsplit(url, '/')[[1]][4]
    write(doc, gsub('html', 'txt', "document/"+name))
  }      
}
sapply(data, getdoc)
install.packages(tm)
install.packages(tmcn)
install.packages(Rwordseg)
library(tm)
library(tmcn)
library(Rwordseg)
d.corpus <- Corpus(DirSource(getwd()+"document"), list(language = NA))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
    gsub("[A-Za-z0-9]", "", word)
})
words <- readLines("http://wubi.sogou.com/dict/download_txt.php?id=9182")
words <- toTrad(words)
insertWords(words)
d.corpus <- tm_map(d.corpus[1:100], segmentCN, nature = TRUE)
d.corpus <- tm_map(d.corpus, function(sentence) {
    noun <- lapply(sentence, function(w) {
        w[names(w) == "n"]
    })
    unlist(noun)
})
d.corpus <- Corpus(VectorSource(d.corpus))
myStopWords <- c(stopwordsCN(), "編輯", "時間", "發信", "標題", "實業", "作者")
d.corpus <- tm_map(d.corpus, removeWords, myStopWords)

head(myStopWords, 20)
tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
inspect(tdm[1:10, 1:2])

library(wordcloud)
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word, d$freq, min.freq = 10,
random.order = F, ordered.colors = F,
colors = rainbow(length(row.names(m1))))