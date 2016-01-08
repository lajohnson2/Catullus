# Catdict41.R
# Sentiment analysis using custom dictionary for Catullus 41

setwd("~/OneDrive/OD_Documents/Hacking the Humanist/Final Paper/corpus")

poem41<-scan("41.txt", what="char", sep="\n") #split on lines
words.v<-tolower(paste(poem41, collapse=" "))
words.l<-strsplit(words.v,"\\W") 
words.v<-unlist(words.l)
words.v<-words.v[which(words.v != "")]


setwd("~/OneDrive/OD_Documents/Hacking the Humanist/Final Paper/")
dict<-read.delim("catdict.txt", header=FALSE, stringsAsFactors=FALSE) # reads a tab-separated file into adataframes
names(dict) <- c('word', 'weight')

cat41.df<-dict[which(dict$word %in% words.v),]
sum(cat41.df$weight)
