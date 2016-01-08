# R-tools
# Built in Brad Pasanek's Hacking for Humanists Class (UVa, Fall 2015)
# Most of these functinos are based on code written by Matt Jockers or Mark Algee-Hewitt

#This function takes a single text and returns the text
#without punctuation or numbers or tabs or multiple spaces
text.clean<-function(text){
  #first, convert text to lower
  text.lower<-tolower(text)
  #remove tabs and tab-like spacings
  text.untabbed<-gsub("\\t+|\\s+", " ", text.lower)
  #pad newlines with spaces
  text.newline<-gsub("$"," ",text.untabbed)
  #then, split the text into characters
  text.letters<-unlist(strsplit(text.newline, ""))
  #then find which characters aren't letters
  bad.index<-which(!text.letters %in% letters)
  #then, find which characters are hyphens
  hyphen.index<-which(text.letters=="-")
  #then, convert hyphens into spaces
  text.letters[hyphen.index]<-" "
  #then, find the location of the spaces
  space.index<-which(text.letters==" ")
  #find which things in bad.index are spaces
  spaces.in.bad<-which(bad.index %in% space.index)
  #remove the spaces from bad.index
  bad.index<-bad.index[-spaces.in.bad]
  #remove all of the non letters from the character vector
  text.letters<-text.letters[-bad.index]
  #collapse the character vector back into the text
  text.all<-paste(text.letters, collapse="")
  #remove any remaining white space (leading/trailing)
  text.final<-gsub("\\s+", " ", text.all)
  text.final<-gsub("^\\s+|\\s+$", "", text.final)
  #finally, return the cleaned text
  return(text.final)
}

# This function extracts files from a directory (dir), turns it into a bag of words, 
# and puts each file's words into a list
getcorpus<-function(dir,type=".txt"){
  curr.folder<-getwd()
  setwd(dir)
  corpus<-list()
  files<-list.files(pattern=type)
  for(i in 1:length(files)){
    text<-scan(files[i],what="char",sep="\n")
    text<-paste(text,collapse=" ")
    lowertext<-tolower(text)
    text.words<-unlist(strsplit(lowertext,"\\W"))
    text.words<-text.words[which(text.words!="")]
    corpus[[files[i]]]<-text.words
  }
  setwd(curr.folder)
  return(corpus)
}

# # This function finds words in a file (text.words) that are collocated with a target keyword 
# # (target.kw) within the window specified (LRspan). Note LRspan is a +/-.
# colloc<-function(target.kw,text.words,LRspan){
#   text<-tolower(text.words)
#   index<-which(text==tolower(target.kw))
#   collocates<-NULL
#   if(length(index)==0){
#     collocates<-c("There are none")
#     return(collocates)
#   }
#   else {
#     for(i in 1:length(index)){
#       (if (i != length(index))){
#         L<-index[i]-LRspan
#         if(index[i]-LRspan<1){L<-1}
#         R<-index[i]+LRspan
#         #Lcontext<-text[L:(index[i]-1)]
#         # Rcontext<-text[(index[i]+1):R]
#         contexts<-text[L:R]
#         collocates<-c(collocates,contexts)
#       }
#     }
#     return(table(collocates))
#   }
# }

# This function goes through a single file (text.words) and matches
# target keywords (target.kw). It returns the keyword in its context,
# that is, its surrounding words. The size of the context is 2*LRspan.
kwic<-function(target.kw,text.words,LRspan){
  text<-tolower(text.words)
  target.index<-grep(target.kw,tolower(text))
  if(length(target.index)==0){
    stop("No match")
  } else { 
    contexts<-NULL
    for(i in 1:length(target.index)){
      if(target.index[i]-LRspan<1){
        L<-1
      } else {
        L<-target.index[i]-LRspan
      }
      if(target.index[i]+LRspan>length(text)){
        R<-length(text)
      } else {
        R<-target.index[i]+LRspan
      }
      kwincontext<-text.words[L:R]
      kwincontext<-paste(kwincontext,collapse=" ")
      contexts<-c(contexts,kwincontext)
    }
  } 
  return(contexts)
}

# This is a second KWIC function for corpus objects. It pulls keywords in context out of
# files in a directory.
kwicorpus<-function(target.kw,corpus,LRspan){
  kwic.corpus.l<-list()
  for(i in 1:length(corpus)){
    target.index<-grep(target.kw,corpus[[i]])
    if(length(target.index)!=0){
      contexts<-NULL
      for(j in 1:length(target.index)){
        if(target.index[j]-LRspan<1){
          L<-1
        } else {
          L<-target.index[j]-LRspan
        }
        if(target.index[j]+LRspan>length(corpus[[i]])){
          R<-length(corpus[[i]])
        } else {
          R<-target.index[j]+LRspan
        }
        kwincontext<-corpus[[i]][L:R]
        kwincontext<-paste(kwincontext,collapse=" ")
        contexts<-c(contexts,kwincontext)
      }
      kwic.corpus.l[[names(corpus[i])]]<-contexts
    }
  }
  return(kwic.corpus.l)
}
