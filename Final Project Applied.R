# Final Project
library(SentimentAnalysis)
library(SnowballC)
#interface to the C 'libstemmer' library that implements Porter's word stemming algorithm for collapsing words to a common root to aid comparison of vocabulary

setwd("~/Desktop/CRIM 602/finalProject")
lastStatementData <- read.csv("texasLastStatement.csv", header=TRUE)
head(lastStatementData)

#========================Valence Dictionary====================================================#

#read in word valence text file
wordSentimentData <- read.delim("AFINN-111.txt", header=FALSE)
head(wordSentimentData)

# Create valence dictionary
wordsValence <- list()
#for loop over all rows in file
for(i.valence in 1:nrow(wordSentimentData)) {
  key <- wordSentimentData[i.valence,]$V1
  value <- wordSentimentData[i.valence,]$V2
  # add key-value pair to wordsValence:
  wordsValence[key] <- value
}

#========================Valence Attribution - Functions=======================================#

removePunct <- function(statement) gsub("[[:punct:] ]+|[\xfc\xbe\x99\xa6\x98\xbc]|[\xab]", " ", statement)

#function gets valence for single word
#sets words that aren't in the list = 0
getWordValence <- function(word) {
  v <- unlist(wordsValence[word])
  if(is.null(v)) {
    v <- 0
  }
  return(as.numeric(v))
}

#total valence function for entire statement
#strsplit returns list within a list
#unlist removes outer list
determineStatementValenceApplied <- function(statement) {
  statement <- tolower(statement)
  splitStatementsList <- unlist(strsplit(statement, " ", useBytes = TRUE)) # If TRUE the matching is done byte-by-byte rather than character-by-character
  wordsValenceIndiv <- lapply(splitStatementsList, getWordValence) #applies to everything in the list
  valenceSum <- Reduce("+",wordsValenceIndiv) #reduces the individual valences to a single value by adding them up
  return(valenceSum)
}

#------------------------apply functions and add columns to determine valence with each method--------------------
#lapply() performs operations on list objects and returns a list object of same length
#sapply() does the same but returns a vector

#get rid of punctuation in statements
lastStatementNoPunct <- lapply(lastStatementData$LastStatement, removePunct)

#create new valenceDIY column
statementValenceListDIY <- lapply(lastStatementNoPunct, determineStatementValenceApplied)
lastStatementData$valenceDIY <- unlist(statementValenceListDIY)

#get sentimentAnalysis package
# statementValencePackageResult <- lapply(lastStatementNoPunct, analyzeSentiment) #takes a long time
# save(statementValencePackageResult,file="sentimentAnalysisPackage.RData",compress=TRUE)
statementValencePackageResult <- get(load("sentimentAnalysisPackage.RData"))

#create new valencePackage column
getSentimentGI <- function(row) {statementValencePackageResult[[row]][2]} #2 is sentimentGI
#a <- do.call(statementValencePackageResult, rbind)
#a$SentimentGI
lastStatementData$valencePackage <- sapply(1:nrow(lastStatementData), getSentimentGI)

#========================Valence Analysis======================================================#

#statement of highest valence - DIY
max(lastStatementData$valenceDIY)
maxValenceRow <- which.max(lastStatementData$valenceDIY)
lastStatementData$LastStatement[maxValenceRow]

#statement of highest valence - Package
max(as.numeric(lastStatementData$valencePackage))
maxValenceRowP <- which.max(as.numeric(lastStatementData$valencePackage))
lastStatementData$LastStatement[maxValenceRowP]

#statement of lowest valence - DIY
min(lastStatementData$valenceDIY)
minValenceRow <- which.min(lastStatementData$valenceDIY)
lastStatementData$LastStatement[minValenceRow]

#statement of lowest valence - Package
min(as.numeric(lastStatementData$valencePackage))
minValenceRowP <- which.min(as.numeric(lastStatementData$valencePackage))
lastStatementData$LastStatement[minValenceRowP]

#------------------------Race and Valence------------------------------------------------------
# total executions by race
raceTable <- aggregate(Execution~Race, data=lastStatementData, length)
#could just do aggregate instead of the stuff afterward:
valence <- aggregate(valenceDIY~Race, data=lastStatementData, mean)
#lastStatement$valence <- as.numeric(lastStatementData$valence)

#overall valence of black individuals
blackRows <- which(lastStatementData$Race=="Black")
#DIY
totalValenceBlackDIY <- sum(lastStatementData$valenceDIY[blackRows]) #sum
avgValenceBlackDIY <- totalValenceBlackDIY/raceTable$Execution[1] #average

#Package
totalValenceBlackPackage <- sum(as.numeric(lastStatementData$valencePackage[blackRows])) #sum
avgValenceBlackPackage <- totalValenceBlackPackage/raceTable$Execution[1] #average

#overall valence of white individuals
whiteRows <- which(lastStatementData$Race=="White")
#DIY
totalValenceWhiteDIY <- sum(lastStatementData$valenceDIY[whiteRows]) #sum
avgValenceWhiteDIY <- totalValenceWhiteDIY/raceTable$Execution[4] #average

#Package
totalValenceWhitePackage <- sum(as.numeric(lastStatementData$valencePackage[whiteRows])) #sum
avgValenceWhitePackage <- totalValenceWhitePackage/raceTable$Execution[4] #average

#overall valence of hispanic individuals
hispRows <- which(lastStatementData$Race=="Hispanic")

#DIY
totalValenceHispDIY <- sum(lastStatementData$valenceDIY[hispRows]) #sum
avgValenceHispDIY <- totalValenceHispDIY/raceTable$Execution[2] #average

#Package
totalValenceHispPackage <- sum(as.numeric(lastStatementData$valencePackage[hispRows])) #sum
avgValenceHispPackage <- totalValenceHispPackage/raceTable$Execution[2] #average

#Tables
rbind(avgValenceBlackDIY, avgValenceWhiteDIY, avgValenceHispDIY)

rbind(avgValenceBlackPackage, avgValenceWhitePackage, avgValenceHispPackage)

#------------------------Age and Valence------------------------------------------------------

# youngest, oldest individual at time of execution, and when they were received
min(lastStatementData$Age)
youngestExecuted <- which.min(lastStatementData$Age)
lastStatementData$AgeWhenReceived[youngestExecuted]
lastStatementData[youngestExecuted,]

max(lastStatementData$Age)
oldestExecuted <- which.max(lastStatementData$Age)
lastStatementData$AgeWhenReceived[oldestExecuted]
lastStatementData[oldestExecuted,]

# total executions by age - large range
aggregate(Execution~Age, data=lastStatementData, length)

# make some age bins #cut makes new categorical variable for you
lastStatementData$ageGroup <- 
  cut(lastStatementData$Age,
      breaks=c(24,30,36,42,48,54,60,67))

ageGroupTable <- aggregate(Execution~ageGroup, data=lastStatementData, length)
aggregate(cbind(valenceDIY, as.numeric(valencePackage))~ageGroup, data=lastStatementData, mean)

ageRangeDIY <- list()
ageRangePackage <- list()
#for loop over all rows in file
for(i.age in 1:7) {
  ageRangeBegin <- as.numeric(18+6*(i.age))
  ageRangeEnd <- ageRangeBegin+6
  if(i.age==7) {ageRangeEnd <- ageRangeEnd+1}
  age <- which(lastStatementData$ageGroup==paste0("(", ageRangeBegin, ",", ageRangeEnd, "]"))
  #paste0 links it together; separator function
  totalValenceDIY <- sum(lastStatementData$valenceDIY[age])
  totalValencePackage <- sum(unlist(lastStatementData$valencePackage[age]))
  avgValenceDIY <- totalValenceDIY/length(age)
  avgValencePackage <- totalValencePackage/length(age)
  ageRangeDIY[i.age] <- avgValenceDIY
  ageRangePackage[i.age] <- avgValencePackage
}
ageGroupTable$averageValenceDIY <- unlist(ageRangeDIY)
ageGroupTable$avgValencePackage <- unlist(ageRangePackage)

ageGroupTable




