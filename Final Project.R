# Final Project
library(SentimentAnalysis)
library(SnowballC)
#interface to the C 'libstemmer' library that implements Porter's word stemming algorithm for collapsing words to a common root to aid comparison of vocabulary
library(doParallel)

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

#function splits statement into individual words, add up values (input statement, output value)
getWordValence <- function(word) {
  v <- unlist(wordsValence[word])
  if(is.null(v)) {
    v <- 0
  }
  return(as.numeric(v))
}

#loop method to get valence function
# determineStatementValenceLoop <- function(statement) {
#   statement <- tolower(statement)
#   splitStatements <- unlist(strsplit(statement, " ", useBytes = TRUE)) #string split
#   valenceValue <- 0
#   
#   for(i.word in 1:length(splitStatements)) {
#     word <- splitStatements[i.word]
#     v <- getWordValence(word)
#     valenceValue <- valenceValue + as.numeric(v)
#   }
#   return(valenceValue)
# }

#applied method to get valence function
determineStatementValenceApplied <- function(statement) {
  statement <- tolower(statement)
  splitStatements <- unlist(strsplit(statement, " ", useBytes = TRUE))
  wordsValenceIndiv <- lapply(splitStatements, getWordValence) #applies to everything in the list
  Reduce("+",wordsValenceIndiv)
}

#------------------------apply and add columns to determine valence with each method--------------------
#lapply() performs operations on list objects and returns a list object of same length
#sapply() does the same but returns a vector

#get rid of punctuation in statements
lastStatementNoPunct <- lapply(lastStatementData$LastStatement, removePunct)

#create new valenceDIY column
statementValenceListDIY <- lapply(lastStatementNoPunct, determineStatementValenceApplied) #or can use determineloop function
#statementValenceListDIY <- lapply(lastStatementNoPunct, determineStatementValenceLoop)
lastStatementData$valenceDIY <- unlist(statementValenceListDIY)

#get sntimentAnalysis package
# statementValencePackageResult <- lapply(lastStatementNoPunct, analyzeSentiment) #takes a long time
# save(statementValencePackageResult,file="sentimentAnalysisPackage.RData",compress=TRUE)
statementValencePackageResult <- get(load("sentimentAnalysisPackage.RData"))

#create new valencePackage column
getSentimentGI <- function(row) {statementValencePackageResult[[row]][2]} #2 is sentimentGI
lastStatementData$valencePackage <- sapply(1:nrow(lastStatementData), getSentimentGI)

#==============================================================================================
#==============================================================================================

#======================Alternative Method: Loops===============================================#

cl <- makeCluster(8)
registerDoParallel(cl)

# foreach loop iterating through all the statements with determineValence function
statementValenceListDIY <-
  foreach(i.statement=1:nrow(lastStatementData)) %dopar%
  {
    statement <- lastStatementData$LastStatement[i.statement]
    statementNoPunct <- gsub("[[:punct:] ]+|[\xfc\xbe\x99\xa6\x98\xbc]", " ", statement)
    statementValenceDIY <- determineValence(statementNoPunct)
    return(statementValenceDIY)
  }
lastStatementData$valenceDIY <- unlist(statementValenceListDIY)
save(statementValenceListDIY,file="statementValenceListDIY.RDATA",compress=TRUE)

#-----------------------------------------------------------------------

# for loop iterating through all the statements with SentimentAnalysis package, in empty dataframe
statementValencePackageResult <- data.frame()
  for(i.statement in 1:nrow(lastStatementData))
  {
    statement <- lastStatementData$LastStatement[i.statement]
    statementNoPunct <- gsub("[[:punct:] ]+|[\xfc\xbe\x99\xa6\x98\xbc]|[\xab]", " ", statement)
    statementValencePackage <- analyzeSentiment(statementNoPunct)
    statementValencePackageResult <- rbind(statementValencePackageResult, statementValencePackage)
  }
save(statementValencePackageResult,file="sentimentAnalysisPackage.RData",compress=TRUE)
#sentimentAnalysisPackage <- get(load("sentimentAnalysisPackage.RData"))

head(sentimentAnalysisPackage)
lastStatementData$valencePackage <- sentimentAnalysisPackage$SentimentGI
#============================================================================
#============================================================================

#========================Valence Analysis======================================================#

#statement of highest valence - DIY
max(lastStatementData$valenceDIY)
maxValenceRow <- which.max(lastStatementData$valenceDIY)
lastStatementData$LastStatement[maxValenceRow]

#statement of lowest valence - DIY
min(lastStatementData$valenceDIY)
minValenceRow <- which.min(lastStatementData$valenceDIY)
lastStatementData$LastStatement[minValenceRow]

#statement of highest valence - Package
max(as.numeric(lastStatementData$valencePackage))
maxValenceRowP <- which.max(as.numeric(lastStatementData$valencePackage))
lastStatementData$LastStatement[maxValenceRowP]

#statement of lowest valence - Package
min(as.numeric(lastStatementData$valencePackage))
minValenceRowP <- which.min(as.numeric(lastStatementData$valencePackage))
lastStatementData$LastStatement[minValenceRowP]

#------------------------Race and Valence------------------------------------------------------
# total executions by race
raceTable <- aggregate(Execution~Race, data=lastStatementData, length)

#overall valence of black individuals
black <- which(lastStatementData$Race=="Black")
#DIY
totalValenceBlackDIY <- sum(lastStatementData[black, 21]) #sum
avgValenceBlackDIY <-
  totalValenceBlackDIY/raceTable$Execution[1] #average

#Package
totalValenceBlackPackage <- sum(as.numeric(lastStatementData[black, 22])) #sum
avgValenceBlackPackage <-
  totalValenceBlackPackage/raceTable$Execution[1] #average

#overall valence of white individuals
white <- which(lastStatementData$Race=="White")
#DIY
totalValenceWhiteDIY <- sum(lastStatementData[white, 21]) #sum
avgValenceWhiteDIY <-
  totalValenceWhiteDIY/raceTable$Execution[4] #average

#Package
totalValenceWhitePackage <- sum(as.numeric(lastStatementData[white, 22])) #sum
avgValenceWhitePackage <-
  totalValenceWhitePackage/raceTable$Execution[4] #average

#overall valence of hispanic individuals
hisp <- which(lastStatementData$Race=="Hispanic")

#DIY
totalValenceHispDIY <- sum(lastStatementData[hisp, 21]) #sum
avgValenceHispDIY <-
  totalValenceHispDIY/raceTable$Execution[2] #average

#Package
totalValenceHispPackage <- sum(as.numeric(lastStatementData[hisp, 22])) #sum
avgValenceHispPackage <-
  totalValenceHispPackage/raceTable$Execution[2] #average

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

# total executions by age
aggregate(Execution~Age, data=lastStatementData, length)

# make some age categories #cut makes new categorical variable for you
lastStatementData$ageGroup <- 
cut(lastStatementData$Age,
    breaks=c(24,30,36,42,48,54,60,67))

aggregate(Execution~ageGroup, data=lastStatementData, length)



age24_30 <- which(lastStatementData$ageGroup=="(24,30]")
#DIY
totalValence24_30DIY <- sum(lastStatementData[age24_30, 21]) #sum
avgValence24_30DIY <-
  totalValence24_30DIY/length(age24_30) #average

#Package
totalValence24_30Package <- sum(as.numeric(lastStatementData[age24_30, 22])) #sum
avgValence24_30Package <-
  totalValence24_30Package/length(age24_30)


age30_36 <- which(lastStatementData$ageGroup=="(30,36]")
#DIY
totalValence30_36DIY <- sum(lastStatementData[age30_36, 21]) #sum
avgValence30_36DIY <-
  totalValence30_36DIY/length(age30_36) #average

#Package
totalValence30_36Package <- sum(as.numeric(lastStatementData[age30_36, 22])) #sum
avgValence30_36Package <-
  totalValence30_36Package/length(age30_36)


age36_42 <- which(lastStatementData$ageGroup=="(36,42]")
#DIY
totalValence36_42DIY <- sum(lastStatementData[age36_42, 21]) #sum
avgValence36_42DIY <-
  totalValence36_42DIY/length(age36_42) #average

#Package
totalValence36_42Package <- sum(as.numeric(lastStatementData[age36_42, 22])) #sum
avgValence36_42Package <-
  totalValence36_42Package/length(age36_42)


age36_42 <- which(lastStatementData$ageGroup=="(42,48]")
#DIY
totalValence36_42DIY <- sum(lastStatementData[age36_42, 21]) #sum
avgValence36_42DIY <-
  totalValence36_42DIY/length(age36_42) #average

#Package
totalValence36_42Package <- sum(as.numeric(lastStatementData[age36_42, 22])) #sum
avgValence36_42Package <-
  totalValence36_42Package/length(age36_42)




cl <- makeCluster(8)
registerDoParallel(cl)
ageRangeValenceAvg <-
  foreach(i.valence=1:nrow(lastStatementData)) %dopar%
  {
    age <- which(lastStatementData$ageGroup==i.valence)
    #DIY
    totalValenceDIY <- sum(lastStatementData[age, 21]) #sum
    avgValenceDIY <-
      totalValenceDIY/length(age)
    return(avgValenceDIY)
  }


ageRange <- list()
#for loop over all rows in file
for(i.valence in 1:nrow(lastStatementData)) {
  age <- which(lastStatementData$ageGroup==i.valence)
  #DIY
  totalValenceDIY <- sum(lastStatementData[age, 21]) #sum
  avgValenceDIY <-
    totalValenceDIY/length(age)
  print(avgValenceDIY)
}



