library('Matrix')
library('wordcloud')
library('glmnet')

# Read train/test data and dictionary
colFeatures <- c("docID", "wordID", "wordCount")

trainFeatures <- read.table(file = '../data/train-features.txt', 
                            col.names = colFeatures)
testFeatures <- read.table(file = '../data/test-features.txt',
                           col.names = colFeatures)

YTrain <- read.table(file = '../data/train-labels.txt', col.names = "Y")
YTest <- read.table(file = '../data/test-labels.txt', col.names = "Y")

# Read dictionary
colDict <- c("wordID", "word")
dict <- read.table(file = '../data/dictionary.txt', col.names = colDict)

## Erzeugen der Featurematrizen

# Build (sparse) feature matrices
XTrain <- sparseMatrix(i = trainFeatures$docID, j = trainFeatures$wordID,
                       x = trainFeatures$wordCount)
XTest <- sparseMatrix(i = testFeatures$docID, j = testFeatures$wordID,
                      x = testFeatures$wordCount)


## Erzeuge Wahrscheinlichkeiten für den Naive-Bayes-Classifier


fnNBTrain <- function(X, Y) {
  # This function generates the probabilities for the Naive-Bayes-Classifier.
  # 
  # Args:
  #   X: Matrix of features
  #   Y: Vector of labels
  #   
  # Returns:
  #   A list containing the following elements:
  #     $phiSpam:   Probabilities for words occuring in SPAM message
  #     $phiHam:    Probabilities for words occuring in HAM message
  #     $gammaSpam: The relative frequency of SPAM messages
  #     $gammaHam:  The relative frequency of HAM messages
  
  nWords <- dim(X)[2]
  indSpam <- as.logical(Y)
  indHam <- !(indSpam)
  NBClass <- list()
  
  # Generate probabilities
  NBClass$phiSpam <- (colSums(X[indSpam, ]) + 1) / (sum(X[indSpam, ]) + nWords)
  NBClass$phiHam <- (colSums(X[indHam, ]) + 1) / (sum(X[indHam, ]) + nWords)
  
  # Get relative frequencies
  NBClass$gammaSpam <- sum(indSpam) / length(Y)
  NBClass$gammaHam <- sum(indHam) / length(Y)
  
  return(NBClass)
}

NBClass1 <- fnNBTrain(XTrain, YTrain[[1]])


fnNBPredict <- function(X, NBClass) {
  # This function predicts one of the two classes (SPAM or HAM) for some test
  # data X using a Naive-Bayes-Classifier trained by fnNBTrain.
  # 
  # Args:
  #   X:        Matrix of features
  #   NBClass:  A list returned by fnNBTrain which contains
  #               $phiSpam:   Probabilities for words occuring in SPAM message
  #               $phiHam:    Probabilities for words occuring in HAM message
  #               $gammaSpam: The relative frequency of SPAM messages
  #               $gammaHam:  The relative frequency of HAM messages
  # 
  # Returns:
  #   A vector with predictions for each example.
  
  # Predict labels (using logarithm)
  postSpam <- rowSums(t(t(X) * log(NBClass$phiSpam))) + log(NBClass$gammaSpam)
  postHam <- rowSums(t(t(X) * log(NBClass$phiHam))) + log(NBClass$gammaHam)
  pred <- (postSpam > postHam) * 1
  
  return(pred)
}

# Predict test labels
predTest1 <- fnNBPredict(XTest, NBClass1)

# Number of errors
nErr1 <- sum(YTest[[1]] != predTest1)


par(mfrow = c(1,2))

# Plot SPAM indicators
wordcloud(dict$word[rank(-NBClass1$phiSpam) <= 50], 
          NBClass1$phiSpam[rank(-NBClass1$phiSpam) <= 50],
          scale = c(2.5, 0.2))

# Plot HAM indicators
wordcloud(dict$word[rank(-NBClass1$phiHam) <= 50], 
          NBClass1$phiHam[rank(-NBClass1$phiHam) <= 50],
          scale = c(2.5, 0.2))



#Remove meaningless words
logRelProbs <- log(NBClass1$phiSpam/NBClass1$phiHam)
hist(logRelProbs, breaks = 50)

# Remove some words
indMeaningful <- !((logRelProbs >= (-1.5)) & (logRelProbs <= 1.5))

# Test classification after removal of meaningless words
NBClass2 <- fnNBTrain(XTrain[, indMeaningful], YTrain[[1]])
predTest2 <- fnNBPredict(XTest[, indMeaningful], NBClass2)

# New classification error
nErr2 <- sum(YTest[[1]] != predTest2)


# wordclouds
wordsMeaningful <- dict$word[indMeaningful]

# New word clouds
par(mfrow = c(1,2))

# Plot SPAM indicators
wordcloud(wordsMeaningful[rank(-NBClass2$phiSpam) <= 50], 
          NBClass2$phiSpam[rank(-NBClass2$phiSpam) <= 50], 
          scale = c(2.5, 0.2))

# Plot HAM indicators
wordcloud(wordsMeaningful[rank(-NBClass2$phiHam) <= 50], 
          NBClass2$phiHam[rank(-NBClass2$phiHam) <= 50],
          scale = c(2.5, 0.2))



# Cross Validation

set.seed(67)
# lump dataset
XData = rbind(XTrain, XTest)
YData = rbind(YTrain, YTest)

fnCrossValidation <- function(X, Y, nFolds, nPercentage) {
  # This functions takes the data and performs nFolds-fold Cross-Validation
  # and returns the errors for each run.
  # Additionally, nPercentage may be used to change the size of the training set to
  # evaluate the utility of more data
  
  nDataPoints <- dim(XData)[1]
  shuffledIndex <- sample(nDataPoints)
  
  # Create folds
  splitInd <- split(shuffledIndex, ceiling(seq_along(shuffledIndex)/round(nDataPoints/nFolds)))
  nErrorRate <- vector('double', nFolds)
  
  for (k in 1:nFolds) {
    # Reduce the size of the training data in accordance with nPercentage
    keepInd <- seq(1:round(nPercentage*(nDataPoints-length(splitInd[[k]]))))
    
    # Create final test/train sets for k-th cross validation
    XDataTrain <- X[-splitInd[[k]], ][keepInd, ]
    YDataTrain <- Y[-splitInd[[k]], ][keepInd]
    XDataTest <- X[splitInd[[k]], ]
    YDataTest <- Y[splitInd[[k]], ]
    
    NBClass3 <- fnNBTrain(XDataTrain, YDataTrain)
    # Predict labels
    prediction2 <- fnNBPredict(XDataTest, NBClass3)
    # Number of errors
    nErrorRate[k] <- sum(YDataTest != prediction2)/length(YDataTest)
  }
  
  return(nErrorRate)
}

folds = 10
CrossValErrors <- fnCrossValidation(XData, YData, folds, 1)
plot(CrossValErrors, ylim = c(0,1))


# Data Utility

Percentages <- seq(0.1,1, 0.05)
UtilityErrors <- NULL

for (i in 1:length(Percentages)) {
  UtilityErrors[i] <- mean(fnCrossValidation(XData, YData, 6, Percentages[i]))
}
plot(UtilityErrors, x=Percentages)
# Data Utility is shown by looking at the error produced under increasing training data
