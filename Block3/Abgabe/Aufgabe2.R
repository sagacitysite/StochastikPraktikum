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



# Build (sparse) feature matrices
XTrain <- sparseMatrix(i = trainFeatures$docID, j = trainFeatures$wordID,
                       x = trainFeatures$wordCount)
XTest <- sparseMatrix(i = testFeatures$docID, j = testFeatures$wordID,
                      x = testFeatures$wordCount)


fnRPredict <- function(X, Y, XTest, ...) {
  # This function predicts the labels for the test set using Ridge/Lasso regression
  # 
  # Args:
  #   X: Matrix of training features
  #   Y: Vector of training labels
  #   XTest: Vector of test features
  #   
  # Returns:
  #   Vector of predicted labels from test features
  
  # Fit spam data via lasso regularization and use binomial function
  fitGlm <- glmnet(X, Y, family = "binomial", ...)
  
  # Do cross validation to find optimal lambda
  cv <- cv.glmnet(X, Y)
  
  # Predict test data
  YPred <- predict(fitGlm, XTest, type="response", s=cv$lambda.min)
  
  # Classify test data
  YPred[YPred >= 0.5] <- 1
  YPred[YPred < 0.5] <- 0
  
  return(YPred)
}

## Lasso Regression
YPredLR <- fnRPredict(XTrain, YTrain[[1]], XTest, alpha = 1)

# Number of errors
nErrLR <- sum(YPredLR != YTest)

## Ridge Regression
YPredRR <- fnRPredict(XTrain, YTrain[[1]], XTest, alpha = 0)

# Number of errors
nErrRR <- sum(YPredRR != YTest)