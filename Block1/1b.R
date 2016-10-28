library(zoo)

monkeyFun <- function(string, nSampleSize) {
  a <- proc.time()
  
  # split given string to vector of chars
  vCharChain <- strsplit(string, "")[[1]]
  # store char chain size
  nCharChainSize <- length(vCharChain)
  
  # set helping variables for counting loop iterations
  # and vector to store overlapping parts if window
  nCounter <- 0
  overlap <- NULL
  
  # take time in the beginning
  t0 <- proc.time()
  
  # while loop, which iterates as long as the string was randonmly created
  while(TRUE) {
    # counts the number of steps necessary for finding
    nCounter <- nCounter + 1
    
    # save a random sample of letters and add overlap from iteration before
    randomLetters <- c(overlap, sample(letters, nSampleSize, replace = TRUE))
    
    # calculate overlap to add it to chain in next iteration
    overlap <- tail(randomLetters, nCharChainSize)
    
    # search for matches
    matches <- rollapply(randomLetters, nCharChainSize, identical, vCharChain)
    
    if(sum(matches) >= 1) {
      # take time in the end
      t1 <- proc.time()
      
      # take position of first hit
      firstHit <- which(matches)[1]
      
      # calculate number of necessary random letters
      nRandomsNecessary <- nCounter*nSampleSize - (nSampleSize-firstHit)
      
      # break loop and return number of necessary random letters
      return(list(randomLetters = nRandomsNecessary, timeElapsed = (t1-t0)[[3]]))
    }
  }
}


evaluateMonkey <- function(string, nSampleSize, nSteps) {
  vRandomLetters <- NULL
  for(i in 1:nSteps) {
    result <- monkeyFun(string, nSampleSize)
    vRandomLetters <- c(vRandomLetters, result$randomLetters)
  }
  hist(vRandomLetters,
       xlab = "Number of random values necessary",
       main = paste("Histogram of necessary random values to have '", string, "'", sep = ""))
  abline(v = mean(vRandomLetters), col = "red")
}

evaluateMonkey("abc", nSampleSize = 10000, nSteps = 100)

