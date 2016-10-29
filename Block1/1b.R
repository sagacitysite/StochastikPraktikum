fnInfiniteMonkey <- function(strTarget) {
  # This function is a simulation of the infinite monkey theorem. It generates a
  # random sequence of letters until a given target string appears.
  # 
  # Args:
  #   strTarget: The target string which should be matched
  #   
  # Returns:
  #   The number of generated letters until the target string appeared
  
  # Split target string to vector of chars
  vecCharTarget <- strsplit(strTarget, "")[[1]]
  # Get the number of letters in target string
  nTarget <- length(vecCharTarget)
  
  # Set counting variable (at least nTarget letters needed)
  nCounter <- nTarget
  
  # Switch on the monkey (i.e. sample the first nTarget letters)
  vecLetterSeqTail <- sample(letters, nTarget, replace = TRUE)
  
  # Let the monkey type until target string was written
  while(!identical(vecCharTarget, vecLetterSeqTail)) {
    
    # Let the monkey hit another key (sample next letter) and store in 
    # vecLetterSeqTail (first in, first out)
    if (nTarget == 1) {
      vecLetterSeqTail <- sample(letters, 1)
    } else {
      vecLetterSeqTail <- c(vecLetterSeqTail[2:nTarget], 
                            sample(letters, 1, replace = TRUE))
    }

    # Count
    nCounter <- nCounter + 1
  }
  
  # Return the length of the generated letter sequence
  return(nCounter)
}


# Repeat the infinite monkey experiment
nSamples <- 1000

# Set a target string
strTarget <- "hi"

# Prepare vector for resulting lengths of letter sequences
vecLetterSeqLen <- integer(nSamples)

# RELEASE THE MONKEYS! (... one after another)
for (i in 1:nSamples) {
  vecLetterSeqLen[i] <- fnInfiniteMonkey(strTarget)
}

# Compute mean with each new sample
vecLetterSeqLenMeans <- cumsum(vecLetterSeqLen) / (1:nSamples)

# Plot means
plot(1:nSamples, vecLetterSeqLenMeans,
     xlab = "Number of samples",
     ylab = "Mean length of letter sequence up to sample",
     pch = 4,
     cex = 0.8)
abline(vecLetterSeqLenMeans[nSamples], 0)