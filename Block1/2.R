fnErrorEstimation <- function(X, fnRandom, m = 100,
                    fnValueCalculation = function(x) {x}) {
  # Calculate values
  X = ifelse(class(X) == "list", X, list(X))
  x <- do.call(fnValueCalculation, X)
  
  # Save length of sample in variable
  n <- length(x)
  
  ################ Error estimation using central limit theorem ################
  
  # Compute mean with each new sample
  vecMeans <- cumsum(x) / (1:n)
  
  # Compute confidence band
  vecConf <- list(top = NULL, bottom = NULL)
  for(i in 1:n) {
    vecConf$top[i] <- vecMeans[n] + 1.96*sqrt(var(x[1:i])/i)
    vecConf$bottom[i] <- vecMeans[n] - 1.96*sqrt(var(x[1:i])/i)
  }
  
  # Plot means against number of samples
  plot(1:n, vecMeans,
       xlab = "Number of Monte-Carlo samples",
       ylab = "Monte-Carlo approximation",
       ylim = c(min(na.omit(vecConf$bottom), vecMeans),
                max(na.omit(vecConf$top), vecMeans)),
       pch = 4, cex = 0.8)
  
  # Plot overall mean
  abline(vecMeans[n], 0, col = "red")
  
  # Plot confidence band
  lines(vecConf$top, col = "blue")
  lines(vecConf$bottom, col = "blue")
  
  ##################### Error estimation using simulation #####################
  
  # Generate n x m matrix with random values
  mSampleMatrix <- NULL
  for(i in 1:m) {
    X <- fnRandom(n)
    X = ifelse(class(X) == "list", X, list(X))
    mSampleMatrix <- cbind(mSampleMatrix, do.call(fnValueCalculation, X))
  }
  
  # Calculate confidence band
  mEst <- apply(mSampleMatrix, 2, cumsum)/(1:n)
  mConf <- apply(mEst, 1, quantile, c(.025, .975))
  
  # Plot upper and lower confidence bound
  lines(mConf[1,], col = "green")
  lines(mConf[2,], col = "green")
  
  # Add legend for all lines
  legend("topright", c("Approximation of pi", "Error estimation (CLT)",
                       "Error estimation (Simulation)"), 
         lty = 1, col=c("red", "blue", "green"), bty="n", cex=.75)
}

# 1. ###########################################################################

# Monte Carlo approximation of pi

fnPiRandom <- function(n) {
  # Create random uniform values for x and y axis
  return(list(x1 = runif(n, min = -1, max = 1),
              x2 = runif(n, min = -1, max = 1)))
}

fnPiIndicator <- function(x1, x2) {
  # Indicate which values are inside of the cirelce and which are outside
  return(((x1^2 + x2^2) <= 1)*4)
}

fnPiPlotCircle <- function(x1, x2) {
  inside <- as.logical(fnPiIndicator(x1, x2))
  outside <- !inside
  
  # Plot points inside and outside
  plot(x1[inside], x2[inside], pch = 20, cex = 0.5, col = "red",
       xlab = "x", ylab = "y", xlim = c(-1,1), ylim = c(-1,1))
  points(x1[outside], x2[outside], pch = 20, cex = 0.5, col = "blue")
  
  # Plot circle points to mark the border
  points(sin(1:10000), cos(1:10000), pch = 20, cex = 0.2, col = "black")
}

# Split plot panel
par(mfrow = c(1,2))

# Create random values
X <- fnPiRandom(2000)

# Plot random values and circle
fnPiPlotCircle(X$x1, X$x2)

# Run error estimation
fnErrorEstimation(X, fnPiRandom, fnValueCalculation = fnPiIndicator)

# Set title for plots
title(paste("Monte-Carlo simulation with sample size of ", length(X$x1),
            " results in pi value of ", mean(fnPiIndicator(X$x1, X$x2)), sep = ""),
      outer = TRUE,  line = -2)

# 2. ###########################################################################

# Set plot panel to one plot
par(mfrow = c(1,1))

# Using pnorm
print(1 - pnorm(20))

# Naive Monte-Carlo approximation
n <- 1000000
x <- rnorm(n)
indicator <- (x > 20) * 1
print(mean(indicator))

# Monte-Carlo integration after substitution Y := 1/X

fnProbRandom <- function(n) {
  # Create random uniform values
  return(runif(n, min = 0, max = 1/20))
}

fnProbG <- function(u) {
  return((1/20) * (1/sqrt(2*pi)) * exp(-1/(2*(u^2))) * (1/(u^2)))
}

# Generate random values
u <- fnProbRandom(10000)

# Calculate values with function g
g <- fnProbG(u)

# Run error estimation
fnErrorEstimation(u, fnProbRandom, fnValueCalculation = fnProbG)

# Set title for plot
title(paste("Monte-Carlo simulation with sample size of ", length(u),
            " results in P value of ", mean(g), sep = ""),
      outer = TRUE,  line = -2)

# 3. ###########################################################################

# Set plot panel to two plots
par(mfrow = c(1,2))

fnIntRandom <- function(n) {
  # Generate random uniform values
  return(runif(n))
}

fnIntH1 <- function(x) {
  return((cos(50*x)+sin(20*x))^2)
}

fnIntH2 <- function(x) {
  return(sin(1/x))
}

# Using R's integrate function
print(integrate(fnIntH1, 0, 1))
print(integrate(fnIntH2, 0, 1))

# Using Monte-Carlo integration
u <- fnIntRandom(10000)
h1 <- fnIntH1(u)
h2 <- fnIntH2(u)

# Run error estimation
fnErrorEstimation(u, fnIntRandom, fnValueCalculation = fnIntH1)
title(paste("h1: ", length(u), " samples, result: ",
            round(mean(h1), 3), sep = ""))
fnErrorEstimation(u, fnIntRandom, fnValueCalculation = fnIntH2)
title(paste("h2: ", length(u), "samples, result ",
            round(mean(h2), 3), sep = ""))

## TODO: Compare errors of integrate function with monte carlo

# 4. ###########################################################################

# Set plot panel to one plot
par(mfrow = c(1,1))

fnAreaRandom <- function(n) {
  areas <- NULL
  for (i in 1:n) {
    x <- runif(3)
    y <- runif(3)
    areas[i] <- abs(0.5 * det(matrix(c(x, y, c(1, 1, 1)), nrow = 3, ncol = 3)))
  }
  return(areas)
}

# Generate random triangle area values
A <- fnAreaRandom(1000)

# Run error estimation
fnErrorEstimation(A, fnAreaRandom)
title(paste("Monte-Carlo simulation with sample size of ", length(A),
            " results in area of ", round(mean(A), 4), sep = ""))
