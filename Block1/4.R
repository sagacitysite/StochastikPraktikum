fnCheckNormality <- function(vecSample) {
  # This function helps to check a sample of generated random numbers for 
  # normality. It helps to compare different random number generators by using 
  # histograms, qq-norm-plots, and Kolmogorov-Smirnov-tests.
  # 
  # Args:
  #   vecSample: A sample of generated random numbers
  #   
  # Returns:
  #   Histogram, qq-norm-plot, and the output from a Kolmogorov-Smirnov-test
  
  # Plot histogram
  hist(vecSample,
       breaks = 100,
       probability = TRUE,
       xlim = c(-4, 4),
       xlab = "Samples from random number generator",
       main = "Histogram of random samples")
  
  # Add density of standard normal to the plot
  vecNorm <- seq(-4, 4, by = 0.1)
  vecNormDensity <- dnorm(vecNorm)
  lines(vecNorm, vecNormDensity, col = "blue")
  legend("topright", legend = "Density of standard normal", 
         lty = 1, col = "blue")
  
  # QQ-norm-plot
  qqnorm(vecSample, ylim = c(-4, 4))
  qqline(vecSample)
  
  # Perform Kolmogorov-Smirnov test
  print(ks.test(vecSample, "pnorm"))
  
  # Plus: Perform Shapiro-Wilk normality test
  print(shapiro.test(vecSample))
}

# Test with rnorm-function
set.seed(42)
fnCheckNormality(rnorm(5000))


# 1. ###########################################################################

# Generate random numbers from the standard normal using the Box-Muller-Method
set.seed(42)
nSamples <- 5000

vecU1 <- runif(nSamples)
vecU2 <- runif(nSamples)
vecZ1 <- sqrt((-2)*log(vecU1)) * cos(2*pi*vecU2)
vecZ2 <- sqrt((-2)*log(vecU1)) * sin(2*pi*vecU2)

# Test for normality
fnCheckNormality(vecZ1)
fnCheckNormality(vecZ2)


# 2. ###########################################################################

# Generate random numbers from the standard normal as sum of i.i.d. 
# U([-0.5, 0.5])-distributed random variables

set.seed(42)
nSamples <- 5000
m <- 12  # number of i.i.d. random variables

matUnif <- matrix(runif(nSamples*m, -0.5, 0.5), ncol = m)
vecZ <- apply(matUnif, 1, sum)

# Test for normality
fnCheckNormality(vecZ)


# 3. ###########################################################################

# Generate random numbers from the standard normal using the 
# accept-reject-algorithm with Laplace proposal density

# Define Laplace density
g <- function(x, alpha = 1) {
  return((alpha/2) * exp((-alpha)*  abs(x)))
}

# Let's have a look at the Laplace proposal g and the standard normal density
M <- sqrt(2/pi) * exp(0.5)
x <- seq(-4, 4, 0.01)
plot(x, M * g(x), type = "l", col = "blue", 
     ylab = "Function values",
     main = "Laplace proposal and standard normal density")
lines(x, dnorm(x))
legend("topright", legend = c("Laplace proposal", "Standard normal density"), 
         lty = c(1, 1), col = c("blue", "black"))

# Set seed and number of samples
set.seed(42)
nSamples <- 5000

# Sample from U([0,1])
vecU1 <- runif(nSamples)

# Sample from the Laplace distibution using the inversion-method
alpha <- 1
vecU2 <- runif(nSamples)
vecLaplace <- rep(0, nSamples)
vecLaplace[vecU2 < 0.5] <- (1/alpha) * log(2*vecU2[vecU2 < 0.5])
vecLaplace[vecU2 >= 0.5] <- (-1/alpha) * log(2*(1-vecU2[vecU2 >= 0.5]))

# Accept or reject?
vecNormal <- vecLaplace[vecU1 <= dnorm(vecLaplace)/(M * g(vecLaplace))]

# Acceptance ratio
fAcceptRatio <- length(vecNormal) / length(vecLaplace)

# Test for normality
fnCheckNormality(vecNormal)