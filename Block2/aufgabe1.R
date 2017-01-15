#################### Aufgabe 1 a) ####################

# Enter data
x <- c(82,69,73,43,58,56,76,65)
y <- c(63,42,74,37,51,43,80,62)
testType <- c('two-sided', 'one-sided')
pValueT <- c(0,0); pValueWilcox <- c(0,0)

# t-test
pValueT[1] <- t.test(x, y, alternative = "two.sided", mu = 0,
                     conf.level = 0.95, paired = TRUE)$p.value
pValueT[2] <- t.test(x, y, alternative = "greater", mu = 0,
                     conf.level = 0.95, paired = TRUE)$p.value

# Wilcoxon signed rank test
pValueWilcox[1] <- wilcox.test(x, y, alternative = "two.sided",
                               mu = 0, conf.level = 0.95, 
                               paired = TRUE, conf.int = TRUE)$p.value
pValueWilcox[2] <- wilcox.test(x, y, alternative = "greater",
                               mu = 0, conf.level = 0.95, 
                               paired = TRUE, conf.int = TRUE)$p.value

data.frame(testType, pValueT, pValueWilcox)

#################### Aufgabe 1 b) ####################

fnTestPowerMC <- function(fnError, n = 30, alpha = 0.05, nSim = 10^4, ...) {
  # This function estimates the probability of rejecting the null hypothesis of a
  # t-test and a Wilcoxon signed rank test using Monte Carlo simulations of iid
  # random variables X_i = \theta + \epsilon_i.
  # 
  # Args:
  #   fnError:  Function which generates random samples from a symmetric error 
  #             distribution \epsilon
  #   n:        Number of random samples used in tests
  #   alpha:    Significance level used in tests
  #   nSim:     Number of MC simulations of size n
  #   ...:      Further arguments to be passed to fnError
  #   
  # Returns:
  #   A list containing the following elements:
  #     $TProb:       MC estimation of rejection probability for the t-test
  #     $WilcoxProb:  MC estimation of rejection probability for the Wilcoxon
  #                   signed rank test
  
  # Perform MC simulation
  matX <- matrix(fnError(n*nSim, ...), ncol = n)
  
  # Define sub-functions which only return p-values from the two tests
  fnPvalT <- function(x) {
    return(t.test(x)$p.value)
  }
  fnPvalWilcox <- function(x) {
    return(wilcox.test(x)$p.value)
  }
  
  # Perform nSim number of tests with sample size n for each of the two tests
  vecPvalT <- apply(matX, 1, fnPvalT)
  vecPvalWilcox <- apply(matX, 1, fnPvalWilcox)
  
  # Compute and return estimations of rejection probabilities
  result <- list()
  result$TProb <- mean(vecPvalT < alpha)
  result$WilcoxProb <- mean(vecPvalWilcox < alpha)
  return(result)
}

# Set seed
set.seed(432)

# Normal errors
normalMean = c(0, 0.5, 1)
normalSD = c(1, 1, 1)
normalPowerT <- list()
normalPowerW <- list()

for (i in 1:3) {
  res <- fnTestPowerMC(fnError = rnorm, nSim = 10^4,
                       mean = normalMean[i], sd=normalSD[i])
  normalPowerT[i] <- res[1]
  normalPowerW[i] <- res[2]
}

# Uniform errors
uniMin <- c(-1, -0.5, 0)
uniMax <- c(1, 1.5, 2)
uniPowerT <- list()
uniPowerW <- list()

for (i in 1:3) {
  res <- fnTestPowerMC(fnError = runif, nSim = 10^4,
                       min = uniMin[i], max = uniMax[i])
  uniPowerT[i] <- res[1]
  uniPowerW[i] <- res[2]
}

# Cauchy errors (t-distribution with df = 1)
cauchyPowerT <- list()
cauchyPowerW <- list()
cauchyLoc = c(0, 0.5, 1)
cauchyScale = c(1,1,1)
for (i in 1:3) {
  res <- fnTestPowerMC(fnError = rcauchy, nSim = 10^4,
                       location = cauchyLoc[i], scale=cauchyScale[i])
  cauchyPowerT[i] <- res[1]
  cauchyPowerW[i] <- res[2]
}

# Print results
dfNormal <- data.frame(normalMean, normalSD,
                       unlist(normalPowerT), unlist(normalPowerW))
colnames(dfNormal) <- c("mean", "sd", "power T-test", "power Wilcox-test")
print(dfNormal)

dfCauchy <- data.frame(cauchyLoc, cauchyScale,
                       unlist(cauchyPowerT), unlist(cauchyPowerW))
colnames(dfCauchy) <- c("location", "scale", "power T-test", "power Wilcox-test")
print(dfCauchy)

dfUni <- data.frame((uniMax+uniMin)/2, (uniMax-uniMin)/2,
                    unlist(uniPowerT), unlist(uniPowerW))
colnames(dfUni) <- c("mean", "+- interval", "power T-test", "power Wilcox-test")
print(dfUni)

