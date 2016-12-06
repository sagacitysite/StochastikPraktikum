############### Kernel Density Estimation ###############

fnKernelDensityEst <- function(x, X, K, h) {
  # This function computes the kernel density estimates for a given sample X and
  # kernel K with bandwidth h at points x.
  # 
  # Args:
  #   x: Points for which the kernel density estimates are computed
  #   X: Data sample on which the kernel density estimation is fitted
  #   K: Kernel function to be used for smoothing
  #   h: Smoothing bandwidth
  #   
  # Returns:
  #   A vector of the kernel density estimates at points x
  
  # Compute kernel density estimation values using outer
  n <- length(X)
  mKernels <- K((1/h) * outer(X, x, FUN = "-"))
  return((1/(n*h)) * colSums(mKernels))
}

# Define different smoothing kernels
fnRectangularKernel <- function(x) {
  return(0.5 * (abs(x) <= 1))
}

fnGaussianKernel <- function(x) {
  return((1/sqrt(2*pi)) * exp((-0.5) * x^2))
}

fnEpanechnikovKernel <- function(x) {
  return(0.75 * (1 - x^2) * (abs(x) <= 1))
}

# Kernel density plot
fnKernelPlot <- function(x, X, K, vh = NULL, vm = NULL,
                         fnEstimation = fnKernelDensityEst,
                         title = NULL, trueDensity = NULL, ...) {
  # This function plots the kernel density estimates for different bandwidths.
  # If trueDensity is specified, the true density will be plotted as well.
  # 
  # Args:
  #   x:            Points for which the kernel density estimates are computed
  #   X:            Data sample on which the kernel density estimation is fitted
  #   K:            Kernel function to be used for smoothing
  #   vh:           Vector of smoothing bandwidths
  #   vm:           Vector of nearest neighbor positions
  #   fnEstimation: Type of estimation used (kernel density or knn)
  #   title:        Main title of the plot
  #   trueDensity:  True density to be plotted (e.g. dnorm)
  #   ...:          Further arguments passed to or from other methods
  #   
  # Returns:
  #   -
  
  if(!is.null(vh)) {
    vPar <- vh
    strPar <- "h"
  } else if(!is.null(vm)) {
    vPar <-  vm
    strPar <- "m"
  } else {
    stop("You need to set bandwiths vh OR neighbor ranks vm")
  }
  
  nPar <- length(vPar)
  
  # Create color palette for number of hyperparameters
  cols <- rainbow(nPar, alpha = 1)
  
  # Initialize plot with first kernel density
  plot(x, fnEstimation(x, X, K, vPar[1]), type = "l", col = cols[1], 
       main = title, xlab = "x", ylab = "Density", ...)
  
  # Add kernel density for each additional bandwidth
  if (nPar > 1) {
    for (i in 2:nPar) {
      lines(x, fnEstimation(x, X, K, vPar[i]), col = cols[i])
    }
  }
  
  # Create legend
  legend <- sapply(vPar, function(par) {paste(strPar, " = ", round(par, 4))})
  
  # Plot true density if specified
  if (!is.null(trueDensity)) {
    lines(x, trueDensity(x), col = "black")
    legend <- c("True density", legend)
    cols <- c("black", cols)
  }
  
  # Plot legend
  legend("topright", legend = legend, col = cols, lty = 1, cex = 0.75)
}

############### Kernel density estimation for standard normal ###############

# Generate sample
set.seed(42)
n <- 50
X <- rnorm(n)

# Define points to estimate kernel density on
x <- seq(-4, 4, 0.01)

# Set different bandwidths
h <- c(0.1, bw.ucv(X), 1, 4)

# Rectangular kernel density estimation
fnKernelPlot(x, X, fnRectangularKernel, h, 
             title = "Rectangular Kernel Density Estimation",
             trueDensity = dnorm, ylim = c(0, 0.6))

# Gaussian kernel density estimation
fnKernelPlot(x, X, fnGaussianKernel, h, 
             title = "Gaussian Kernel Density Estimation",
             trueDensity = dnorm, ylim = c(0, 0.6))

# Epanechnikov kernel density estimation
fnKernelPlot(x, X, fnEpanechnikovKernel, h, 
             title = "Epanechnikov Kernel Density Estimation",
             trueDensity = dnorm, ylim = c(0, 0.6))

############### Kernel density estimation for the exponential ###############

# Generate sample
set.seed(42)
n <- 50
X <- rexp(n)

# Define points to estimate kernel density on
x <- seq(0, 6, 0.01)

# Set different bandwidths
h <- c(0.1, 0.5, 1)

# Rectangular kernel density estimation
fnKernelPlot(x, X, fnRectangularKernel, h, 
             title = "Rectangular Kernel Density Estimation",
             trueDensity = dexp, ylim = c(0, 2))

# Gaussian kernel density estimation
fnKernelPlot(x, X, fnGaussianKernel, h, 
             title = "Gaussian Kernel Density Estimation",
             trueDensity = dexp, ylim = c(0, 2))

# Epanechnikov kernel density estimation
fnKernelPlot(x, X, fnEpanechnikovKernel, h, 
             title = "Epanechnikov Kernel Density Estimation",
             trueDensity = dexp, ylim = c(0, 2))

############### Kernel density estimation for Cauchy ###############

# Generate sample
set.seed(42)
n <- 50
X <- rt(n, df = 1)

# Define points to estimate kernel density on
x <- seq(-5, 5, 0.01)

# Set different bandwidths
h <- c(0.5, 1, 5)

# Define Cauchy density function
dcauchy <- function(x) {y <- dt(x, df = 1)}

# Rectangular kernel density estimation
fnKernelPlot(x, X, fnRectangularKernel, h, 
             title = "Rectangular Kernel Density Estimation",
             trueDensity = dcauchy, ylim = c(0, 0.4))

# Gaussian kernel density estimation
fnKernelPlot(x, X, fnGaussianKernel, h, 
             title = "Gaussian Kernel Density Estimation",
             trueDensity = dcauchy, ylim = c(0, 0.4))

# Epanechnikov kernel density estimation
fnKernelPlot(x, X, fnEpanechnikovKernel, h, 
             title = "Epanechnikov Kernel Density Estimation",
             trueDensity = dcauchy, ylim = c(0, 0.4))

############### Faithful kernel density estimation ###############

# Faithful kernel density estimation
X <- faithful$eruptions

# Define points to estimate kernel density on
x <- seq(min(X), max(X), 0.01)

# Set different bandwidths
h <- c(0.05, bw.ucv(X), 0.5, 1)

# Gaussian kernel density estimation
title <- "Gaussian Kernel Density Estimation for faithful eruption data"
fnKernelPlot(x, X, fnGaussianKernel, h, title = title)

############### Kernel density estimation cross validation ###############

fnJ <- function(X, K, h) {
  # This function computes the value of the unbiased cross validation criteria.
  # The minimum of this function in h gives an estimated “optimal” (in the
  # sense of mean integrated squared error) bandwidth.
  # 
  # Args:
  #   X: Data sample on which the kernel density estimation is fitted
  #   K: Kernel function to be used for smoothing
  #   h: Smoothing bandwidth
  #   
  # Returns:
  #   Function value
  
  n <- length(X)
  
  # Compute integral of squared kernel density estimator
  integrand <- function(x) {return(fnKernelDensityEst(x, X, K, h)^2)}
  SKDEInt <- integrate(integrand, -Inf, Inf)$value
  
  # Compute G
  mKernels <- K((1/h) * outer(X, X, FUN = "-"))
  diag(mKernels) <- 0
  G <- (1/(n*(n-1)*h)) * sum(mKernels)
  
  # Return function value of J
  return(SKDEInt - 2*G)
}

fnUCV <- function(X, K, hmin, hmax, tol = 0.1 * hmin) {
  # This function returns the minimum of the unbiased cross validation criteria
  # in h which is “optimal” in the sense of mean integrated squared error.
  # We use R's optimize function to find the minimum.
  # 
  # Args:
  #   X:    Data sample on which the kernel density estimation is fitted
  #   K:    Kernel function to be used for smoothing
  #   hmin: Lower bound for optimal h
  #   hmax: Upper bound for optimal h
  #   tol:  The desired accuracy
  #   
  # Returns:
  #   Optimal bandwidth h*
  
  # Find and return the minimum using optimize
  fnTarget <- function(h) {return(fnJ(X, K, h))}
  hOpt <- optimize(fnTarget, c(hmin, hmax), tol = tol)
  return(hOpt$minimum)
}

# CV with own implementation
hmin <- 0.01
hmax <- 10
fnUCV(faithful$eruptions, fnGaussianKernel, hmin, hmax)

# CV with R implementation
bw.ucv(faithful$eruptions)

############### Kernel M-Nearest Neighbors density estimation ###############

fnMNearestNeighbors <- function(x, X, K, m) {
  # This function computes the kernel density estimates for a given sample X and
  # kernel K with variable bandwidths h for every point, which are determined by
  # the m-nearest neighbor approach
  # 
  # Args:
  #   x: Points for which the kernel density estimates are computed
  #   X: Data sample on which the kernel density estimation is fitted
  #   K: Kernel function to be used for smoothing
  #   m: Neighbor position
  #   
  # Returns:
  #   A vector of the nearest neighbor kernel density estimates at points x
  
  n <- length(X)
  
  # Compute bandwidth for every point x
  h <- apply(abs(outer(X, x, "-")), 2, sort)[m,]
  
  # Compute kernel density estimation values using outer
  mKernels <- K((1/t(matrix(rep(h, n), ncol = n))) * outer(X, x, FUN = "-"))
  return((1/(n*h)) * colSums(mKernels))
}

############### M-Nearest Neighbors test ###############

# Generate sample
set.seed(42)
n <- 50
X <- rnorm(n)

# Define points to estimate kernel density on
x <- seq(-4, 4, 0.01)

# Define vector of neighbor positions m
m <- c(5,10,25,40,50)

# Nearest Neighbor Rectangular kernel density estimation
fnKernelPlot(x, X, fnRectangularKernel, vm = m,
             fnEstimation = fnMNearestNeighbors,
             title = "Nearest Neighbor Rectangular Kernel Density Estimation",
             trueDensity = dnorm, ylim = c(0, 0.6))

# Nearest Neighbor Gaussian kernel density estimation
fnKernelPlot(x, X, fnGaussianKernel, vm = m, 
             fnEstimation = fnMNearestNeighbors,
             title = "Nearest Neighbor Gaussian Kernel Density Estimation",
             trueDensity = dnorm, ylim = c(0, 0.6))

# Nearest Neighbor Epanechnikov kernel density estimation
fnKernelPlot(x, X, fnEpanechnikovKernel, vm = m, 
             fnEstimation = fnMNearestNeighbors, 
             title = "Nearest Neighbor Epanechnikov Kernel Density Estimation",
             trueDensity = dnorm, ylim = c(0, 0.6))
