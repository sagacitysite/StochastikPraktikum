# 1. ###########################################################################

# Monte-Carlo approximation of pi
fnMcPi <- function(n, lPlotPoints = 10000) {
  # Create random uniform values for x and y axis
  x1 <- runif(n, min = -1, max = 1)
  x2 <- runif(n, min = -1, max = 1)
  
  # Indicate which values are inside of the circle and which are not
  # Multiply by 4 to have 1 in sum again (area size of 2*2 = 4)
  indicator <- ((x1^2 + x2^2) <= 1) * 4
  
  # Check if points to plot are lower than sample size
  # If so, reduce sample size
  if(lPlotPoints > n) {
    lPlotPoints <- n
  }
  
  # Define reduced values for plotting
  s1 <- x1[1:lPlotPoints]
  s2 <- x2[1:lPlotPoints]
  
  # Indicate which values are inside of the cirelce and which are outside
  inside <- ((s1^2 + s2^2) <= 1)
  outside <- !inside
  
  # plot points inside and outside
  plot(s1[inside], s2[inside], pch = 20, cex = 0.5, col = "red",
       xlab = "x", ylab = "y",
       main = paste("Monte-Carlo simulation with sample size of ", n,
                    " results in pi value of ", mean(indicator), sep = ""))
  points(s1[outside], s2[outside], pch = 20, cex = 0.5, col = "blue")
  
  # plot circle points to mark the border
  points(sin(1:10000), cos(1:10000), pch = 20, cex = 0.2, col = "black")
  return(list(s1,s2))
}

X <- fnMcPi(1000000)

# Compute mean with each new sample
vecMeans <- cumsum(X[[1]]) / (1:length(X[[1]]))

# Plot means against number of samples
plot(1:length(X[[1]]), vecMeans,
     xlab = "Number of Monte-Carlo samples",
     ylab = "Monte-Carlo approximation",
     pch = 4,
     cex = 0.8)
abline(vecMeans[length(X[[1]])], 0, col = "red")

vecVariances <- NULL
for(i in 1:length(X[[1]])) {
  vecVariances[i] <- var(X[[1]][1:i])
}

cTop <- vecMeans[length(X[[1]])] - 2*sqrt(length(X[[1]])*vecVariances)
#cBottom <- 
lines(cTop, col = "red")


# 2. ###########################################################################

# Using pnorm
1- pnorm(20)

# Naive Monte-Carlo approximation
n <- 1000000
x <- rnorm(n)
indicator <- (x > 20) * 1
result <- mean(indicator)

# Monte-Carlo integration after substitution Y := 1/X
n <- 10000
u <- runif(n, min = 0, max = 1/20)
g <- (1/20) * (1/sqrt(2*pi)) * exp(-1/(2*(u^2))) * (1/(u^2))
result <- mean(g)

# Compute mean with each new sample
vecMeans <- cumsum(g) / (1:n)

# Plot means against number of samples
plot(1:n, vecMeans,
     xlab = "Number of Monte-Carlo samples",
     ylab = "Monte-Carlo approximation",
     pch = 4,
     cex = 0.8)
abline(vecMeans[n], 0, col = "red")

# Plot MSE against number of samples (inefficient implementation...)
MSE <- rep(0, n)
for (i in 1:n) {
  MSE[i] <- mean((g[1:i] - vecMeans[i])^2)
}

plot(1:n, MSE,
     xlab = "Number of Monte-Carlo samples",
     ylab = "Mean Squared Error",
     type = "l",
     cex = 0.8)

# TODO
# error estimation
# implement as a function

# 3. ###########################################################################

# Using R's integrate function
h1 <- function(x) {
  return((cos(50*x)+sin(20*x))^2)
}
resultInt <- integrate(h1, 0, 1)

# Using Monte-Carlo integration
n <- 1000000
u <- runif(n)
resultMC <- mean(h1(u))

# TODO
# error estimation and compare with error of integrate function

# Using R's integrate function
h2 <- function(x) {
  return(sin(1/x))
}
resultInt <- integrate(h2, 0, 1)

# Using Monte-Carlo integration
n <- 1000000
u <- runif(n)
resultMC <- mean(h2(u))

# TODO
# error estimation and compare with error of integrate function


# 4. ###########################################################################

n <- 1000
areas <- rep(0, n)

for (i in 1:n) {
  x <- runif(3)
  y <- runif(3)
  areas[i] <- abs(0.5 * det(matrix(c(x, y, c(1, 1, 1)), nrow = 3, ncol = 3)))
}

result <- mean(areas)

# Compute mean with each new sample
vecMeans <- cumsum(areas) / (1:n)

# Plot means against number of samples
plot(1:n, vecMeans,
     xlab = "Number of Monte-Carlo samples",
     ylab = "Monte-Carlo approximation",
     pch = 4,
     cex = 0.8)
abline(vecMeans[n], 0, col = "red")

# TODO
# error estimation
