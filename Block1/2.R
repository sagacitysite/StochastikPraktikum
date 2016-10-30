# 1. ###########################################################################

# Monte-Carlo approximation of pi
n <- 1000000
x1 <- runif(n, min = -1, max = 1)
x2 <- runif(n, min = -1, max = 1)
indicator <- ((x1^2 + x2^2) <= 1) * 4
result <- mean(indicator)


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


# 4. ###########################################################################

n <- 1000000
areas <- rep(0, n)

for (i in 1:n) {
  x <- runif(3)
  y <- runif(3)
  areas[i] <- abs(0.5 * det(matrix(c(x, y, c(1, 1, 1)), nrow = 3, ncol = 3)))
}

result <- mean(areas)