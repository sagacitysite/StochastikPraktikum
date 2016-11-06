# Create subsets of faithful
faithLow  <- subset(faithful, eruptions <= 3)
faithHigh <- subset(faithful, eruptions > 3)

# Define some string variables for labeling plots 
stEruption <- "Eruption time (in min)"
stWaiting  <- "Waiting time (in min)"

##### Histograms #####

par(mfrow = c(1,1))

# Histogram with all eruption time
hist(faithful$eruptions,  main = "Eruption time", xlab = stEruption,
     breaks = seq(min(faithful$eruptions), max(faithful$eruptions),
                  length = 20))

par(mfrow = c(1,2))

# Histogram with waiting time, sparated by low/high eruption time
hist(faithLow$waiting,  xlab = stWaiting,
     main = "Waiting time (eruption time < 3 min)",
     xlim= c(min(faithful$waiting), max(faithful$waiting)))
hist(faithHigh$waiting,  xlab = stWaiting,
     main = "Waiting time (eruption time > 3 min)",
     xlim= c(min(faithful$waiting), max(faithful$waiting)))

##### Boxplot #####

par(mfrow = c(1,1))

# Boxplot with waiting time, one for low/hogh eruption time
boxplot(faithLow$waiting, faithHigh$waiting,
        names = c("eruption time < 3 min", "eruption time > 3 min"),
        main = "Waiting time")

##### QQ-Plot #####

par(mfrow = c(2,3))

# QQ-Plot and QQ-Line for eruption time
# One for (1) all, (2) low and (3) high eruptions time

qqnorm(faithful$eruptions, pch = 20, cex = 0.75, ylab = stEruption,
       main = "Normal QQ-Plot for eruption time (all eruption times)")
qqline(faithful$eruptions)

qqnorm(faithLow$eruptions, pch = 20, cex = 0.75, ylab = stEruption,
       main = "Normal QQ-Plot for eruption time (eruption time < 3 min)")
qqline(faithLow$eruptions)

qqnorm(faithHigh$eruptions, pch = 20, cex = 0.75, ylab = stEruption,
       main = "Normal QQ-Plot for eruption time (eruption time > 3 min)")
qqline(faithHigh$eruptions)

# QQ-Plot and QQ-Line for waiting time
# One for (1) all, (2) low and (3) high eruptions time

qqnorm(faithful$waiting, pch = 20, cex = 0.75, ylab = stWaiting,
       main = "Normal QQ-Plot for waiting time (all eruption times)")
qqline(faithful$waiting)

qqnorm(faithLow$waiting, pch = 20, cex = 0.75, ylab = stWaiting,
       main = "Normal QQ-Plot for waiting time (eruption time < 3 min)")
qqline(faithLow$waiting)

qqnorm(faithHigh$waiting, pch = 20, cex = 0.75, ylab = stWaiting,
       main = "Normal QQ-Plot for waiting time (eruption time > 3 min)")
qqline(faithHigh$waiting)

##### Scatter-Plot and Regression #####

par(mfrow = c(2,3))

# Define some variables for scaling
vecMinmaxEruption <- c(min(faithful$eruptions), max(faithful$eruptions))
vecMinmaxWaiting  <- c(min(faithful$waiting), max(faithful$waiting))

# 6 Scatter-Plots with regression lines and coefficients in all combinations

# x: eruption, y: waiting

plot(faithful$eruptions, faithful$waiting, main = "All eruption times",
     xlab = stEruption, ylab = stWaiting)
linRegression <- lm(waiting ~ eruptions, data = faithful)
abline(linRegression)
summary(linRegression)

plot(faithLow$eruptions, faithLow$waiting, main = "Eruption time < 3 min",
     xlab = stEruption, ylab = stWaiting,
     xlim = vecMinmaxEruption, ylim = vecMinmaxWaiting)
linRegression <- lm(waiting ~ eruptions, data = faithLow)
abline(linRegression)
summary(linRegression)

plot(faithHigh$eruptions, faithHigh$waiting, main = "Eruption time > 3 min",
     xlab = stEruption, ylab = stWaiting,
     xlim = vecMinmaxEruption, ylim = vecMinmaxWaiting)
linRegression <- lm(waiting ~ eruptions, data = faithHigh)
abline(linRegression)
summary(linRegression)

# x: waiting, y: eruption

plot(faithful$waiting, faithful$eruptions, main = "All eruption times",
     xlab = stWaiting, ylab = stEruption)
linRegression <- lm(eruptions ~ waiting, data = faithful)
abline(linRegression)
summary(linRegression)

plot(faithLow$waiting, faithLow$eruptions, main = "Eruption time < 3 min",
     xlab = stWaiting, ylab = stEruption,
     xlim = vecMinmaxWaiting, ylim = vecMinmaxEruption)
linRegression <- lm(eruptions ~ waiting, data = faithLow)
abline(linRegression)
summary(linRegression)

plot(faithHigh$waiting, faithHigh$eruptions, main = "Eruption time > 3 min",
     xlab = stWaiting, ylab = stEruption,
     xlim = vecMinmaxWaiting, ylim = vecMinmaxEruption)
linRegression <- lm(eruptions ~ waiting, data = faithHigh)
abline(linRegression)
summary(linRegression)

# -> In all three cases significant slope, seems like there is a connection

##### Mean, Median and Variance #####

# use mean as estimator for expectation value

# for all eruption times
print(paste("mean of waiting time:", round(mean(faithful$waiting), 3), "min"))
print(paste("standard error of estimator:",
            round(sqrt(var(faithful$waiting)/length(faithful$waiting)), 3), "min"))

# for low eruption times
print(paste("mean of waiting time:", round(mean(faithLow$waiting), 3), "min"))
print(paste("standard error of estimator:",
            round(sqrt(var(faithLow$waiting)/length(faithLow$waiting)), 3), "min"))

# for high eruption times
print(paste("mean of waiting time:", round(mean(faithHigh$waiting), 3), "min"))
print(paste("standard error of estimator:",
            round(sqrt(var(faithHigh$waiting)/length(faithHigh$waiting)), 3), "min"))

# -> std error is lower in seperate case
