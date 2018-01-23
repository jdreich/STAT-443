
library(tseries)
library(zoo)

Bradford <- read.csv("bradforddata.csv")

BradfordTS <- ts(data = Bradford$Max.Temp, start = c(1930,1), frequency = 12)

plot(BradfordTS, xlab = "Date", ylab = "Max Temp (C)")

training <- window(BradfordTS, 1950, c(2014,12))
plot(training)

test <- window(BradfordTS, 2015, c(2017,12))
plot(test)

## Decomposing the training set
training_decomp <- decompose(training, type="additive")
plot(training_decomp)

training_stl <- stl(training, s.window="periodic")
plot(training_stl)

## Fitting a linear model to "trend"

dates <- seq(1, length(training), 1)
decomp_trend <- lm(training_decomp$trend ~ dates)
summary(decomp_trend)
plot(decomp_trend)

