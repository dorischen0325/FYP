library(MortalityGaps)
Sys.setenv(LANG = "en")

#Read the data from files
DataF <- read.csv(file="UKexF.csv")
DataM <- read.csv(file="UKexM.csv")

fitModel <- DoubleGap(DF = DataF,
                DM = DataM,
                age = 35, #change for different age
                country = "GBRTENW",
                years = 1922:2020)

preModel<- predict(fitModel, h = 30)
plot(preModel,
     main = "Forecast of Life Expectancy at Age 35")
summary(fitModel)

#UK at birth
fitModel <- DoubleGap(DF = DataF,
                      DM = DataM,
                      age = 0,
                      country = "GBRTENW",
                      years = 1947:2020)

preModel<- predict(fitModel, h = 30)
plot(preModel,
     main = "Forecast of Life Expectancy at Age 0")
summary(fitModel)

#Japan at birth
DataF <- read.csv(file="JapanexF.csv")
DataM <- read.csv(file="JapanexM.csv")
fitModel <- DoubleGap(DF = DataF,
                      DM = DataM,
                      age = 0,
                      country = "JPN",
                      years = 1947:2020)

preModel<- predict(fitModel, h = 30)
plot(preModel,
     main = "Forecast of Life Expectancy at Age 0")
summary(fitModel)
