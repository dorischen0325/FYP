library(MortalityGaps)

#Read the data from files
DataF <- read.csv(file="UKexF.csv")
DataM <- read.csv(file="UKexM.csv")

#Fit the DGM to the data for ages 35, 55 and 75
fitModel <- DoubleGap(DF = DataF,
                DM = DataM,
                age = 35, 
                country = "GBRTENW",
                years = 1922:2020)
preModel<- predict(fitModel, h = 30) #for the next 30 years
plot(preModel,
     main = "Forecast of Life Expectancy at Age 35")
summary(fitModel)

fitModel <- DoubleGap(DF = DataF,
                DM = DataM,
                age = 55, 
                country = "GBRTENW",
                years = 1922:2020)
preModel<- predict(fitModel, h = 30)
plot(preModel,
     main = "Forecast of Life Expectancy at Age 55")
summary(fitModel)

fitModel <- DoubleGap(DF = DataF,
                DM = DataM,
                age = 75, 
                country = "GBRTENW",
                years = 1922:2020)
preModel<- predict(fitModel, h = 30)
plot(preModel,
     main = "Forecast of Life Expectancy at Age 75")
summary(fitModel)

#UK life expectancy plot at birth
fitModel <- DoubleGap(DF = DataF,
                      DM = DataM,
                      age = 0,
                      country = "GBRTENW",
                      years = 1947:2020)
preModel<- predict(fitModel, h = 30)
plot(preModel,
     main = "Forecast of Life Expectancy at Age 0")
summary(fitModel)

#Japan life expectancy plot at birth
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
