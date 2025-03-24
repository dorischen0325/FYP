library(MortalityGaps)
Sys.setenv(LANG = "en")

#Read the data from files
DataF <- read.csv(file="UKexF.csv")
DataM <- read.csv(file="UKexM.csv")

fitModel <- DoubleGap(DF = DataF,
                DM = DataM,
                age = 75,
                country = "GBRTENW",
                years = 1922:2020)

preModel<- predict(fitModel, h = 30)
plot(preModel,
     main = "Forecast of Life Expectancy at Age 35")
summary(fitModel)

# Collection of life expectancies for female populations
exF <- MortalityGaps.data$exF
exF
# Life expectancy for male populations
exM <- MortalityGaps.data$exM
M0 <- DoubleGap(DF = exF,
                DM = exM,
                age = 65,
                country = "GBRTENW",
                years = 1950:2013)
P0 <- predict(M0, h = 37)
plot(P0)
