#Load libraries
library("StMoMo")
library("demography")

#Login to HMD
UKdata <- hmd.mx(country="GBR_NP", username = "your username", password = "your password") 

#Ages and gender for fitting
UKfemale <- StMoMoData(UKdata, series = "female")
UKmale <- StMoMoData(UKdata, series = "male")
ages.fit <- 0:100
years.fit <- 1922:2020

#Fit the Model
LC <- lc()
LC <- lc(link = "log", const = "sum")
LCfitF <- fit(object = LC, # model
              data = UKfemale, # a StMoModata object
              ages.fit = ages.fit, # ages in fit
              years.fit = years.fit # years in fit
)

LCfitM <- fit(object = LC, # model
              data = UKmale, # a StMoModata object
              ages.fit = ages.fit, # ages in fit
              years.fit = years.fit # years in fit
)

#Forecasting
#LC Female
LCforF <- forecast(LCfitF, h = 30)
plot(LCforF, only.kt = TRUE)
LCforF$kt.f$mean
LCforF$rates
LCforF$rates[1:5, 1:5]
LCfor_qxtF <- 1 - exp(- LCforF$rates)
LCfor_qxtF[, "2041"] 

#Loop for creating a data frame
dfLCF <- data.frame(x = ages.fit,
                    qx = as.numeric(LCfor_qxtF[, "2022"]))
i <- 2
while (i <= 30) {
  dfLCF <- cbind(dfLCF, qx = as.numeric(LCfor_qxtF[, i]))
  i <- i + 1
}

#LC Male
LCforM <- forecast(LCfitM, h = 30)
plot(LCforM, only.kt = TRUE)
LCforF$kt.f$mean
LCforM$rates
LCforM$rates[1:5, 1:5]
LCfor_qxtM <- 1 - exp(- LCforM$rates)
dfLCM <- data.frame(x = ages.fit,
                    qx = as.numeric(LCfor_qxtM[, "2022"]))

#Loop for creating a data frame
i <- 2
while (i <= 30) {
  dfLCM <- cbind(dfLCM, qx = as.numeric(LCfor_qxtM[, i]))
  i <- i + 1
}

#Plot the mortality rates' graphs for ages 35, 55 and 75 for females
par(mfrow = c(1, 3))
plot(LCfitF$years, (LCfitF$Dxt / LCfitF$Ext)["35", ],
     xlim = range(LCfitF$years, LCforF$years),
     ylim = range((LCfitF$Dxt / LCfitF$Ext)["35", ], LCforF$rates["35", ]),
     type = "p", xlab = "year", ylab = "rate",
     main = "Mortality Rates at Age 35")
lines(LCfitF$years, fitted(LCfitF, type = "rates")["35", ])
lines(LCforF$years, LCforF$rates["35", ], lty = 2, col="blue")
legend("topright",legend = c("Fitted", "Forecast"), 
       lty = 1:2, col = c("black", "blue"))

plot(LCfitF$years, (LCfitF$Dxt / LCfitF$Ext)["55", ],
     xlim = range(LCfitF$years, LCforF$years),
     ylim = range((LCfitF$Dxt / LCfitF$Ext)["55", ], LCforF$rates["55", ]),
     type = "p", xlab = "year", ylab = "rate",
     main = "Mortality Rates at Age 55")
lines(LCfitF$years, fitted(LCfitF, type = "rates")["55", ])
lines(LCforF$years, LCforF$rates["55", ], lty = 2, col="blue")
legend("topright",legend = c("Fitted", "Forecast"), 
       lty = 1:2, col = c("black", "blue"))

plot(LCfitF$years, (LCfitF$Dxt / LCfitF$Ext)["75", ],
     xlim = range(LCfitF$years, LCforF$years),
     ylim = range((LCfitF$Dxt / LCfitF$Ext)["75", ], LCforF$rates["75", ]),
     type = "p", xlab = "year", ylab = "rate",
     main = "Mortality Rates at Age 75")
lines(LCfitF$years, fitted(LCfitF, type = "rates")["75", ])
lines(LCforF$years, LCforF$rates["75", ], lty = 2, col="blue")
legend("topright",legend = c("Fitted", "Forecast"), 
       lty = 1:2, col = c("black", "blue"))

#Plot the mortality rates' graphs for ages 35, 55 and 75 for males
par(mfrow = c(1, 3))
plot(LCfitM$years, (LCfitM$Dxt / LCfitM$Ext)["35", ],
     xlim = range(LCfitM$years, LCforM$years),
     ylim = range((LCfitM$Dxt / LCfitM$Ext)["35", ], LCforM$rates["35", ]),
     type = "p", xlab = "year", ylab = "rate",
     main = "Mortality Rates at Age 35")
lines(LCfitM$years, fitted(LCfitM, type = "rates")["35", ])
lines(LCforM$years, LCforM$rates["35", ], lty = 2, col="blue")
legend("topright",legend = c("Fitted", "Forecast"), 
       lty = 1:2, col = c("black", "blue"))

plot(LCfitM$years, (LCfitM$Dxt / LCfitM$Ext)["55", ],
     xlim = range(LCfitM$years, LCforM$years),
     ylim = range((LCfitM$Dxt / LCfitM$Ext)["55", ], LCforM$rates["55", ]),
     type = "p", xlab = "year", ylab = "rate",
     main = "Mortality Rates at Age 55")
lines(LCfitM$years, fitted(LCfitM, type = "rates")["55", ])
lines(LCforM$years, LCforM$rates["55", ], lty = 2, col="blue")
legend("topright",legend = c("Fitted", "Forecast"), 
       lty = 1:2, col = c("black", "blue"))

plot(LCfitM$years, (LCfitM$Dxt / LCfitM$Ext)["75", ],
     xlim = range(LCfitM$years, LCforM$years),
     ylim = range((LCfitM$Dxt / LCfitM$Ext)["75", ], LCforM$rates["75", ]),
     type = "p", xlab = "year", ylab = "rate",
     main = "Mortality Rates at Age 75")
lines(LCfitM$years, fitted(LCfitM, type = "rates")["75", ])
lines(LCforM$years, LCforM$rates["75", ], lty = 2, col="blue")
legend("topright",legend = c("Fitted", "Forecast"), 
       lty = 1:2, col = c("black", "blue"))
