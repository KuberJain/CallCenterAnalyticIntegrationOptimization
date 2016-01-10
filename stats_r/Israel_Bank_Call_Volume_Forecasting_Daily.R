library(forecast)
library(ISOweek)
library(dtw)
library(xts)
library(NbClust)
library(tsDyn)
library(caret)
library(neuralnet)
library(wavethresh)

# Data Load
setClass('yyyy-mm-dd hh:mm:ss')
setAs("character","yyyy-mm-dd hh:mm:ss", function(from) as.POSIXct(from, format="%Y-%m-%d %H:%M:%S"))

Israel_DataD <- read.table("/Users/bzheng/Documents/workspace/ContactCenterNext/Israel_Bank_Data/agg_D.csv", 
                        header = FALSE, sep=",",colClasses = c("Date", "numeric"));

colnames(Israel_DataD) <- c("Time","CallVolume");

Israel_DataD_Full_xts <- xts(Israel_DataD[,2], as.Date(0:(length(Israel_DataD[,2])-1), origin="1999-01-01"),frequency=7)


# Data
## Training Data 1999 01-01 [1] - 1999 06-30 [181]
## Testing Data 1999 07-01 [182] - 1999 07-31 [212]
## Training Data 1999 01-01 [1] - 1999 04-30 [120]
## Testing Data 1999 05-01 [121] - 1999 05-07 [127]
## Training Data 1999 01-01 [1] - 1999 05-31 [151]
## Testing Data 1999 06-01 [152] - 1999 06-07 [158]
## Training Data 1999 01-01 [1] - 1999 01-31 [32]
## Testing Data 1999 02-01 [33] - 1999 02-28 [158]
Forecast_D <- 28
Start_Train_D <- 1
End_Train_D <- 151
Start_Test_D <- 152
End_Test_D <- Start_Test_D+Forecast_D-1

Israel_DataD_TrainData <-Israel_DataD[Start_Train_D:End_Train_D,]
Israel_DataD_TestData <- Israel_DataD[Start_Test_D:End_Test_D,]

Israel_DataD_TS <- xts(Israel_DataD[Start_Train_D:End_Test_D, 2], 
                      as.Date(0:(End_Test_D-1), origin="1999-01-01"),frequency=7)
Israel_DataD_TrainData_TS <- xts(Israel_DataD[Start_Train_D:End_Train_D, 2], 
                      as.Date(0:(End_Train_D-1),origin="1999-01-01"),frequency=7)
Israel_DataD_TestData_TS <- xts(Israel_DataD[Start_Test_D:End_Test_D, 2],
                      as.Date(0:(Forecast_D-1),origin="1999-05-01"),frequency=7)


# Data Visualization
plot(Israel_DataD$CallVolume, type="l", 
     ylim=range(Israel_DataD$CallVolume), xlab="Day",
     ylab="Call Volume")

plot.xts(Israel_DataD_Full_xts, ylab="Call Volume", main = "Israel Bank Daily Call Volumes")


# Forecasting
plot.xts(Israel_DataD_TS, ylab="Call Volume", main = "Israel Bank Daily Call Volumes")

## Multiple Linear Regression
Israel_DataD_fit<-lm(CallVolume ~ Time, data=Israel_DataD_TrainData)
summary(Israel_DataD_fit)


## ARIMA
Israel_DataD_fModel_ARIMA<- Arima(Israel_DataD_TrainData_TS, order=c(2,2,1),seasonal=list(order=c(2,0,0),period=7))
#Israel_DataD_fModel_ARIMA <-  auto.arima(Israel_DataD_TrainData_TS)
Israel_DataD_forecast_ARIMA <- forecast(Israel_DataD_fModel_ARIMA, h=Forecast_D)
accuracy(Israel_DataD_forecast_ARIMA, Israel_DataD_TestData_TS)
acf(Israel_DataD_TrainData_TS)
pacf(Israel_DataD_TrainData_TS)
plot.ts(Israel_DataD_forecast_ARIMA$mean)
plot.ts(Israel_DataD_TestData_TS)
Israel_DataD_fModel_ARIMA_performance <- cbind(xts(Israel_DataD_forecast_ARIMA$mean, 
                                as.Date(0:(Forecast_D-1),origin="1999-05-01"),
                                frequency = 7), Israel_DataD_TestData_TS)
colnames(Israel_DataD_fModel_ARIMA_performance)<-c('Forecasting', 'Acutal')
plot(x = as.zoo(Israel_DataD_fModel_ARIMA_performance), ylab = "Call Volumes", xlab="Time",
     main = "Prediction and Forecasting Comparisons",
     col = c("#A6A8AB","#199DD9"), screen=1)
legend(x = "topright", legend = c("Forecasting","Actual"), 
       lty = 1, lwd = 3, col = c("#A6A8AB","#199DD9"))


## Simple Expontial Smoothing 
Israel_DataD_fModel_SES <- HoltWinters(ts(Israel_DataD_TrainData_TS,frequency=7), beta=TRUE, gamma=TRUE)
Israel_DataD_forecast_SES <- forecast.HoltWinters(Israel_DataD_fModel_SES, h = Forecast_D)
accuracy(Israel_DataD_forecast_SES, Israel_DataD_TestData_TS)

## Holt's Expontial Smoothing 
Israel_DataD_fModel_HES <- HoltWinters(ts(Israel_DataD_TrainData_TS,frequency=7), gamma=FALSE)
Israel_DataD_forecast_HES <- forecast.HoltWinters(Israel_DataD_fModel_HES, h = Forecast_D)
accuracy(Israel_DataD_forecast_HES, Israel_DataD_TestData_TS)

## Holt-Winters Exponential Smoothing
Israel_DataD_fModel_HWES <- HoltWinters(ts(Israel_DataD_TrainData_TS,frequency=7))
Israel_DataD_forecast_HWES <- forecast.HoltWinters(Israel_DataD_fModel_HWES, h = Forecast_D)
accuracy(GHS_Data_forecast_HWES, Israel_DataD_TestData_TS)


## Automated ARIMA with seasonality adjustment
Israel_DataD_forecast_sARIMA <- stlf(ts(as.ts(Israel_DataD_TrainData_TS[,1]),frequency=7), h=Forecast_D, method="arima") 
accuracy(Israel_DataD_forecast_sARIMA, Israel_DataD_TestData_TS)

## Automated exponential smoothing with seasonality adjustment
Israel_DataD_forecast_ETS <- stlf(ts(as.ts(Israel_DataD_TrainData_TS[,1]),frequency=7), h =Forecast_D, method="ets") 
accuracy(Israel_DataD_forecast_ETS, Israel_DataD_TestData_TS)


## Periodogram Analysis on differencing order of 1
diff1 <- diff(Israel_DataD_TrainData_TS[,1])

diff1_ts <- ts(diff1)
I = abs(fft(diff1)/sqrt(length(diff1)))^2/length(diff1) # the periodogram
P = (4/length(diff1))*I[0:(length(diff1)/2)] # the scaled periodogram
f = 0:(length(diff1)/2-1)/length(diff1) # frequencies

plot(f, P, type="l", xlab="Frequency", ylab="Scaled Periodogram")
p1 = which.max(P)
p2 = which.max(P[P!=P[p1]])
p3 = 700
p4 = 933
tTrainFrame = (1:length(diff1))
t1 = tTrainFrame - mean(tTrainFrame)
t2 = t1^2
t3 = t1^3
t4 = t1^4

z11 = cos(2*pi*tTrainFrame*p1/length(diff1)); z12 = sin(2*pi*tTrainFrame*p1/length(diff1))
z21 = cos(2*pi*tTrainFrame*p2/length(diff1)); z22 = sin(2*pi*tTrainFrame*p2/length(diff1))
z31 = cos(2*pi*tTrainFrame*p3/length(diff1)); z32 = sin(2*pi*tTrainFrame*p3/length(diff1))
z41 = cos(2*pi*tTrainFrame*p4/length(diff1)); z42 = sin(2*pi*tTrainFrame*p4/length(diff1))

#trainFrame <- data.frame(z11,z12,z21,z22,z31,z32, t1,t2)
trainFrame <- data.frame(z11,z12,z21,z22,z31,z32,z41,z42,t1,t2,t3)
fit <- tslm(diff1_ts~0+z11+z12+z21+z22+z31+z32+z41+z42+t1+t2+t3)
#fit <- tslm(diff1_ts~0+z11+z12+z21+z22+z31+z32)
#fit <- tslm(diff1_ts~0+z11+z12)
#layout(matrix(c(1,2,3), 3, 1, byrow = FALSE))
plot.ts(ts(diff1,freq=1), lty=1, ylab="Differenced Call Volume")
lines(fitted(fit), lwd=1, col = "red")
legend(1200, 660, c("Original Differenced Call Volume","Fitted Trend and Seasonality"), cex=1, col=c("black","red"),
       pch=c(1,1), lty=c(1,1))
plot.ts(fitted(fit), ylab="Trend and Seasonality")
plot.ts(resid(fit), ylab="Residuals")

tTestFrame = ((length(diff1)+1):(length(diff1)+100))
t1 = tTestFrame - mean(tTrainFrame)
t2 = t1^2
t3 = t1^3
t4 = t1^4

z11 = cos(2*pi*tTestFrame*p1/length(diff1)); z12 = sin(2*pi*tTestFrame*p1/length(diff1))
z21 = cos(2*pi*tTestFrame*p2/length(diff1)); z22 = sin(2*pi*tTestFrame*p2/length(diff1))
z31 = cos(2*pi*tTestFrame*p3/length(diff1)); z32 = sin(2*pi*tTestFrame*p3/length(diff1))
z41 = cos(2*pi*tTestFrame*p4/length(diff1)); z42 = sin(2*pi*tTestFrame*p4/length(diff1))

#forecasttFrame <- data.frame(z11,z12,z21,z22,z31,z32, t1,t2,t3, t4)
forecasttFrame <- data.frame(z11,z12,z21,z22,z31,z32,z41,z42,t1,t2,t3)
#forecasttFrame <- data.frame(zz11,zz12,zz21,zz22,zz31,zz32)
#forecasttFrame <- data.frame(zz11,zz12)
GHS_DetrendDeseason_TS = ts(resid(fit), frequency = 50)
arima_fit <-auto.arima(GHS_DetrendDeseason_TS)
GHS_Resid_NN <- ts(resid(arima_fit))
nn_fit <- nnetar(GHS_Resid_NN,p=30)

GHS_Data_forecast_sub_arima <- forecast(arima_fit, h =100)$mean
GHS_Data_forecast_sub_lm<- forecast(fit, forecasttFrame)$mean
GHS_Data_forecast_sub_nn<- forecast(nn_fit, h=100)$mean

normalityCheck <-resid(nn_fit)
normalityCheck<-normalityCheck[!is.na(normalityCheck)]
qqnorm(normalityCheck, 
       ylab="Residuals", 
       xlab="Normal Theoretical Quantitles", 
       main="Error Normality Check") 
qqline(normalityCheck)

GHS_Data_forecast_diff <- as.vector(GHS_Data_forecast_sub_arima)+as.vector(GHS_Data_forecast_sub_lm)+as.vector(GHS_Data_forecast_sub_nn)
GHS_Data_forecast_diff_inv_xi <- diffinv(GHS_Data_forecast_diff, xi=GHS_TrainData_TS[length(GHS_TrainData_TS)])
GHS_Data_forecast<-GHS_Data_forecast_diff_inv_xi[2: length(GHS_Data_forecast_diff_inv_xi)]
accuracy(GHS_Data_forecast, GHS_TestData_Raw)

plot(GHS_Resid_NN, ylab="Residuals after SARIMA and Smoothing")
plot(acf(GHS_Resid_NN))
plot(pacf(GHS_Resid_NN))
plot(ts(fitted(arima_fit),freq=1))
lines(ts(GHS_DetrendDeseason_TS,freq=1), lwd=1, col = "red")

#train
GHS_DetrendDeseason_TS_train_lm <- forecast(fit, trainFrame)$mean
GHS_DetrendDeseason_TS_train_arima <- fitted(arima_fit)
trainDataSetInput <- data.frame(GHS_DetrendDeseason_TS_train_lm,GHS_DetrendDeseason_TS_train_arima)

trainDataSetOutput <- data.frame(diff1)
trainingData <- cbind(trainDataSetInput, trainDataSetOutput)
colnames(trainingData) <- c("Input_Smoother","Input_ARIMA","Output")
neuralNetwork <- neuralnet(Output~Input_Smoother+Input_ARIMA,trainingData, hidden=1,  rep = 1, threshold = 1000)
#test
testDataSet <- data.frame(GHS_Data_forecast_sub_lm, GHS_Data_forecast_sub_arima)
nn.results <- compute(neuralNetwork, testDataSet)
xx <- as.vector(nn.results$net.result)

b<-diffinv(xx, xi=GHS_TrainData_TS[length(GHS_TrainData_TS)])
c<- b[2:length(b)]
accuracy(c,GHS_TestData_Raw)
plot(neuralNetwork)

# Plot
plot(x=seq(0:100), y=seq(50,1150,length=101),type="n", ylab="Call Volumes", xlab="Time Index")
lines(GHS_Data_forecast, type="l",col="red",lwd=2)
lines(GHS_TestData_Raw, type="l", col="black",lwd=2)
lines(as.vector(GHS_Data_forecast_ARIMA$mean), type="l", col="blue",lwd=2)
legend(65,1150, c("PPRS-SARIMA-ANN", "SARIMA(2,1,1)(2,0,0)[50]", "Original"), lty=c(1,1,1), 
       lwd=c(2.5,2.5,2.5),col=c("red","blue", "black" ))

plot(x=seq(0:100), y=seq(50,1350,length=101),type="n", ylab="Call Volumes", xlab="Time Index")
lines(as.vector(GHS_Data_forecast_wavelet_ARIMA$mean), type="l", col="yellow",lwd=2)
lines(c, type="l",col="red",lwd=2)
lines(GHS_TestData_Raw, type="l", col="black",lwd=2)
lines(as.vector(GHS_Data_forecast_ARIMA$mean), type="l", col="blue",lwd=2)
legend(65,1350, c("Wavelet ARIMA","PPRS-SARIMA-ANN", "ARIMA", "Original"), lty=c(1,1,1,1), 
       lwd=c(2.5,2.5,2.5,2.5),col=c("yellow","red","blue", "black" ))


PPRS_ARIMA_ANN_error<- abs(GHS_Data_forecast- GHS_TestData_Raw)
GHS_Data_forecast_ARIMA_error<- abs(as.vector(GHS_Data_forecast_ARIMA$mean) - GHS_TestData_Raw)
GHS_Data_forecast_HWES_error<- abs(as.vector(GHS_Data_forecast_HWES$mean) - GHS_TestData_Raw)

boxplot(PPRS_ARIMA_ANN_error,  GHS_Data_forecast_ARIMA_error,
        names=c("PPRS_SARIMA_ANN","SARIMA(2,1,1)(2,0,0)[50]"), col=c("red","blue"))
legend(1.2,200, c("PPRS-SARIMA-ANN", "SARIMA(2,1,1)(2,0,0)[50]" ), lty=c(1,1), 
       lwd=c(2.5,2.5),col=c("red","blue"))

plot(GHS_Data_forecast_diff, type="l")
plot(GHS_Data_forecast_diff_res, type="l")
plot(GHSDataMVtimeseriescomponents)
plot(GHS_Data_forecast_ARIMA)
plot.forecast(GHS_Data_forecast_SES)
plot.forecast(GHS_Data_forecast_HES)



## Plot mean of call volumes
xrange <- c(8:17)
yrange <- colMeans(MonDayData, na.rm=TRUE)
colors <- c("red1","gold1","blueviolet","limegreen","black")

plot(xrange, yrange, type="n",main = "Mean of call volumes (Feb. 07, 2011 - Jun. 10, 2014)", xlab="Time",ylab="Call volume (mean)",xaxt="n")
axis(1, at=xrange)
lines(xrange, colMeans(MonDayData, na.rm=TRUE), lty=1,lwd = 2, pch =1, col=colors[1])
lines(xrange, colMeans(TueDayData, na.rm=TRUE), lty=2,lwd = 2, pch =2, col=colors[2])
lines(xrange, colMeans(WedDayData, na.rm=TRUE), lty=3,lwd = 2, pch =3, col=colors[3])
lines(xrange, colMeans(ThuDayData, na.rm=TRUE), lty=4,lwd = 2, pch =4, col=colors[4])
lines(xrange, colMeans(FriDayData, na.rm=TRUE), lty=5,lwd = 2, pch =5, col=colors[5])
legend(16, 610, c("Mon","Tue","Wed","Thu","Fri"), cex=1, col=colors,
       pch=c(1:5), lty=c(1:5), title="Weekdays")

## Plot variance of call volumes
xrange <- c(8:17)
yrange <- apply(FriDayData, na.rm=TRUE, 2, sd)
colors <- c("red1","gold1","blueviolet","limegreen","black")

plot(xrange, yrange,type="n",main = "Standard deviation (SD) of call volumes (Feb. 07, 2011 - Jun. 10, 2014)", xlab="Time",ylab="Call volume (SD)",xaxt="n")
axis(1, at=xrange)
lines(xrange, apply(MonDayData, na.rm=TRUE, 2, sd), lty=1,lwd = 2, pch =1, col=colors[1])
lines(xrange, apply(TueDayData, na.rm=TRUE, 2, sd), lty=2,lwd = 2, pch =2, col=colors[2])
lines(xrange, apply(WedDayData, na.rm=TRUE, 2, sd), lty=3,lwd = 2, pch =3, col=colors[3])
lines(xrange, apply(ThuDayData, na.rm=TRUE, 2, sd), lty=4,lwd = 2, pch =4, col=colors[4])
lines(xrange, apply(FriDayData, na.rm=TRUE, 2, sd), lty=5,lwd = 2, pch =5, col=colors[5])
legend(16, 215, c("Mon","Tue","Wed","Thu","Fri"), cex=1, col=colors,
       pch=c(1:5), lty=c(1:5), title="Weekdays")




