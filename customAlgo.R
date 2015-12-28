prepare_data <-function(currentData)
{
  minTime = min(currentData[,1])
  maxTime = max(currentData[,1])
  testData = NULL
  testData2 = NULL
  trainData2 = NULL
  trainData = NULL
  
  timeDiff = difftime(currentData[,1],minTime,"mins")
  thresholdValue = difftime(maxTime,minTime,"mins")*0.8
  for(j in 1:length(timeDiff))
  {
  	if(timeDiff[j]>thresholdValue)
  	{
  		testData = rbind(testData,timeDiff[j])
  		testData2 = rbind(testData2,currentData[j,2])
  	}
  	else
  	{
  		trainData = rbind(trainData,timeDiff[j])
  		trainData2 = rbind(trainData2,currentData[j,2])
  	}	
  }
  
  trainData <- cbind(trainData,trainData2)
  testData <- cbind(testData,testData2)
  return(list(trainData,testData))
}
calculate_errors <- function(residualErrors,trainData)
{
   RMSE = 0
   MAPE = 0
  ########## RMSE ##############
  for(k in 1:length(residualErrors))
  {
  	RMSE = RMSE + residualErrors[k]*residualErrors[k]
  }
  RMSE = RMSE/length(residualErrors)
  RMSE = sqrt(RMSE)
  print(paste("RMSE  = ",RMSE))
  
  ######### Mean Absolute Percentage Error ############## preferred because this is not affected heavily by outliers 
  for(k in 1:length(residualErrors))
  {
  if(trainData[k,2] != 0)
  {
  MAPE = MAPE + (abs(residualErrors[k])/trainData[k,2])   
  }
  else 
  {
  MAPE = MAPE + abs(residualErrors[k])
  }
  }
  MAPE = MAPE/length(residualErrors)
  print(paste("MAPE = ",MAPE))
return(list(RMSE,MAPE))  
}

fit_lm_plot <- function(trainData)
{
  print("--------LM----------")
  trainData = as.data.frame(trainData)
  fitLM <- lm(as.numeric(trainData[,2])~trainData[,1],data = as.data.frame(trainData))
  coeff <- coefficients(fitLM)
  residualErrors <- residuals(fitLM)
  rSquared = summary(fitLM)$adj.r.squared
  errors <- calculate_errors(residualErrors,trainData)
  RMSE <- errors[1]
  MAPE <- errors[2]
  print(paste("R squared : ",rSquared))

  lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\LMplot\\"
  lmFileName = paste(lmPlotsFolder,"LM_plot")
  lmFileName = paste(lmFileName,".jpg")
  jpeg(lmFileName)
  plot(as.numeric(trainData[,1]), as.numeric(trainData[,2]),type="l",xlab="Time Stamp",ylab="Run Time",main=paste(uniqueJobsList[i],"'s Plot"))
  abline(lm(as.numeric(trainData[,2])~trainData[,1],data = as.data.frame(trainData)),col="red")
  dev.off()
 
  return(coeff)
}


detrend <- function(coeff,trainData)
{
  intercept = coeff[1]
  slope = coeff[2]
  trainData = as.data.frame(trainData)
  trainData = as.data.frame(trainData)
  detrendedTrainData = trainData[,2]-trainData[,1]*slope
  detrendedTrainData = cbind(trainData[,1],detrendedTrainData)
  return(detrendedTrainData)
}


plot_data <- function(relevantData)
{
plot(relevantData[,1],relevantData[,2],type="l",xlab=colnames(relevantData)[1],ylab=colnames(relevantData)[2])

}

apply_AutoArima <- function(detrendedTrainData)
{
library(forecast)
fit_auto_arima <- auto.arima(detrendedTrainData)
print(fit_auto_arima)

lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\LMplot\\"
lmFileName = paste(lmPlotsFolder,"Auto_Arima_plot")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(fit_auto_arima$x,col="blue")
lines(fitted(fit_auto_arima),col="red")
dev.off()

}

predict_lm <- function(trainData,testData,testvar)
{ 
  #fitLM1 <- testvar
  trainData <- as.data.frame(trainData)
  testData <- as.data.frame(testData)
  fitLM <- lm(as.numeric(trainData[,2])~trainData[,1],data = as.data.frame(trainData))
  p_conf1 <- predict(fitLM,interval="confidence")
  p_pred1 <- predict(fitLM,interval="prediction")
  new_data <- testData[,1]
  new_data <- as.data.frame(new_data)
  
  temp1 <- 0
  temp1 = as.data.frame(temp1)
  temp1 = fitLM
  
  #p_conf2 <- predict(x.fitLM,interval="confidence",data = new_data)
  #p_pred2 <- predict(x.fitLM,interval="prediction",data = new_data)
  
  lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\LMplot\\"
  lmFileName = paste(lmPlotsFolder,"LM_prediction_plot")
  lmFileName = paste(lmFileName,".jpg")
  jpeg(lmFileName)
  plot(as.numeric(trainData[,1]), as.numeric(trainData[,2]),type="l",xlab="Time Stamp",ylab="Run Time",main=paste(uniqueJobsList[i],"'s Plot"))
  abline(fitLM,col="blue")
  matlines(trainData[,1],p_conf1[,c("lwr","upr")],col=2,lty=1,type="b",pch="+")
  matlines(trainData[,1],p_pred1[,c("lwr","upr")],col=2,lty=2,type="b",pch=1)
  #matlines(testData[,1],p_conf2[,c("lwr","upr")],col=4,lty=1,type="b",pch="+")
  #matlines(testData[,1],p_pred2[,c("lwr","upr")],col=4,lty=2,type="b",pch=1)
  dev.off()
  
}

test <- function(trainData)
{
A= 0
  fitLM1 <- lm(as.numeric(trainData[,2])~trainData[,1],data = as.data.frame(trainData))
return(list(A,fitLM1))
}


convert_to_time_series <- function(detrendedTrainData)
{
	startTime = detrendedTrainData[1]
	endTime = detrendedTrainData[length(detrendedTrainData)]
	asTimeSeries <- ts(detrendedTrainData,start = startTime,deltat = (1/12)*(1/60))
	return(asTimeSeries)
}

apply_Arima <- function(detrendedTrainData,testData)
{
	data1 = detrendedTrainData[,2]
	tsdata1 = ts(data1,frequency=10008)
	ArimaFit = Arima(tsdata1,c(1,0,0))
	arimaOb <- auto.arima(tsdataHW)
	fc <- forecast(ArimaFit,h=20)
	fc <- 
	plot(fc)
	
	
}

decompose_data <- function(detrendedTrainData)
{
	data1 <- detrendedTrainData[,2]
	tsdata1 <- ts(data1,frequency=10008)
    dec1 <- decompose(tsdata1)
	plot(dec1)

}

apply_HoltWinters <- function(detrendedTrainData)
{
	tsdataHW = ts(data1,frequency=10008)
	hwObject = HoltWinters(tsdataHW)
    fcObject <- forecast(hwObject,h=100)	
}

compute_periodogram <- function(detrendedTrainData)
{
	temp = spec.pgram(detrendedTrainData[,2],spans=c(19,19))
	spectralFreq = temp$spec
	timeFrequency = temp$freq
	periodogramData <- cbind(spectralFreq,timeFrequency)
	print(periodogramData)
}

data = read.csv("C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\020.csv",sep=",")
relevantData = data[,2:3]
temp = relevantData[,1]
temp = strptime(temp,format="%m/%d/%Y %H:%M")
relevantData[,1] = as.POSIXct(temp)
colnames(relevantData) <- c("Time Stamp","CPU Util")
plot_data(relevantData) 
trainPlusTest <- prepare_data(relevantData)
trainData <- trainPlusTest[1]
testData <- trainPlusTest[2]
coeff <- fit_lm_plot(trainData)
trainData = as.data.frame(trainData)
vari <- test(trainData)
predict_lm(trainData,testData,vari[2])
detrendedTrainData <- detrend(coeff,trainData)

compute_periodogram(detrendedTrainData)

apply_AutoArima(detrendedTrainData)
decompose_data(detrendedTrainData)
apply_HoltWinters(detrendedTrainData)


print(relevantData)
