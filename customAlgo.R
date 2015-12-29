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

find.freq_AR <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(na.contiguous(x)),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        if(nextmax <= length(spec$freq))
          period <- round(1/spec$freq[nextmax])
        else
          period <- 1
      }
      else
        period <- 1
    }
  }
  else
    period <- 1

  return(period)
}

compute_periodogram <- function(detrendedTrainData,testData)
{
	testData <- testData
	print(testData)
	temp = spec.pgram(detrendedTrainData[,2],spans=c(19,19))
	spectralFreq = temp$spec
	timeFrequency = temp$freq
	periodogramData <- cbind(spectralFreq,timeFrequency)
	#print(periodogramData)
	bandwidth = temp$bandwidth
	indexStep = as.integer(0.1/bandwidth)
	if(indexStep > 20){
	temp1 = periodogramData[1:indexStep,]
	}else
	{
	temp1 = periodogramData
	}
	sortedTemp1 = temp1[order(temp1[,1],decreasing=TRUE),]
	#sortedTemp1 <- colnames("Spectral Energy","Inverse Frequency")
	top20periods <- 1/sortedTemp1[1:20,2]
	print(top20periods)
	predicted_bin <- predict_next_bin_detrended(top20periods,detrendedTrainData,testData)
	#temp2 = periodogramData[indexStep:(2*indexStep),]
	#sortedTemp2 = temp2[order(temp2[,1],decreasing=TRUE),]
	#sortedTemp2 <- colnames("Spectral Energy","Inverse Frequency")
	#top20periods <- 1/sortedTemp1[1:20,2]
	#predict_next_bin_detrended(top20periods,detrendedTrainData)
	return(predicted_bin)
}

predict_next_bin_detrended <- function(top20periods,detrendedTrainData,testData)
{
	testData <- testData
	index = 0
	for(i in 1: length(top20periods))
	{
		current_period = top20periods[i]
		temp = matrix(data = 0,nrow=current_period,ncol=1)

		startIndex = 0
		endIndex = current_period
		repeat{
		if(endIndex > length(detrendedTrainData[,2]))
		{
			temp1 = detrendedTrainData[startIndex:length(detrendedTrainData[,2]),2]
			length1 = endIndex-length(detrendedTrainData[,2])
			zero_temp = vector(,length1)
			temp1 = c(temp1,zero_temp)
			temp = cbind(temp,temp1)
			break
		}
		#print(detrendedTrainData[startIndex:endIndex,2])
		temp = cbind(temp,detrendedTrainData[startIndex:endIndex,2])
		startIndex = startIndex + current_period
		endIndex = startIndex + current_period
		}
	
	
    predicted_bin = vector(,current_period)
	folds = length(detrendedTrainData[,2])/current_period
	folds = as.integer(folds)
	folds = folds + 2
	for(k in 1: length(temp[,1]))
	{
	if(temp[k,folds]==0.000000)
	{
		pred_temp = sum(temp[k,])
		pred_temp = pred_temp/(folds-1)
		predicted_bin[k] = pred_temp
	}
	pred_temp = sum(temp[k,])
	pred_temp = pred_temp/folds
	predicted_bin[k] = pred_temp
	}
	
	combined = c(detrendedTrainData[,2],predicted_bin)
	predictedPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Predicted_plots020\\"
	lmFileName = paste(predictedPlotsFolder,"Prediction_with_period")
	lmFileName = paste(lmFileName,top20periods[i])
    lmFileName = paste(lmFileName,".jpg")
	jpeg(lmFileName)
	plot(combined,type="l",)
	lines(combined[length(detrendedTrainData[,1]):(length(combined))],type="l",col="red")
	#lines(combined[length(detrendedTrainData[,1]):length(combined)],type="l",col="red")
	dev.off()
	testData <- testData
	calculate_errors(predicted_bin,testData)
	}
	return(predicted_bin)
}

calculate_errors <- function(predicted_bin,testData)
{
	testData <- as.data.frame(testData)
	error1 = testData[1:length(predicted_bin),2] - predicted_bin 
	MAPE =0
	for(k in 1:length(error1))
  {
  if(testData[k,2] != 0)
  {
  MAPE = MAPE + (abs(error1[k])/testData[k,2])   
  }
  else 
  {
  MAPE = MAPE + abs(error1[k])
  }
  }
  MAPE = MAPE/length(error1)

  
  print(MAPE)
}

predict_next_bin_AR <- function(detrendedTrainData, period)
{
	current_period = period
		temp = matrix(data = 0,nrow=current_period,ncol=1)

		startIndex = 0
		endIndex = current_period
		repeat{
		if(endIndex > length(detrendedTrainData[,2]))
		{
			temp1 = detrendedTrainData[startIndex:length(detrendedTrainData[,2]),2]
			length1 = endIndex-length(detrendedTrainData[,2])
			zero_temp = vector(,length1)
			temp1 = c(temp1,zero_temp)
			temp = cbind(temp,temp1)
			break
		}
		#print(detrendedTrainData[startIndex:endIndex,2])
		temp = cbind(temp,detrendedTrainData[startIndex:endIndex,2])
		startIndex = startIndex + current_period
		endIndex = startIndex + current_period
		}
	
	
    predicted_bin = vector(,current_period)
	folds = length(detrendedTrainData[,2])/current_period
	folds = as.integer(folds)
	folds = folds + 2
	for(k in 1: length(temp[,1]))
	{
	if(temp[k,folds]==0.000000)
	{
		pred_temp = sum(temp[k,])
		pred_temp = pred_temp/(folds-1)
		predicted_bin[k] = pred_temp
	}
	pred_temp = sum(temp[k,])
	pred_temp = pred_temp/folds
	predicted_bin[k] = pred_temp
	}
	
	combined = c(detrendedTrainData[,2],predicted_bin)
	predictedPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Predicted_plots020\\"
	lmFileName = paste(predictedPlotsFolder,"Prediction_with_period_AR")
	lmFileName = paste(lmFileName,top20periods[i])
    lmFileName = paste(lmFileName,".jpg")
	jpeg(lmFileName)
	plot(combined,type="l",)
	lines(combined[length(detrendedTrainData[,1]):(length(combined))],type="l",col="red")
	#lines(combined[length(detrendedTrainData[,1]):length(combined)],type="l",col="red")
	dev.off()

}

data = read.csv("C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\1.csv",sep=",")
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

predicted_bin <- compute_periodogram(detrendedTrainData,testData)



period <- find.freq_AR(detrendedTrainData[,2])
predict_next_bin_AR(detrendedTrainData,period)
#calculate_peaks(periodogramData)

apply_AutoArima(detrendedTrainData)
decompose_data(detrendedTrainData)
apply_HoltWinters(detrendedTrainData)


#print(relevantData)
