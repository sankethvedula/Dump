CONST_DATE_FORMAT = "%d/%m/%Y %H:%M"

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


calculate_errors_1 <- function(residualErrors,trainData)
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
  errors <- calculate_errors_1(residualErrors,trainData)
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

final_AutoArima <- function(min_arima_index,trainData,testData,best5errors,file1)
{
	detrendedTrainData <- trainData
	if(length(min_arima_index) > 1)
	{
		min_arima_index = min_arima_index[1]
	}
	best_period <- best5errors[min_arima_index,1]
	data1 = detrendedTrainData[,2]
	tsdata1 = ts(data1,frequency=best_period)
	library(forecast)
	fit_auto_arima <- auto.arima(tsdata1)
	testData = as.data.frame(testData)
	len = length(testData[,2])
	fc <- try(predict(fit_auto_arima,n.ahead=len), silent = TRUE)
	if (inherits(fc, 'try-error'))
	{
		message_string = paste('Error in predicting')
		print (message_string)
		error_list_arima <- c(error_list_arima,"NA")
		next
	}
	temp_pred_arima = fc$pred
	temp_pred_arima = as.data.frame(temp_pred_arima)
	temp_sd = fc$se
	temp_sd = as.data.frame(temp_sd)
	temp_low = temp_pred_arima - temp_sd
	temp_high = temp_pred_arima + temp_sd
	temp_low = as.numeric(temp_low$x)
	temp_high = as.numeric(temp_high$x)
	temp_pred_arima = as.numeric(temp_pred_arima$x)
	
	lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Output_final\\"
	lmFileName = paste(lmPlotsFolder,file1)
	lmFileName = paste(lmFileName,".jpg")
	totalLength = length(detrendedTrainData[,2]) + length(testData[,2])

	jpeg(lmFileName)
	plot(1:length(detrendedTrainData[,2]),detrendedTrainData[,2],type="l",xlim=c(0,totalLength))
	lines((length(detrendedTrainData[,2])+1):totalLength,testData[,2],type="l",lty=1)
	lines((length(detrendedTrainData[,2])+1):totalLength,temp_pred_arima,type="l",col="red",lty=1)
	lines((length(detrendedTrainData[,2])+1):totalLength,temp_low,type="l",col=5,lty=3)
	lines((length(detrendedTrainData[,2])+1):totalLength,temp_high,type="l",col=6,lty=3)  
	dev.off()
}

apply_AutoArima <- function(detrendedTrainData,best5errors,testData,output_directory_path,file1)
{
  error_list_arima <- NULL
  for(err in 1:length(best5errors[,1]))
  {
  #print(err)
  best_period = best5errors[err,1]
  data1 = detrendedTrainData[,2]
  tsdata1 = ts(data1,frequency=best_period)
  library(forecast)
  fit_auto_arima <- auto.arima(tsdata1)
  testData = as.data.frame(testData)
  len = length(testData[,2])
  fc <- try(predict(fit_auto_arima,n.ahead=len), silent = TRUE)
  if (inherits(fc, 'try-error'))
    {
    message_string = paste('Error in predicting')
    print (message_string)
    error_list_arima <- c(error_list_arima,"NA")
    next
    }
  temp_pred_arima = fc$pred
  temp_pred_arima = as.data.frame(temp_pred_arima)
  temp_sd = fc$se
  temp_sd = as.data.frame(temp_sd)
  temp_low = temp_pred_arima - temp_sd
  temp_high = temp_pred_arima + temp_sd
  temp_low = as.numeric(temp_low$x)
  temp_high = as.numeric(temp_high$x)
  temp_pred_arima = as.numeric(temp_pred_arima$x)
  #print(fit_auto_arima)
  
  lmPlotsFolder = output_directory_path
  lmFileName = paste(lmPlotsFolder,err,"Arima with period")
  lmFileName = paste(lmFileName,best_period)
  lmFileName = paste(lmFileName,".jpg")
  totalLength = length(detrendedTrainData[,2]) + length(testData[,2])
  jpeg(lmFileName)
  plot(1:length(detrendedTrainData[,2]),detrendedTrainData[,2],type="l",xlim=c(0,totalLength))
  lines((length(detrendedTrainData[,2])+1):totalLength,testData[,2],type="l",lty=1)
  lines((length(detrendedTrainData[,2])+1):totalLength,temp_pred_arima,type="l",col="red",lty=1)
  lines((length(detrendedTrainData[,2])+1):totalLength,temp_low,type="l",col=5,lty=3)
  lines((length(detrendedTrainData[,2])+1):totalLength,temp_high,type="l",col=6,lty=3)  
  #plot(testData[,2],type="l",col="blue")
  #lines(temp_pred_arima,col="red")
  #lines(temp_low,col=3,lty=2)
  #lines(temp_high,col=3,lty=2)
  dev.off()
  
  error <- calculate_errors_arima(temp_pred_arima,testData)
  error_list_arima <- c(error_list_arima,error)
  print(paste("The error of Arima #",err,"is",error))
  }
  
  return(error_list_arima)
}

calculate_errors_arima <- function(temp_pred_arima,testData)
{ 
  temp_pred_arima = unlist(temp_pred_arima)
  testData = as.data.frame(testData)
  error1 = as.numeric(temp_pred_arima) - testData[,2]
  error1 = unlist(error1)
  MdAPE = 0
  list_MdAPE <- NULL  
  for(k in 1:length(error1))
  {
    if(testData[k,2] != 0)
    {
      temp_err <- (abs(error1[k])/abs(testData[k,2]))
      list_MdAPE <- c(list_MdAPE,temp_err)
    }else{
    temp_err <-  abs(error1[k])
    list_MdAPE <- c(list_MdAPE,temp_err)    
    }
  }
  list_MdAPE <- remove_outliers_errors(list_MdAPE)
  MdAPE <- median(list_MdAPE)
  
  # MAPE =0
  # for(k in 1:length(error1))
  # {
  # error1[k] = as.numeric(error1[k])
  # if(testData[k,2] != 0)
  # {
  # MAPE = MAPE + (abs(error1[k])/abs(testData[k,2]))   
  # }
  # else 
  # {
  # MAPE = MAPE + abs(error1[k])
  # }
  # }
  # MAPE = MAPE/length(error1)
   MdAPE = as.numeric(MdAPE)
  return(MdAPE)
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


apply_Arima <- function(detrendedTrainData,testData)
{
  data1 = detrendedTrainData[,2]
  tsdata1 = ts(data1,frequency=10008)
  ArimaFit = Arima(tsdata1,c(1,0,0))
  arimaOb <- auto.arima(tsdataHW)
  fc <- forecast(ArimaFit,h=20)
  plot(fc)
  
  
}

decompose_data <- function(detrendedTrainData)
{
  data1 <- detrendedTrainData[,2]
  tsdata1 <- ts(data1,frequency=10008)
    dec1 <- decompose(tsdata1)
  plot(dec1)

}

final_HoltWinters  <- function(min_hw_index,trainData,testData,best5errors,file1)
{
	detrendedTrainData = trainData
	best_period = best5errors[min_hw_index,1]
	print(min_hw_index)
	testData = as.data.frame(testData)
    tsdataHW = ts(data1,frequency=best_period)
    hwObject = HoltWinters(tsdataHW)
    fcObject <- forecast(hwObject,h=length(testData[,2]),level=c(80,95))
    pred_hw <- fcObject$mean
    pred_hw_upper_80 <- fcObject$upper[,1]
    pred_hw_upper_95 <- fcObject$upper[,2]
    pred_hw_lower_80 <- fcObject$lower[,1]
    pred_hw_lower_95 <- fcObject$lower[,2]
    pred_hw = as.numeric(pred_hw)
    pred_hw_lower = as.numeric(fcObject$lower)
    pred_hw_upper = as.numeric(fcObject$upper)
	
	lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Output_final\\"
	lmFileName = paste(lmPlotsFolder,file1)
	lmFileName = paste(lmFileName,".jpg")
	totalLength = length(detrendedTrainData[,2]) + length(testData[,2])

	jpeg(lmFileName)
    plot(1:length(detrendedTrainData[,2]),detrendedTrainData[,2],type="l",xlim=c(0,totalLength))
    lines((length(detrendedTrainData[,2])+1):totalLength,testData[,2],type="l",lty=1)
    lines((length(detrendedTrainData[,2])+1):totalLength,pred_hw,type="l",col="red",lty=1)
    lines((length(detrendedTrainData[,2])+1):totalLength,pred_hw_lower_80,type="l",col=5,lty=3)
    lines((length(detrendedTrainData[,2])+1):totalLength,pred_hw_upper_80,type="l",col=6,lty=3)  
	dev.off()
	return(TRUE)
}


apply_HoltWinters <- function(detrendedTrainData,best5errors,testData,output_directory_path,file1)
{
  error_hw_list <- NULL
  for(err in 1:length(best5errors[,1]))
  {
  best_period <- best5errors[err,1]
  pred_hw <- 123
  error <- NA
  if(best_period <= length(detrendedTrainData[,2])/2)
  {
    testData = as.data.frame(testData)
    tsdataHW = ts(data1,frequency=best_period)
    hwObject = HoltWinters(tsdataHW)
    fcObject <- forecast(hwObject,h=length(testData[,2]),level=c(80,95))
    pred_hw <- fcObject$mean
    pred_hw_upper_80 <- fcObject$upper[,1]
    pred_hw_upper_95 <- fcObject$upper[,2]
    pred_hw_lower_80 <- fcObject$lower[,1]
    pred_hw_lower_95 <- fcObject$lower[,2]
    pred_hw = as.numeric(pred_hw)
    pred_hw_lower = as.numeric(fcObject$lower)
    pred_hw_upper = as.numeric(fcObject$upper)
    lmPlotsFolder = output_directory_path
    lmFileName = paste(lmPlotsFolder,err,"HW with period")
    lmFileName = paste(lmFileName,best_period)
    lmFileName = paste(lmFileName,".jpg")
    totalLength = length(detrendedTrainData[,2]) + length(testData[,2])
    
    jpeg(lmFileName)
    plot(1:length(detrendedTrainData[,2]),detrendedTrainData[,2],type="l",xlim=c(0,totalLength))
    lines((length(detrendedTrainData[,2])+1):totalLength,testData[,2],type="l",lty=1)
    lines((length(detrendedTrainData[,2])+1):totalLength,pred_hw,type="l",col="red",lty=1)
    lines((length(detrendedTrainData[,2])+1):totalLength,pred_hw_lower_80,type="l",col=5,lty=3)
    lines((length(detrendedTrainData[,2])+1):totalLength,pred_hw_upper_80,type="l",col=6,lty=3)   
    #plot(testData[,2],type="l",col="blue")
    #lines(pred_hw,col="red")
    #lines(pred_hw_lower,col=3,lty=2)
    #lines(pred_hw_upper,col=3,lty=2)
    dev.off()
	error <- calculate_errors_hw(pred_hw,testData)
	print(paste("HoltWinters",err,"period",error))
	error_hw_list <- c(error_hw_list,error)
  }
 if(is.na(error)){
 error_hw_list <- c(error_hw_list,error)
 }
  }
 return(error_hw_list)  
}


calculate_errors_hw <- function(temp_pred_hw,testData)
{ 
 
  temp_pred_hw = unlist(temp_pred_hw)
  testData = as.data.frame(testData)
  error1 = as.numeric(temp_pred_hw) - testData[,2]
  error1 = unlist(error1)
  MdAPE = 0
  list_MdAPE <- NULL  
  for(k in 1:length(error1))
  {
    if(testData[k,2] != 0)
    {
      temp_err <- (abs(error1[k])/abs(testData[k,2]))
      list_MdAPE <- c(list_MdAPE,temp_err)
    }else{
    temp_err <-  abs(error1[k]/temp_err)
    list_MdAPE <- c(list_MdAPE)   
    }
  }
  
  list_MdAPE <- remove_outliers_errors(list_MdAPE)
  MdAPE <- median(list_MdAPE) 
  
  # MAPE =0
  # for(k in 1:length(error1))
  # {
  # error1[k] = as.numeric(error1[k])
  # if(testData[k,2] != 0)
  # {
  # MAPE = MAPE + (abs(error1[k])/abs(testData[k,2]))   
  # }else 
  # {
  # MAPE = MAPE + abs(error1[k])
  # }
  # }
  # MAPE = MAPE/length(error1)
  MdAPE = as.numeric(MdAPE)
  return(MdAPE)
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

final_compute_periodogram <- function(detrendedTrainData,testData,damp,granularity,coeff,min_custom_index,file1)
{
	file1 <- file1
	testData <- testData
	slope <- coeff[2]
	if(damp==1){
		temp = spec.pgram(detrendedTrainData[,2],spans=c(19,19))
	}else{
    temp = spec.pgram(detrendedTrainData[,2])
	}

	spectralFreq = temp$spec
	timeFrequency = temp$freq
	periodogramData <- cbind(spectralFreq,timeFrequency)
	#print(periodogramData)
	bandwidth = temp$bandwidth
	indexStep = as.integer((1/60)/bandwidth)
	if(granularity < 2){
	temp1 = periodogramData[1:indexStep,]
	}else
	{
	temp1 = periodogramData
	}
	
	temp_freq_list <- NULL
	for(temp_freq_index in 1:length(periodogramData[,1]))
	{
		if(periodogramData[temp_freq_index,2] > 0.40)
		{
			temp_freq_list <- c(temp_freq_list,temp_freq_index)
		}
	
	}
	temp1 <- temp1[-temp_freq_list,]
	
	sortedTemp1 = temp1[order(temp1[,1],decreasing=TRUE),]
  #sortedTemp1 <- colnames("Spectral Energy","Inverse Frequency")
	top20periods <- 1/sortedTemp1[1:20,2]
	
	if(length(min_custom_index) > 1)
	{
		min_custom_index = min_custom_index[1]
	}
	
	testData <- as.data.frame(testData)
	 current_period = top20periods[min_custom_index]
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
	
	current_period = top20periods[min_custom_index]
	 predicted_bin = vector(,current_period)
  std_bin = vector(,current_period)
  folds = length(detrendedTrainData[,2])/current_period
  if((folds-round(folds))!=0) # check if folds is an integer
  {
  folds = as.integer(folds)
  folds = folds + 2
  }else{
  folds = folds + 1
  }
  for(k in 1: length(temp[,1]))
  {
  if(temp[k,folds]==0.000000)
  {
    #pred_temp = sum(temp[k,])
    #pred_temp = median(temp[k,],na.rm=TRUE)
    var1 = quantile(temp[k,], probs = c(0, 0.25, 0.5, 0.75, 1)) # quartile
      pred_temp = as.numeric(var1[3])
    #pred_temp = pred_temp/(folds-1)
    predicted_bin[k] = pred_temp
  }
  #pred_temp = sum(temp[k,])
  #pred_temp = median(temp[k,],na.rm=TRUE)
  var1 = quantile(temp[k,], probs = c(0, 0.25, 0.5, 0.75, 1)) # quartile
  pred_temp = as.numeric(var1[3])
  #pred_temp = pred_temp/folds
  predicted_bin[k] = pred_temp
  std_temp = sd(temp[k,],na.rm=TRUE)
  std_bin[k] = std_temp
  }
  lower_band = predicted_bin-std_bin 
  upper_band = predicted_bin + std_bin
  
  
  testData <- as.data.frame(testData)
  if(length(predicted_bin)<length(testData[,2]))
  {
    temp_list <- NULL
    temp_lower <- NULL
    temp_higher <- NULL
    repeat{
    if(length(temp_list) >= length(testData[,2]))
    {
      break
    }
    temp_list <- c(temp_list,predicted_bin)
    temp_lower <- c(temp_lower,lower_band)
    temp_higher <- c(temp_higher,upper_band)
    }
    #print(head(temp_list))
  }else{
    temp_list = predicted_bin
    temp_higher = upper_band
    temp_lower = lower_band
  }
  #print(temp_list)
  
  predicted_bin = temp_list[1:length(testData[,1])]
  upper_band = temp_higher[1:length(testData[,1])]
  lower_band = temp_lower[1:length(testData[,1])]
  
  combined = c(detrendedTrainData[,2],predicted_bin)
  combined_lower = c(detrendedTrainData[,2],upper_band)
  combined_higher = c(detrendedTrainData[,2],lower_band)
  
  gen_seq = seq(from = 0, by = (granularity*60), length.out = length(combined))
  combined = slope*gen_seq + combined
  combined_lower = slope*gen_seq + combined_lower
  combined_higher = slope*gen_seq + combined_higher

	lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Output_final\\"
	lmFileName = paste(lmPlotsFolder,file1)
	lmFileName = paste(lmFileName,".jpg")
	
	totalLength = length(detrendedTrainData[,2]) + length(testData[,2])
	
	
  jpeg(lmFileName)
  plot(1:length(detrendedTrainData[,2]),detrendedTrainData[,2],type="l",xlim=c(0,totalLength))
  lines((length(detrendedTrainData[,2])+1):totalLength,testData[,2],type="l",lty=1)
  lines((length(detrendedTrainData[,2])+1):totalLength,combined[(length(detrendedTrainData[,1])+1):(length(combined))],type="l",col="red",lty=1)
  lines((length(detrendedTrainData[,2])+1):totalLength,combined_lower[(length(detrendedTrainData[,1])+1):(length(combined))],type="l",col=5,lty=3)
  lines((length(detrendedTrainData[,2])+1):totalLength,combined_higher[(length(detrendedTrainData[,1])+1):(length(combined))],type="l",col=6,lty=3)
	dev.off()
}


compute_periodogram <- function(detrendedTrainData,testData,damp,granularity,coeff,output_directory_path,file1)
{
    file1 <- file1
  testData <- testData
  #print(testData)
  output_directory_path = output_directory_path
  slope <- coeff[2]
  if(damp==1){
    temp = spec.pgram(detrendedTrainData[,2],spans=c(19,19))
  }else{
    temp = spec.pgram(detrendedTrainData[,2])
  }

  spectralFreq = temp$spec
  timeFrequency = temp$freq
  periodogramData <- cbind(spectralFreq,timeFrequency)
  #print(periodogramData)
  bandwidth = temp$bandwidth
  indexStep = as.integer((1/60)/bandwidth)
  if(granularity < 2){
  temp1 = periodogramData[1:indexStep,]
  }else
  {
  temp1 = periodogramData
  }
  
  	temp_freq_list <- NULL
	for(temp_freq_index in 1:length(periodogramData[,1]))
	{
		if(periodogramData[temp_freq_index,2] > 0.40)
		{
			temp_freq_list <- c(temp_freq_list,temp_freq_index)
		}
	
	}
	temp1 <- temp1[-temp_freq_list,]
	
  sortedTemp1 = temp1[order(temp1[,1],decreasing=TRUE),]
  #sortedTemp1 <- colnames("Spectral Energy","Inverse Frequency")
  top20periods <- 1/sortedTemp1[1:20,2]
  print(top20periods)
  #if(file1==9)
  
  for(ind in 1:length(top20periods))
  {
  if(round(top20periods[ind])-top20periods[ind] > 0.5)
  {
    top20periods[ind] = round(top20periods[ind])+1
  }else{
  top20periods = round(top20periods)
  }
  }
  print(top20periods)
  #rounded_vals <- NULL
  #for(i in 1:length(top20periods))
  #{
  # if(round(top20periods[i])!=top20periods[i])
  # {
  #   rounded_vals <- c(rounded_vals,round(top20periods[i]))  
  # }
  #}
  #top20periods <- c(top20periods,rounded_vals)
  error_list <- predict_next_bin_detrended(top20periods,detrendedTrainData,testData,granularity,slope,output_directory_path,file1)
  #best_period_index <- which(error_list==min(error_list))
  errors_and_periods <- cbind(top20periods,error_list)
  errors_and_periods  <- errors_and_periods[order(errors_and_periods[,2]),]
  best5errors <- errors_and_periods[1:5,]
  
  error_list <- as.data.frame(error_list)
  return(list(best5errors,error_list))
}

predict_next_bin_detrended <- function(top20periods,detrendedTrainData,testData,granularity,slope,output_directory_path,file1)
{
  testData <- as.data.frame(testData)
  index = 0
  error_list <- NULL
  for(i in 1: length(top20periods))
  {
  #if(file1 != 7 || i != 3)
  #{
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
  
  #print(temp) 
    
  predicted_bin = vector(,current_period)
  std_bin = vector(,current_period)
  folds = length(detrendedTrainData[,2])/current_period
  if((folds-round(folds))!=0) # check if folds is an integer
  {
  folds = as.integer(folds)
  folds = folds + 2
  }else{
  folds = folds + 1
  }
  for(k in 1: length(temp[,1]))
  {
  if(temp[k,folds]==0.000000)
  {
    #pred_temp = sum(temp[k,])
    #pred_temp = median(temp[k,],na.rm=TRUE)
    var1 = quantile(temp[k,], probs = c(0, 0.25, 0.5, 0.75, 1)) # quartile
      pred_temp = as.numeric(var1[3])
    #pred_temp = pred_temp/(folds-1)
    predicted_bin[k] = pred_temp
  }
  #pred_temp = sum(temp[k,])
  #pred_temp = median(temp[k,],na.rm=TRUE)
  var1 = quantile(temp[k,], probs = c(0, 0.25, 0.5, 0.75, 1)) # quartile
  pred_temp = as.numeric(var1[3])
  #pred_temp = pred_temp/folds
  predicted_bin[k] = pred_temp
  std_temp = sd(temp[k,],na.rm=TRUE)
  std_bin[k] = std_temp
  }
  lower_band = predicted_bin-std_bin 
  upper_band = predicted_bin + std_bin
  
  
  testData <- as.data.frame(testData)
  if(length(predicted_bin)<length(testData[,2]))
  {
    temp_list <- NULL
    temp_lower <- NULL
    temp_higher <- NULL
    repeat{
    if(length(temp_list) >= length(testData[,2]))
    {
      break
    }
    temp_list <- c(temp_list,predicted_bin)
    temp_lower <- c(temp_lower,lower_band)
    temp_higher <- c(temp_higher,upper_band)
    }
    #print(head(temp_list))
  }else{
    temp_list = predicted_bin
    temp_higher = upper_band
    temp_lower = lower_band
  }
  #print(temp_list)
  
  predicted_bin = temp_list[1:length(testData[,1])]
  upper_band = temp_higher[1:length(testData[,1])]
  lower_band = temp_lower[1:length(testData[,1])]
  
  combined = c(detrendedTrainData[,2],predicted_bin)
  combined_lower = c(detrendedTrainData[,2],upper_band)
  combined_higher = c(detrendedTrainData[,2],lower_band)
  
  gen_seq = seq(from = 0, by = (granularity*60), length.out = length(combined))
  combined = slope*gen_seq + combined
  combined_lower = slope*gen_seq + combined_lower
  combined_higher = slope*gen_seq + combined_higher

  
  predictedPlotsFolder = output_directory_path
  lmFileName = paste(predictedPlotsFolder,i," Custom with Period ")
  lmFileName = paste(lmFileName,top20periods[i])
    lmFileName = paste(lmFileName,".jpg")
  totalLength = length(detrendedTrainData[,2]) + length(testData[,2])
  jpeg(lmFileName)
  plot(1:length(detrendedTrainData[,2]),detrendedTrainData[,2],type="l",xlim=c(0,totalLength))
  lines((length(detrendedTrainData[,2])+1):totalLength,testData[,2],type="l",lty=1)
  lines((length(detrendedTrainData[,2])+1):totalLength,combined[(length(detrendedTrainData[,1])+1):(length(combined))],type="l",col="red",lty=1)
  lines((length(detrendedTrainData[,2])+1):totalLength,combined_lower[(length(detrendedTrainData[,1])+1):(length(combined))],type="l",col=5,lty=3)
  lines((length(detrendedTrainData[,2])+1):totalLength,combined_higher[(length(detrendedTrainData[,1])+1):(length(combined))],type="l",col=6,lty=3)
  #lines(combined[length(detrendedTrainData[,1]):length(combined)],type="l",col="red")
  dev.off()
  testData <- testData
  error <- calculate_errors(predicted_bin,testData)
  #print(error)
  error_list <- rbind(error_list,error)
  #print(error_list)
  }
  #}
  return(error_list)
}

calculate_errors <- function(predicted_bin,testData)
{
  testData <- as.data.frame(testData)
  if(length(predicted_bin) > length(testData[,2]))
  {
    error1 = testData[,2] - predicted_bin[1:(length(testData[,1]))]
  }else{
  error1 = testData[1:length(predicted_bin),2] - predicted_bin 
  }
  #---------- MdAPE-------------
  MdAPE = 0
  list_MdAPE <- NULL  
  for(k in 1:length(error1))
  {
    if(testData[k,2] != 0)
    {
      temp_err <- (abs(error1[k])/abs(testData[k,2]))
      list_MdAPE <- c(list_MdAPE,temp_err)
    }else{
    temp_err <-  abs(error1[k])
    list_MdAPE <- c(list_MdAPE,temp_err)    
    }
  }
  list_MdAPE <- remove_outliers_errors(list_MdAPE)
  print("Successful")
  MdAPE <- median(list_MdAPE)
  
  
  
  #------------MAPE--------------
  #MAPE =0
  #for(k in 1:length(error1))
  #{
  #if(testData[k,2] != 0)
  #{
  #MAPE = MAPE + (abs(error1[k])/abs(testData[k,2]))   
  #}
  #else 
  #{
  #MAPE = MAPE + abs(error1[k])
  #}
  #}
  #MAPE = MAPE/length(error1)
  #error_matrix <- matrix(data = 0,nrow = 20)
  #temp_vec = c(temp_vec,MAPE)
  #print(MAPE)
  print(MdAPE)
  #print(error_matrix)
  return(MdAPE)
 
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
  predictedPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Predicted_plots_1\\"
  lmFileName = paste(predictedPlotsFolder,"Prediction_with_period_AR")
  lmFileName = paste(lmFileName,top20periods[i])
    lmFileName = paste(lmFileName,".jpg")
  jpeg(lmFileName)
  plot(combined,type="l",)
  lines(combined[length(detrendedTrainData[,1]):(length(combined))],type="l",col="red")
  #lines(combined[length(detrendedTrainData[,1]):length(combined)],type="l",col="red")
  dev.off()

}

check_granularity <- function(relevantData)
{
  granularity <- difftime(relevantData[2,1],relevantData[1,1],units="mins")
  print(granularity)
  if(granularity > 720){
    damp = FALSE
  }else{
  damp = TRUE
  }
  return(list(damp,granularity))
}

remove_outliers_errors <- function(list_values)
{
  relevantData <- list_values
  mean_whole_data = mean(relevantData)
  std_whole_data = sd(relevantData) 
  lower_threshold_for_outliers = mean_whole_data - 3*std_whole_data
  #print(lower_threshold_for_outliers)
  upper_threshold_for_outliers = mean_whole_data + 3*std_whole_data
  #print(upper_threshold_for_outliers)
  size_data = length(relevantData)
  temp_relevant <- NULL
  
  for(index1 in 1:size_data){
    #print(index1)
    if(index1 > size_data){
    next
    }
    if(relevantData[index1] < lower_threshold_for_outliers || relevantData[index1] > upper_threshold_for_outliers)
      {
        #print("outlier removed")
        #print(index1)
        temp_relevant <- c(temp_relevant,index1)
      }
  }
  if(!is.null(temp_relevant)){
    relevantData <- relevantData[-temp_relevant]
  }
  return(relevantData)
  }


}

remove_outliers <- function(relevantData)
{
  if(length(relevantData) > 1)
  {
  mean_whole_data = mean(relevantData[,2])
  std_whole_data = sd(relevantData[,2])
  
  lower_threshold_for_outliers = mean_whole_data - 3*std_whole_data
  upper_threshold_for_outliers = mean_whole_data + 3*std_whole_data
  
  size_data = length(relevantData[,2])
  temp_relevant <- NULL
  for(index1 in 1:size_data){
    #print(index1)
    if(index1 > size_data){
    next
    }
    if(relevantData[index1,2] < lower_threshold_for_outliers)
      {
        print("outlier detected")
        #print(index1)
        relevantData[index1,2] = lower_threshold_for_outliers
      }else{
        if(relevantData[index1,2] > upper_threshold_for_outliers)
        {
          print("outlier detected")
          relevantData[index1,2] = upper_threshold_for_outliers
        }
      }
  }
  }
  return(relevantData)
}

manage_holes <- function(relevantData)
{


}



fileNames = list.files(path = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Input data\\",pattern=".csv",full.names=TRUE)
temp_vec <- NULL
col_names <- NULL
for(file1 in 1:length(fileNames))
{ 
	  print(fileNames[file1])
	  # ----reads data from csv----
	  data = read.csv(fileNames[file1],sep=",")
	  relevantData = data[,1:2]
	  temp = relevantData[,1]
	  
	  # ----convert data into required format----
	  temp = strptime(temp,format=CONST_DATE_FORMAT)
	  relevantData[,1] = as.POSIXct(temp)
	  # ----Check Granularity of data-------
	  tempList <- check_granularity(relevantData)
	  tempList <- unlist(tempList)
	  granularity <- tempList[2]
	  damp <- tempList[1]
	  colnames(relevantData) <- c("Time Stamp","CPU Util")
	  #---- plot data -----
	  plot_data(relevantData)
	  #------ remove outliers from data -----------
	  relevantData <- remove_outliers(relevantData)
	  
	  #----- Divide data into train and test  -------
	  trainPlusTest <- prepare_data(relevantData)
	  trainData <- trainPlusTest[1]
	  testData <- trainPlusTest[2]
	  # ------ fit LM and return the coefficients ---
	  coeff <- fit_lm_plot(trainData)
	  trainData = as.data.frame(trainData)
	  # ------ Detrend the data --------
	  detrendedTrainData <- detrend(coeff,trainData)
	  #------------ Set output Directory path ----------
	  output_folder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Input data\\"
	  subDirectory = paste("Output_",file1,sep="")
	  dir.create(file.path(output_folder,subDirectory),showWarnings=FALSE)
	  output_directory_path = paste("C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Input data\\Output_",file1,sep="")
	  output_directory_path = paste(output_directory_path,"\\",sep="")
	  # --------- The periodogram approach, returns errors of this approach and the best 5 periods --------
	  bestErrorIndexAnderror_list <- compute_periodogram(detrendedTrainData,testData,damp,granularity,coeff,output_directory_path,file1)
	  best5errors <- bestErrorIndexAnderror_list[1]
	  best5errors <- as.data.frame(best5errors)
	  error_list <- bestErrorIndexAnderror_list[2]
	  error_list <- as.data.frame(error_list)
	  testData <- as.data.frame(testData)
	  error_list = error_list$V1
	  min_custom_index <- which(error_list == min(error_list))
	  print(min_custom_index)
	  if(length(min_custom_index)>1)
	  {
		  min_custom_value = error_list[min_custom_index[1]]
	  }else{
		min_custom_value <- error_list[min_custom_index]
	  }
	  print(min_custom_value)
	  #-------- Apply Auto Arima with top 5 periods retrieved from the periodogram approach, returns 
	  # -error list for each period ------
	  error_list_arima <- apply_AutoArima(trainData,best5errors,testData,output_directory_path,file1)
	  # ------- Apply HoltWinters with top 5 periods retireved from periodogram approach, returns li
	  #-st of errors----------
	  min_arima_index = which(error_list_arima == min(error_list_arima))
	  if(length(min_arima_index) > 1)
	 {
		min_arima_value <- error_list_arima[min_arima_index[1]]
	 }else{
		min_arima_value <- error_list_arima[min_arima_index]

	  }

	  
	  if(min_custom_value < min_arima_value)
	  {
		min_whole = min_custom_value
	  }else{
	  min_whole = min_arima_value
	  }
	  
	  error_hw <- apply_HoltWinters(trainData,best5errors,testData,output_directory_path,file1)
	   min_hw_index <- which(error_hw == min(error_hw,na.rm=TRUE))
	  if(min_hw_index >1)
	  {
		min_hw_value = error_hw[min_hw_index[1]]
	  }else{
		min_hw_value <- error_hw[min_hw_index]
	  }
	  

	  if(min_whole < min_hw_value)
	  {
		min_whole = min_whole
	  }else{
	  min_whole = min_hw_value
	  }
	  
	  print(min_whole)
	  if(min_whole == min_arima_value)
	  {
		print(paste("Arima is best",min_arima_index,"Period of ",best5errors[min_hw_index,1]))
		final_AutoArima(min_arima_index,trainData,testData,best5errors,file1)
	  }else{
		if(min_whole== min_hw_value){
		  print(paste("Holt Winters is best",min_hw_index,"Period of ",best5errors[min_hw_index,1]))
		  final_HoltWinters(min_hw_index,trainData,testData,best5errors,file1)
		}else{
		  if(min_custom_value == min_whole){
			print(paste("Custom algo does best",min_custom_index,"period of",best5errors[min_custom_index,1]))
			final_compute_periodogram(detrendedTrainData,testData,damp,granularity,coeff,min_custom_index,file1)
		  }
		}
	  }
	  error_list_arima <- as.data.frame(error_list_arima)
	  error_list <- c(error_list,error_list_arima)
	  error_hw <- as.data.frame(error_hw)
	  error_list <- c(error_list,error_hw)
	  temp = c(error_list$V1,error_list$error_list_arima,error_list$error_hw)
	  temp_vec <- cbind(temp_vec,temp)
	  print(paste("Out of file..",file1))
	  col_names <- c(col_names,file1)

}
colnames(temp_vec) <- col_names
# write results to the output file
write.table(temp_vec,"C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\Input data\\Results\\results1.csv",append=FALSE,sep=",",col.names=TRUE,row.names = FALSE)


# another approach to find the best frequency
#period <- find.freq_AR(detrendedTrainData[,2])
#predict_next_bin_AR(detrendedTrainData,period)
