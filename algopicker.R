data = read.csv("C:\\Users\\SaiSakanki\\Desktop\\TRDDC Stuff\\coding\\Dump.csv")
jobsList = data[,3]
uniqueJobsList = unique(jobsList)
#fitSummary <- c(0)

plot_jobs <- function(currentData)
{
  ###### This Plots the whole data ##########
  plotsFolder = "C:\\Users\\SaiSakanki\\Desktop\\TRDDC Stuff\\coding\\SamplePlots\\"
  fileName = paste(plotsFolder,uniqueJobsList[i])
  fileName = paste(fileName,".jpg")
  currentData= as.data.frame(currentData)
  jpeg(fileName)
  plot(strptime(currentData[,1],format="%Y-%m-%d %H:%M:%S"), as.numeric(currentData[,2]),type="l",xlab="Time Stamp",ylab="Run Time",main=paste(uniqueJobsList[i],"'s Plot"))
  dev.off() 
  
}

fit_lm_plot <- function(currentData)
{
  ############## Takes training data and tries to fit LM to the same #######################
  xValuesForFit = 1:length(currentData)
  #fit <- lm(as.numeric(currentData[,2])~currentData[,1],data = currentData)
  #fitSummary = coefficients(fit)
  #print(coefficients(fit))
  
  temp = cbind(xValuesForFit,currentData[,2])
  
  lmPlotsFolder = "C:\\Users\\SaiSakanki\\Desktop\\TRDDC Stuff\\coding\\FitPlots\\"
  lmFileName = paste(lmPlotsFolder,uniqueJobsList[i])
  lmFileName = paste(lmFileName,".jpg")
  jpeg(lmFileName)
  plot(strptime(currentData[,1],format="%Y-%m-%d %H:%M:%S"), as.numeric(currentData[,2]),type="l",xlab="Time Stamp",ylab="Run Time",main=paste(uniqueJobsList[i],"'s Plot"))
  abline(lm(as.numeric(currentData[,2])~currentData[,1],data = currentData),col="red")
  dev.off()
  
  fit <- lm(as.numeric(currentData[,2])~currentData[,1],data = currentData,na.action = na.exclude)
  coeff <- coefficients(fit)
  return(coeff)
}

divide_data <- function(newArray)
{
  trainData <- newArray[1:(0.8*length(newArray[,1])),]
  testData <- newArray[(0.8*length(newArray[,1])):(length(newArray[,1])),]
  return(list(trainData,testData))
  
}

# evaluate_test_data <- function(coeff,testData)
# {
#     
# }
prepare_data_for_lm_plot <-function(currentData)
{
  minTime = min(currentData[,1])
  maxTime = max(currentData[,1])
  generatedSequence = seq(minTime,maxTime,"min")
  emptyArray = matrix(,nrow=length(generatedSequence),ncol=1)
  for(j in 1:length(generatedSequence))
  {
    index = which(currentData[,1] == generatedSequence[j])
    if(length(index)!=0){
    emptyArray[j]=currentData[index,2]
    }
      }
  newArray <- cbind(as.character(generatedSequence),emptyArray)
  #print(newArray)
  testPlusTrain <- divide_data(newArray)
  return(testPlusTrain)
}


for( i in 1:length(uniqueJobsList))
{
  if(i!=173)
  {
	indexes = which(data[,3]==uniqueJobsList[i])
	#print(indexes)
	currentData = data[indexes, c(4,8)]
	temp = currentData[,1]
	temp = strptime(as.character(temp),format = "%m/%d/%Y %H:%M")
  currentData[,1] = as.POSIXct(temp)
  currentData = currentData[order(strptime(currentData[,1],format="%Y-%m-%d"),decreasing = FALSE),]
	
  #currentData[,1] = strptime(currentData[,1],"%Y-%m-%d %H:%M:%S")
  plot_jobs(currentData)
  testPlusTrain <- prepare_data_for_lm_plot(currentData)
  currentData <- testPlusTrain[1]
  testData <- testPlusTrain[2]
  coeff <- fit_lm_plot(currentData)
  #currentData <- prepare_data_for_lm_plot(currentData)
  
  #evaluate_test_data(coeff,testData)
  
  }

}