print_error <- function(fit1)
{
  error1 = arima.errors(fit1)

  for(k in 1:length(error1))
  {
  if(ts1[k] != 0)
  {
  MAPE = MAPE + (abs(error1[k])/ts1[k])   
  }
  else 
  {
  MAPE = MAPE + abs(error1[k])
  }
  }
  MAPE = MAPE/length(error1)
print(MAPE)
}

ts1 = rep(1000,1000)
MAPE = 0

for(i in 1:length(ts1))
{
	if(i %% 5 == 3 || i %% 5 == 4)
	{
		ts1[i] = 20
	}
}

fit1 = auto.arima(ts1[0:800])
predict(fit1,n.ahead=length(ts1))
print_error(fit1)


lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Auto Arima")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(fit1)[0:50],col="red",type="l")
dev.off()

ts1Arima1 = arima(ts1[1:800],c(1,0,0))
print_error(ts1Arima1)

lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"ARIMA 100")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(ts1Arima1)[0:50],col="red",type="l")
dev.off()


ts1Arima2 = arima(ts1[1:800],c(2,0,0))
print_error(ts1Arima2)

lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Arima 200")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="ARIMA 300")
lines(fitted(ts1Arima2)[0:50],col="red",type="l")
dev.off()

ts1Arima3 = arima(ts1[1:800],c(3,0,0))
print_error(ts1Arima3)
lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Arima 300")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(ts1Arima3)[0:50],col="red",type="l")
dev.off()


ts1Arima4 = arima(ts1[1:800],c(1,0,1))
print_error(ts1Arima4)
lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Arima 101")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(ts1Arima4)[0:50],col="red",type="l")
dev.off()

ts1Arima5 = arima(ts1[1:800],c(1,0,2))
print_error(ts1Arima3)
lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Arima 102")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(ts1Arima5)[0:50],col="red",type="l")
dev.off()

ts1Arima6 = arima(ts1[1:800],c(1,0,3))
print_error(ts1Arima3)
lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Arima 103")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(ts1Arima6)[0:50],col="red",type="l")
dev.off()





ts1Arima7 = arima(ts1[1:800],c(2,0,1))
print_error(ts1Arima7)
lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Arima 201")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(ts1Arima7)[0:50],col="red",type="l")
dev.off()

ts1Arima8 = arima(ts1[1:800],c(2,0,2))
print_error(ts1Arima8)
lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Arima 202")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(ts1Arima8)[0:50],col="red",type="l")
dev.off()

ts1Arima9 = arima(ts1[1:800],c(2,0,3))
print_error(ts1Arima9)
lmPlotsFolder = "C:\\Users\\735201\\Desktop\\Sanketh-Test\\CPU_Util_data\\ts3\\"
lmFileName = paste(lmPlotsFolder,"Arima 203")
lmFileName = paste(lmFileName,".jpg")
jpeg(lmFileName)
plot(ts1[0:50],type="l",main="Auto Arima ts2")
lines(fitted(ts1Arima9)[0:50],col="red",type="l")
dev.off()
