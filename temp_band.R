CONST_SD_MULTIPLIER = 3
CONST_INPUT_TIME_FORMAT = "%m/%d/%Y %H:%M"
CONST_SERVER_COLUMN_NUMBER = 1
CONST_TIMESTAMP_COLUMN_NUMBER = 2
#CONST_REQUIRED_COLUMN_NUMBERS = c(3,6,7,8,9,10,11,12,13)
CONST_REQUIRED_COLUMN_NUMBERS = c(3)
CONST_INPUT_FILE_PATH = c("servers_cpu_weekday_weekend_pattern.csv")
CONST_OUTPUT_FILE_PATH = c("/home/praveen/Coding/TRDDC/Morgan/new_stuff/hour_of_week")  #KEEP THIS AS OUTPUT DIRECTORY
CONST_CPU_UTILIZATION_COLUMN = 3

hdrm_cm_remove_outliers <- function(metric_data)
{
   deviation_value = sd(metric_data)
   if(is.na(deviation_value))
      deviation_value = 0
   upper_outlier_threshold = mean(metric_data) + CONST_SD_MULTIPLIER*deviation_value
   lower_outlier_threshold = mean(metric_data) - CONST_SD_MULTIPLIER*deviation_value
   values_result =  metric_data[which(metric_data >= lower_outlier_threshold & metric_data <= upper_outlier_threshold)]    
   return(values_result)
}


read_data = read.csv(CONST_INPUT_FILE_PATH,header=TRUE,sep=",")
col_names = colnames(read_data)
read_data = as.matrix(read_data)


server_name_list = unique(read_data[,CONST_SERVER_COLUMN_NUMBER])
timestamp = read_data[,CONST_TIMESTAMP_COLUMN_NUMBER]
server_data = read_data[,CONST_SERVER_COLUMN_NUMBER]
setwd(CONST_OUTPUT_FILE_PATH)

for(j in 1:length(col_names))
{		

	print(j)
	if(!(j %in% CONST_REQUIRED_COLUMN_NUMBERS))
		next
	output_df = data.frame("entityName"=c(),"timestamp"=c(),"runtime"=c(),"trendline"=c(),"lowertrendline"=c(),
			"uppertrendline"=c(),"threshold"=c(),
			"monthmarks"=c(),"prediction"=c(),"outlierFlag"=c())
	output_summary_df = data.frame("entityName" = c(),"currentMetricValue"=c(),"metricThreshold"=c(),"saturationdate"=c(),
			"valueafter3months"=c(),"valueafter6months"=c(),"valueafter9months"=c())
	for(k in 1:length(server_name_list))
	{
		
		#print(k)
		current_server = as.character(server_name_list[k])
		
		metric_value = as.numeric(read_data[,j][which(read_data[,CONST_SERVER_COLUMN_NUMBER]==current_server)])
		#last_changepoint = hdrm_cm_find_changepoints(metric_value)
		last_changepoint = 0
		timestamp = read_data[,CONST_TIMESTAMP_COLUMN_NUMBER][which(read_data[,CONST_SERVER_COLUMN_NUMBER]==current_server)]
		timestamp = strptime(timestamp,format=CONST_INPUT_TIME_FORMAT)
		original_metric = metric_value
		original_timestamp = timestamp
		metric_value = metric_value[(last_changepoint+1):length(metric_value)]
		
		#print(original_timestamp)
		processed_timestamp = strftime(original_timestamp,format="%Y-%m-%d %H")
		unique_processed_timestamp = unique(processed_timestamp)
		output_metric = c()
		for(p in 1:length(unique_processed_timestamp))
		{
			required_metric_values = metric_value[which(processed_timestamp==unique_processed_timestamp[p])]
			#quantile_value = floor(quantile(required_metric_values,0.75))
			quantile_value = max(required_metric_values)
			output_metric = c(output_metric,quantile_value)
		}
		unique_processed_timestamp = strptime(unique_processed_timestamp,format="%Y-%m-%d %H")
		df = data.frame(unique_processed_timestamp,output_metric)
		original_timestamp = unique_processed_timestamp
		original_metric = output_metric

		if(j==CONST_CPU_UTILIZATION_COLUMN)
			final_threshold = 75
		if(j!=CONST_CPU_UTILIZATION_COLUMN)
			final_threshold = mean(metric_value) + (2*sd(metric_value))

		timestamp = timestamp[(last_changepoint+1):length(metric_value)]

		timestamp_day = as.numeric(strftime(timestamp,format="%u"))
		timestamp_hour = as.numeric(strftime(timestamp,format="%H"))
		required_hour_of_week = ((timestamp_day-1) * 24) + timestamp_hour



		processed_timestamp = strftime(timestamp,format="%H:%M")
		processed_hour_timestamp = strftime(timestamp,format="%H")
		metric_value_without_outlier = hdrm_cm_remove_outliers(metric_value)
		fit_without_outlier = seq(1:length(metric_value_without_outlier))
		fit_sequence = seq(1:length(metric_value))
		fit = lm(metric_value_without_outlier~fit_without_outlier)
		slope_value = fit$coefficients[[2]]
		intercept_value = fit$coefficients[[1]]

		#unique_timestamp_values = strftime(timestamp,format="%H")
		unique_timestamp_values = unique(required_hour_of_week)

		metric_value_after_detrending = metric_value - (slope_value * fit_sequence)

		threshold_value = c()
		representative_value = c()
		temp_uppertrendline = c()
		temp_lowertrendline = c()
		for(i in 1:length(unique_timestamp_values))
		{
			current_timestamp = unique_timestamp_values[i]
			index = which(required_hour_of_week==current_timestamp)
			current_timestamp_metrics = metric_value_after_detrending[index]
			threshold_timestamp_metrics = metric_value[index]
			upper_threshold = mean(current_timestamp_metrics) + (3*sd(current_timestamp_metrics))
			lower_threshold = mean(current_timestamp_metrics) - (3*sd(current_timestamp_metrics))
			#current_timestamp_metrics = current_timestamp_metrics[which(current_timestamp_metrics>=lower_threshold & current_timestamp_metrics<=upper_threshold)]
			current_timestamp_metrics = current_timestamp_metrics[which(current_timestamp_metrics>=lower_threshold)]
			#representative_value = c(representative_value,mean(current_timestamp_metrics))
			#temp_value = floor(quantile(current_timestamp_metrics,0.75))
			temp_value = max(current_timestamp_metrics)
			representative_value = c(representative_value,temp_value)
			temp_value = floor(quantile(current_timestamp_metrics,0.5))
			temp_lowertrendline = c(temp_lowertrendline,temp_value)
			temp_value = floor(quantile(current_timestamp_metrics,0.9))
			temp_uppertrendline = c(temp_uppertrendline,temp_value)
			upper_threshold = mean(threshold_timestamp_metrics) + (3*sd(threshold_timestamp_metrics))
			lower_threshold = mean(threshold_timestamp_metrics) - (3*sd(threshold_timestamp_metrics))
			threshold_timestamp_metrics = threshold_timestamp_metrics[which(threshold_timestamp_metrics>=lower_threshold & threshold_timestamp_metrics<=upper_threshold)]
			threshold_value = c(threshold_value,mean(threshold_timestamp_metrics) + (2*sd(threshold_timestamp_metrics)))

		}



		df = data.frame(unique_timestamp_values,representative_value)

		final_timestamp = required_hour_of_week[length(required_hour_of_week)]
		output_timestamp = timestamp[length(timestamp)]
		output_timestamp = strftime(strptime(output_timestamp,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d %H")
		output_timestamp = strptime(output_timestamp,format="%Y-%m-%d %H")
		timestamp_index = which(unique_timestamp_values==final_timestamp)
		forecast_timestamp = c()
		forecast_values = c()
		forecast_threshold = c()
		output_forecast_timestamp = c()
		lowertrendline = c()
		uppertrendline = c()
		prior_forecast_values = c()

		for(p in 1:3)
		{
		for(i in (timestamp_index+1):length(unique_timestamp_values))
		{	
			forecast_values = c(forecast_values,representative_value[i])
			lowertrendline = c(lowertrendline,temp_lowertrendline[i])
			uppertrendline = c(uppertrendline,temp_uppertrendline[i])
			forecast_timestamp = c(forecast_timestamp,unique_timestamp_values[i])
			forecast_threshold = c(forecast_threshold,threshold_value[i])
			output_forecast_timestamp = c(output_forecast_timestamp,as.character(output_timestamp+3600))
			output_timestamp = output_timestamp + 3600
		}
		for(i in 1:timestamp_index)
		{
			forecast_values = c(forecast_values,representative_value[i])
			lowertrendline = c(lowertrendline,temp_lowertrendline[i])
			uppertrendline = c(uppertrendline,temp_uppertrendline[i])
			forecast_timestamp = c(forecast_timestamp,unique_timestamp_values[i])
			forecast_threshold = c(forecast_threshold,threshold_value[i])
			output_forecast_timestamp = c(output_forecast_timestamp,as.character(output_timestamp+3600))
			output_timestamp = output_timestamp + 3600
		}
		}

		#df = data.frame(output_forecast_timestamp,forecast_values)

		for(i in 1:length(processed_timestamp))
		{
			index = which(unique_timestamp_values==processed_timestamp[i])
			prior_forecast_values = c(prior_forecast_values,representative_value[index])
		}

		forecasting_sequence = seq((length(metric_value)+1),(length(metric_value)+length(forecast_values)),by=1)
		forecast_values = forecast_values + (slope_value * forecasting_sequence)
		forecast_values[which(forecast_values<0)] = 0

		lowertrendline = lowertrendline + (slope_value * forecasting_sequence)
		uppertrendline = uppertrendline + (slope_value * forecasting_sequence)
		lowertrendline[which(lowertrendline<0)] = 0

		prior_forecasting_sequence = seq(1:length(processed_timestamp))
		prior_forecast_values = prior_forecast_values + (slope_value * prior_forecasting_sequence)
		prior_forecast_values[which(prior_forecast_values<0)] = 0


		original_values = original_metric[1:length(prior_forecast_values)]
		difference_vector = abs(original_values - prior_forecast_values)
		required_sd = sd(difference_vector)

		output_uppertrendline = c(rep("",length(original_metric)),uppertrendline)
		output_lowertrendline = c(rep("",length(original_metric)),lowertrendline)

		final_df = data.frame(forecast_timestamp,forecast_values,forecast_threshold)
		#print(which(final_df$forecast_values>final_df$forecast_threshold))
		final_output = c(original_metric,forecast_values)
		#print(final_output)
		sequence_output = seq(1:length(final_output))
		output_trendline = (slope_value*sequence_output) + intercept_value
		timestamp = as.character(strftime(timestamp,format="%m/%d/%Y %H:%M"))

		output_forecast_timestamp = strptime(output_forecast_timestamp,format="%Y-%m-%d %H:%M:%S")	
		na_index = which(is.na(output_forecast_timestamp))
		for(i in 1:length(na_index))
		{
			reqd_date = strftime(output_forecast_timestamp[na_index[i]+1],format="%Y-%m-%d")
			output_value = paste0(reqd_date," 00:00:00",collapse="")
			output_value = strptime(output_value,format="%Y-%m-%d %H:%M:%S")
			output_forecast_timestamp[na_index[i]] = output_value
		}
		
		output_forecast_timestamp = as.character(strftime(output_forecast_timestamp,format="%m/%d/%Y %H:%M"))
		original_timestamp = strftime(original_timestamp,format="%m/%d/%Y %H:%M")
		final_timestamp = c(as.character(original_timestamp),as.character(output_forecast_timestamp))
		monthmark_limit = length(output_forecast_timestamp)/3
		
		threeday_value = forecast_values[monthmark_limit]
		sixday_value = forecast_values[2*monthmark_limit]
		nineday_value = forecast_values[3*monthmark_limit]
		monthmark_limit = monthmark_limit-1
		final_monthmarks = c(rep("",length(original_metric)),rep("",monthmark_limit),"3_days",rep("",monthmark_limit),"6_days",rep("",monthmark_limit),"9_days")
		
		outlier_flag = rep("FALSE",length(forecast_values))
		outlier_flag[which(forecast_values>final_threshold)] = "TRUE"
		final_outlier_flag = c(rep("FALSE",length(original_metric)),outlier_flag)

		final_df = data.frame("entityName"=rep(current_server,length(output_trendline)),"timestamp"=final_timestamp,
			"runtime"=c(original_metric,rep("",length(forecast_values))),"trendline"=output_trendline,"lowertrendline"=output_lowertrendline,
			"uppertrendline"= output_uppertrendline,"threshold"=rep(final_threshold,length(output_trendline)),
			"monthmarks"=final_monthmarks,"prediction"=c(rep("",length(original_metric)),forecast_values),"outlierFlag"=final_outlier_flag)
		final_summary_df = data.frame("entityName" = current_server,"currentMetricValue"="","metricThreshold"=final_threshold,"saturationdate"="",
			"valueafter3months"=threeday_value,"valueafter6months"=sixday_value,"valueafter9months"=nineday_value)
		
		final_df = final_df[!is.nan(final_df$prediction),]
		#output_df = rbind(output_df,final_df)			
		output_summary_df = rbind(output_summary_df,final_summary_df)

		subDir = current_server
		mainDir = CONST_OUTPUT_FILE_PATH
		if (file.exists(subDir)){
 		   setwd(file.path(mainDir, subDir))
		} else {
    	dir.create(file.path(mainDir, subDir))
    	setwd(file.path(mainDir, subDir))
		}

		output_file_string_total = paste0(col_names[j],"_entityForecasting.csv") 
		write.table(final_df,file=output_file_string_total,sep=",",row.names=FALSE)
		setwd('..')
	}
			
	output_file_string = paste0(col_names[j],"_entityForecastingSummary.csv")
	write.table(output_summary_df,file=output_file_string,sep=",",row.names=FALSE)
		
}
