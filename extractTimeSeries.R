data1 = read.csv("C:\\Users\\SaiSakanki\\Desktop\\TRDDC stuff\\coding\\Dump.csv",sep=",")
jobsList = c("calc_balance_sheet","sdsetx_mi_aacm_ds1700","sdsetx_actimize_rocs","sdsetx_mi_ptb_acct","sbwpch_extr_gl_bal_full","sbwpch_extr_card_md_delta","sdsetl_addr_sup_cards",
"sdsetx_mi_aaci_ds0200","proc_collect_flag_file","proc_card_create_cancel","sdsetl_visa_base_ii","load_visa_base_ii_file","sbwpch_extr_ce_feat_delta","sbwpch_extr_events_delta_1")
for( job in 1:length(jobsList))
{
	current_job = jobsList[job]
	index_list = which(data1$jobname == current_job)
	start_index = index_list[1]
	end_index = index_list[length(index_list)]
	relevant_data_runtime = data1$runtime[start_index:end_index]
	temp = data1$starttime[start_index:end_index]
	temp = as.character(temp)
	relevant_data = cbind(temp,relevant_data_runtime)
	name_of_file = paste(current_job,".csv",sep="")
	path = "C:\\Users\\SaiSakanki\\Desktop\\TRDDC stuff\\coding\\datasets\\"
	path = paste(path,name_of_file,sep="")
	write.table(relevant_data,path,append=FALSE,sep=",",row.names=FALSE)
}