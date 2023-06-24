#this script produces a table that counts the number of outliers before and after log transforming the data 
library(xtable)

# load data
load('../../../../local_data/codes/create_master/master_pms_df.Rdata')

# amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

# labels
labs = c('Employment', 'Pharmacy', 'Childcare', 'Healthcare', 'Grocery', 'Pri. Educ.', 'Sec. Educ.', 'Library', 'Parks', 'Transit')



#producing table
summary_table = data.frame()


for (i in amenities){
	num_out = length(boxplot(master[,i], plot=F)$out)
	summary_table[i, 1:2] = c(
		formatC(num_out, format="d", big.mark=","),
		format(round(100*num_out/nrow(master), 2), nsmall=2)
	)
	#log data
	logdata = log(master[,i]+0.0001)
	
	num_out = length(boxplot(logdata, plot=F)$out)
	summary_table[i, 3:4] = c(
		formatC(num_out, format="d", big.mark=","),
		format(round(100*num_out/nrow(master), 2), nsmall=2)
	)
}




#change row and col names
colnames(summary_table) = c('Counts', 'Percentages', 'Log Transformed Counts', 'Log Transformed Percentages')
row.names(summary_table) = labs




#export 
print(summary_table)
print(xtable(summary_table, type = "latex"), file = "outlier_table.txt")



