# this script produces the summary table for the amenities
library(moments)
library(xtable)

# load data
load('../../../../local_data/codes/create_master/master_pms_df.Rdata')

#convert dbpop to numerical
master$PMS_DBPOP = as.numeric(gsub("[^0-9.-]", "", as.character(master$PMS_DBPOP)))

# amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

# numerical variables
num_vars = c(amenities, 'PMS_DBPOP')

# labels
labs = c('Employment', 'Pharmacy', 'Childcare', 'Healthcare', 'Grocery', 'Pri. Educ.', 'Sec. Educ.', 'Library', 'Parks', 'Transit')




#producing table
summary_table = data.frame()


for (i in num_vars){
	#deciles
	summary_table[i, 1:9] = round(quantile(master[,i], probs = seq(.1, .9, by = .1), na.rm=T), 5)
	#min, mean, median, max, std dev
	summary_table[i, 10:14] = c(
		round(min(master[,i], na.rm=T), 6),
		round(median(master[,i], na.rm=T), 5),
		round(mean(master[,i], na.rm=T), 5),
		round(max(master[,i], na.rm=T), 5),
		round(sd(master[,i], na.rm=T), 4)
	)
	#skew and kurtosis
	summary_table[i, 15:16] = c(
		round(skewness(master[,i], na.rm=T), 3),
		round(kurtosis(master[,i], na.rm=T), 2)
	)
}


#round dbpop column
summary_table[11,] = format(round(summary_table[11,]), nsmall=0)


#convert numbers to strings to prevent rounding
summary_table = apply(summary_table, 1:2, as.character)


#change row and col names
colnames(summary_table) = c(paste(1:9, 'Dec.'), 'Min.', 'Median', 'Mean', 'Max.', 'Std. Dev.', 'Skew', 'Kurtosis')
row.names(summary_table) = c(labs, "DB Pop.")


#flip rows and cols
summary_table = t(summary_table)


#export 
print(summary_table)
print(xtable(summary_table, type = "latex"), file = "summary_table.txt", floating = TRUE, floating.environment = "sidewaystable")

