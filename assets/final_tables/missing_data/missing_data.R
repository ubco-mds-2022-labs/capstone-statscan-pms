#this script creates the missing value table for the numerical variables in the PMD
library(xtable)

# load data
load('../../../../local_data/codes/create_master/master_pms_df.Rdata')

#convert dbpop to numerical
master[,'PMS_DBPOP'] = as.numeric(as.character(master[,'PMS_DBPOP']))


# amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

# numerical variables
num_vars = c(amenities, 'PMS_DBPOP')


# labels
labs = c('Employment', 'Pharmacy', 'Childcare', 'Healthcare', 'Grocery', 'Pri. Educ.', 'Sec. Educ.', 'Library', 'Parks', 'Transit')


#creating df
missing_data = data.frame()

for (i in num_vars){
	#complete values
	missing_data[i,1] = formatC(length(na.omit(master[,i])), , format="d", big.mark=",")
	#percentage
	missing_data[i,2] = format(round(100*(length(na.omit(master[,i])))/(nrow(master)), 1), nsmall=1)
}


#convert numbers to strings to prevent rounding
missing_data = apply(missing_data, 1:2, as.character)



#change row and col names
colnames(missing_data) = c('DBs with Data Available', 'Percentage')
row.names(missing_data) = c(labs, "DB Pop.")




#export
print(missing_data)
print(xtable(missing_data, type = "latex"), file = "missing_data.txt")





