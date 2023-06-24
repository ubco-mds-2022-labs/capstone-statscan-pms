# this script produces the categorical summary table for the categorical variables in the PMD

# load data
load('../../../../local_data/codes/create_master/master_pms_df.Rdata')

vars = c('PMS_PRNAME', 'PMS_CMATYPE', 'PMS_amenity_dense', 'PMS_suppressed')

for (i in vars){
	print(i)
	print(table(master[,i], useNA = 'always'))
}
