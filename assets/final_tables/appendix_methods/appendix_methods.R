#this script produces the tables that show the results from clustering results that are in the appendix  
library(xtable)

# amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

# labels
labs = c('Employment', 'Pharmacy', 'Childcare', 'Healthcare', 'Grocery', 'Pri. Educ.', 'Sec. Educ.', 'Library', 'Parks', 'Transit')


#import & organize data

#approach names
anames = c('MCLUST', 'PAM k-means', 'MixAll')

#cutoffs
cut_offs_mclust = list(
  PMS_prox_idx_emp = c(0.00004142133, 0.0004477224, 0.001249074, 0.003349638, 0.008549853, 0.02064994, 0.05184998, 0.1629499),
  PMS_prox_idx_pharma = c(0.006417666, 0.010799537, 0.018149931, 0.033198664, 0.055421553, 0.011361244),
  PMS_prox_idx_childcare = c(0.001874842, 0.066850014),
  PMS_prox_idx_health = c(0.0002464102, 0.0034496474, 0.0093498633),
  PMS_prox_idx_grocery = c(0.011689830, 0.007219839),
  PMS_prox_idx_educpri = c(0.02349916, 0.02650828, 0.04439987, 0.09005003, 0.13115005, 0.18500001),
  PMS_prox_idx_educsec = c(0.03474969, 0.03459509, 0.04384998, 0.06179871, 0.10114965, 0.14344998, 0.08546577),
  PMS_prox_idx_lib = c(0.04879960, 0.05379961, 0.06824909, 0.09274833, 0.11634910, 0.04169711),
  PMS_prox_idx_parks = c(0.08254624, 0.01594993, 0.03244996, 0.04634999, 0.06235000, 0.08445002, 0.12194994),
  PMS_prox_idx_transit = c(0.001048913, 0.011649895)
)
cut_offs_kmeans_pam = list(
  PMS_prox_idx_emp = c(0.003549657),
  PMS_prox_idx_pharma = c(0.02634995),
  PMS_prox_idx_childcare = c(0.03644997),
  PMS_prox_idx_health = c(0.002249469),
  PMS_prox_idx_grocery = c(0.01129957, 0.01894993, 0.02754996, 0.03894998, 0.05744995, 0.09185003, 0.16739932),
  PMS_prox_idx_educpri = c(0.08265002),
  PMS_prox_idx_educsec = c(0.05569919, 0.09904984, 0.17829930),
  PMS_prox_idx_lib = c(0.09434931),
  PMS_prox_idx_parks = c(0.04504995),
  PMS_prox_idx_transit = c(0.007649841)
)
cut_offs_mixall = list(
  PMS_prox_idx_emp = c(0.003649668),
  PMS_prox_idx_pharma = c(0.02654995),
  PMS_prox_idx_childcare = c(0.03634996),
  PMS_prox_idx_health = c(0.002549527),
  PMS_prox_idx_grocery = c(0.04844996),
  PMS_prox_idx_educpri = c(0.08565002),
  PMS_prox_idx_educsec = c(0.06324952, 0.14094998),
  PMS_prox_idx_lib = c(0.09929952),
  PMS_prox_idx_parks = c(0.04474999),
  PMS_prox_idx_transit = c(0.00654981)
)
cutoffs = list(cut_offs_mclust, cut_offs_kmeans_pam, cut_offs_mixall)

#num of clusters
num_mclust = c(9, 7, 3, 4, 3, 7, 8, 7, 8, 3)
num_kmeans_pam = c(2, 2, 2, 2, 8, 2, 4, 2, 2, 2)
num_mixall = c(2, 2, 2, 2, 2, 2, 3, 2, 2, 2)
num_clusts = list(num_mclust, num_kmeans_pam, num_mixall)

#sil coefs
sil_mclust = c(0.5867320, 0.4837194, 0.6027205, 0.5222099, 0.5909881, 0.4577264, 0.4784640, 0.4892339, 0.4577640, 0.5762659)
sil_kmeans_pam = c(0.6251910, 0.5923426, 0.5728351, 0.5891724, 0.5561223, 0.5901501, 0.5577481, 0.5655205, 0.5793991, 0.5354237)
sil_mixall = c(0.6209505, 0.5897513, 0.5821014, 0.5776743, 0.5475178, 0.5837362, 0.5817027, 0.5814213, 0.5717646, 0.5471547)
coefs = list(sil_mclust, sil_kmeans_pam, sil_mixall)




# make output file
file.create("appendix_methods.tex")
write('
  \\documentclass[10pt, a4paper]{article} \n
  \\usepackage[utf8]{inputenc} \n
  \\usepackage[margin=1in]{geometry} \n
  \\usepackage{rotating} \n
  \\usepackage{longtable} \n
  \\begin{document} \n\n\n\n',
  file = 'appendix_methods.tex', append = TRUE
)


#creating the tables
for (i in 1:length(cutoffs)){
	#producing table
	summary_table = data.frame()
	
	#num of clusters
	summary_table[1,1:10] = num_clusts[[i]]
	
	#cutoffs
	summary_table[2,1:10] = lapply(cutoffs[[i]], function(x) paste0(round(x, 3), collapse = ', '))
	
	#coefs
	summary_table[3,1:10] = format(round(coefs[[i]], 2), nsmall=2)
	
	#swap rows and cols
	summary_table = t(summary_table)
	
	#assign row and column names
	colnames(summary_table) = c('Num. of Clusters', 'Cutoffs', 'Sil. Coef.')
	row.names(summary_table) = labs
	
	#export
	write(print(xtable(summary_table, type = "latex", caption = anames[i]), floating = FALSE, floating.environment = "sidewaystable", tabular.environment = "longtable", hline.after = c(-1:nrow(summary_table))), file = 'appendix_methods.tex', append = TRUE)
  write('\n\n\n\n \\pagebreak \n ', file = 'appendix_methods.tex', append = TRUE)
}

 write('\n\n\n\n \\end{document} ', file = 'appendix_methods.tex', append = TRUE)


#edit longtables
# read in the file
file_content = readLines("appendix_methods.tex")

# find and replace the text
new_content = gsub("begin\\{longtable\\}\\{rllllllllll\\}", "begin\\{longtable\\}{r p\\{1.5cm\\} p\\{1.5cm\\} p\\{1.5cm\\} p\\{1.5cm\\} p\\{1.5cm\\} p\\{1.5cm\\} p\\{1.5cm\\} p\\{1.5cm\\} p\\{1.5cm\\} p\\{1.5cm\\} \\}", file_content)

# write the new content back to the file
writeLines(new_content, "appendix_methods.tex")





