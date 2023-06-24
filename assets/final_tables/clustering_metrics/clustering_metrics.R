#this script produces 10 tables (1 per amenity) for the clustering validation metrics 
library(xtable)


# amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

# labels
labs = c('Employment', 'Pharmacy', 'Childcare', 'Healthcare', 'Grocery', 'Pri. Educ.', 'Sec. Educ.', 'Library', 'Parks', 'Transit')


###### coefficients
# silhouette
squintiles = c(0.3450460, 0.4316893, 0.4448841, 0.3908186, 0.4036867, 0.4670033, 0.4362782, 0.4292932, 0.4758332, 0.4227016)
smixall = c(0.6209505, 0.5897513, 0.5821014, 0.5776743, 0.5475178, 0.5837362, 0.5817027, 0.5814213, 0.5717646, 0.5471547)
shdbscan = c(0.689974748481503, 0.440268773914686, 0.436142083007771, 0.732102639194725, 0.488393299110873, 0.332930000531499, 0.414630345367582, 0.465481793809224, 0.356977727090723, 0.272397309613473)
skpam = c(0.6251910, 0.5923426, 0.5728351, 0.5891724, 0.5561223, 0.5901501, 0.5577481, 0.5655205, 0.5793991, 0.5354237)
smclust = c(0.5867320, 0.4837194, 0.6027205, 0.5222099, 0.5909881, 0.4577264, 0.4784640, 0.4892339, 0.4577640, 0.5762659)
smanual = c(0.600661, 0.3802132, 0.4034572, 0.6390608, 0.382051, 0.4507627, 0.4440815, 0.8780825, 0.4296511, 0.7392542)
sil_df = data.frame(squintiles, smixall, shdbscan, skpam, smclust, smanual)


# dunn
squintiles = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
smixall = c(0.0049176395, 0.0010469004, 0.0006682637, 0.0070728623, 0.0007131269, 0.0003336386, 0.0002818422, 0.0024340897, 0.0005169575, 0.0035452905)
shdbscan = c(0.00338328122863714, 0, 0, 
             0.00290586745518542, 0, 8.5279952299452e-05, 
             0.000181093887703734, 0.000279315860663253, 0, 0)
skpam = c(0.0049762117, 0.0008444133, 0.0007161396, 0.0077941279, 0.0007049463, 0.0003786560, 0.0017758166, 0.0032023605, 0.0004418321, 0.0029705301)
smclust = c(0.0012634720, 0.0001982587, 0.0003168529, 0.0023371238, 0.0011548845, 0.0004263013, 0.0004016697, 0.0016705470, 0.0001100838, 0.0024851275)
smanual = c(0.0001380643, 0.0001034982, 0.0001102657, 0.0001480166, 0.0001319609, 0.0001507159, 0.0005180005, 0.01045884, 0.0001260239, 0.0001688619)
dunn_df = data.frame(squintiles, smixall, shdbscan, skpam, smclust, smanual)


# calinski herzerbatz
squintiles = c(3544.783, 1409.012, 3696.046, 1724.468, 1786.910, 6012.506, 2685.533, 1385.507, 4675.859, 1519.004)
smixall = c(35403.788, 12007.320, 15949.190, 18546.084, 7460.945, 15103.890, 13920.339, 7174.031, 14413.881, 9465.626)
shdbscan = c(3655.75180109406, 4571.01854374982, 3853.86902961714, 
             2259.78258147251, 1953.01889097893, 2594.06029937711,
             2709.50766404369, 1394.70607709457, 4007.92463660904,
             957.722323613857)
skpam = c(36371.960, 11854.182, 15190.339, 18858.442, 19255.211, 15238.574, 16406.098, 6138.014, 14512.118, 8940.283)
smclust = c(98538.764, 4928.255, 9950.952, 23477.133, 1959.630, 18423.626, 7935.748, 4323.034, 17243.941, 11501.814)
smanual = c(255.8242, 639.4712, 542.9362, 102.8402, 219.6314, 2853.267, 2306.01, 1545.816, 1342.026, 25.83534) 
cal_df = data.frame(squintiles, smixall, shdbscan, skpam, smclust, smanual)


# davies bouldin
squintiles = c(0.9512837, 1.0148282, 0.7870594, 1.1251735, 0.8276177, 0.7055577, 0.7502652, 0.8403175, 0.7377130, 1.0044476)
smixall = c(0.5951392, 0.6627599, 0.6728390, 0.6756872, 0.7575368, 0.6668264, 0.6339540, 0.6974680, 0.7022714, 0.7576272)
shdbscan = c(0.397646393855886, 0.796395288967002, 1.77390364670791,
             0.348803365749021, 1.15635989806671, 2.69408852023911, 
             1.3678474078649, 0.692116879785653, 4.05632596932578, 
             2.4566378939336)
skpam = c(0.5887389, 0.6655722, 0.6862804, 0.6555938, 0.5812439, 0.6575715, 0.6209026, 0.7263383, 0.6942425, 0.7757473)
smclust = c(0.5575865, 25.1732969, 0.6417944, 0.6392660, 0.6932940, 0.6476328, 0.5943470, 0.6776949, 0.9642396, 0.6424359)
smanual = c(1.008774, 0.800227, 0.7642339, 1.01061, 0.8163105, 0.6393068, 0.7133388, 0.1588381, 0.7019145, 0.8657572)
dav_df = data.frame(squintiles, smixall, shdbscan, skpam, smclust, smanual)

#all coefficients list
all_coef = list(sil_df, dunn_df, cal_df, dav_df)
all_coef_names = c("Silhouette", "Dunn", "Calinski Herzebatz", "Davies Bouldin")


anames = c('Quintiles', 'MixAll', 'HDBSCAN', 'PAM k-means', 'MCLUST', 'Min/Max')



# make output file
file.create("clustering_metrics.tex")
write('
  \\documentclass[10pt, a4paper]{article} \n
  \\usepackage[utf8]{inputenc} \n
  \\usepackage[margin=1in]{geometry} \n
  \\usepackage{rotating} \n
  \\usepackage{longtable} \n
  \\begin{document} \n\n\n\n',
  file = 'clustering_metrics.tex', append = TRUE
)




#adding values
counter = 1
for (i in 1:length(amenities)){
	#producing table
	summary_table = data.frame()
	
	for (j in 1:length(all_coef)){
		summary_table[j,1:length(anames)] = unname(all_coef[[j]][i,])
	}
	
	#formatting numbers
	summary_table[1,] = format(round(summary_table[1,], 2), nsmall = 2)
	summary_table[2,] = format(round(as.numeric(summary_table[2,]), 5), nsmall = 5)
	summary_table[3,] = format(round(as.numeric(summary_table[3,])), nsmall = 0)
	summary_table[4,] = format(round(as.numeric(summary_table[4,]), 2), nsmall = 2)
	 
	#switching columns and rows
	summary_table = t(summary_table)
	
	#assign row and column names
	row.names(summary_table) = anames
	colnames(summary_table) = all_coef_names
	
	
	
	
	
	#export
	write(print(xtable(summary_table, type = "latex", caption = labs[counter]), floating = FALSE, floating.environment = "sidewaystable", tabular.environment = "longtable", hline.after = c(-1:nrow(summary_table))), file = 'clustering_metrics.tex', append = TRUE)
  write('\n\n\n\n \\pagebreak \n ', file = 'clustering_metrics.tex', append = TRUE)
  counter = counter + 1 
}



 write('\n\n\n\n \\end{document} ', file = 'clustering_metrics.tex', append = TRUE)








