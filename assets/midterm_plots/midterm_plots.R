set.seed(2023)
library(ggplot2)
library(tidyverse)

library(data.table)
library(gridExtra)


###################
# IMPORT PMD
###################

# pms
load('../../../local_data/codes/create_master/master_pms_df.Rdata')


# subsampling data (if needed)
perc = 3 #percentage of data to subsample
subsample = (nrow(master)/100)*perc
master = master[sample(nrow(master), subsample),]


# labels
library(stringr)
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

labs = c('Employment', 'Pharmacy', 'Childcare', 'Healthcare', 'Grocery', 'Pri. Educ.', 'Sec. Educ.', 'Library', 'Parks', 'Transit')

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}






###################
# PLOTTING DATA
###################



# num clusters
nquintiles = rep(5, 10)
nmixall = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
nhdbscan = c(2, 2, 2, 2, 2, 2, 3, 2, 2, 3)
num_df = data.frame(nquintiles, nmixall, nhdbscan)


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






# cutoffs

chdbscan =  list(PMS_prox_idx_emp = 0.22985, PMS_prox_idx_pharma = c(0.01175, 0.0525), PMS_prox_idx_childcare = 0.009, PMS_prox_idx_health = 0.1052,PMS_prox_idx_grocery = c(0.0763, 0.0124), PMS_prox_idx_educpri = c(0.04495,0.22045, 0.1449), PMS_prox_idx_educsec = c(0.0576, 0.0863), PMS_prox_idx_lib = c(0.06575, 0.0691, 0.05465), PMS_prox_idx_parks = 0.01995,PMS_prox_idx_transit = 0.03875)

cquintiles = list(
  PMS_prox_idx_emp = c(0.0004, 0.0030, 0.0127, 0.0368),
  PMS_prox_idx_pharma = c(0.0098, 0.0193, 0.0341, 0.0641),
  PMS_prox_idx_childcare = c(0.0152, 0.0348, 0.0636, 0.1167),
  PMS_prox_idx_health = c(0.0007, 0.0032, 0.0074, 0.0184),
  PMS_prox_idx_grocery = c(0.0221, 0.0348, 0.0555, 0.0985),
  PMS_prox_idx_educpri = c(0.0416, 0.0720, 0.1105, 0.1720),
  PMS_prox_idx_educsec = c(0.0421, 0.0586, 0.0910, 0.1492),
  PMS_prox_idx_lib = c(0.0558, 0.0707, 0.0960, 0.1488),
  PMS_prox_idx_parks = c(0.0203, 0.0372, 0.0614, 0.1050),
  PMS_prox_idx_transit = c(0.0026, 0.0067, 0.0131, 0.0272)
)

cmixall = list(
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

cpamkmeans = list(
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

cmclust = list(
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

cmanual = list(
  PMS_prox_idx_emp = c(0.0000423603, 0.0001573125, 0.0002743458, 0.0005010775),
  PMS_prox_idx_pharma = c(0.01142592, 0.01948168),
  PMS_prox_idx_childcare = c(0.008407409, 0.013850871),
  PMS_prox_idx_health = c(0.0000405736, 0.0001524638, 0.0002658077),
  PMS_prox_idx_grocery = c(0.01208361, 0.01846220),
  PMS_prox_idx_educpri = c(0.04965835, 0.08169011),
  PMS_prox_idx_educsec = c(0.06609424),
  PMS_prox_idx_lib = c(0.6149259),
  PMS_prox_idx_parks = c(0.01834893, 0.02944933),
  PMS_prox_idx_transit = c(0.0000390986, 0.0001481958, 0.0002512994, 0.0003514904)
)


cjenks = list(
  PMS_prox_idx_emp = c(0.0003, 0.0011, 0.0032, 0.0083, 0.0194, 0.0441, 0.1076, 0.7249),
  PMS_prox_idx_pharma = c(0.0018, 0.9857),
  PMS_prox_idx_childcare = c(0.001, 0.0079, 0.0155, 0.0267, 0.0429, 0.0668, 0.1035, 0.1654, 0.2941, 0.9208),
  PMS_prox_idx_health = c(0.6759),
  PMS_prox_idx_grocery = c(0.002, 0.7763),
  PMS_prox_idx_educpri = c(0.0095, 0.7452),
  PMS_prox_idx_educsec = c(0.0153, 0.8384),
  PMS_prox_idx_lib = c(0.0166, 0.8665),
  PMS_prox_idx_parks = c(0.0002, 0.823),
  PMS_prox_idx_transit = c(0.5926)
)








###################
# NUM OF CLUSTERS PLOT
###################

data = pivot_longer(num_df, cols = names(num_df))
data[data$value==0, 'value'] = 1
data$amenities = rep(amenities, each=3)
data$amenities = factor(data$amenities, levels = rev(amenities), ordered = TRUE)
numclusts = ggplot(data, aes(x=amenities, y=value, color=name)) + 
  geom_jitter(width=0.1, height=0, size=5) + 
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
  ) + 
  ylab('Number of Clusters') + xlab('') +
  scale_colour_discrete(name = "Algorithm",
                        labels = c("Quintiles", "MixAll", "HDBSCAN"),
                        breaks = c("nquintiles", "nmixall", "nhdbscan"))

#bar version
t = list()
t[[1]] = ggplot(data[1:15,], aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') + 
  scale_x_discrete(labels=labs[5:1]) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm")
  ) + guides(fill = 'none') +
  ylab('Number of Clusters') + xlab('') + 
   coord_flip(ylim=c(1,4)) 
t[[2]] = ggplot(data[16:30,], aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') + 
  scale_x_discrete(labels=labs[10:6]) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0.2,0,0),"cm"),
  ) + guides(fill = 'none') +
ylab('Number of Clusters') + xlab('') + 
  coord_flip(ylim=c(1,4))
temp = ggplot(data, aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') + 
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
    legend.direction = "horizontal"  
    ) +  coord_flip() +
ylab('Number of Clusters') + xlab('') +
  scale_fill_discrete(name = "Algorithm   ",
                      labels = c("Quintiles", "MixAll", "HDBSCAN"),
                      breaks = c("nquintiles", "nmixall", "nhdbscan"))
t[[3]] = g_legend(temp)

layout_mat = rbind(c(1,2),
                    c(1,2),
                    c(1,2),
                    c(1,2),
                    c(3,3))
numclusts = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat))








###################
# SILHOUETTE COEFFICIENT PLOT
###################

counter = 1
for (i in all_coef){

	data = pivot_longer(i, cols = names(i))
	data[is.na(data$value), 'value'] = 0.02
	data$amenities = rep(amenities, each=ncol(all_coef[[1]]))
	data$amenities = factor(data$amenities, levels = rev(amenities), ordered = TRUE)


	#bar version
	t = list()
	t[[1]] = ggplot(data[1:(nrow(data)/2),], aes(x=amenities, y=value, fill=name)) + 
	  geom_bar(stat='identity', position='dodge') +
	  scale_x_discrete(labels=labs[5:1]) + 
	  theme(
		axis.text.y = element_text(size=10), 
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
		plot.margin=unit(c(0.1,0,0,0),"cm"),
	  ) + coord_flip() + guides(fill = 'none') + 
	  ylab(all_coef_names[counter]) + xlab('') 
	t[[2]] = ggplot(data[((nrow(data)/2)+1):(nrow(data)),], aes(x=amenities, y=value, fill=name)) + 
	  geom_bar(stat='identity', position='dodge') +
	  scale_x_discrete(labels=labs[10:6]) + 
	  theme(
		axis.text.y = element_text(size=10), 
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
		plot.margin=unit(c(0.1,0.2,0,0),"cm"),
	  ) + coord_flip() + guides(fill = 'none') + 
	  ylab(all_coef_names[counter]) + xlab('') 
	temp = ggplot(data, aes(x=amenities, y=value, fill=name)) + 
	  geom_bar(stat='identity', position='dodge') +
	  scale_x_discrete(labels=labs) + 
	  theme(
		axis.text.y = element_text(size=10), 
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
		plot.margin=unit(c(0.1,0,0,0),"cm"),
		legend.direction = "horizontal",
		legend.position="bottom"
	  ) + coord_flip() +
	  ylab(all_coef_names[counter]) + xlab('') +
	  scale_fill_discrete(name = "Algorithm    ",
		                  labels = c("Quintiles", "MixAll", "HDBSCAN", "PAM k-means", "MCLUST", 'Min/Max'),
		                  breaks = names(all_coef[[1]]))
	t[[3]] = g_legend(temp)
	rm(temp)
	layout_mat = rbind(c(1,2),
		                c(1,2),
		                c(1,2),
		                c(1,2),
		                c(1,2),
		                c(1,2),
		                c(3,3))
	silcoef = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat))
	ggsave(paste0(all_coef_names[counter], ".png"), silcoef, dpi = 400, width=8, height=7)
	counter = counter+1
}








###################
# DISTRIBUTION CUTOFF PLOT
###################

labs = c('Employment', 'Pharmacy', 'Child care', 'Health care', 'Grocery', 'Primary Education', 'Secondary Education', 'Library', 'Parks', 'Transit')

#log transform
for (i in amenities){
  master[,i] = log(master[,i]+0.0001)
}

#hdbscan
p <- list()
most = 0
counter = 1
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = chdbscan[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, unique(c(min(df$x), logged, max(df$x))),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
      ) +
    ggtitle(labs[counter])
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
  counter = counter + 1 
}
most = names(lapply(chdbscan, length)[lapply(chdbscan, length) == max(unlist(lapply(chdbscan, length)))][1])
p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))









#mixall
counter = 1
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = cmixall[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    ) +
    ggtitle(labs[counter])
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
  counter = counter + 1 
}
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
most = names(lapply(chdbscan, length)[lapply(chdbscan, length) == max(unlist(lapply(chdbscan, length)))][1])
p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs2 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))









#quintiles
counter = 1
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = cquintiles[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    ) +
    ggtitle(labs[counter])
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
  counter = counter + 1 
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs3 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))










# PAM k-means
counter = 1
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = cpamkmeans[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    ) +
    ggtitle(labs[counter])
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
  counter = counter + 1 
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs4 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))











# mclust
counter = 1
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = cmclust[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    ) +
    ggtitle(labs[counter])
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
  counter = counter + 1 
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs5 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))






# manual
counter = 1
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = cmanual[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    ) +
    ggtitle(labs[counter])
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
  counter = counter + 1 
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs6 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))








# jenks
counter = 1
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = cjenks[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    ) +
    ggtitle(labs[counter])
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
  counter = counter + 1 
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs7 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))
















###################
# SAVING PLOTS
###################

ggsave("numclusts.png", numclusts, dpi = 400, width=8, height=5)
#ggsave("silcoef.png", silcoef, dpi = 400, width=8, height=5)
ggsave("cutoffs_hdbscan.png", cutoffs, dpi = 400, width=8, height=5)
ggsave("cutoffs_mixall.png", cutoffs2, dpi = 400, width=8, height=5)
ggsave("cutoffs_quintiles.png", cutoffs3, dpi = 400, width=8, height=5)
ggsave("cutoffs_pamkmeans.png", cutoffs4, dpi = 400, width=8, height=5)
ggsave("cutoffs_mclust.png", cutoffs5, dpi = 400, width=8, height=5)
ggsave("cutoffs_manual.png", cutoffs6, dpi = 400, width=8, height=5)
ggsave("cutoffs_jenks.png", cutoffs7, dpi = 400, width=8, height=5)













































#purgatory

#old silhouette comparison plot
silcoef = ggplot(data, aes(x=amenities, y=value, color=name)) + 
  geom_jitter(width=0, height=0, size=5) + 
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
  ) + 
  ylab('Silhouette Coefficient') + xlab('') +
  scale_colour_discrete(name = "Algorithm",
                        labels = c("Quintiles", "MixAll", "HDBSCAN"),
                        breaks = c("squintiles", "smixall", "shdbscan"))
