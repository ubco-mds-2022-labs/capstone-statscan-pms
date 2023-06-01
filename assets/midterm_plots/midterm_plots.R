set.seed(2023)
library(ggplot2)
library(tidyverse)

library(data.table)
library(gridExtra)


###################
# IMPORT DATA
###################

# pms
load('../../../local_data/codes/create_master/master_pms_df.Rdata')



# num clusters
nquintiles = rep(5, 10)
nmixall = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#nhdbscan = c(2, 2, 3, 2, 0, 2, 2, 0, 2, 0)
nhdbscan = c(2, 2, 2, 2, 2, 2, 3, 2, 2, 3)
num_df = data.frame(nquintiles, nmixall, nhdbscan)



# silhouette
# squartiles = c(0.5301638, 0.5352868, 0.5071032, 0.5340226, 0.4664641, 0.5497091, 0.5222450, 0.5211492, 0.5146823, 0.5116242)
# smixall = c(0.62416982312449, 0.578246901617834, 0.579170989394048, 0.581603902641992, 0.555919113100562, 0.589022193745387, 0.594242094440351, 0.587765890776524, 0.573329830620795, 0.549231317427407)
# shdbscan = c(0.687337430388787, 0.434971414389449, 
# 			0.410851184885891, 0.726800535711154, 
# 			0.405722984464385, 0.532067955884544, 
# 			0.457023848620274, 0.454816989136992, 
# 			0.332151764469907, 0.340639023025164)
squintiles = c(0.54734391, 0.45408143, 0.30015977, 0.59154223, 0.32008321, 0.19455710, 0.08093067, 0.20113969, 0.32982235, 0.54705554)
smixall = c(0.6209505, 0.5897513, 0.5821014, 0.5776743, 0.5475178, 0.5837362, 0.5817027, 0.5814213, 0.5717646, 0.5471547)
shdbscan = c(0.689974748481503, 0.440268773914686, 0.436142083007771, 0.732102639194725, 0.488393299110873, 0.332930000531499, 0.414630345367582, 0.465481793809224, 0.356977727090723, 0.272397309613473)
sil_df = data.frame(squintiles, smixall, shdbscan)


# dunn
squintiles = c(0.0001524855, 0.0001090988, 0.0001174122, 0.0001648533, 0.0002828054, 0.0001479071, 0.0002602472, 0.0003763171, 0.0001326436, 0.0001910220)
smixall = c(0.0049176395, 0.0010469004, 0.0006682637, 0.0070728623, 0.0007131269, 0.0003336386, 0.0002818422, 0.0024340897, 0.0005169575, 0.0035452905)
shdbscan = c(0.00338328122863714, 0, 0, 
             0.00290586745518542, 0, 8.5279952299452e-05, 
             0.000181093887703734, 0.000279315860663253, 0, 0)
#sil_df = data.frame(squintiles, smixall, shdbscan)


# calinski herzerbatz
squintiles = c(5963.6320, 1590.3831, 1753.8854, 6157.0656, 1033.9202, 1051.6750, 625.9694, 258.7814, 2414.5033, 4134.9338)
smixall = c(35403.788, 12007.320, 15949.190, 18546.084, 7460.945, 15103.890, 13920.339, 7174.031, 14413.881, 9465.626)
shdbscan = c(3655.75180109406, 4571.01854374982, 3853.86902961714, 
             2259.78258147251, 1953.01889097893, 2594.06029937711,
             2709.50766404369, 1394.70607709457, 4007.92463660904,
             957.722323613857)
#sil_df = data.frame(squintiles, smixall, shdbscan)


# davies bouldin
squintiles = c(0.8927360, 0.9843783, 0.9028139, 0.8766295, 0.8980886, 0.8353581, 0.8737781, 0.9922624, 0.8949231, 0.8726548)
smixall = c(0.5951392, 0.6627599, 0.6728390, 0.6756872, 0.7575368, 0.6668264, 0.6339540, 0.6974680, 0.7022714, 0.7576272)
shdbscan = c(0.397646393855886, 0.796395288967002, 1.77390364670791,
             0.348803365749021, 1.15635989806671, 2.69408852023911, 
             1.3678474078649, 0.692116879785653, 4.05632596932578, 
             2.4566378939336)
#sil_df = data.frame(squintiles, smixall, shdbscan)




# labels
library(stringr)
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")
labs = str_sub(amenities, 14) 
labs = c('Employment', 'Pharmacy', 'Childcare', 'Healthcare', 'Grocery', 'Pri. Educ.', 'Sec. Educ.', 'Library', 'Parks', 'Transit')

# cutoffs
# cquarttiles = list(PMS_prox_idx_emp = c(0.000700000047463944, 0.0062999994950429,  0.0280000130208772), 
#                   PMS_prox_idx_pharma = c(0.013100000022532,  0.026199999882501, 0.0529000000575213), 
#                   PMS_prox_idx_childcare = c(0.0198000060793181,  0.0481000119893211, 0.0970999914075018), 
#                   PMS_prox_idx_health = c(0.00120000004095931,  0.00490000016561574, 0.014100000530935), 
#                   PMS_prox_idx_grocery = c(0.0258999997948698,  0.0430999998469438, 0.0850999972059218), 
#                   PMS_prox_idx_educpri = c(0.0482999998981212,  0.0897000019827969, 0.153999994119748), 
#                   PMS_prox_idx_educsec = c(0.0443000021061397,  0.0728999963809679, 0.12629999661621), 
#                   PMS_prox_idx_lib = c(0.0592000016053752,  0.0814000034662971, 0.132099993821641), 
#                   PMS_prox_idx_parks = c(0.0244000001259242,  0.047700002381118, 0.0900999976966481), 
#                   PMS_prox_idx_transit = c(0.0035999998872406,  0.00959999976535963, 0.0224999990090526))
cquintiles = list(
  PMS_prox_idx_emp = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_pharma = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_childcare = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_health = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_grocery = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_educpri = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_educsec = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_lib = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_parks = c(0.012, 0.021, 0.036, 0.069),
  PMS_prox_idx_transit = c(0.012, 0.021, 0.036, 0.069)
)
# cmixall = list(
#   PMS_prox_idx_emp = c(0.00365),
#   PMS_prox_idx_pharma = c(0.02675),
#   PMS_prox_idx_childcare = c(0.03565),
#   PMS_prox_idx_health = c(0.00245),
#   PMS_prox_idx_grocery = c(0.05225),
#   PMS_prox_idx_educpri = c(0.08515),
#   PMS_prox_idx_educsec = c(0.08125),
#   PMS_prox_idx_lib = c(0.1002),
#   PMS_prox_idx_parks = c(0.04425),
#   PMS_prox_idx_transit = c(0.00595))
cmixall = list(PMS_prox_idx_emp = 0.0047496679109546, PMS_prox_idx_pharma = 0.0276499501211147, 
               PMS_prox_idx_childcare = 0.0374499570082516, PMS_prox_idx_health = 0.00364952738545688, 
               PMS_prox_idx_grocery = 0.0495499555401056, PMS_prox_idx_educpri = 0.0867500165442711, 
               PMS_prox_idx_educsec = c(0.0643495167816625, 0.142049977648391
               ), PMS_prox_idx_lib = 0.100399519433931, PMS_prox_idx_parks = 0.0458499861179105, 
               PMS_prox_idx_transit = 0.00764980997440566)
#chdbscan = list(PMS_prox_idx_emp = 0.16035, PMS_prox_idx_pharma = 0.01155,
#                PMS_prox_idx_childcare = c(0.01205, 0.101), PMS_prox_idx_health = 0.06835,
#                NA, PMS_prox_idx_educpri = 0.04895, PMS_prox_idx_educsec = 0.0655,
#                NA, PMS_prox_idx_parks = 0.01935, NA)
#chdbscan = list(PMS_prox_idx_emp = 0.21475, PMS_prox_idx_pharma = 0.0114, PMS_prox_idx_childcare = 0.0087, PMS_prox_idx_health = 0.09555, PMS_prox_idx_grocery = 0.1146, PMS_prox_idx_educpri = 0.05295, PMS_prox_idx_educsec = c(0.0625, 0.096), PMS_prox_idx_lib = 0.0577, PMS_prox_idx_parks = 0.01955, PMS_prox_idx_transit = c(0.0039, 0.0355))
chdbscan =  list(PMS_prox_idx_emp = 0.22985, PMS_prox_idx_pharma = c(0.01175, 0.0525), PMS_prox_idx_childcare = 0.009, PMS_prox_idx_health = 0.1052,PMS_prox_idx_grocery = c(0.0763, 0.0124), PMS_prox_idx_educpri = c(0.04495,0.22045, 0.1449), PMS_prox_idx_educsec = c(0.0576, 0.0863), PMS_prox_idx_lib = c(0.06575, 0.0691, 0.05465), PMS_prox_idx_parks = 0.01995,PMS_prox_idx_transit = 0.03875)

cpamkmeans = list(PMS_prox_idx_emp = 0.00354965732265224, PMS_prox_idx_pharma = 0.0263499502919753, 
    PMS_prox_idx_childcare = 0.0364499668413561, PMS_prox_idx_health = 0.00224946884447716, 
    PMS_prox_idx_grocery = c(0.0112995659370951, 0.0189499271797888, 
    0.0275499599355604, 0.038949976675303, 0.0574499507265309, 
    0.0918500301533346, 0.167399317893989), PMS_prox_idx_educpri = 0.0826500219059937, 
    PMS_prox_idx_educsec = c(0.0556991858613332, 0.0990498437413425, 
    0.178299296897769), PMS_prox_idx_lib = 0.0943493091040467, 
    PMS_prox_idx_parks = 0.0450499548102519, PMS_prox_idx_transit = 0.00764984062766953) 

cmclust = list(PMS_prox_idx_emp = c(4.14213253644949e-05, 0.000447722382861175, 
0.00124907379468787, 0.0033496379343892, 0.00854985258828782, 
0.020649938420429, 0.051849977216652, 0.162949898378599), PMS_prox_idx_pharma = c(0.0064176656258352, 
0.0107995366480914, 0.0181499306310357, 0.0331986640972552, 0.055421552515451, 
0.0113612442533326), PMS_prox_idx_childcare = c(0.00187484177058108, 
0.0668500138482712), PMS_prox_idx_health = c(0.000246410177823522, 
0.00344964741551701, 0.00934986333014108), PMS_prox_idx_grocery = c(0.0116898298058918, 
0.0072198386031646), PMS_prox_idx_educpri = c(0.0234991637948482, 
0.0265082755680189, 0.0443998704967309, 0.0900500296893578, 0.13115004954825, 
0.185000010995437), PMS_prox_idx_educsec = c(0.0347496912922471, 
0.0345950934903355, 0.0438499848307019, 0.0617987062596693, 0.101149653016388, 
0.143449981685184, 0.0854657664906577), PMS_prox_idx_lib = c(0.0487996030559252, 
0.053799611978918, 0.0682490920317932, 0.0927483278964371, 0.116349101318268, 
0.0416971133687469), PMS_prox_idx_parks = c(0.0825462357679255, 
0.0159499266421358, 0.0324499645311814, 0.0463499913474011, 0.0623500026485389, 
0.0844500172175564, 0.12194993679559), PMS_prox_idx_transit = c(0.00104891300114654, 
0.0116498947015775)) 










g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

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

data = pivot_longer(sil_df, cols = names(sil_df))
data[is.na(data$value), 'value'] = 0.02
data$amenities = rep(amenities, each=3)
data$amenities = factor(data$amenities, levels = rev(amenities), ordered = TRUE)
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

#bar version
t = list()
t[[1]] = ggplot(data[1:15,], aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') +
  scale_x_discrete(labels=labs[5:1]) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
  ) + coord_flip() + guides(fill = 'none') + 
  ylab('Silhouette Coefficient') + xlab('') 
t[[2]] = ggplot(data[16:30,], aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') +
  scale_x_discrete(labels=labs[10:6]) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0.2,0,0),"cm"),
  ) + coord_flip() + guides(fill = 'none') + 
  ylab('Silhouette Coefficient') + xlab('') 
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
  ylab('Silhouette Coefficient') + xlab('') +
  scale_fill_discrete(name = "Algorithm    ",
                      labels = c("Quintiles", "MixAll", "HDBSCAN"),
                      breaks = c("squintiles", "smixall", "shdbscan"))
t[[3]] = g_legend(temp)
rm(temp)
layout_mat = rbind(c(1,2),
                    c(1,2),
                    c(1,2),
                    c(1,2),
                    c(3,3))
silcoef = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat))










###################
# DISTRIBUTION CUTOFF PLOT
###################

#log transform
for (i in amenities){
  master[,i] = log(master[,i]+0.0001)
}

#hdbscan
p <- list()
most = 0
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
    ggtitle(str_sub(i, 14))
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
}
most = names(lapply(chdbscan, length)[lapply(chdbscan, length) == max(unlist(lapply(chdbscan, length)))][1])
p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))


#mixall
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
    ggtitle(str_sub(i, 14))
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
    ggtitle(str_sub(i, 14))
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
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs3 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))




# PAM k-means
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
    ggtitle(str_sub(i, 14))
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
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs4 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))






# mclust
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
    ggtitle(str_sub(i, 14))
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
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs5 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))






###################
# SAVING PLOTS
###################

ggsave("numclusts.png", numclusts, dpi = 400, width=8, height=5)
ggsave("silcoef.png", silcoef, dpi = 400, width=8, height=5)
ggsave("cutoffs_hdbscan.png", cutoffs, dpi = 400, width=8, height=5)
ggsave("cutoffs_mixall.png", cutoffs2, dpi = 400, width=8, height=5)
ggsave("cutoffs_quintiles.png", cutoffs3, dpi = 400, width=8, height=5)
ggsave("cutoffs_pamkmeans.png", cutoffs4, dpi = 400, width=8, height=5)
ggsave("cutoffs_mclust.png", cutoffs5, dpi = 400, width=8, height=5)


