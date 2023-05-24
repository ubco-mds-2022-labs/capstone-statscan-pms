set.seed(2023)
library(ggplot2)
library(tidyverse)


###################
# IMPORT DATA
###################

# pms
load('../../../local_data/codes/create_master/master_pms_df.Rdata')



# num clusters
nquintiles = sample.int(5, 10, replace=TRUE)
nvarsel = sample.int(5, 10, replace=TRUE)
nmixall = sample.int(5, 10, replace=TRUE)
nhdbscan = c(2, 2, 3, 2, 0, 2, 2, 0, 2, 0)
num_df = data.frame(nquintiles, nvarsel, nmixall, nhdbscan)



# silhouette
squintiles = runif(10)
svarsel = runif(10)
smixall = runif(10)
shdbscan = c(0.669838211728378, 0.443449260743095, 
             0.400812993352801, 0.71150342405053, 
             NA, 0.491369087890728, 
             0.502911585364128, NA, 
             0.343965286339036, NA)
sil_df = data.frame(squintiles, svarsel, smixall, shdbscan)


# labels
library(stringr)
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")
labs = str_sub(amenities, 14) 


# cutoffs
cquintiles = list()
cvarsel = list()
cmixall = list()
chdbscan = list(PMS_prox_idx_emp = 0.16035, PMS_prox_idx_pharma = 0.01155,
                PMS_prox_idx_childcare = c(0.01205, 0.101), PMS_prox_idx_health = 0.06835,
                NA, PMS_prox_idx_educpri = 0.04895, PMS_prox_idx_educsec = 0.0655,
                NA, PMS_prox_idx_parks = 0.01935, NA)

#for quintiles
# library(dplyr)
# for (i in amenities){
# 	temp = ntile(master[,i], 5)
# }

#to remove later
for (i in amenities){
	cvarsel[[i]] = runif(sample.int(4, 1))
	cmixall[[i]] = runif(sample.int(4, 1))
}



###################
# NUM OF CLUSTERS PLOT
###################

data = pivot_longer(num_df, cols = names(num_df))
data$amenities = rep(amenities, each=4)
numclusts = ggplot(data, aes(x=amenities, y=value, color=name)) + 
  geom_jitter(width=0, height=0, size=5) + 
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0,0,0.1,0),"cm"),
  ) + 
  ylab('Number of Clusters') + xlab('') +
  scale_colour_discrete(name = "Algorithm",
                        labels = c("Quartiles", "Varsel", "MixAll", "HDBSCAN"),
                        breaks = c("nquintiles", "nvarsel", "nmixall", "nhdbscan"))













###################
# SILHOUETTE COEFFICIENT PLOT
###################

data = pivot_longer(sil_df, cols = names(sil_df))
data$amenities = rep(amenities, each=4)
silcoef = ggplot(data, aes(x=amenities, y=value, color=name)) + 
  geom_jitter(width=0, height=0, size=5) + 
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0,0,0.1,0),"cm"),
  ) + 
  ylab('Silhouette Coefficient') + xlab('') +
  scale_colour_discrete(name = "Algorithm",
                        labels = c("Quartiles", "Varsel", "MixAll", "HDBSCAN"),
                        breaks = c("squintiles", "svarsel", "smixall", "shdbscan"))

  














###################
# DISTRIBUTION CUTOFF PLOT
###################

#log transform
for (i in amenities){
  master[,i] = log(master[,i]+0.0001)
}


library(data.table)
library(gridExtra)
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = chdbscan[[i]] #change this for different algorithms!
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
cutoffs = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))





###################
# SAVING PLOTS
###################

ggsave("numclusts.png", numclusts, dpi = 400, width=8, height=5)
ggsave("silcoef.png", silcoef, dpi = 400, width=8, height=5)
ggsave("cutoffs.png", cutoffs, dpi = 400, width=8, height=5)

