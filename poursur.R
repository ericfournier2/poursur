setwd("D:/Dev/Projects/poursur/")

# Load the data.
index_data = read.table("index.txt", sep="\t", header=TRUE)

# Generate inferred data columns.
index_data$no_chapitre = floor((index_data$no_sequentiel - 1) / 144) + 1
index_data$no_dans_chapitre = (index_data$no_sequentiel - 1) %% 144 + 1

# Get the thread titles in order so we cancrate a leveled factor.
one_row_per_name = index_data[!duplicated(index_data$no_trame),]
one_row_per_name = one_row_per_name[order(one_row_per_name$no_trame),]
name_order = one_row_per_name$nom_trame

# Factorize the thread names.
index_data$nom_trame = factor(index_data$nom_trame, levels=name_order)

# Build a 144 color palette.
library(RColorBrewer)
coul <- brewer.pal(9, "Set1") 
coul <- colorRampPalette(coul)(144)



library(ggplot2)

# Long linear graph
ggplot(data=index_data) + 
  geom_tile(mapping=aes(x=no_sequentiel, y=1, fill=nom_trame)) + 
  theme(legend.position="bottom")
ggsave(filename="long linear.tiff", width=24, height=10)

# Tile graph with one line per chapter.
ggplot(data=index_data) + 
  geom_tile(mapping=aes(x=no_dans_chapitre, y=no_chapitre, fill=nom_trame), color="black") +
  scale_y_reverse() +
  scale_fill_manual(values=coul) +
  coord_equal() +
  theme(legend.position="bottom",
        panel.background = element_blank(),
        panel.border = element_blank(), 
		panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black"))
ggsave(filename="tiles.tiff", width=16, height=10)  

ggplot(data=index_data) + 
  geom_point(mapping=aes(x=no_sequentiel, y=nom_trame, color=nom_trame)) +
  theme(legend.position="bottom",
        panel.background = element_blank(),
        panel.border = element_blank(), 
		panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black"))
ggsave(filename="points.png", width=24, height=24)  

library(circlize)

drawCircos = function(index_data) {
	circos.par("track.height" = 0.1)
	circos.initialize(factor(index_data$no_chapitre), xlim=c(0,145))
	circos.track(ylim = c(0, 1))
	#circos.rect(index_data$no_dans_chapitre, 0, index_data$no_dans_chapitre + 1, 1, coul[index_data$no_trame])
	#circos.trackPoints(index_data$no_chapitre, index_data$no_dans_chapitre, rep(0.5, nrow(index_data)), col=coul[index_data$no_trame])
	#circos.barplot(value=rep(1, nrow(index_data)), 
	#               pos=index_data$no_dans_chapitre - 0.5,
	#			   bar_width=1,
	#			   col=coul[index_data$no_trame],
	#			   sector.index=as.numeric(index_data$no_chapitre)) 	
	for(i in 1:nrow(index_data)) {
	  circos.rect(index_data$no_dans_chapitre[i] - 0.5, 0, index_data$no_dans_chapitre[i] + 0.5, 1, sector.index=index_data$no_chapitre[i], track.index=1, col=coul[index_data$no_trame[i]], border=NA)
	}

	index_links = index_data[order(index_data$no_trame, index_data$ordre_dans_trame),]
	index_links$start_chapitre = index_links$no_chapitre
	index_links$start_position = index_links$no_dans_chapitre
	index_links$end_chapitre = c(index_links$no_chapitre[2:nrow(index_links)], 0)
	index_links$end_position = c(index_links$no_dans_chapitre[2:nrow(index_links)], 0)

	index_links = index_links[index_links$ordre_dans_trame != 12,]
	index_links$col = coul[index_links$no_trame]

	for(i in 1:nrow(index_links)) {
	  cat("Processing ", i, "\n")
	  circos.link(index_links$start_chapitre[i], index_links$start_position[i], index_links$end_chapitre[i], index_links$end_position[i], col=index_links$col[i], directional=1)
	}
}


png("circos one.png", width=6000, height=6000)
drawCircos(index_data)
dev.off()

png("circos 144.png", width=6000, height=6000)
par(mfrow=c(12, 12))
for(i in 1:144) {
#  plot.new()
  drawCircos(index_data[index_data$no_trame == i,])
  title(index_data$nom_trame[index_data$no_trame == i][1])  
  circos.clear()
}

dev.off()

