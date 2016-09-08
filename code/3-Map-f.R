## get online map background


googleterrainsyria <- get_map(location = c(lon =39.0560, lat = 34.8148), color = "bw", source = "google",maptype = "terrain", zoom = 7)
googleterraincham <- get_map(location = c(lon =36.33, lat =33.56 ), color = "bw", source = "google",maptype = "terrain", zoom = 10)
googleterrainlat <- get_map(location = c(lon =36.1, lat = 35.22), color = "bw", source = "google",maptype = "terrain", zoom = 10)
googleterrainhoms <- get_map(location = c(lon =36.71158, lat = 34.73767), color = "bw", source = "google",maptype = "terrain", zoom = 12)
googleterrainhamas <- get_map(location = c(lon =36.7450, lat = 35.1409), color = "bw", source = "google",maptype = "terrain", zoom = 12)
googleterrainqarah <- get_map(location = c(lon =36.7367, lat = 34.1711), color = "bw", source = "google",maptype = "terrain", zoom = 11)
googleterraindaraa <- get_map(location = c(lon =36.4, lat = 32.7721), color = "bw", source = "google",maptype = "terrain", zoom = 10)
googleterrainaleppo <- get_map(location = c(lon =37.1302, lat = 36.215), color = "bw", source = "google",maptype = "terrain", zoom = 13)
googleterrainhassake <- get_map(location = c(lon =40.7023, lat = 36.5971), color = "bw", source = "google",maptype = "terrain", zoom = 9)

#####


lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############################################################
# function to map variable


## Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.

p <- ggplot(data.frame(datasp), aes(y=datasp$benef)) + ylab("# of Ind") + scale_y_continuous()
for (i in 10:57 ) {
  #i <- 10
  rm(variablename)
  variablename <- names(datasp)[i]
  title <- attributes(data)$variable.labels[i]
  rm(plot)
  plot <- p + 
    aes_string(x = names(datasp)[i]) +
    xlab(colnames(datasp[i])) + 
    geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8")  +
    guides(fill=FALSE) + 
    # coord_flip()+
    xlab("") + 
    coord_flip() + 
    ggtitle(title)
  assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/plot_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}

#### Now trying with maps
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.
# http://danielphadley.com/Automate-Map-Making/

for (i in 10:57 ) {
  rm(variablename)
  variablename <- names(datasp)[i]
  title <- attributes(data)$variable.labels[i]
  
  rm(plot)
  plot <- ggmap(googleterrainsyria)  + 
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp ,size = 2, alpha = 0.95 ) +
    ggtitle(paste("Map: ", title , sep="")) +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/map_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)


  rm(centersyriacham)
  centersyriacham <- ggmap(googleterraincham) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600")  +
    ggtitle("Community Centers in Damascus Area") +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyrialat)
  centersyrialat <- ggmap(googleterrainlat) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
    ggtitle("Community Centers near the coast ( Latakia, Tartous)") +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriahoms)
  centersyriahoms <- ggmap(googleterrainhoms) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
    ggtitle("Community Centers near Homs") +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriahamas)
  centersyriahamas <- ggmap(googleterrainhamas) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
    ggtitle("Community Centers near Hamas") +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriaqarah)
  centersyriaqarah <- ggmap(googleterrainqarah) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
    ggtitle("Community Centers near qarah") +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriadaraa)
  centersyriadaraa <- ggmap(googleterraindaraa) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
    ggtitle("Community Centers near Suwayda") +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriaaleppo)
  centersyriaaleppo <- ggmap(googleterrainaleppo) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
    ggtitle("Community Centers near Aleppo") +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriahassake)
  centersyriahassake <- ggmap(googleterrainhassake) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2], colour= names(datasp)[i], shape = names(datasp)[i]), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
    ggtitle("Community Centers near Hassake") +
    theme(plot.title=element_text(face="bold", size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  ## Set the size of the PDF to an exact landscape A4 -- measure in inche
  #png( paste("out/mapall_",i,variablename,".png",sep=""),width=400,height=800,res=300)
  #png( paste("out/mapall_",i,variablename,".png",sep=""), width=8, height=16,units="in", dpi=300)
  # pdf( paste("out/mapall_",i,variablename,".pdf",sep=""), width = 8.267 , height =11.692 )
  
  #grid.rect(gp = gpar(fill = "#FFFFFF", col = "#FFFFFF"))
  #lay_out(          list(centersyriacham, 1, 1),
                    list(centersyriaaleppo, 1, 2),
                    list(centersyriahoms, 2, 1),
                    list(centersyriahamas, 2, 2),
                    list(centersyrialat, 3, 1),
                    list(centersyriaqarah, 3, 2),
                    list(centersyriadaraa, 4, 1),
                    list(centersyriahassake, 4, 2))
  
  #dev.off()
  
  #allmap <- multiplot(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake, cols=2)
  #allmap <- grid.arrange(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake, ncol=2)
  #
  rm(allmap)
  allmap <- arrangeGrob(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake, ncol=2)
  ggsave(filename=paste("out/allmap_",i,variablename,".png",sep=""), allmap, width=8, height=16, units="in", dpi=300)

  ##

}


