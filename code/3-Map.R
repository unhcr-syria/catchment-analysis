


############################################################
# quick Maps to check


# ploting for a quick check
##plot(datasp)
#str(datasp)
#centersyria <-  ggplot( aes( x = long, y = lat), data= datasp) + geom_point()

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

library(grid)
library(gridExtra)
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

##### printing Staff & Benef & hardware

rm(centersyriastaff)
centersyriastaff <- ggmap(googleterrainsyria) +
  geom_point(aes( x = long, y = lat, size = staff), data=datasp , alpha = 0.85,  color="#FFA600", fill="#FFA600")  +
  #Plot the voronoi lines
  #geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
  ggtitle("Community Centers in Syria - # of Staff") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
ggsave("out/map-centersyriastaff.png", centersyriastaff, width=8, height=6,units="in", dpi=300)


rm(centersyriabenef)
centersyriabenef <- ggmap(googleterrainsyria) +
  geom_point(aes( x = long, y = lat, size = benef), data=datasp , alpha = 0.85,  color="#FFA600", fill="#FFA600")  +
  #Plot the voronoi lines
  #geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
  ggtitle("Community Centers in Syria - # of Beneficiaries") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
ggsave("out/map-centersyriabenef.png", centersyriabenef, width=8, height=6,units="in", dpi=300)

rm(centersyriahardware)
centersyriahardware <- ggmap(googleterrainsyria) +
  geom_point(aes( x = long, y = lat, size = hardware), data=datasp , alpha = 0.85,  color="#FFA600", fill="#FFA600")  +
  #Plot the voronoi lines
  #geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
  ggtitle("Community Centers in Syria - # of Hardware") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
ggsave("out/map-centersyriahardware.png", centersyriahardware, width=8, height=6,units="in", dpi=300)

rm(centersyriastaffbenef)
centersyriastaffbenef <- ggmap(googleterrainsyria) +
  geom_point(aes( x = long, y = lat, size = ratio.staff.benef), data=datasp , alpha = 0.85,  color="#FFA600", fill="#FFA600")  +
  #Plot the voronoi lines
  #geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
  ggtitle("Community Centers in Syria - Ratio Staff / Benef") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
ggsave("out/map-centersyriastaffbenef.png", centersyriastaffbenef, width=8, height=6,units="in", dpi=300)

rm(centersyriahardwarestaff)
centersyriahardwarestaff <- ggmap(googleterrainsyria) +
  geom_point(aes( x = long, y = lat, size = ratio.hardware.staff), data=datasp , alpha = 0.85,  color="#FFA600", fill="#FFA600")  +
  #Plot the voronoi lines
  #geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
  ggtitle("Community Centers in Syria - Ratio Hardware / Staff") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
ggsave("out/map-centersyriahardwarestaff.png", centersyriahardwarestaff, width=8, height=6,units="in", dpi=300)



##### need to make a map a map with small map inset

googleterraincham <- get_map(location = c(lon =36.33, lat =33.56 ), color = "bw", source = "google",maptype = "terrain", zoom = 10)
#googleterrain1cham <- ggmap(googleterraincham)
#googleterrain1cham
rm(centersyriacham)
centersyriacham <- ggmap(googleterraincham) +
  geom_point(aes( x = long, y = lat), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600")  +
  #Plot the voronoi lines
  #geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
  ggtitle("Community Centers in Damascus Area") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
#centersyriacham
ggsave("out/map-centersyriacham.png", centersyriacham, width=8, height=6,units="in", dpi=300)


###
googleterrainlat <- get_map(location = c(lon =36.1, lat = 35.22), color = "bw", source = "google",maptype = "terrain", zoom = 10)
rm(centersyrialat)
centersyrialat <- ggmap(googleterrainlat) +
  geom_point(aes( x = long, y = lat), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
  #add proportional symbol at centroids
  ggtitle("Community Centers near the coast ( Latakia, Tartous)") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
#centersyrialat
ggsave("out/map-centersyrialat.png", centersyrialat, width=8, height=6,units="in", dpi=300)

###
googleterrainhoms <- get_map(location = c(lon =36.71158, lat = 34.73767), color = "bw", source = "google",maptype = "terrain", zoom = 12)
rm(centersyriahoms)
centersyriahoms <- ggmap(googleterrainhoms) +
  geom_point(aes( x = long, y = lat), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
  #add proportional symbol at centroids
  ggtitle("Community Centers near Homs") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
#centersyriahoms
ggsave("out/map-centersyriahoms.png", centersyriahoms, width=8, height=6,units="in", dpi=300)

###
googleterrainhamas <- get_map(location = c(lon =36.7450, lat = 35.1409), color = "bw", source = "google",maptype = "terrain", zoom = 12)
rm(centersyriahamas)
centersyriahamas <- ggmap(googleterrainhamas) +
  geom_point(aes( x = long, y = lat), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
  #add proportional symbol at centroids
  ggtitle("Community Centers near Hamas") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
#centersyriahamas
ggsave("out/map-centersyriahamas.png", centersyriahamas, width=8, height=6,units="in", dpi=300)

###
googleterrainqarah <- get_map(location = c(lon =36.7367, lat = 34.1711), color = "bw", source = "google",maptype = "terrain", zoom = 11)
rm(centersyriaqarah)
centersyriaqarah <- ggmap(googleterrainqarah) +
  geom_point(aes( x = long, y = lat), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
  #add proportional symbol at centroids
  ggtitle("Community Centers near qarah") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
#centersyriaqarah
ggsave("out/map-centersyriaqarah.png", centersyriaqarah, width=8, height=6,units="in", dpi=300)

###
googleterraindaraa <- get_map(location = c(lon =36.4, lat = 32.7721), color = "bw", source = "google",maptype = "terrain", zoom = 10)
rm(centersyriadaraa)
centersyriadaraa <- ggmap(googleterraindaraa) +
  geom_point(aes( x = long, y = lat), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
  #add proportional symbol at centroids
  ggtitle("Community Centers near Suwayda") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
#centersyriadaraa
ggsave("out/map-centersyriadaraa.png", centersyriadaraa, width=8, height=6,units="in", dpi=300)

####
googleterrainaleppo <- get_map(location = c(lon =37.1302, lat = 36.215), color = "bw", source = "google",maptype = "terrain", zoom = 13)
rm(centersyriaaleppo)
centersyriaaleppo <- ggmap(googleterrainaleppo) +
  geom_point(aes( x = long, y = lat), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
  #Plot the voronoi lines
  #geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
  ggtitle("Community Centers near Aleppo") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
#centersyriaaleppo
ggsave("out/map-centersyriaaleppo.png", centersyriaaleppo, width=8, height=6,units="in", dpi=300)

####
googleterrainhassake <- get_map(location = c(lon =40.7023, lat = 36.5971), color = "bw", source = "google",maptype = "terrain", zoom = 9)
rm(centersyriahassake)
centersyriahassake <- ggmap(googleterrainhassake) +
  geom_point(aes( x = long, y = lat), data=datasp, alpha = 0.85, size = 2,  color="#FFA600", fill="#FFA600") +
  #Plot the voronoi lines
  #geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
  ggtitle("Community Centers near Hassake") +
  theme(plot.title=element_text(face="bold", size=11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
#centersyriahassake
ggsave("out/map-centersyriahassake.png", centersyriahassake, width=8, height=6,units="in", dpi=300)


### Assemble all of them in oone page

# Generate Infographic in PDF format
library(grid)
library(gridExtra)
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

## Set the size of the PDF to an exact landscape A4 -- measure in inche
pdf("out/mapall.pdf", width = 8.267 , height =11.692 )
grid.rect(gp = gpar(fill = "#FFFFFF", col = "#FFFFFF"))
lay_out(list(centersyriacham, 1, 1),
        list(centersyriaaleppo, 1, 2),
        list(centersyriahoms, 2, 1),
        list(centersyriahamas, 2, 2),
        list(centersyrialat, 3, 1),
        list(centersyriaqarah, 3, 2),
        list(centersyriadaraa, 4, 1),
        list(centersyriahassake, 4, 2))
dev.off()#
