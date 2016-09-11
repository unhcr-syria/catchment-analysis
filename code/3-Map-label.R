### Get map Background
## source("code/3-Map-require.R")
############################################################
## Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.



for (i in 3:5 ) {
  #i <- 3
  variablename <- names(datasp)[i]
  title <- attributes(datasp)$variable.labels[i]
  plot <- ggmap(googleterrainsyria)  + 
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2] ), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    
    ggtitle(paste("Map: ", title , sep="")) +
    theme(plot.title=element_text(face="bold", size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/map_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
  rm(plot)
  
  #rm(centersyriacham)
  centersyriacham <- ggmap(googleterraincham) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2]), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    ggtitle("Community Centers in Damascus Area") +
    theme(plot.title=element_text(face="bold", size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  #rm(centersyrialat)
  centersyrialat <- ggmap(googleterrainlat) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2]), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    ggtitle("Community Centers near the coast ( Latakia, Tartous)") +
    theme(plot.title=element_text(face="bold", size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  #rm(centersyriahoms)
  centersyriahoms <- ggmap(googleterrainhoms) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2]), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    ggtitle("Community Centers near Homs") +
    theme(plot.title=element_text(face="bold", size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  #rm(centersyriahamas)
  centersyriahamas <- ggmap(googleterrainhamas) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2]), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    ggtitle("Community Centers near Hamas") +
    theme(plot.title=element_text(face="bold", size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  #rm(centersyriaqarah)
  centersyriaqarah <- ggmap(googleterrainqarah) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2]), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    ggtitle("Community Centers near qarah") +
    theme(plot.title=element_text(face="bold", size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  #rm(centersyriadaraa)
  centersyriadaraa <- ggmap(googleterraindaraa) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2]), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    ggtitle("Community Centers near Suwayda") +
    theme(plot.title=element_text(face="bold", size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  #rm(centersyriaaleppo)
  centersyriaaleppo <- ggmap(googleterrainaleppo) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2]), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    ggtitle("Community Centers near Aleppo") +
    theme(plot.title=element_text(face="bold", size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  #rm(centersyriahassake)
  centersyriahassake <- ggmap(googleterrainhassake) +
    geom_point(aes_string( x = names(datasp)[1], y = names(datasp)[2]), data=datasp , size=2, alpha = 0.85,  color="#FFA600", fill="#FFA600" ) +
    geom_label_repel(aes_string(label= names(datasp)[i], x = names(datasp)[1], y = names(datasp)[2] ), data=datasp ,size = 2, fontface = 'bold', color = 'black',  box.padding = unit(0.25, "lines"), point.padding = unit(0.5, "lines")) +
    ggtitle("Community Centers near Hassake") +
    theme(plot.title=element_text(face="bold", size=10),
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
  #lay_out(          list(centersyriacham, 1, 1),  list(centersyriaaleppo, 1, 2), list(centersyriahoms, 2, 1),
  #                  list(centersyriahamas, 2, 2),  list(centersyrialat, 3, 1), list(centersyriaqarah, 3, 2),
  #                  list(centersyriadaraa, 4, 1), list(centersyriahassake, 4, 2))
  
  #dev.off()
  
  #allmap <- multiplot(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake, cols=2)
  #allmap <- grid.arrange(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake, ncol=2)
  #
  
  allmap <- arrangeGrob(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake, ncol=2)
  ggsave(filename=paste("out/map_",i,variablename,"all.png",sep=""), allmap, width=8, height=16, units="in", dpi=300)
  ##
  rm(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake)
  rm(variablename, title)
  rm(allmap)
}