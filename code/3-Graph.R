

############################################################
# function to map variable


## Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.

p <- ggplot(data.frame(datasp), aes(y=datasp$benef)) + ylab("# of Ind") + scale_y_continuous()
for (i in 9:57 ) {
  #i <- 10
  rm(variablename)
  variablename <- names(datasp)[i]
  title <- attributes(datasp)$variable.labels[i]
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
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/plot_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}