load_mytheme<-function(){
  mytheme <- theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(panel.border = element_rect(colour = "black"),      #put a black box around the plotting area
          axis.line = element_line(colour = "black"),                 #axis lines are in black
          panel.grid.major = element_blank(),                         #turn off the gridlines
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(face='bold.italic', hjust=0.05),         #turn off the x axis facet labels
          strip.text.y = element_text(face='bold.italic', hjust=0.05),
          strip.background = element_rect(fill = 'white', colour='black'),    #make y axis facet labels be italic and top justified
          legend.key = element_blank(),                               #turn off box around legend
          plot.title=element_text(hjust=0, vjust=0.5, face='bold'), #style and position of the panel label
          plot.margin = unit(c(0.05,0.05,0.05,0.05),"in")
    )
  return(mytheme)
}

assign_plottingParams<-function(){
  #logLoc shapes
  logLocLevels<-c("t","mush","b")
  logLocShapes<-c("t"=17, "mush"=16, "b"=6)
  logLocLabels<-c("top","mush","bottom")
  logLoc<-list(levels=logLocLevels, labels=logLocLabels, shapes=logLocShapes)
  
  #species4 colors
  species4Levels<-c("ACRU","AEGL","AMAR","ASTR","CATO","CEDA","CEOC","CHRY","COFL","DIVI","FRAM","GLTR","HDMP","HICK","JUNI","JUVI","LOMA","PIEC","PINE","PIST","PLOC","PRSE/PRVI","QUAL","QUVE","ROAK","ROSE","ULRU","VIVU","ZEBRA")
  species4Colors<-rainbow_hcl(29)
  names(species4Colors)<-species4Levels
  species4<-list(levels=species4Levels, labels=species4Levels, colors=species4Colors)
  
  #identify overlapping species
  overlapSpLevels<-c("CEOC","JUVI","QUVE")
  overlapSpColors<-rainbow_hcl(3)
  names(overlapSpColors)<-overlapSpLevels
  overlapSp<-list(levels=overlapSpLevels, labels=overlapSpLevels, colors=overlapSpColors)
  
  #identify yrdeploy2009 species
  species09Levels<-c("ACRU","AEGL","AMAR","ASTR" ,"CATO","CEOC","COFL","DIVI","GLTR","JUVI","PIST","PLOC" ,"PRSE/PRVI","QUVE","ULRU","VIVU")
  species09Colors<-rainbow_hcl(16)
  names(species09Colors)<-species09Levels
  species09<-list(levels=species09Levels, labels=species09Levels, colors=species09Colors)
  
  #identify yrdeploy2011 species
  species11Levels<-c("CEOC","FRAM", "JUNI", "JUVI", "LOMA", "PIEC", "QUAL", "QUVE")
  species11Colors<-rainbow_hcl(8)
  names(species11Colors)<-species11Levels
  species11<-list(levels=species11Levels, labels=species11Levels, colors=species11Colors)
  
  #topo colors
  topoLevels<-c("H","L")
  topoColors<-c("black","darkgray")
  names(topoColors)<-topoLevels
  topo<-list(levels=topoLevels, labels=topoLevels, colors=topoColors)
  
  plotpars<-list(logLoc=logLoc, species4=species4, topo=topo, overlapSp=overlapSp, species09=species09, species11=species11)
  return(plotpars)
}