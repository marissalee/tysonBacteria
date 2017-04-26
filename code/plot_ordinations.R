# species x topo x yrdeploy
makePlot_overlap_sppTopoYr<-function(dataset_overlap_otu.its.12){
  
  par(mfrow=c(1,1))
  
  datasetList<-list(dataset_overlap_otu.its.12)
  datasetListNames<-c("2012 Harvest")
  for(o in 1:length(datasetList)){
    dataset_overlap<-datasetList[[o]]
    
    #dataset_overlap<-dataset_overlap_otu.its.12
    mat.otu.pres<-dataset_overlap[["otus"]]
    envVars<-dataset_overlap[["envs"]]
    
    #params for overlapSp
    plotpars<-assign_plottingParams()
    overlapSpLevels<-plotpars[['overlapSp']][['levels']]
    overlapSpColors<-plotpars[['overlapSp']][['colors']]
    logLocShapes<-plotpars[['logLoc']][['shapes']]
    
    #calc dist
    dist.jacc <- vegdist(mat.otu.pres, method = "jaccard", binary = TRUE)  #calculate jaccard index
    #do PCoA
    ord<-cmdscale(dist.jacc)
    df<-envVars #rename environmental matrix
    
    #plotting dataframe
    dford<-data.frame(PCoA1=ord[,1], PCoA2=ord[,2])
    df$grp<-paste(df$topo, df$species4, df$yrdeploy)
    dford$grp<-df$grp
    dford$grp<-factor(dford$grp)
    LEVELS<-levels(dford$grp)
    LEVELS
    
    # define colors
    overlapSpColors_dark<-c("#a05564","#385130","#355470")
    COLORS<-c(rep(overlapSpColors, each=2), rep(overlapSpColors_dark, each=2))
    dford$color<-NA
    i<-0
    for(i in 1:length(LEVELS)){
      dford[dford$grp==LEVELS[i],"color"]<-COLORS[i]
    }
    
    # define linetypes
    LINETYPES<-rep(c(1,2), length=12)
    
    # define point shapes for top/bottom
    dford$logLoc<-df$logLoc
    LEVELS2<-levels(dford$logLoc)
    SHAPES<-logLocShapes
    dford$shape<-NA
    i<-0
    for(i in 1:length(LEVELS2)){
      dford[dford$logLoc==LEVELS2[i],"shape"]<-SHAPES[i]
    }
    
    #plot
    fit<-envfit(ord ~ grp, df)
    with(dford, plot(PCoA1, PCoA2, type="n", asp=1, xlim=c(-0.25, 0.35), ylim=c(-0.35, 0.3)))
    with(dford, points(PCoA1, PCoA2, col=dford$color, pch=dford$shape))
    i<-0
    for(i in 1:length(LEVELS)){
      with(fit, ordiellipse(ord, df$grp, kind="se", conf=0.95, lwd = 2,
                            lty=LINETYPES[i], col=COLORS[i], show.groups = LEVELS[i], label=TRUE))
    }
    
    # add centroid arrow to connect cohort levels
    firstOne<-c(1,3,5,7,9,11)
    i<-0
    for(i in 1:length(firstOne)){
      arrows(x1=fit$factors$centroids[firstOne[i],1], y1=fit$factors$centroids[firstOne[i],2],
             x0=fit$factors$centroids[firstOne[i]+1,1], y0=fit$factors$centroids[firstOne[i]+1,2], col=COLORS[firstOne[i]], lwd=2)
    }
    mtext(datasetListNames[o], adj=0, font=2)
  }
  
  # pdf("output/fancyOrdPlot_legend.pdf")
  # par(mfrow=c(1,1))
  # plot(ord, type="n", axes=FALSE, ylab="", xlab="")
  # legend("center", legend=c(LEVELS), col=COLORS, lty=LINETYPES, lwd=2)
  # legend("bottom", legend=c("top","bottom"), pch=c(SHAPES[1], SHAPES[3]))
  # dev.off()
  
}


subsetPlottingBy<-function(fileName, datasetList, GRPname){
  par(mfrow=c(1,2))
  par(mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
  datasetListNames<-datasetList.longnames
  o<-0
  for(o in 1:length(datasetList)){
    dataset_overlap<-datasetList[[o]]
    
    #dataset_overlap<-dataset_overlap_otu.its.12
    mat.otu.pres<-dataset_overlap[["otus"]]
    envVars<-dataset_overlap[["envs"]]
    
    #calc dist
    dist.jacc <- vegdist(mat.otu.pres, method = "jaccard", binary = TRUE)  #calculate jaccard index
    #do PCoA
    ord<-cmdscale(dist.jacc)
    #mat.plot <- data.frame(ord) #do classical (metri) multidimensional scaling, aka PCoA
    #colnames(mat.plot)<-c("pcoa1","pcoa2") #rename cols
    df<-envVars #rename environmental matrix
    
    #plotting dataframe
    dford<-data.frame(PCoA1=ord[,1], PCoA2=ord[,2])
    df$grp<-df[,colnames(df)==GRPname]
    dford$grp<-df$grp
    dford$grp<-factor(dford$grp)
    LEVELS<-levels(dford$grp)
    LEVELS
    
    #plot
    fit<-envfit(ord ~ grp, df)
    with(dford, plot(PCoA1, PCoA2, axes=FALSE, type="n", asp=1, xlim=c(-0.3, 0.3), ylim=c(-0.3,0.3)))
    box()
    if(o %in% c(1,2)){
      axis(1, at=seq(-0.25, 0.25, 0.25))
    }
    if(o %in% c(1)){
      axis(2, at=seq(-0.25, 0.25, 0.25))
    }
    
    i<-0
    for(i in 1:length(LEVELS)){
      if(GRPname=="logLoc"){
        with(fit, ordiellipse(ord, df$grp, kind="se", conf=0.95, lwd = 2,
                              lty=1, col=c(1,4), show.groups = LEVELS[i], label=TRUE))
      }
      if(GRPname=="topo"){
        with(fit, ordiellipse(ord, df$grp, kind="se", conf=0.95, lwd = 2,
                              lty=1, col=c(4,1), show.groups = LEVELS[i], label=TRUE))
      }
      if(GRPname=="species4"){
        with(fit, ordiellipse(ord, df$grp, kind="se", conf=0.95, lwd = 2,
                              lty=1, col=1:length(LEVELS), show.groups = LEVELS[i], label=TRUE))
      }
      
    }
    mtext(datasetListNames[o], side=3, line=-1, adj=0.1, cex=0.8)
  }
  mtext("PCoA 1", side = 1, outer = TRUE, cex = 1, line = 2.2)
  mtext("PCoA 2", side = 2, outer = TRUE, cex = 1, line = 2.2)
}

