makeDataset_overlap<-function(otu){
  
  #params for overlapSp
  plotpars<-assign_plottingParams()
  overlapSpLevels<-plotpars[['overlapSp']][['levels']]
  
  #subset otu data
  mat.otu<-otu[['mat.otu']]
  mat.samp<-otu[['mat.samp']]
  mat.samp.sub<-mat.samp[mat.samp$species4 %in% overlapSpLevels & mat.samp$bag == "none"  & mat.samp$logLoc != "mush",] 
  mat.otu.sub<-mat.otu[mat.samp$species4 %in% overlapSpLevels & mat.samp$bag == "none"  & mat.samp$logLoc != "mush",]
  rownames(mat.otu.sub)<-mat.samp.sub$sampleName #add back row names
  
  #presence/absence data
  mat.otu.pres <- 1 * (mat.otu.sub > 0)  #perform presence/absence standardization
  
  #environment variables
  envVars <- data.frame(mat.samp.sub[, c("yrdeploy","topo", "logLoc","species4","angio.gymno")])
  envVars$yrdeploy<-factor(envVars$yrdeploy)
  envVars$topo<-factor(envVars$topo)
  envVars$angio.gymno<-factor(envVars$angio.gymno)
  envVars<-droplevels.data.frame(envVars)
  #levels(envVars$yrdeploy)<-c("2011","2009")
  #levels(envVars$topo)<-c("Low","High")
  
  dataset_overlap<-list(otus=mat.otu.pres, envs=envVars)
  return(dataset_overlap)
}

makeDataset_subset09<-function(otu){
  #subset otu data
  mat.otu<-otu[['mat.otu']]
  mat.samp<-otu[['mat.samp']]
  
  mat.samp.sub<-mat.samp[mat.samp$yrdeploy==2009 & mat.samp$bag == "none" & mat.samp$logLoc != "mush",] 
  mat.otu.sub<-mat.otu[mat.samp$yrdeploy==2009 & mat.samp$bag == "none" & mat.samp$logLoc != "mush",]
  rownames(mat.otu.sub)<-mat.samp.sub$sampleName #add back row names
  
  #presence/absence data
  mat.otu.pres <- 1 * (mat.otu.sub > 0)  #perform presence/absence standardization
  
  #environment variables
  envVars <- data.frame(mat.samp.sub[, c("topo", "logLoc","species4","angio.gymno")])
  envVars$topo<-factor(envVars$topo)
  envVars$angio.gymno<-factor(envVars$angio.gymno)
  envVars<-droplevels.data.frame(envVars)
  #levels(envVars$topo)<-c("Low","High")
  
  dataset_d09<-list(otus=mat.otu.pres, envs=envVars)
  return(dataset_d09)
}

makeDataset_subset11<-function(otu){
  #subset otu data
  mat.otu<-otu[['mat.otu']]
  mat.samp<-otu[['mat.samp']]
  mat.samp.sub<-mat.samp[mat.samp$yrdeploy==2011 & mat.samp$bag == "none" & mat.samp$logLoc != "mush",] 
  mat.otu.sub<-mat.otu[mat.samp$yrdeploy==2011 & mat.samp$bag == "none" & mat.samp$logLoc != "mush",]
  rownames(mat.otu.sub)<-mat.samp.sub$sampleName #add back row names
  
  #presence/absence data
  mat.otu.pres <- 1 * (mat.otu.sub > 0)  #perform presence/absence standardization
  
  #environment variables
  envVars <- data.frame(mat.samp.sub[, c("topo", "logLoc","species4","angio.gymno")])
  envVars$topo<-factor(envVars$topo)
  envVars$angio.gymno<-factor(envVars$angio.gymno)
  envVars<-droplevels.data.frame(envVars)
  #levels(envVars$topo)<-c("Low","High")
  
  dataset_d11<-list(otus=mat.otu.pres, envs=envVars)
  return(dataset_d11)
}