#help_tidyMicrobData.R


# Function to read-in many OTU tables with a similar pattern filename
ReadOTUcsv<-function(table.names, fileName.part1, fileName.part2){
  otuTab.list<-list()
  i<-0
  for(i in 1:length(table.names)){
    fileName<-paste(fileName.part1, table.names[i],fileName.part2, sep = "")
    otuTab.list[[i]]<-read.delim(fileName,
                                 header=TRUE,                 #make the barcodes the headers
                                 stringsAsFactors = FALSE,    #don't automatically create factors
                                 strip.white = TRUE,          #column names have trailing white space
                                 row.names = 1)               #make the OTU ids the row names
  }
  names(otuTab.list)<-table.names
  return(otuTab.list)
}

##############################################

# Function to change rownames from barcodes to sampleNames
BarcodeToSamp<-function(otuTab.list.t, lookupTab, yrharv){
  
  otuTab.list.ta<-list()
  i<-0
  for(i in 1:length(otuTab.list.t)){
    
    #make the barcode that is in otutable rownames a real column
    Barcode<-row.names(otuTab.list.t[[i]])
    otuTab.t.curr<-data.frame(cbind(otuTab.list.t[[i]],Barcode))
    
    ##### starts to differ between 2012 and 2014 OTU datasets
    if(yrharv==2012){
      #for 2012 #############
      #simplify the lookup table between sample ids and barcode
      lookupTab.relevant<-lookupTab[lookupTab$Barcode %in% otuTab.t.curr$Barcode,]
      lookupTab.relevant<-lookupTab.relevant[,c("Barcode","sampleName")]
      
      #merge the otutable with the lookup table to match a sampleName to each row
      otuTab.t.curr<-merge(otuTab.t.curr, lookupTab.relevant, by="Barcode", all.x=TRUE)
      
      #put the sampleName as the rownames and get rid of extra columns
      row.names(otuTab.t.curr)<-otuTab.t.curr$sampleName
      otuTab.list.ta[[i]]<-otuTab.t.curr[,grepl("OTU_", colnames(otuTab.t.curr))]
    }else{
      #for 2014 ###############
      #separate that column to make it like sampleName1
      otuTab.t.curr<-separate(otuTab.t.curr, col=otuRowName, into=c("dir","gene","sampleName1"), sep="_", remove=FALSE, extra="merge")
      
      #put the sampleName1 as the rownames and get rid of extra columns
      row.names(otuTab.t.curr)<-otuTab.t.curr$sampleName1
      otuTab.list.ta[[i]]<-otuTab.t.curr[,grepl("OTU_", colnames(otuTab.t.curr))]
    }
    
    
  }
  
  names(otuTab.list.ta)<-names(otuTab.list.t)
  return(otuTab.list.ta)
}



#######################

# Function to write many OTU tables with a similar pattern filename
WriteOTUcsv<-function(otuTab.list.t, fileName.part1, fileName.part2){

  fileNames<-paste(fileName.part1, names(otuTab.list.t), fileName.part2, sep="")
  i<-0
  for(i in 1:length(otuTab.list.t)){
    write.csv(otuTab.list.t[[i]], file = fileNames[i], row.names = TRUE)
  }
  
}


# Function to identify samples in OTU table that are relevant to rotplot study
FindSamples<-function(otuTab.list.t, lookupTab){
  
  otuTab.list.samps<-list()
  i<-0
  for(i in 1:length(otuTab.list.t)){
    
    #make the sampleName that is in otutable rownames a real column
    sampleName<-row.names(otuTab.list.t[[i]])
    otu.curr<-data.frame(cbind(otuTab.list.t[[i]],sampleName))
    
    #simplify the lookup table between sample ids and barcode
    lookupTab.relevant<-lookupTab[lookupTab$sampleName %in% otu.curr$sampleName,]
    lookupTab.relevant<-lookupTab.relevant[,c("sampleName","sampleType")]
    
    #merge the otutable with the lookup table to match a sampleType to each row
    otu.curr<-merge(otu.curr, lookupTab.relevant, by="sampleName", all.x=TRUE)
    
    #subset the rotplot samples
    otu.curr<-otu.curr[otu.curr$sampleType=="rotplot",]
    
    #get rid of OTU columns that now have only 0s (in other other words, these OTUs were exclusively found in non rotplot samples)
    dataCols<-grep("OTU_", colnames(otu.curr))
    otu.tmp<-data.frame(apply(otu.curr[,dataCols], 2, function(x) as.numeric(as.character(x)))) # everything had been coded as a factor, ugh
    extra.otus<-names(colSums(otu.tmp)[colSums(otu.tmp) == 0]) # otus that do not show up in the study samples
    otu.curr.trim<-otu.curr[,!colnames(otu.curr) %in% extra.otus]
    
    #put the sampleName as the rownames and get rid of extra columns
    row.names(otu.curr.trim)<-otu.curr.trim$sampleName
    otuTab.list.samps[[i]]<-otu.curr.trim[,grepl("OTU_", colnames(otu.curr.trim))]
    
  }
  names(otuTab.list.samps)<-names(otuTab.list.t)
  
  return(otuTab.list.samps)
}
