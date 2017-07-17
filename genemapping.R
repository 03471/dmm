#update the line blow with working directory on computer
setwd('D:/Google Drive/masters/MSDA/CS5310/cp/phase/6')
##checking and installing required packages
packages <- c('plyr')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
##referencing required packages
require(plyr)

original<- as.data.frame(read.csv('columnnamesmap.csv'))
original <- original[,2:3]
original <- as.data.frame(original)

#after feature elemination with random forest importance

rfe <- as.data.frame(read.csv('rforest importance.csv'))
rfeSubset <- rfe[which(rfe$IncNodePurity > 0),]
names(rfeSubset) <- 
join(df1, df2, type = "inner")

