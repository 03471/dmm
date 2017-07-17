#update the line blow with working directory on computer
setwd('D:/Google Drive/masters/MSDA/CS5310/cp/phase/6')
##checking and installing required packages
packages <- c('caret','lfda','FSelector')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
##referencing required packages
require(caret)
require(MicrosoftML)
require(MicrosoftR)
require(RevoScaleR)
require(lfda)
require(FSelector)
data <- rxImport("basedata_t_V05.xdf")


#check column names see working on correct data set

#data <- data[,-c(1,3)]

formula <- as.formula(paste("c2~",paste(names(data), collapse='+'),sep=""))

lrModel <- rxLogisticRegression(formula = formula, data, type = c('multiClass'),memorySize = 100,showTrainingStats = TRUE,normalize = 'auto', blocksPerRead = 10000,reportProgress = rxGetOption('reportProgress'), verbose = 1)

rxDForest(formula = formula, data = data, method = "class", parms = list(loss = c(0, 4, 1, 0)), maxDepth = 5, nTree = 20, mTry = 2, seed = 8, blocksPerRead = 30)

filteredData <- rawData <- data[,!names(data) %in% c('c1','c3')]

corFormula <- as.formula(paste("~",paste(names(filteredData), collapse='+'),sep=""))
#rxCor(formula = corFormula, data = filteredData)


#https://blogs.msdn.microsoft.com/microsoftrservertigerteam/2017/03/23/feature-engineering-using-r/
informationGain <- information.gain(c2~., filteredData)
top200Features <- cutoff.k(ig_values, 200)#out of memory error

rfeController <- rfeControl(functions=lmFuncs, method="repeatedcv", repeats = 100, verbose = TRUE)
lmProfiler <- rfe(filteredData[,2:5351], filteredData[,1], sizes =  c(1:200), rfeControl = rfeController)

help(rfe)



