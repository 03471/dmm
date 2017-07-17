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
data <- rxImport("basedata_t_V06.xdf")

#disable exponential notation
options(scipen=999)

#check column names see working on correct data set

#data <- data[,-c(1,3)]

formula <- as.formula(paste("c2~",paste(names(data), collapse='+'),sep=""))

lrModel <- rxLogisticRegression(formula = formula, data, type = c('multiClass'),memorySize = 100,showTrainingStats = TRUE,normalize = 'auto', blocksPerRead = 10000,reportProgress = rxGetOption('reportProgress'), verbose = 1)

rxDForest(formula = formula, data = data, method = "class", parms = list(loss = c(0, 4, 1, 0)), maxDepth = 5, nTree = 20, mTry = 2, seed = 8, blocksPerRead = 30)

filteredData <- data[,!names(data) %in% c('c1','c3')]

corFormula <- as.formula(paste("~",paste(names(filteredData), collapse='+'),sep=""))
#rxCor(formula = corFormula, data = filteredData)


#https://blogs.msdn.microsoft.com/microsoftrservertigerteam/2017/03/23/feature-engineering-using-r/
informationGain <- information.gain(c2~., filteredData)
top200Features <- cutoff.k(ig_values, 200)#out of memory error

rfeController <- rfeControl(functions=lmFuncs, method="repeatedcv", repeats = 100, verbose = TRUE)
lmProfiler <- rfe(filteredData[,2:5351], filteredData[,1], sizes =  c(1:200), rfeControl = rfeController)

help(rfe)

"Embedded Methods
Certain Machine Learning Algorithms perform variable selection as a part of the training process.
Ranking Features by Importance:
Random Forest is one such algorithm that is not only used for predictions but also for understanding 
the importance of the variables. In Microsoft R Server, there is a parameter called 'importance' in the 
randomForest library - rxDForest call that could be set to TRUE to retrieve this information."

rfFormula <- as.formula(paste("c2~",paste(names(filteredData[,names(filteredData) != c('c2')]), collapse='+'), sep=""))

#blocksPerRead = 30, maxDepth = 5,nTree=20, mTry=2, method="class", seed = 8
randomForestModel <- rxDForest(formula = rfFormula, data = filteredData, importance = TRUE, blocksPerRead = 30, maxDepth = 5, nTree=20)
#this requires 2hrs computation - can be precised with max depth and nTree params
#187 features selected
write.csv(randomForestModel$importance,'dforest importance.csv')

featureImportance <- as.data.frame(read.csv('dforest importance.csv'))
featureImportanceSubset <- featureImportance[which(featureImportance$IncNodePurity > 0),]

#see selected features
dim(featureImportanceSubset)

#highlight column importance with csv

featureSelectedData <- data[,names(data) %in% featureImportanceSubset$Feature]
#bind class features with original dataset
featureSelectedData <- cbind(data[,names(data) %in% featureImportanceSubset$Feature], data[,names(data) %in% c('c1','c2','c3')])

#exporting both csv and xdf formats  basedata_t_V06.csv = basedata_t_V06.xdf required in feature use or auditing
write.csv(featureSelectedData, 'basedata_t_V06.csv')
genomeData <- rxImport(inData=featureSelectedData, outFile = "basedata_t_V06.xdf",stringsAsFactors = TRUE, missingValueString = "M", rowsPerRead = 200000)

#help(trainControl)
evalData <- filteredData[,!names(filteredData) %in% c('c1','c3')]

evalFormula <- as.formula(paste("c2~",paste(names(evalData[,names(evalData) != c('c2')]), collapse='+'), sep=""))

trainControl <- trainControl(method='repeatedcv', number =10, selectionFunction = "oneSE",allowParallel = TRUE,verboseIter = TRUE)
mtryGrid <- expand.grid(mtry = 100)

rfTune <- train(evalFormula, data = evalData, method="rf",ntree = 100, metric='kappa', tuneGrid=mtryGrid,trControl=trainControl)

"
mtryGrid <- expand.grid(mtry = 100) # you can put different values for mtry
rfTune<- train(x = trainX,
y = trainY,
method = 'rf',
trControl = ctrl,
metric = 'Kappa',
ntree = 1000,
tuneGrid = mtryGrid,
strata = factor(trainY),
sampsize = c(80, 80), 
importance = TRUE)
"
#include http://www.rprogramming.info/snippet/rxneuralnet_withtuningr_qwei2011_r

