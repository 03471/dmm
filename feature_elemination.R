#==============================================================================================================================================================================================================================
#
#
#Descirption: 
#Inputs:  
#Output file: 
#==============================================================================================================================================================================================================================
#update the line blow with working directory on computer
setwd('D:/Google Drive/masters/MSDA/CS5310/cp/phase/6')
##checking and installing required packages
packages <- c('caret')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
##referencing required packages
require(caret)
require(MicrosoftML)
require(MicrosoftR)
require(RevoScaleR)


data <- rxImport("basedata_t_V05.xdf")
#check column names see working on correct data set
colnames(data)

nzv_cols <- nearZeroVar(data)
data <- data[,-nzv_cols]

dararevo <- rxImport(inData = data,outFile = "basedata_t_V05.xdf",stringsAsFactors = TRUE,missingValueString="M",rowsPerRead = 200000)


formula <- as.formula(paste("c2~",paste(names(data), collapse='+'),sep=""))

logitModel <- rxLogisticRegression(formula = formula, data, type = c('multiClass'),memorySize = 20,showTrainingStats = TRUE,normalize = 'auto', blocksPerRead = 10000,reportProgress = rxGetOption('reportProgress'), verbose = 1)
summary(logitModel)
data <- cbind(data,isCase)
scoreDF <- rxPredict(logitModel, data = data, extraVarsToWrite = "isCase")


"rxLogisticRegression(formula = NULL, data, type = c('binary', 'multiClass'),
                     l2Weight = 1, l1Weight = 1, optTol = 1e-07, memorySize = 20,
                     initWtsScale = 0, maxIterations = 2147483647, showTrainingStats = FALSE,
                     sgdInitTol = 0, trainThreads = NULL, denseOptimizer = FALSE,
                     normalize = 'auto', mlTransforms = NULL, mlTransformVars = NULL,
                     rowSelection = NULL, transforms = NULL, transformObjects = NULL,
                     transformFunc = NULL, transformVars = NULL, transformPackages = NULL,
                     transformEnvir = NULL, blocksPerRead = rxGetOption('blocksPerRead'),
                     reportProgress = rxGetOption('reportProgress'), verbose = 1,
                     computeContext = rxGetOption('computeContext'), ...)"

