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
packages <- c('caret','rpart')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
##referencing required packages
require(caret)
require(MicrosoftML)
require(MicrosoftR)
require(RevoScaleR)
require(rpart)

data <- read.csv('reduced_data.csv')
data <- data[,names(data) != c('X')]

fit <- rpart(as.factor(factor_vector)~ .,method="class", data=data)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

fit$variable.importance

