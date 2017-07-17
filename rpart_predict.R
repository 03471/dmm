#==============================================================================================================================================================================================================================
#Descirption: 
#Inputs:  
#Output file: 
#==============================================================================================================================================================================================================================
#update the line blow with working directory on computer
setwd('D:/Google Drive/masters/MSDA/CS5310/cp/phase/6')
##checking and installing required packages
packages <- c('caret','rpart','gmodels','rpart.plot')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
##referencing required packages
require(caret)
require(MicrosoftML)
require(MicrosoftR)
require(RevoScaleR)
require(rpart)
require(gmodels)
require(rpart.plot)

data <- read.csv('reduced_data.csv')
#data <- data[,names(data) != c('X')]

factor_redData <- data$factor_vector 
num_redData <- data[-c(1:2)] 
str(num_redData) 

baseData <- cbind(factor_redData,num_redData)

rpart_melanoma <- rpart(factor_redData~., data=baseData)
pred_melanoma <- predict(rpart_melanoma,baseData[1:100,],type = "class")
CrossTable(factor_redData[1:100],pred_melanoma)

rpart.plot(rpart_melanoma, digits = 2)

rpart_melanoma$variable.importance # rpart is running on 5565 variables and getting a list of 33 imp. variables

Var_rpart <- rpart_melanoma$variable.importance

columnNames <- paste(names(Var_rpart),collapse=',')
con <- c(names(Var_rpart))
dim(con)
reduced_data <- baseData[,names(baseData) %in% c(strsplit(names(Var_rpart), " "))]
dim(reduced_data)
reduced_data <- cbind(factor_redData,reduced_data)

# let's run a train model with train using cv and tuning parameters

ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "oneSE")

grid <- expand.grid(cp=c(.010,.014,.018,.021,.039,.102,.718))
#modelLookup("rpart")

rpart_train <- train(factor_redData~., data=reduced_data,method="rpart", trControl = ctrl, tuneGrid = grid, metric = "kappa")

predict_rpart <- predict(rpart_train,baseData[1:100,])
CrossTable(factor_redData[1:100],predict_rpart)

rpart_train$results

rpart_train$finalModel
rpart.plot(rpart_train$finalModel, digits = 2)
rpart_train$metric

data_z <- data.frame(lapply(num_redData,scale))
melanoma_pca <- prcomp(data_z)
cumsum_pca <- (cumsum(melanoma_pca$sdev^2)/sum(melanoma_pca$sdev^2))*100
sum(cumsum_pca[1:5]) # 62 % contribution from these 5 PCs






















