##setting working directory
setwd("D:/Google Drive/masters/MSDA/STAT5311/homeworks/1")
##checking and installing required packages
packages <- c("car","readxl","factoextra","ggplot2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
##referencing required packages
require("car")
require("readxl")
require("factoextra")
require("ggplot2")
## importing data
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",", header = FALSE)

## standardise the variables
standardisedconcentrations <- as.data.frame(scale(wine[2:14]))
wine.pca <- prcomp(standardisedconcentrations)

summary(wine.pca)

#screeplot(wine.pca,npcs = min(10, length(wine.pca$sdev)),type = "lines")
head(wine.pca$x)
##--------------------------------------------------------------------------------------------------------##
##Question-3
"3.a Identify the variable which has the largest contribution to AC's first principal component (PC1)."
fviz_contrib(wine.pca, choice = "var", axes = 1)
"3.b 1.	Also, identify the variable with the largest contribution to PC2."
fviz_contrib(wine.pca, choice = "var", axes = 2)
##--------------------------------------------------------------------------------------------------------##
##Question-4
"Remove these two variables from wine, and call the new data set wine2. Perform a principal
components analysis of wine2."

wine2 = wine[-c(8,11)]
standardisedconcentrations4 <- as.data.frame(scale(wine2[2:12]))
wine2.pca <- prcomp(standardisedconcentrations4)
summary(wine2.pca)
##--------------------------------------------------------------------------------------------------------##
##Question-5
"What is the total variance of the PC's? What is the standard deviation of PC1? What is the
variance of PC1? What proportion of the total variance does PC1 explain?"

##a What is the total variance of the PC's?
totalVariance <-sum(wine2.pca$sdev^2)
##b What is the standard deviation of PC1?
#summary(wine2.pca)[1]
sdPC1 <- 1.9700541
##c What is the variance of PC1?
variancePC1 <- sdPC1 ^2
##d What proportion of the total variance does PC1 explain?"
fviz_screeplot(wine2.pca, ncp=11, addlabels=TRUE)
##--------------------------------------------------------------------------------------------------------##
##Question-6
"Compute PC1 for each of the 178 wine samples. Now find the sample variance of these 178
values and verify that it matches the variance in the previous question."
calcpc <- function(variables,loadings)
{
  # find the number of samples in the data set
  as.data.frame(variables)
  numsamples <- nrow(variables)
  # make a vector to store the component
  pc <- numeric(numsamples)
  # find the number of variables
  numvariables <- length(variables)
  # calculate the value of the component for each sample
  for (i in 1:numsamples)
  {
    valuei <- 0
    for (j in 1:numvariables)
    {
      valueij <- variables[i,j]
      loadingj <- loadings[j]
      valuei <- valuei + (valueij * loadingj)
    }
    pc[i] <- valuei
  }
  return(pc)
}
#function call calcpc(standardisedconcentrations, wine.pca$rotation[,1])
var(calcpc(standardisedconcentrations, wine.pca$rotation[,1]))
##--------------------------------------------------------------------------------------------------------##
##Question-7
"Produce a scree plot. What proportion of the total variance is explained by the PC's 
to the left of the elbow? (Don't include the PC associated with the elbow.)"
fviz_screeplot(wine.pca, ncp=14, addlabels=TRUE)
varianceWine <- wine.pca$sdev^2
sum(varianceWine[1:3]) / sum(varianceWine[1:3],varianceWine[5:13]) *100
##--------------------------------------------------------------------------------------------------------##
##Question-8
"How many PC's should be retained according to Kaiser's criterion?"
table((wine.pca$sdev)^2 >1)["TRUE"]
##--------------------------------------------------------------------------------------------------------##
##Question-9
"Our boss (the vintner) insists on explaining 85% of the variance. How many PC's should we
keep?"
##ref question 7 First 6 component explains 85.1% of PCs
##--------------------------------------------------------------------------------------------------------##
##Question-10
"Which variable makes the largest contribution to PC1? to PC2?"
##ref question 7 PC1 36.2% PC2 19.2 PC1 largest contribution
##--------------------------------------------------------------------------------------------------------##
## set data for next 2 questions
data <- data.frame(wine.pca$x)
##--------------------------------------------------------------------------------------------------------##
##Question-11
"Produce a scatterplot with x = PC1 and y = PC2 and label the points by cultivar. 
Compare your plot to AC's scatterplot. Which does a better job of separating the cultivars?"
ggplot(data, aes(x=wine.pca$x[,1], y=wine.pca$x[,2], color=wine[,1])) +   geom_point(shape=19) + geom_text(aes(label=wine[,1]),size=3,hjust=2, vjust=0, check_overlap = TRUE) 
##--------------------------------------------------------------------------------------------------------##
##Question-12
"Make the same scatterplot, but skip the labels, and plot each cultivar in a different color.
Add a title, labels for the axes, and a legend to identify the cultivars. Choose excellent colors."
ggplot(data, aes(x=wine.pca$x[,1], y=wine.pca$x[,2], color=wine[,1])) + geom_point(shape=19) +   labs(title = "Wine Sample - Principle Component Analysis", x = "PC1", y = "PC2", color = "Cultivar")
#+ xlab("PC1") + ylab("PC2") + theme(legend.text="Cultivar", legend.title = element_text(colour="blue", size=12, face="bold"))
##--------------------------------------------------------------------------------------------------------##