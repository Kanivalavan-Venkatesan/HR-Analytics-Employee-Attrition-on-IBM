################################################################################################
#                                                                                              #
#                                         HR ANALYTICS                                         #
#                                                                                              #
################################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ LOAD THE LIBRARIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(ggplot2)          # Data Visualization
library(dplyr)            # Data Manipulation
library(gridExtra)        # Plot Layouts
library(corrplot)         # Find correlation coefficient b/w variables
library(cowplot)          # Retrives plot attributes
library(ROSE)             # Handle imbalanced dataset
library(rpart)            # Decision tree model
library(dummies)          # Create dummy variables
library(caret)            # Modelling (Used here for confusion matrix)
library(randomForest)     # Random forest Model



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ LOAD THE DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
hrData <- read.csv(file.choose(), header = T)

dim(hrData)             # Shape of the data
names(hrData)           # Variable names in the dataset
summary(hrData)         # Discriptive statistics of the data
sapply(hrData, class)   # Datatypes of the variables

# Removing the variables 'EmployeeCount', 'EmployeeNumber', 'Over18', 'StandardHours', 
# 'StockOptionLevel' from the dataset
hrData <- hrData[ , -c(9, 10, 22, 27, 28)]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CHECK THE MISSING VALUES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
sum(is.na(hrData))      # No missing values in the dataset



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DATA EXPLORATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Get only the numeric columns
numericColumns <- unlist(lapply(hrData, is.numeric))
numericHrData <- hrData[ , numericColumns]


# Function to plot the Distribution Plot for the independent variables by Attrition
distributionPlot <- function(columnName, titleValue) {
  hrData %>%
    ggplot(., aes(eval(parse(text = columnName)), fill = Attrition)) + 
    geom_density(alpha = 0.4) + 
    ggtitle(titleValue) +
    theme_bw() +
    # Formatting the graph, such as removing the grid lines and legend position
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1,
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.background = element_rect(colour = "black"),
          legend.key.size = unit(0.4, "cm"),
          legend.position = "top")
}

# Distribution plot for some independent variables
d1 <- distributionPlot("Age", "Age")
d2 <- distributionPlot("DailyRate", "Daily Rate")
d3 <- distributionPlot("HourlyRate", "Hourly Rate")
d4 <- distributionPlot("MonthlyIncome", "Monthly Income")
d5 <- distributionPlot("MonthlyRate", "Monthly Rate")

# Get the legend
getLegend <- get_legend(d1)


# Remove the legend from the plots
d1 <- d1 + theme(legend.position = "none")
d2 <- d2 + theme(legend.position = "none")
d3 <- d3 + theme(legend.position = "none")
d4 <- d4 + theme(legend.position = "none")
d5 <- d5 + theme(legend.position = "none")


# Display the graphs in an ordered layout
grid.arrange(top = getLegend,
             arrangeGrob(d1, d2, ncol = 2, widths = c(20,20)), 
             arrangeGrob(d3, d4, ncol = 2), 
             arrangeGrob(d5, ncol = 2),
             nrow = 3)


# Function to calculate the attrition rate and plot the bar chart
plotAttrition <- function(columnName, titleValue){
  # Retrives the total count of each category in a given column
  total <- hrData %>%
    group_by(columnName = eval(parse(text = columnName))) %>%
    summarise(total = n())
  # Retrives the count of attrition in each category
  countValue <- hrData %>%
    filter(hrData$Attrition == 'Yes') %>%
    group_by(columnName = eval(parse(text = columnName))) %>%
    summarise(attrition = n())
  
  valueList <- list(total, countValue)
  
  # Combines the total count and attrition count
  plotData <- cbind(valueList[[1]], valueList[[2]][2])
  
  # Rename the column name of the first column
  colnames(plotData)[1] <- columnName
  
  # Calculates the attrition rate
  plotData <- plotData %>%
    mutate(attritionRate = round((attrition/total)*100)) 
  
  # Plot the attrition rate as a bar chart
  ggplot(plotData, aes(x= factor(eval(parse(text = columnName))), y= attritionRate, 
                       fill = eval(parse(text = columnName)))) +
    geom_bar(stat = 'identity', show.legend = F, width = 0.4) +
    geom_text(aes(label = paste0(plotData$attritionRate, '%')), 
              position = position_dodge(0.9), vjust = -0.3, show.legend = F) +
    ggtitle(titleValue) +
    labs(y = "Attrition Rate") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1, 
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title.x = element_blank())
}

# Attrition by Business Travel
p1 <- plotAttrition("BusinessTravel", "Business Travel")

# Attrition by Department
p2 <- plotAttrition("Department", "Department")

# Attrition by Education
p3 <- plotAttrition("Education", "Education") +
        scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                         labels = c("Below College", "College", "Bachelor", "Master", "Doctor"))

# Attrition by Education Field
p4 <- plotAttrition("EducationField", "Education Field")

# Attrition by Gender
p5 <- plotAttrition("Gender", "Gender")

# Attrition by Marital Status
p6 <- plotAttrition("MaritalStatus", "Marital Status")

# Attrition by Job Role
p7 <- plotAttrition("JobRole", "Job Role")

# Attrition by Over Time
p8 <- plotAttrition("OverTime", "Over Time")

# Attrition by Performance Rating
p9 <- plotAttrition("PerformanceRating", "Performance Rating") + 
        scale_x_discrete(breaks = c("1", "2", "3", "4"), 
                         labels = c("Low", "Good", "Excellent", "Outstanding"))

# Attrition by Environment Satisfaction
p10 <- plotAttrition("EnvironmentSatisfaction", "Environment Satisfaction") + 
         scale_x_discrete(breaks = c("1", "2", "3", "4"), 
                          labels = c("Low", "Medium", "High", "Very High"))

# Attrition by Work Life Balance
p11 <- plotAttrition("WorkLifeBalance", "Work Life Balance") + 
         scale_x_discrete(breaks = c("1", "2", "3", "4"), 
                          labels = c("Bad", "Good", "Better", "Best"))

# Attrition by Job Involvement
p12 <- plotAttrition("JobInvolvement", "Job Involvement") + 
         scale_x_discrete(breaks = c("1", "2", "3", "4"), 
                          labels = c("Low", "Medium", "High", "Very High"))

# Display the graphs
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
grid.arrange(p5, p6, p7, p8, nrow = 2, ncol = 2)
grid.arrange(p9, p10, p11, p12, nrow = 2, ncol = 2)


# Create lables for age group
labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))

# Attrition by age group
hrData %>%
  filter(hrData$Attrition == 'Yes') %>%
  mutate(ageGroup = cut(Age, breaks = c(seq(0, 100, by = 5), Inf), 
                        labels = labs, right = FALSE)) %>%
  ggplot(., aes(x = ageGroup, y = round(prop.table(..count..) * 100), 
                fill = ageGroup)) +
  geom_bar(show.legend = F) +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100), '%')), 
            stat = "count", vjust = -0.4, show.legend = F) +
  ggtitle("Attrition by Age Group") +
  labs(x = "Age Group", y = "Percentage") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1, 
        panel.grid.major = element_blank())

# Monthly Income Vs Attrition
ggplot(hrData, aes(hrData$Attrition, hrData$MonthlyIncome)) +
  geom_violin(aes(fill= hrData$Attrition)) +
  geom_boxplot(width = 0.1, outlier.colour = "red", outlier.shape = 20, 
               outlier.size = 2) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2, 
               fill = "red") +
  theme_bw() +
  ggtitle("Monthly Income Vs Attrition") +
  labs(x = "Attrition", y = "Monthly Income", fill = "Attrition") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        legend.position = c(0.10, 0.70),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(labels = c("No", "Yes"),
                    values = c("#E97D72", "#50BCC1"))

# Monthly Income Vs Job Level
ggplot(hrData, aes(factor(hrData$JobLevel), hrData$MonthlyIncome, fill = factor(hrData$Attrition))) +
  geom_violin(position = position_dodge(width = 0.4)) +
  geom_boxplot(width = 0.1, outlier.colour = "red", outlier.shape = 20, 
               outlier.size = 2, position = position_dodge(0.4)) +
  theme_bw() +
  ggtitle("Monthly Income Vs Job Level") +
  labs(x = "Job Level", y = "Monthly Income", fill = "Attrition") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        legend.position = c(0.10, 0.70)) +
  scale_fill_manual(labels = c("No", "Yes"),
                    values = c("#E97D72", "#50BCC1"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CORRELATION CHECK ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Correlation coefficient
correlationCoefficient <- round(cor(numericHrData), 2)

# Plot the correlation matrix
corrplot(correlationCoefficient, method = "number", number.cex = 0.8)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DATA PREPARATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
set.seed(1)     # Set seed to generate the same samples


# Split the data into train (80 %) and test (20 %)

partition <- sample(2, nrow(hrData), replace = T, 
                    prob = c(0.8, 0.2))


hrTrainData <- hrData[partition == 1, ]             # Dataset to train the model         
hrTestData <- hrData[partition == 2, ]              # Dataset to test the model

dim(hrTrainData)
dim(hrTestData)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HANDLING IMBALANCED DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Proportion of target variable
ggplot(hrTrainData, aes(x = hrTrainData$Attrition, y = round(prop.table(..count..) * 100), 
                   fill = hrTrainData$Attrition)) +
  geom_bar(show.legend = F, width = 0.3) +
  geom_text(aes(label = paste(round(prop.table(..count..) * 100), '%')), 
            stat = "count", vjust = -0.4) +
  labs(x = "Attrition", y = "Percentage") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1, 
        panel.grid.major = element_blank())

# Since the training dataset is imbalanced, I am using over sampling technique to
# balance the data

# Over Sampling
hrDataBalancedOver <- ovun.sample(Attrition ~., data = hrTrainData, method = 'over', 
                                  N = 1986, seed = 1)$data
table(hrDataBalancedOver$Attrition)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ENCODING THE CATEGORICAL DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove the dependent/response variable
independentData <- hrDataBalancedOver[, -2]


# Get the categorical column names
factorColumns <- unlist(lapply(independentData, is.factor))
factorHrData <- independentData[ , factorColumns]

# Create dummy variables for the categorical variables
encodedData <- dummy.data.frame(independentData, names = colnames(factorHrData))

head(encodedData)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PRINCIPLE COMPONENT ANALYSIS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Split the dataset for train PCA and test PCA
set.seed(123)

partitionPCA <- sample(2, nrow(encodedData), replace = T, 
                       prob = c(0.8, 0.2))

trainPCA <- encodedData[partitionPCA == 1, ]        
testPCA <- encodedData[partitionPCA == 2, ]

# PCA for the train data
principleComp <- prcomp(trainPCA, scale. = T)

# Plot PCA
biplot(principleComp, scale = 0)

# Compute the variance
pcaVariance <- principleComp$sdev ^ 2

# Proportion of variance explained
prop.Variance <- round(pcaVariance / sum(pcaVariance), 2)

# Screeplot to find the optimal number of components
plot(prop.Variance, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", type = 'b')

# Cumulative proportion of variance
plot(cumsum(prop.Variance), xlab = "Principal Component", 
     ylab = " Cumulative Proportion of Variance Explained", type = 'b')


# Split the balanced Dataset
set.seed(123)

partitionOriginal <- sample(2, nrow(hrDataBalancedOver), replace = T, 
                       prob = c(0.8, 0.2))

trainOriginal <- hrDataBalancedOver[partitionOriginal == 1, ]        
testOriginal <- hrDataBalancedOver[partitionOriginal == 2, ]

# Combine the PCA with the train dataset

finalTrainData <- data.frame(Attrition = trainOriginal$Attrition, principleComp$x)

# Extract only first 30 components as 30 is the optimal number of components
finalTrainData <- finalTrainData[, 1:31]


# Transform the test data with PCA components
finalTestData <- predict(principleComp, newdata = testPCA)
finalTestData <- as.data.frame(finalTestData[, 1:30])



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MODELING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to plot ROC curve
plotROC <- function(model, modelName) {
  ROCRpred <- prediction(model, testOriginal$Attrition) 
  ROCRperf <- performance(ROCRpred, 'tpr','fpr') 
  plot(ROCRperf, colorize = TRUE, 	print.cutoffs.at=seq(0.1,by=0.1),
       text.adj = c(-0.2,1.7), main = modelName)
}


###################################### DECISON TREE MODEL ######################################

# Fit the model
decisionTreeModel <- rpart(Attrition ~., data = finalTrainData, method = "class")

# Prediction using the fitted model
predictionDecisionTree <- predict(decisionTreeModel, newdata = finalTestData, type = 'class')

# Confusion Matrix
confusionDecision <- confusionMatrix(predictionDecisionTree, testOriginal$Attrition, positive = 'Yes')


# Decision ROC curve
ROCPrediction <- predict(decisionTreeModel, newdata = finalTestData)
plotROC(ROCPrediction[, 2], "Decision Tree")


###################################### LOGISTIC REGRESSION #####################################
# Fit the model
logisticModel <- glm(Attrition ~., family = "binomial", data = finalTrainData)

summary(logisticModel)


# Prediction using the fitted model
predictionLogistics <- predict(logisticModel, newdata = finalTestData, type = 'response')

# Confusion Matrix
confusionLogistic <- confusionMatrix(as.factor(as.numeric(predictionLogistics) > 0.5), 
                                     as.factor(testOriginal$Attrition == 'Yes'), 
                                     positive = levels(as.factor(testOriginal$Attrition == 'Yes'))[2])


# Logistic ROC Curve
plotROC(predictionLogistics, "Logistic Regression")


######################################## RANDOM FOREST ########################################
numberOfTrees = 150

# Built the model
set.seed(1234)
randomForestModel <- randomForest(Attrition ~., data = finalTrainData, ntree = numberOfTrees)


# Prediction
predictionRandom <- predict(randomForestModel, finalTestData)

# Confusion Matrix
confusionRandom <- confusionMatrix(predictionRandom, testOriginal$Attrition, positive = 'Yes')

# Average of out of bag error rate
mean(randomForestModel$err.rate)

# Random ROC curve
ROCPredictionRan <- predict(randomForestModel, newdata = finalTestData, type='prob')
plotROC(ROCPredictionRan[, 2], "Random Forest")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ EVALUATE MODELS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to retrive accuracy measures for different models
accMeasures <- function(modelName) {
  accuracy.meas(testOriginal$Attrition, eval(parse(text = modelName)))
}


# Build the accuracy matrix
AccuracyMeasures <- matrix(nrow = 3, ncol = 4, 
                           dimnames = list(c("Decision", "Logistics", "Random Forest"), 
                                           c("Precision", "Recall", "F", "Accuracy")))

# List of model names
models = list("predictionDecisionTree", "predictionLogistics", "predictionRandom")

# Create list of confusion matrix from the created model
accuracyModel <- list(confusionDecision, confusionLogistic, confusionRandom)

# Populate the accuracy measure matrix with values
position <- 3
for (i in 1:3) {
  for (j in 1:3){
    AccuracyMeasures[i, j] <- round(as.numeric(accMeasures(models[i])[position]), 2)
    position <- position + 1
  }
  AccuracyMeasures[i, 4] <- round(as.numeric(accuracyModel[[i]]$overall[1]), 2)
  position <- 3
}

# Display the accuracy measures
AccuracyMeasures
