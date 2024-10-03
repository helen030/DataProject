rm(list=ls()) #clean environment

library(tidyverse)
library(dplyr)
library(pander)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)
library(pander)
library(gains)
library(pROC)
library(klaR)
library(randomForest)
library(ROSE) # for random over- and under-sampling


#import dataset
trainData <- read.csv("training_data.csv")

#remove duplicates and keep unique data entries
trainData <- unique(trainData)
trainData[complete.cases(trainData),]

#remove unnecessary columns - Handsets, CurrentEquipmentDays, TruckOwner, RVOwner, OwnsMotorcycle
trainData <- subset(trainData, select = -c(Handsets, CurrentEquipmentDays, 
                                           TruckOwner, RVOwner, OwnsMotorcycle))

#change all integer values into to numeric to correlate to the given dataset
trainData$MonthsInService <- as.numeric(trainData$MonthsInService)
trainData$ActiveSubs <- as.numeric(trainData$ActiveSubs)
trainData$RetentionCalls <- as.numeric(trainData$RetentionCalls)
trainData$RetentionOffersAccepted <- as.numeric(trainData$RetentionOffersAccepted)
trainData$ReferralsMadeBySubscriber <- as.numeric(trainData$ReferralsMadeBySubscriber)
trainData$CreditRating <- as.numeric(trainData$CreditRating)

#remove negative number
trainData <- trainData %>% filter(if_any(where(is.numeric), ~ . >= 0))

#find all N/A values in the columns + recheck
colSums(is.na(trainData))

#replace all N/A values with the mean
avgMonthlyRevenue <- round(mean(trainData$MonthlyRevenue, na.rm = TRUE))
trainData$MonthlyRevenue[is.na(trainData$MonthlyRevenue)] <- avgMonthlyRevenue

avgMonthlyMinutes  <- round(mean(trainData$MonthlyMinutes , na.rm = TRUE))
trainData$MonthlyMinutes [is.na(trainData$MonthlyMinutes )] <- avgMonthlyMinutes 

avgTotalRecurringCharge <- round(mean(trainData$TotalRecurringCharge, na.rm = TRUE))
trainData$TotalRecurringCharge[is.na(trainData$TotalRecurringCharge)] <- avgTotalRecurringCharge

avgOverageMinutes <- round(mean(trainData$OverageMinutes, na.rm = TRUE))
trainData$OverageMinutes[is.na(trainData$OverageMinutes)] <- avgOverageMinutes

avgRoamingCalls <- round(mean(trainData$RoamingCalls, na.rm = TRUE))
trainData$RoamingCalls[is.na(trainData$RoamingCalls)] <- avgRoamingCalls

avgAgeHH1 <- round(mean(trainData$AgeHH1, na.rm = TRUE))
trainData$AgeHH1[is.na(trainData$AgeHH1)] <- avgAgeHH1

avgAgeHH2 <- round(mean(trainData$AgeHH2, na.rm = TRUE))
trainData$AgeHH2[is.na(trainData$AgeHH2)] <- avgAgeHH2

#change all binary values into '1' for 'yes' and '0' for 'no'
trainData$Churn = ifelse(trainData$Churn == 'Yes', 1, 0);
trainData$ChildrenInHH = ifelse(trainData$ChildrenInHH == 'Yes', 1, 0);
trainData$HandsetRefurbished = ifelse(trainData$HandsetRefurbished == 'Yes', 1, 0);
trainData$HandsetWebCapable = ifelse(trainData$HandsetWebCapable == 'Yes', 1, 0);
trainData$BuysViaMailOrder = ifelse(trainData$BuysViaMailOrder == 'Yes', 1, 0);
trainData$RespondsToMailOffers = ifelse(trainData$RespondsToMailOffers == 'Yes', 1, 0);
trainData$OptOutMailings = ifelse(trainData$OptOutMailings == 'Yes', 1, 0);
trainData$OwnsComputer = ifelse(trainData$OwnsComputer == 'Yes', 1, 0);
trainData$HasCreditCard = ifelse(trainData$HasCreditCard == 'Yes', 1, 0);
trainData$MadeCallToRetentionTeam = ifelse(trainData$MadeCallToRetentionTeam == 'Yes', 1, 0);

trainData <- trainData %>%
  mutate(Occupation = case_when(
    Occupation == "Other" ~ 0,
    Occupation == "Self" ~ 1,
    Occupation == "Professional" ~ 2,
    Occupation == "Crafts" ~ 3,
    Occupation == "Clerical" ~ 4,
    Occupation == "Retired" ~ 5,
    Occupation == "Homemaker" ~ 6,
    Occupation == "Student" ~ 7,
  ),
  PrizmCode == case_when(
    PrizmCode == "Outer regional" ~ 0,
    PrizmCode == "Inner regional" ~ 1,
    PrizmCode == "Remote" ~ 2,
    PrizmCode == "Other" ~ 3,
  )
  )


#descriptive statistics

#churn
par(mar = c(6, 6, 3, 8), mgp=c(3,0.5,0), cex = 0.8)
barplot(table(trainData$Churn), las = 1, col = "lightblue", 
        xlab = "Churn", ylab = "Frequency", main = "Customer Churn Frequency")
legend("topright",c("1 = Yes","0 = No"), pch = c(1,1))

plotdf <- trainData %>%
  group_by(Churn) %>%
  summarise(counts=n())
plotdf

ggplot(plotdf, aes(x=Churn, y=counts)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) +
  ggtitle("Numbers of Churned and Not Churned Customers")
  

#monthly revenue
summary(trainData$MonthlyRevenue)

revChurned <- trainData %>% filter(Churn == '1') %>% count(MonthlyRevenue)
summary(revChurned)
revNotChurned <- trainData %>% filter(Churn == '0') %>% count(MonthlyRevenue)
summary(revNotChurned)


#pie chart for credit rating of churned and not churned customers
dtaChurned <- trainData %>% filter(Churn == '1') %>% count(CreditRating)
dtaChurned$Churn <- "Yes"
dtaNotChurned <- trainData %>% filter(Churn == "0") %>% count(CreditRating)
dtaNotChurned$Churn <- "No"
dtaBoth <- rbind(dtaChurned, dtaNotChurned)
dtaBoth <- dtaBoth %>%
  group_by(Churn) %>%
  mutate(Percentage = n/sum(n))

ggplot(dtaBoth, aes(x="", y=Percentage, fill=as.factor(CreditRating))) +
  facet_wrap(~Churn) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Pie Chart of Credit Ratings for Not Churned and Chunred Customers")

#number of churned customers by income groups
table(trainData$IncomeGroup)

ggplot(data=trainData) +
  geom_bar(aes(x=IncomeGroup, fill=Churn)) +
  scale_x_continuous(breaks = seq(from = 0, to = 9, by = 1)) +
  ggtitle("Churn by Income Group") +
  labs(x = "Income Group", y = "Count")
  theme(plot.title = element_text(hjust = 1,face="bold"))

#credit card
trainData %>%
  ggplot(aes(HasCreditCard, fill=Churn)) + 
  geom_bar(position = 'dodge') +
  labs(y="Percent") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("red","blue"))

#age
pander(summary(trainData$AgeHH1))
pander(summary(trainData$AgeHH2))

ggplot(trainData, aes(x=Churn, y=AgeHH1)) + geom_boxplot()


#MODELLING

#Training and Validation data set

myData = trainData[sample(nrow(trainData), 20000),c(2:21,31,34,36,38)]
myData <- subset(myData, select = -c(2:19))
myData$Churn <- as.factor(myData$Churn)
set.seed(1)
myIndex <- createDataPartition(myData$Churn, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]
set.seed(1)


#random forest

set.seed(1)
randomforest_tree <- randomForest(Churn ~., data = trainSet, ntree = 100, mtry = 2, 
                                  importance = TRUE)
varImpPlot(randomforest_tree, type=1)
predicted_class <- predict(randomforest_tree, validationSet)
confusionMatrix(predicted_class, as.factor(validationSet$Churn), positive = "1")
predicted_prob <- predict(randomforest_tree, validationSet, type= 'prob')
validationSet$Churn <- as.numeric(as.character(validationSet$Churn))
gains_table <- gains(validationSet$Churn, predicted_prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Churn)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', ylab = "Cumulative",  main="Cumulative Lift Chart of Random Forest Model", type = "l")
lines(c(0, sum(validationSet$Churn))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$Churn), names.arg=gains_table$depth, 
        xlab="Percentile", ylab="Lift", ylim=c(0,1.5), 
        main="Decile-Wise Lift Chart of Random Forest Model")
roc_object <- roc(validationSet$Churn, predicted_prob[,2])
plot.roc(roc_object, legacy.axes = TRUE, main = 'ROC curve of of Random Forest Model', 
         print.auc = TRUE)


#KNN

#set parameters for train() function below to perform 10-fold CV
myCtrl <- trainControl(method="cv", number = 10)
myGrid <- expand.grid(.k=c(1:10))
#use all other variables in provided data set as predictors
set.seed(1)
KNN_fit <- train(Churn ~ ., data=trainSet, method = "knn", trControl=myCtrl, tuneGrid = myGrid)
#trControl = do CV within training sample to optimize k
KNN_Class <- predict(KNN_fit, newdata = validationSet)
confusionMatrix(KNN_Class, validationSet$Churn, positive = "1")
KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
confusionMatrix(as.factor(ifelse(KNN_Class_prob[,2]>0.403, '1', '0')), 
                validationSet$Churn, positive = '1')
validationSet$Churn <- as.numeric(as.character(validationSet$Churn))
gains_table <- gains(validationSet$Churn, KNN_Class_prob[,2])
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Churn))~c(0, gains_table$cume.obs), 
     xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(validationSet$Churn))~c(0, dim(validationSet)[1]), col="red", lty=2)
roc_object <- roc(validationSet$Churn, predicted_prob[,2])
plot.roc(roc_object, legacy.axes = TRUE, main = 'ROC curve', print.auc = TRUE)


#NB

#As for KNN - but this time not turning a "k" parameter
#instead it's the üsekernel" parameter - method = "nb"
myCtrl <- trainControl(method="cv", number=10)
set.seed(1)
nb_fit <- train(Churn ~., data = trainSet, method = "nb", trControl = myCtrl)
#look at TRUE accuracy

#holdout sample; cutoff = 0.5
nb_class <- predict(nb_fit, newdata = validationSet)
confusionMatrix(nb_class, validationSet$Churn, positive = '1')
#analyse Sensitivity and Specificity

#create performance charts
validationSet$Churn <- as.numeric(as.character(validationSet$Churn))
gains_table <- gains(validationSet$Churn, nb_class_prob[,2])
gains_table
#cumulative lift chart
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Churn)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', ylab = "Cumulative", type = "l", main = "Cumulative Lift Chart")
lines(c(0, sum(validationSet$Churn))~c(0, dim(validationSet)[1]), col="red", lty=2)
#Decile-wise lift chart
barplot(gains_table$mean.resp/mean(validationSet$Churn), names.arg=gains_table$depth, 
        xlab="Percentile", ylab="Lift", ylim=c(0,1.5), main="Decile-Wise Lift Chart")
#ROC curve
roc_object <- roc(validationSet$Churn, predicted_prob[,2])
plot.roc(roc_object, legacy.axes = TRUE, main = 'ROC curve', print.auc = TRUE)


#imbalanced

trainData <- subset(trainData, select = -c(6:20,23:30,32,33,35,37:39))

df <- data.frame(trainData)

#encode character variables representing categorical information as numeric 
#dummy variables. 
dummy <- dummyVars(" ~ .", df, fullRank=TRUE)
df <- data.frame(predict(dummy, newdata = df))
#encode the outcome/target variable as a factor variable
df$ChurnYes <- as.factor(df$ChurnYes)

set.seed(1)
myIndex <- createDataPartition(df$ChurnYes, p=0.7, list=FALSE)
trainSet <- df[myIndex,]
validationSet <- df[-myIndex,]

#randomForest() function calls
sqrt.num.vars = floor(sqrt(ncol(df)-1))

#fit randomForest on original (imbalanced) data
table(trainSet$ChurnYes) # observe imbalancedness 
print(table(trainSet$ChurnYes)/nrow(trainSet)) %>% round(.,2) # imbalancedness in proportional terms

#RF
set.seed(1) # for replicability and so that each randomForest call uses the same seed 
fit_RF_orig_data <- randomForest(ChurnYes~.,data=trainSet, ntree=100, mtry=sqrt.num.vars) 

#predict classes using default cutoff of 0.5 using this fit
pred_RF_orig_data <-  predict(fit_RF_orig_data, newdata = validationSet)
#predict classes using cutoff = prevalence (proportion of positive class in training data)
prev = sum(trainSet$ChurnYes==1)/nrow(trainSet)
pred_prob_RF_orig_data = predict(fit_RF_orig_data, newdata = validationSet, type = 'prob')
pred_RF_orig_data_altCutoff = ifelse(pred_prob_RF_orig_data[ ,2] > prev, 1, 0)

#fit randomForest using the Random Undersampling technique
set.seed(1)
randomUndersamp_trainSet <- ovun.sample(ChurnYes~., data=trainSet, method="under", 
                                        N = 2*sum(trainSet$ChurnYes==1)) 
table(randomUndersamp_trainSet$data$ChurnYes)
set.seed(1)
fit_RF_randomUndersamp <- randomForest(ChurnYes~.,
                                       data=randomUndersamp_trainSet$data, ntree=200, 
                                       mtry=sqrt.num.vars) 
pred_RF_randomUndersamp <-  predict(fit_RF_randomUndersamp, newdata = validationSet)

#fit randomForest using Random Oversampling
set.seed(1)
randomOversamp_trainSet <- ovun.sample(ChurnYes~., data=trainSet, method="over", 
                                       N = 2*sum(trainSet$ChurnYes==0))
table(randomOversamp_trainSet$data$ChurnYes)
set.seed(1)
fit_RF_randomOversamp <- 
  randomForest(ChurnYes~.,data=randomOversamp_trainSet$data, ntree=100, mtry=sqrt.num.vars) 
pred_RF_randomOversamp <-  predict(fit_RF_randomOversamp, newdata = validationSet)

# collate validationSet overall accuracy, sensitivity, specificity, 
# PPV and NPV into a table
validationResultsMatrix = matrix(data = NA, nrow = 4, ncol = 6)
colnames(validationResultsMatrix) = c('Method', 'Accuracy', 'Sensitivity', 
                                      'Specificity', 'PPV', 'NPV')
validationResultsMatrix[1, ] = c('Raw Imbalanced Data: Cutoff = 0.5',
                                 confusionMatrix(pred_RF_orig_data, validationSet$ChurnYes, 
                                                 positive = "1")$overall[1] %>% round(., 3),
                                 confusionMatrix(pred_RF_orig_data, validationSet$ChurnYes, 
                                                 positive = "1")$byClass[1:4] %>% round(., 3))
validationResultsMatrix[2, ] = 
  c('Raw Imbalanced Data: Cutoff = Prevalence',
    confusionMatrix(as.factor(pred_RF_orig_data_altCutoff), validationSet$ChurnYes, 
                    positive = "1")$overall[1] %>% round(., 3),
    confusionMatrix(as.factor(pred_RF_orig_data_altCutoff), validationSet$ChurnYes, 
                    positive = "1")$byClass[1:4] %>% round(., 3))
validationResultsMatrix[3, ] = c('Random Undersampling Majority Class',
                                 confusionMatrix(pred_RF_randomUndersamp, validationSet$ChurnYes, 
                                                 positive = "1")$overall[1] %>% round(., 3),
                                 confusionMatrix(pred_RF_randomUndersamp, validationSet$ChurnYes, 
                                                 positive = "1")$byClass[1:4] %>% round(., 3))
validationResultsMatrix[4, ] = c('Random Oversampling Minority Class',
                                 confusionMatrix(pred_RF_randomOversamp, validationSet$ChurnYes, 
                                                 positive = "1")$overall[1] %>% round(., 3),
                                 confusionMatrix(pred_RF_randomOversamp, validationSet$ChurnYes, 
                                                 positive = "1")$byClass[1:4] %>% round(., 3))

View(validationResultsMatrix)

# view OOB error rate plots for the randomForest() fits
par(mfrow = c(2,2))
plot(fit_RF_orig_data, ylim = c(0,1), main = 'Raw Imbalanced Data: Cutoff = 0.5')
plot(fit_RF_randomUndersamp, ylim = c(0,1), main = 'Random Undersampling Majority Class')
plot(fit_RF_randomOversamp, ylim = c(0,1), main = 'Random Oversampling Minority Class')
par(mfrow = c(1,1))


#selected - undersmapling
predicted_prob <- predict(fit_RF_randomUndersamp, validationSet, type= 'prob')
validationSet$ChurnYes <- as.numeric(as.character(validationSet$ChurnYes))
gains_table <- gains(validationSet$ChurnYes, predicted_prob[,2])
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$ChurnYes)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', ylab = "Cumulative",  
     main="Cumulative Lift Chart of Random Undersampling Majority Class Model", type = "l")
lines(c(0, sum(validationSet$ChurnYes))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$Churn), names.arg=gains_table$depth, 
        xlab="Percentile", ylab="Lift", ylim=c(0,1.5), 
        main="Decile-Wise Lift Chart of Random Undersampling Majority Class Model")
roc_object <- roc(validationSet$Churn, predicted_prob[,2])
plot.roc(roc_object, legacy.axes = TRUE, 
         main = 'ROC curve of of Random Undersampling Majority Class Model', 
         print.auc = TRUE)


#predict
evalData <- read.csv("eval_data.csv")
evalData <- subset(evalData, select = -c(5:21,24:33,35,36,38,39,41:43))
evalData <- data.frame(evalData)
#dummy variables
dummy1 <- dummyVars(" ~ .", evalData, fullRank=TRUE)
evalData <- data.frame(predict(dummy1, newdata = evalData))

evalDataScore <- predict(fit_RF_randomUndersamp, evalData, type = "class")
evalData$Churn <- evalDataScore

evalData <- subset(evalData, select = -c(2:9))

#export csv file
write.csv(evalData,"evalData.csv")

#shiny app save
save(trainData, fit_RF_randomUndersamp, file = 'modelling-output.Rdata')


