getwd()
setwd("C:\\Users\\purwa\\Downloads\\")
loandata <- read.csv("train_u6lujuX_CVtuZ9i (1).csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?","Not Available")) 

nrow(loandata)
summary(loandata)
str(loandata)

library(randomForest)

#-------------------------------------IMPUTING MISSING VALUES--------------------------------#


pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(loandata,2,pMiss)
#apply(hospdata,1,pMiss)
#install.packages("mice")
library(mice)
md.pattern(loandata)
#install.packages("VIM")
library(VIM)
aggr_plot <- aggr(loandata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(loandata), cex.axis=.4, gap=3, ylab=c("Histogram of missing data","Pattern"))

imp <- mice(loandata, maxit = 0,print=FALSE)
pred <- imp$predictorMatrix
pred[,"Loan_Status"] <- 0
pred
imp <- mice(loandata, pred=pred, pri=F,meth='rf',seed=51346)
completedData <- complete(imp)
summary(completedData)
hdata <- completedData

nrow(completedData)
--------------------------------------------
  
loandata <- completedData 
  summary(loandata)
  
  

loandata$Loan_ID <- NULL
loandata$Gender <- as.factor(loandata$Gender)
loandata$Married <- as.factor(loandata$Married)
loandata$Dependents <- as.factor(loandata$Dependents)
loandata$Education <- as.factor(loandata$Education)
loandata$Self_Employed <- as.factor(loandata$Self_Employed)
loandata$Credit_History <- as.factor(loandata$Credit_History)
loandata$Property_Area <- as.factor(loandata$Property_Area)
loandata$Loan_Status <- as.factor(loandata$Loan_Status)
colnames(loandata)
str(loandata)


loandata$total_income <-  loandata$ApplicantIncome + loandata$CoapplicantIncome
loandata$final_income <- ifelse(loandata$Dependents =="3+",(loandata$total_income/4),
                  (loandata$total_income/( as.numeric(loandata$Dependents))))
head(loandata)

#loandata$Loan_Amount_Term <- as.factor(loandata$Loan_Amount_Term)
summary(loandata$total_income)

loandata$ApplicantIncome <- NULL
loandata$CoapplicantIncome <- NULL
colnames(loandata)
str(loandata)
summary(loandata$total_income)








rf1 <-randomForest(Loan_Status~., data=loandata, ntree=10, na.action=na.exclude, importance=T, proximity=T) 
print(rf1)
rf2 <-randomForest(Loan_Status~., data=loandata, ntree=20, na.action=na.exclude, importance=T, proximity=T) 
print(rf2)
rf3 <-randomForest(Loan_Status~., data=loandata, ntree=30, na.action=na.exclude, importance=T, proximity=T) 
print(rf3)
rf4 <-randomForest(Loan_Status~., data=loandata, ntree=40, na.action=na.exclude, importance=T, proximity=T) 
print(rf4)

rf5<-randomForest(Loan_Status~., data=loandata, ntree=50, na.action=na.exclude, importance=T, proximity=T) 
print(rf5)
rf6 <-randomForest(Loan_Status~., data=loandata, ntree=60, na.action=na.exclude, importance=T, proximity=T) 
print(rf6)
rf7 <-randomForest(Loan_Status~., data=loandata, ntree=70, na.action=na.exclude, importance=T, proximity=T) 
print(rf7)
rf8 <-randomForest(Loan_Status~., data=loandata, ntree=80, na.action=na.exclude, importance=T, proximity=T) 
print(rf8)
rf9 <-randomForest(Loan_Status~., data=loandata, ntree=90, na.action=na.exclude, importance=T, proximity=T) 
print(rf9)
rf10 <-randomForest(Loan_Status~., data=loandata, ntree=100, na.action=na.exclude, importance=T, proximity=T) 
print(rf10)

colnames(loandata)

mtry <- tuneRF(loandata[-12], loandata$Loan_Status, ntreeTry=100, 
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, na.action=na.exclude)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


rf <-randomForest(Loan_Status~., data=loandata, mtry=2, importance=TRUE, ntree=100)
print(rf)
importance(rf)
varImpPlot(rf)

install.packages("e1071")
install.packages('e1071', dependencies=TRUE)
install.packages('caret', dependencies = TRUE)

#######TEST DATA ###########


loandatatest <- read.csv("test_Y3wMUE5_7gLdaTN (1).csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?","Not Available")) 
nrow(loandatatest)
summary(loandatatest)

str(loandatatest)



#-------------------------------------IMPUTING MISSING VALUES--------------------------------#


pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(loandatatest,2,pMiss)
#apply(hospdata,1,pMiss)
#install.packages("mice")
library(mice)
md.pattern(loandatatest)
#install.packages("VIM")
library(VIM)
aggr_plot <- aggr(loandatatest, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(loandatatest), cex.axis=.4, gap=3, ylab=c("Histogram of missing data","Pattern"))

imp <- mice(loandatatest, maxit = 0,print=FALSE)
pred <- imp$predictorMatrix
pred[,"Loan_Status"] <- 0
pred
imp <- mice(loandatatest, pred=pred, pri=F,meth='rf',seed=51349)
completedDatatest <- complete(imp)
summary(completedDatatest)
hdata <- completedDatatest

nrow(completedDatatest)
--------------------------------------------
  
loandatatest <- completedDatatest 
summary(loandatatest)

#loandatatest$Loan_ID <- NULL
loandatatest$Gender <- as.factor(loandatatest$Gender)
loandatatest$Married <- as.factor(loandatatest$Married)
loandatatest$Dependents <- as.factor(loandatatest$Dependents)
loandatatest$Education <- as.factor(loandatatest$Education)
loandatatest$Self_Employed <- as.factor(loandatatest$Self_Employed)
loandatatest$Credit_History <- as.factor(loandatatest$Credit_History)
loandatatest$Property_Area <- as.factor(loandatatest$Property_Area)
#loandatatest$Loan_Status <- as.factor(loandatatest$Loan_Status)
colnames(loandatatest)
str(loandatatest)
#summary(loandatatest$Loan_Status)

loandatatest$total_income <-  loandatatest$ApplicantIncome + loandatatest$CoapplicantIncome
loandatatest$final_income <- ifelse(loandatatest$Dependents =="3+",(loandatatest$total_income/4),
                                (loandatatest$total_income/( as.numeric(loandatatest$Dependents))))



#loandatatest$Loan_Amount_Term <- as.factor(loandatatest$Loan_Amount_Term)
summary(loandatatest$final_income)

loandatatest$ApplicantIncome <- NULL
loandatatest$CoapplicantIncome <- NULL
colnames(loandatatest)
str(loandatatest)
summary(loandatatest$total_income)

view
library(caret)
str(loandatatest)

#loandatatest$Loan_Status <- NULL
loandatatest1 <- loandatatest
loandatatest$Loan_ID <- NULL
predicted_values <- predict(rf, loandatatest,type= "prob")
head(predicted_values)

final_data <- cbind( loandatatest,pred)
colnames <- c(colnames(loandatatest),"Loan_Status")
write.table(final_data,file="RF3.csv",sep =",",row.names=F,col.names=colnames)
getwd()
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0) )
colnames(Loan_Status)


# TRYING ANOTHER MODEL FOR BETTER ACCURACY
##################       H20  PACKAGE            ##########################

getwd()
setwd("C:/Users/purwa/Downloads")
library(h2o)
#h2o.removeAll()
localH2O <- h2o.init() # Create an H2O cloud
loandata <- read.csv("Train1.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?","Not Available")) 
write.table(completedData, file="Train1.csv", sep=",", row.names=F) 
loandata.hex = h2o.importFile(path="Train1.csv", 
                              na.strings = c("?"," ?","? "," ? ",""," ","NA","NaN"))  

write.table(completedDatatest, file="Test1.csv", sep=",", row.names=F)  
loandatatest.hex = h2o.importFile(path="Test1.csv", 
                                  na.strings = c("?"," ?","? "," ? ",""," ","NA","NaN"))  
summary(loandata.hex)
loandata.hex$Loan_ID <- NULL
loandata.hex$Gender <- as.factor(loandata.hex$Gender)
loandata.hex$Married <- as.factor(loandata.hex$Married)
loandata.hex$Dependents <- as.factor(loandata.hex$Dependents)
loandata.hex$Education <- as.factor(loandata.hex$Education)
loandata.hex$Self_Employed <- as.factor(loandata.hex$Self_Employed)
loandata.hex$Credit_History <- as.factor(loandata.hex$Credit_History)
loandata.hex$Property_Area <- as.factor(loandata.hex$Property_Area)
loandata.hex$Loan_Status <- as.factor(loandata.hex$Loan_Status)
colnames(loandata.hex)
str(loandata.hex)

loandata.hex$total_income <-  loandata.hex$ApplicantIncome + loandata.hex$CoapplicantIncome
loandata.hex$final_income <- h2o.ifelse(loandata.hex$Dependents =="3+",(loandata.hex$total_income/4),
                                    (loandata.hex$total_income/( 1 + as.numeric(loandata.hex$Dependents))))


utils::View(loandata.hex)

summary(loandata.hex$final_income)

loandata.hex$ApplicantIncome <- NULL
loandata.hex$CoapplicantIncome <- NULL
colnames(loandata.hex)
str(loandata.hex)
summary(loandata.hex)

y = colnames(loandata.hex[c(12)])
x = colnames(loandata.hex[c(-12)])


gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 4, 5, 6)
)

# Train a cartesian grid of GBMs
gbm_grid1 <- h2o.grid("gbm",
                      x = x, 
                      y = y,
                      grid_id = "hack_grid1",
                      training_frame = loandata.hex, 
                      ntrees = 100,
                      seed = 10000,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "hack_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)

# Grab the top GBM model, chosen by validation AUC
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])

best_gbm2 <- h2o.getModel(gbm_gridperf1@model_ids[[2]])

best_gbm3 <- h2o.getModel(gbm_gridperf1@model_ids[[3]])
best_gbm4 <- h2o.getModel(gbm_gridperf1@model_ids[[4]])
best_gbm5 <- h2o.getModel(gbm_gridperf1@model_ids[[5]])

summary(loandatatest.hex)

loandatatest$Loan_ID <- NULL
loandatatest.hex$Gender <- as.factor(loandatatest.hex$Gender)
loandatatest.hex$Married <- as.factor(loandatatest.hex$Married)
loandatatest.hex$Dependents <- as.factor(loandatatest.hex$Dependents)
loandatatest.hex$Education <- as.factor(loandatatest.hex$Education)
loandatatest.hex$Self_Employed <- as.factor(loandatatest.hex$Self_Employed)
loandatatest.hex$Credit_History <- as.factor(loandatatest.hex$Credit_History)
loandatatest.hex$Property_Area <- as.factor(loandatatest.hex$Property_Area)
#loandatatest$Loan_Status <- as.factor(loandatatest$Loan_Status)
colnames(loandatatest.hex)
str(loandatatest.hex)
#summary(loandatatest$Loan_Status)

loandatatest.hex$total_income <-  loandatatest.hex$ApplicantIncome + loandatatest.hex$CoapplicantIncome
summary(loandatatest.hex$total_income)

loandatatest.hex$ApplicantIncome <- NULL
loandatatest.hex$CoapplicantIncome <- NULL
colnames(loandatatest.hex)

loandatatest.hex$Loan_ID <- NULL


final_predictions_b1 <-h2o.predict(object=best_gbm1,newdata = loandatatest.hex)
final_predictions_b2 <-h2o.predict(object=best_gbm2,newdata = loandatatest.hex)
final_predictions_b3 <-h2o.predict(object=best_gbm3,newdata = loandatatest.hex)
final_predictions_b4 <-h2o.predict(object=best_gbm4,newdata = loandatatest.hex)
final_predictions_b5 <-h2o.predict(object=best_gbm5,newdata = loandatatest.hex)
final_predictions_b1$predict[1:10]
nrow(final_predictions_b1)
str(final_predictions_b1)
View(final_predictions_b1)

utils::View(final_predictions_b1)

fix(final_predictions_b1)

summary(final_predictions_b1$predict)
summary(final_predictions_b2$predict)
summary(final_predictions_b3$predict)
summary(final_predictions_b4$predict)
summary(final_predictions_b5$predict)
getwd()

str(final_predictions_b1$predict)
final_data <- cbind( loandatatest.hex,final_predictions_b1$predict)
colnames <- c(colnames(loandatatest.hex),"Loan_Status")
write.table(final_data,file="RF2.csv",sep =",",row.names=F)

h2o.exportFile(final_predictions_b5, "Sub2.csv", force = FALSE, parts = 1)
write.table(utils::View(final_predictions_b1), file="Submission2.csv"), header = T,rm(list=ls()), sep="\t", row.names = F, quote=FALSE)
,sep =",",row.names=F,col.names=colnames)

write.csv(final_predictions_b1, file = "MyData.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_gbm_perf1 <- h2o.performance(model = best_gbm1,
                                  newdata = trafficstop.testb)
h2o.auc(best_gbm_perf1)  # 0.7781932

# Look at the hyperparamters for the best model
print(best_gbm1@model[["model_summary"]])








