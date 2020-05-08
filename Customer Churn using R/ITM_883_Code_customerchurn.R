                                          #####Step 1: Data Loading
library(dplyr)
library(ggplot2)
library(leaps)
library(MASS)
library(pROC)
setwd("C:/Users/karan/Desktop/Study_MSU_MSBA/ITM883_BAProblemsolving/BUS_Project")
getwd()
churncsv <- read.csv("C:/Users/karan/Desktop/Study_MSU_MSBA/ITM883_BAProblemsolving/BUS_Project/Customer Churn Data.csv")
summary(churncsv)
nrow(churncsv)

                                            ####Step 2 : Exploratory Analysis

#Identifying the NA values in the quantitative variable
sum(is.na(churncsv))
churncsv[is.na(churncsv$TotalCharges),]

#Understanding the distibution of Churn vs Retention
ggplot(churncsv, aes(x = Churn))+ geom_histogram(stat = "count", fill = c("blue", "orange"))

#Plotting graphs to understand churn distribution for various Categorical variables
variables <- list(  'gender', 'SeniorCitizen', 'Partner', 'Dependents', 'PhoneService', 'MultipleLines', 'InternetService','OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport','StreamingTV', 'StreamingMovies', 'Contract', 'PaperlessBilling','PaymentMethod' )
plotG <- list() 
for (i in variables){
  plotG <-  ggplot(churncsv, aes_string(x = i, fill = as.factor(churncsv$Churn)))+
    geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

  print(plotG)
}

                                              ####Step 3: Data Preprocessing

#ASSIGNING CUSTOMER CHURN('Yes') AS 1 AND CUSTOMER RETENTION('No') AS 0
churncsv$Churn = ifelse(churncsv$Churn == 'No', 0 , 1)

# Assuming Total charge value as (Monthly charge * tenure) for only those values identified where Total Charge is NA
churncsv <- churncsv %>% mutate(TotalCharges = ifelse(is.na(churncsv$TotalCharges), churncsv$MonthlyCharges*churncsv$tenure, TotalCharges))

#Handeling quantative variable which are categorical. Also, Standardizing the data for model simplicity and reducing mulitcollinearity
churncsv$SeniorCitizen = as.factor(ifelse(churncsv$SeniorCitizen == 1, 'SeniorCitizen' , 'NotSeniorCitizen'))
churncsv$MultipleLines = as.factor(ifelse(churncsv$MultipleLines == 'No phone service' | churncsv$MultipleLines == 'No' , 'No' , 'Yes'))
churncsv$OnlineSecurity = as.factor(ifelse(churncsv$OnlineSecurity == 'No internet service' | churncsv$OnlineSecurity == 'No' , 'No' , 'Yes'))
churncsv$OnlineBackup = as.factor(ifelse(churncsv$OnlineBackup == 'No internet service'  | churncsv$OnlineBackup == 'No', 'No' , 'Yes'))
churncsv$DeviceProtection = as.factor(ifelse(churncsv$DeviceProtection == 'No internet service'  | churncsv$DeviceProtection == 'No', 'No' , 'Yes'))
churncsv$TechSupport = as.factor(ifelse(churncsv$TechSupport == 'No internet service' | churncsv$TechSupport == 'No', 'No' , 'Yes'))
churncsv$StreamingTV = as.factor(ifelse(churncsv$StreamingTV == 'No internet service' | churncsv$StreamingTV == 'No', 'No' , 'Yes'))
churncsv$StreamingMovies = as.factor(ifelse(churncsv$StreamingMovies == 'No internet service' | churncsv$StreamingMovies == 'No', 'No' , 'Yes'))

set.seed(1)
ind= sample(1:nrow(churncsv), (0.7*nrow(churncsv)), replace = FALSE)
train = churncsv[ind,]
test = churncsv[-ind,]

                                            ####Step 4: Statistical Analysis

#Method 1 - All variable model

model1chk = glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService +
                  MultipleLines + InternetService + OnlineSecurity + OnlineBackup +
                  DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
                  Contract + PaperlessBilling + PaymentMethod + MonthlyCharges +
                  TotalCharges, data = train, family = binomial)
summary(model1chk)

test$prob_1 = predict(model1chk, test, type = "response")
myroc_1 = roc(test$Churn, test$prob_1)
accuracy_1 = coords(myroc_1, "best")
accuracy_1
test$pred_1 = ifelse(test$prob_1>accuracy_1$threshold,1,0)

confusion_1=table(actual=test$Churn,predicted=test$pred_1)
confusion_1
TP=confusion_1[1,1]
FN=confusion_1[1,2]
FP=confusion_1[2,1]
TN=confusion_1[2,2]
accuracy=(TP+TN)/nrow(test)
precision=TP/(TP+FP)
recall=TP/(TP+FN)
error=1-accuracy
result_1 = c(accuracy,precision,recall,error)
result_1
f1 = 2*precision*recall/(precision + recall)
f1


#Method 2 - STEP AIC Method

a = stepAIC(model1chk, direction= "both")
stepaicmodel = glm(Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + 
                     InternetService + OnlineBackup + DeviceProtection + StreamingTV + 
                     StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                     MonthlyCharges + TotalCharges, data = train, family = binomial)
summary(stepaicmodel)
test$prob_2 = predict(stepaicmodel, test, type = "response")
myroc_2 = roc(test$Churn, test$prob_2)
accuracy_2 = coords(myroc_2, "best")
accuracy_2
test$pred_2 = ifelse(test$prob_2>accuracy_2$threshold,1,0)

confusion_2=table(actual=test$Churn,predicted=test$pred_2)
confusion_2
TP=confusion_2[1,1]
FN=confusion_2[1,2]
FP=confusion_2[2,1]
TN=confusion_2[2,2]
accuracy=(TP+TN)/nrow(test)
precision=TP/(TP+FP)
recall=TP/(TP+FN)
error=1-accuracy
result_2 = c(accuracy,precision,recall,error)
result_2


f2 = 2*precision*recall/(precision + recall)
f2

#Method 3 - Exhaustive search
exhaustivemodelchk = leaps::regsubsets(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService +
                                         MultipleLines + InternetService + OnlineSecurity + OnlineBackup +
                                         DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
                                         Contract + PaperlessBilling + PaymentMethod + MonthlyCharges +
                                         TotalCharges,data=train,nbest=1,nvmax=ncol(train), method="exhaustive")
res=summary(exhaustivemodelchk)
res$bic
plot(1:21,res$adjr2,type="b")
plot(1:21,res$bic,typ="b",col="red")
res$which
exhaustivemodel = glm(Churn ~    tenure + MultipleLines +
                        InternetService + OnlineSecurity +
                        TechSupport + StreamingMovies +
                        Contract + PaperlessBilling + PaymentMethod +
                        TotalCharges, data = train, family = binomial)
summary(exhaustivemodel)
test$prob_3 = predict(exhaustivemodel, test, type = "response")
myroc_3 = roc(test$Churn, test$prob_3)
accuracy_3 = coords(myroc_3, "best")
accuracy_3
test$pred_3 = ifelse(test$prob_3>accuracy_3$threshold,1,0)

confusion_3=table(actual=test$Churn,predicted=test$pred_3)
confusion_3
TP=confusion_3[1,1]
FN=confusion_3[1,2]
FP=confusion_3[2,1]
TN=confusion_3[2,2]
accuracy=(TP+TN)/nrow(test)
precision=TP/(TP+FP)
recall=TP/(TP+FN)
error=1-accuracy
result_3 = c(accuracy,precision,recall,error)
result_3

f3 = 2*precision*recall/(precision + recall)
f3


####Step 4: Summary and deciding the best model for prediction

#accuracy,precision,recall,error
#Method1
result_1
#Method 2
result_2
#Method 3
result_3

#Confusion Matrix
#Method1
confusion_1
#Method 2
confusion_2
#Method 3
confusion_3

#accuracy Results from coords
#Method1
accuracy_1
#Method 2
accuracy_2
#Method 3
accuracy_3

#F score
#Method1
f1
#Method 2
f2
#Method 3
f3


####Step 5: inference

churncsv$Tenure_Cat = ifelse(data$tenure < 12, "NewCustomer", ifelse(data$tenure < 24, "GoodCustomer", "LoyalCustomer"))


plot_fiber = ggplot(churncsv) + geom_bar(mapping = aes(x = as.factor(Churn),fill = InternetService), position="dodge")
plot_OnlineSecurity = ggplot(churncsv) + geom_bar(mapping = aes(x = Churn,fill = OnlineSecurity), position="dodge")
plot_payment = ggplot(churncsv) + geom_bar(mapping = aes(x = Churn,fill = PaymentMethod), position="dodge")
plot_contract = ggplot(churncsv) + geom_bar(mapping = aes(x = Churn,fill = Contract), position="dodge")
plot_tenure = ggplot(churncsv) + geom_point(mapping = aes(x=TotalCharges, y = tenure)) +facet_wrap(~Churn)
plot_tenure_cat = ggplot(churncsv) + geom_histogram(mapping = aes(x = tenure, color = "red"))+facet_grid(~Churn)

a = coef(stepaicmodel) %>% round(2)
a

summary(stepaicmodel)
summary(churncsv)

#ordered on the basis of z score from summary.
Intercept = 1.26 
tenure = -0.07
InternetServiceFiberoptic= 1.8
InternetServiceNo = -1.7
StreamingMoviesYes = 0.61
ContractOneyear = -0.72
ContractTwoyear = -1.43
MonthlyCharges = -0.04
TotalCharges = 0.0004
PaymentMethodCreditcard_automatic = 0.05
PaymentMethodElectroniccheck = 0.5
PaymentMethodMailedcheck = 0.04
MultipleLinesYes = 0.43
PaperlessBillingYes = 0.35
StreamingTVYes = 0.48
OnlineBackupYes = -0.01
SeniorCitizenSeniorCitizen = 0.17
DependentsYes = -0.19
DeviceProtectionYes = 0.14
                          ####Step 6: Developing and Understanding Business objectives


#Business Question 1 : Impact of Unitchange in Tenure on customer churn
oddsofchurning_1a = exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                       + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                       + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                       + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_1b = exp(Intercept + tenure*(30) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                          + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                          + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                          + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_1a
oddsofchurning_1b

#Business Question 2 : Impact of Internet Service categories on customer churn
oddsofchurning_2a = exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                       + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                       + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                       + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_2b = exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(0) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                          + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                          + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                          + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_2a/oddsofchurning_2b

#Business Question 3 : Impact of Streaming Movies category on customer churn
oddsofchurning_3a =  exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                        + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                        + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                        + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_3b =  exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(0) 
                           + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                           + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                           + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_3a / oddsofchurning_3b

#Business Question 4 : Impact of Contract category on customer churn
oddsofchurning_4a = exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                       + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                       + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                       + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_4b = exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                          + ContractOneyear*(1) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                          + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                          + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_4c = exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                        + ContractOneyear*(0) + ContractTwoyear*(1) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                        + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                        + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1

oddsofchurning_4a/oddsofchurning_4b
oddsofchurning_4a/oddsofchurning_4c

#Business Question 5 : Impact of Payment method on customer churn
oddsofchurning_5a =  exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                        + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(0)
                        + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                        + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_5b =  exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                           + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(1)
                           + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                           + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_5c =  exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                         + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(0)
                         + PaymentMethodElectroniccheck*(1) + PaymentMethodMailedcheck*(0) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                         + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_5d =  exp(Intercept + tenure*(29) +InternetServiceFiberoptic*(1) + InternetServiceNo*(0) + StreamingMoviesYes*(1) 
                         + ContractOneyear*(0) + ContractTwoyear*(0) + MonthlyCharges*(70.35) + TotalCharges*(1394.5) + PaymentMethodCreditcard_automatic*(0)
                         + PaymentMethodElectroniccheck*(0) + PaymentMethodMailedcheck*(1) + MultipleLinesYes*(1) + PaperlessBillingYes*(1) + StreamingTVYes*(1)
                         + OnlineBackupYes*(1) + SeniorCitizenSeniorCitizen*(0) + DependentsYes*(0) + DeviceProtectionYes*(0)) # to 1
oddsofchurning_5b/oddsofchurning_5a
oddsofchurning_5c/oddsofchurning_5a
oddsofchurning_5d/oddsofchurning_5a