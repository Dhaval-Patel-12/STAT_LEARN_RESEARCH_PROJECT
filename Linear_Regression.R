#This is an R script.

#Those are the libraries.
library(creditR)
library(magrittr)
library(dplyr)
library(ROCR)

#Extracting data from file
medicaldatarawfile = "Medicalpremium.csv"
medicaldataraw = read.csv(medicaldatarawfile)

#Basic info about the data
medicaldataraw %>% nrow()
str(medicaldataraw)
head(medicaldataraw)
missing_ratio(medicaldataraw)

#Verify missing data
list_na <- colnames(medicaldataraw)[ apply(medicaldataraw, 2, anyNA) ]
list_na

#Replace missing data with the mean
average_missing <- apply(medicaldataraw[list_na],
                         2,
                         mean,
                         na.rm =  TRUE)

#Transforming the incorrect variables
medicaldataraw = medicaldataraw %>%
  mutate(Diabetes=as.factor(Diabetes), BloodPressureProblems=as.factor(BloodPressureProblems),
         AnyTransplants=as.factor(AnyTransplants), AnyChronicDiseases=as.factor(AnyChronicDiseases), 
         KnownAllergies=as.factor(KnownAllergies), HistoryOfCancerInFamily=as.factor(HistoryOfCancerInFamily))

summary(medicaldataraw)

#Preparing a sample medicaldataraw set
sample_data <- medicaldataraw[]
str(sample_data)

#Splitting the medicaldataraw into train and test sets
traintest <- train_test_split(sample_data,123,0.70)
train <- traintest$train
test <- traintest$test
str(train)
str(test)

#Train linear models
Linear_Model_01=glm(PremiumPrice~.,family="gaussian",data=train)
summary(Linear_Model_01)

Prediction_01=predict(Linear_Model_01,newdata=test,type="response")
cbind(Prediction_01,test)[sort.list(Prediction_01,decreasing=TRUE)[1:10],]

Linear_Model_02=step(Linear_Model_01,trace=FALSE)

#Create interactions between independent numerical variables.
medicaldatarawplus=medicaldataraw %>%
  select(PremiumPrice,Diabetes,BloodPressureProblems,AnyTransplants,
         AnyChronicDiseases,KnownAllergies,HistoryOfCancerInFamily,everything())

for(i in 8:11){
  for(j in i:11){
    medicaldatarawplus = medicaldatarawplus %>%
      mutate(!!paste(names(medicaldatarawplus)[[i]],names(medicaldatarawplus)[[j]],sep="_"):=as.numeric(medicaldatarawplus[[i]])*medicaldatarawplus[[j]])
  }
}

#Preparing a sample medicaldatarawplus set
sample_data_plus <- medicaldatarawplus[]
str(sample_data_plus)

#Splitting the medicaldatarawplus into train and test sets
traintestplus <- train_test_split(sample_data_plus,123,0.70)
trainplus <- traintestplus$train
testplus <- traintestplus$test
str(trainplus)
str(testplus)

Linear_Model_03=glm(PremiumPrice~.,family="gaussian",data=trainplus)
Linear_Model_04=step(Linear_Model_03,trace=FALSE)

Prediction_02=predict(Linear_Model_02,newdata=test,type="response")
Prediction_03=predict(Linear_Model_03,newdata=testplus,type="response")
Prediction_04=predict(Linear_Model_04,newdata=testplus,type="response")

cbind(Prediction_02,test)[sort.list(Prediction_02,decreasing=TRUE)[1:10],]
cbind(Prediction_03,testplus)[sort.list(Prediction_03,decreasing=TRUE)[1:10],]
cbind(Prediction_04,testplus)[sort.list(Prediction_04,decreasing=TRUE)[1:10],]

temp = cbind(Prediction_04,testplus)[sort.list(Prediction_04,decreasing=TRUE)[1:10],]
temp=temp %>%
  select(Prediction_04,PremiumPrice)

temp

#Predicting the values when data is removed from file.

#Extracting data from file
medicalfile_missing = "Medicalpremium_missing.csv"
medical_missing = read.csv(medicalfile_missing)

#Basic info about the data
medical_missing %>% nrow()
str(medical_missing)
head(medical_missing)
missing_ratio(medical_missing)

#Verify missing data
list_na_missing <- colnames(medical_missing)[ apply(medical_missing, 2, anyNA) ]
list_na_missing

#Replace missing data with the mean
average_actual_missing <- apply(medical_missing[list_na_missing],
                         2,
                         mean,
                         na.rm =  TRUE)

#Transforming the incorrect variables
medical_missing = medical_missing %>%
  mutate(Diabetes=as.factor(Diabetes), BloodPressureProblems=as.factor(BloodPressureProblems),
         AnyTransplants=as.factor(AnyTransplants), AnyChronicDiseases=as.factor(AnyChronicDiseases), 
         KnownAllergies=as.factor(KnownAllergies), HistoryOfCancerInFamily=as.factor(HistoryOfCancerInFamily))

medical_missing$predicted.actual_01 = predict(Linear_Model_01, newdata=actual, type="response")
medical_missing$predicted.actual_02 = predict(Linear_Model_02, newdata=actual, type="response")
medical_missing$predicted.actual_03 = predict(Linear_Model_03, newdata=actual, type="response")
medical_missing$predicted.actual_04 = predict(Linear_Model_04, newdata=actual, type="response")
