Medicalpremium <- read.csv("~/HEC/Stat Learning/data/Medicalpremium.csv")

mpraw = Medicalpremium

install.packages("randomForest")
library(randomForest)

mp = mpraw

set.seed(20606)
trainID=sample(1:986,750)
train=mp[trainID,]
test=mp[-trainID,]


rf1=randomForest(PremiumPrice~.,data=train)
rf1

prf1=predict(rf1,test)
c(MSE=mean((prf1-test$PremiumPrice)^2),RMSE=sqrt(mean((prf1-test$PremiumPrice)^2)))