########### packages required #############

install.packages("C50")
library(C50)
library(tree)

library(caret)
library(gmodels)

library(party)
library(knitr)
install.packages("png")
library(png)



######### Reading and understanding the data ########

FraudCheck <- read.csv(file.choose())
View(FraudCheck)
colnames(FraudCheck)
nrow(FraudCheck)#[1] 600
ncol(FraudCheck)#[1] 6
summary(FraudCheck)

########### Splitting data into trainand test datas #############



hist(FraudCheck$Taxable.Income)


Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)


FC_train <- FC[1:300,]
nrow(FC_train)#[1] 300


FC_test <- FC[301:600,]
nrow(FC_test)#[1] 300




png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)


plot(opall_tree)



######## using the training Data 

png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)





plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) #[1] 0.82, 




CrossTable(FC_test$Risky_Good,pred_test_df)

####### Confusion Matrix

confusionMatrix(FC_test$Risky_Good,pred_test_df)#Accuracy : 0.82 
