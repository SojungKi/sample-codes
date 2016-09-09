############################ Description ##############################
######## This code selects out predictors for the variables ICD477 and Diabetes type 2
######## using chi-square test and correlation matrix
######## The goal of this project is to find the connection between ICD477 and diabetes type 2
######## owner: sojung ki 
#######################################################################


################### Import data #############################
base <- read.csv("~/Dropbox/16spring/STATS 141SL/final project/diabetese data winter 16.csv")


####################### preprocessing data ###############################

# recode variables

#Type 2 diabetes as 1, others as 0
base$Diabetes[which(base$Diabetes == 1)] <- 0
base$Diabetes[which(base$Diabetes == 2)] <- 1
#Check the column number of interested variable
which(names(base) == "FamilyDiabetesHistory")

#Delete variable that is not relevant
test <- base[,-c(91,109,96,92,77,86,141,106,128,136,14,118,46,80,89,10,42,5,73,27,30,17,114,58,33,144,122,84,
                 113,144,127,107,54,108,47,78,8,23,67,139,123)]
#Choose diabetes, ICD variables, Hypertension and Family Diabetes History
diatest <- test[,c(1, 4,6:106)]


##################Compute chi-square test matrix, save p-value in the matrix
n <- ncol(diatest)
result_dia <- matrix(0,n,n)
for(i in 1:n){
  for (j in 1:i){
    ttest <- chisq.test(diatest[,i], diatest[,j])
    result_dia[i,j] <- ttest$p.value
    result_dia[j,i] <- ttest$p.value
  }
}
#renames the column names of the chi-squrae matrix
colnames(result_dia) <- colnames(diatest)
#Choose ICD477, other ICD variables, Hypertension and Family Diabetes History
ICDtest = test[,c(4:106)]
#reorder the dataset to make ICD477 the first column for the sake of later steps
ICDtest=ICDtest[,c(2,1,3:103)]
#Compute chi-square test matrix, save p-value in the matrix
n <- ncol(ICDtest)
result_477 <- matrix(0,n,n)
for(i in 1:n){
  for (j in 1:i){
    ttest <- chisq.test(ICDtest[,i], ICDtest[,j])
    result_477[i,j] <- ttest$p.value
    result_477[j,i] <- ttest$p.value
  }
}
#renames the column names of the chi-squrae matrix
colnames(result_477) <- colnames(ICDtest)


################ this function selects variables with no overlapping correlation ################ 
bestpredictors = function(cortable, num.predictors, cor.cutoff) {
  temptable = cortable
  temptable[1,1] = 100
  preds = 0
  answer = c()
  while (preds < num.predictors) {
    index = which.min(abs(temptable[,1]))#find the var with highest correlation to the reponse
    answer = c(answer,colnames(temptable)[index])#add that variable to the answer
    keep = which(abs(temptable[index,]) > cor.cutoff) #find the vars least correlated with the last answer
    keep[1] = T #keep the response in the table
    temptable = temptable[keep,keep] #remove the rows/columns too highly correlated with answer
    preds = preds + 1
  }
  return(answer)
}
#Choose predictors for diabetes and ICD477 with function above
pred_dia <- bestpredictors(result_dia, 10, 0.01)
pred_477 <- bestpredictors(result_477, 10, 0.01)

#Create predictors set for diabetes
dia_model <- diatest[,pred_dia]
dia_model$Diabetes <- base$Diabetes
dia_model$BMI <- base$BMI
dia_model$Age <- base$Age


############# Run regression with AIC ############# 
library(leaps)
############# for diabetes variable
fullmod <- glm(Diabetes~.,family=binomial,data=dia_model)
AIC <- step(fullmod,direction="backward")

#Delete non-significant variable
delete_var <- c("ICD714", "ICD783")
dia_model <- dia_model[,!names(dia_model) %in% delete_var]

#Construct final model
dia_model <- glm(Diabetes~., family = binomial, data = dia_model)
summary(dia_model)


############# for ICD477 variable
ICD477_model <- diatest[,pred_477]
ICD477_model$ICD477 <- base$ICD477
ICD477_model$BMI <- base$BMI
ICD477_model$Age <- base$Age

#Run model with AIC
fullmod <- glm(ICD477~.,family=binomial,data=ICD477_model)
AIC <- step(fullmod,direction="backward")

#Delete non-significant variable
delete_var <- c("ICD703","Age","BMI")
ICD477_model <- ICD477_model[,!names(ICD477_model) %in% delete_var]

#Construct final model
ICD_model <- glm(ICD477~., family = binomial, data = ICD477_model)
summary(ICD_model)