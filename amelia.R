
############################ Description ##############################
######## This code predicts three outcomes (adopted, euthanized, other)
######## using packages xgboost under 3~5-fold cross-validation
######## 
######## 
#######################################################################


################### Import data #############################
finaltrain <- read.csv("~/Downloads/finaltrain.csv")
finaltest <- read.csv("~/Downloads/finaltest.csv")
puretrain <- read.csv("~/Downloads/puretrain")
puretest <- read.csv("~/Downloads/puretest")
natr <- read.csv("~/Downloads/train_na", stringsAsFactors = FALSE)
nate <- read.csv("~/Downloads/test_na", stringsAsFactors = FALSE)
str(natr)
str(puretrain)
#work on this data 
train <- finaltrain
test <- finaltest

train <- data.frame(train,waittime = natr$waittime, age = natr$age.intake, pure = puretrain$pure)
test <- data.frame(test,waittime = nate$waittime, age = nate$age.intake, pure = puretest$pure)


####################### preprocessing training and testing data ###############################


########### 1 = dog, 0 = cat #for calculating waittime
dogcat <- train$Species
dogcat[dogcat == 'DOG'] <- 1
dogcat[dogcat == 'CAT'] <- 0
train$dog.or.cat <- as.numeric(dogcat)

########### has name? 1 = yes, 0 = no
name <- as.factor(train$NAME)
name <- as.factor(is.na(name))
name <- as.numeric(name) 
name[name == 2] <- 0 #change no name to 0
train$has.name <- as.factor(name)


########### microchip status, 1 = yes, 0 = no
microchip <- as.factor(train$Microchip.Status)
levels(microchip)
levels(microchip)[1] <- 1
train$has.microchip <- as.numeric(microchip) 
train$has.microchip[is.na(train$has.microchip)] <- 0 #no microchip 
train$has.microchip <- as.factor(train$has.microchip)

########### spayed or neutered? 1 = yes #### this is for calculating waittime
sn <- train$Sex
sn[sn == 'F'] <- 0
sn[sn == 'M'] <- 0
sn[sn == 'U'] <- 0
sn[sn == 'N'] <- 1
sn[sn == 'S'] <- 1
train$sn <- as.numeric(sn)

########### age at intake in months
intakeage <- as.Date(as.character(train$Intake.Date), format="%Y-%m-%d") - as.Date(as.character(train$DOB), format="%Y-%m-%d")
intakeage <- as.numeric(intakeage)
#convert to months
train$age.at.intake <- 12*floor(intakeage/365) + floor((intakeage %% 365)/30)
#dont forget to deal with missing data...


########## WAIT TIME in DAYS
waitdays <- as.Date(as.character(train$Outcome.Date), format="%Y-%m-%d") - as.Date(as.character(train$Intake.Date), format="%Y-%m-%d")
waitdays <- as.numeric(waitdays)
train$waittime <- waitdays


#explore waittime
sum(is.na(waitdays))
hist(waitdays, xlab="x", xlim = c(0,30), ylab="N", breaks="FD")
hist(ss, xlab="x", xlim = c(0,300), ylab="N", breaks="FD")
length(which(waitdays == 0))
length(which(waitdays == 1))
length(which(waitdays == 2))
length(which(waitdays == 3))
me <- mean(waitdays, na.rm = TRUE)
ss <- (waitdays - me)^2


######### outcome category into three #this is for waittime and zipcode
train$outcome <- train$OutCatg
table(train$outcome)
levels(train$outcome)[1] <- 1
levels(train$outcome)[2] <- 0
levels(train$outcome)[3] <- 1
levels(train$outcome)[3] <- 1
levels(train$outcome)[3] <- 1
levels(train$outcome)[3] <- 1
levels(train$outcome)[3] <- 1

#############
outcat <- train$OutCatg
table(outcat)
levels(outcat)[3] <- 'OTHER'
levels(outcat)[4] <- 'OTHER'
levels(outcat)[4] <- 'OTHER'
levels(outcat)[4] <- 'OTHER'
levels(outcat)[4] <- 'OTHER'
train$out.category <- outcat

############## most and least outcome by zipcode
zipp <- as.factor(train$Intake.Zip.Code) 
length(zip_id)
zip_id <- as.factor(levels(zipp)) #849 levels
zip_id[1:10]
temp <- data.frame(zipp, out = train$out.category)
dim(temp)
head(temp)

chooseout <- function(df, id){
  colnames(df) <- c("zipcode","outcome") #rename for convenicence
  #create temp dataframe of all levels of zipcodes
  df2 <- data.frame(zipcodes = id, mostly = NA, least = NA)
  loops <- dim(df2)[1]
  
  for(i in 1:loops){
    #find rows with same zipcodes
    match <- df[df$zipcode == df2$zipcodes[i],] #matches first zipcode
    colnames(match) <- c("zipcode","outcome") #rename for convenicence
    
    ## euthanasia
    euth <- length(match$outcome[match$outcome == 'EUTHANASIA'])
    ##other
    other <- length(match$outcome[match$outcome == 'OTHER'])
    ##adop
    adop <- length(match$outcome[match$outcome == 'ADOPTION'])
    
    #find which is most occuring, least occuring
    major <- which.max(c(euth,other,adop)) # if 1 = euth, 2 = other, 3 = adop is max
    minor <- which.min(c(euth,other,adop)) 
    
    #store in temp dataframe
    df2$mostly[i] <- major
    df2$least[i] <- minor
  }
  df2
}
#run the function
byzip <- chooseout(temp,zip_id) 


#zipp is the zipcode variable in training set
mostlikely <- byzip$mostly[match(zipp,byzip$zipcodes)] 
leastlikely <- byzip$least[match(zipp,byzip$zipcodes)] 

length(mostlikely) #lenght check: correct
length(leastlikely) #lenght check: correct
zipagg <- data.frame(ARN = train$ARN, zip = train$Intake.Zip.Code, mostlikely2 = mostlikely, leastlikely2 = leastlikely)
zipagg[9:19,]
zipagg$middle <- NA
zipagg$middle[zipagg$mostlikely2 == 1 & zipagg$leastlikely2 == 2] <- 3
zipagg$middle[zipagg$mostlikely2 == 2 & zipagg$leastlikely2 == 1] <- 3
zipagg$middle[zipagg$mostlikely2 == 1 & zipagg$leastlikely2 == 3] <- 2
zipagg$middle[zipagg$mostlikely2 == 3 & zipagg$leastlikely2 == 1] <- 2
zipagg$middle[zipagg$mostlikely2 == 2 & zipagg$leastlikely2 == 3] <- 1
zipagg$middle[zipagg$mostlikely2 == 3 & zipagg$leastlikely2 == 2] <- 1

zipagg$division <- NA
zipagg$division[zipagg$mostlikely2 == 1 & zipagg$leastlikely2 == 2 & zipagg$middle == 3] <- 1
zipagg$division[zipagg$mostlikely2 == 1 & zipagg$leastlikely2 == 3 & zipagg$middle == 2] <- 2
zipagg$division[zipagg$mostlikely2 == 2 & zipagg$leastlikely2 == 1 & zipagg$middle == 3] <- 3
zipagg$division[zipagg$mostlikely2 == 2 & zipagg$leastlikely2 == 3 & zipagg$middle == 2] <- 4
zipagg$division[zipagg$mostlikely2 == 3 & zipagg$leastlikely2 == 1 & zipagg$middle == 2] <- 5
zipagg$division[zipagg$mostlikely2 == 3 & zipagg$leastlikely2 == 2 & zipagg$middle == 1] <- 6


#check if correct
new <- data.frame(mostlikely2 = mostlikely, leastlikely2 = leastlikely, outc = train$out.category)
# if 1 = euth, 2 = other, 3 = adop is max
spineplot(as.factor(new$mostlikely2),new$outc)
#check if correct
head(zipagg)
write.csv(zipagg, file = "outbyzip", row.names = FALSE)


############## outcome month
dt <- as.Date(train$Outcome.Date)
mt <- as.numeric(format(dt, '%m') )
train$month.out <- mt
#cdplot(as.factor(outcome) ~ month.out, data = train) ####***********#######


############## outcome year
dt <- as.Date(train$Outcome.Date)
mt <- as.numeric(format(dt, '%Y') )
train$year.out <- mt
#cdplot(as.factor(outcome) ~ year.out, data = train) ####***********#######


############## outcome dow
dt <- as.Date(train$Outcome.Date)
day <- weekdays(dt)
train$out.dow <- as.factor(day)

############## intake month (good predictor)
dt <- as.Date(train$Intake.Date)
mt <- as.numeric(format(dt, '%Y') )
train$month.in <- as.factor(mt)
#spineplot(train$month.in,as.factor(train$outcome))

############## dob month (good predictor)
dt <- as.Date(train$DOB)
mt <- as.numeric(format(dt, '%m') )
train$dom <- as.factor(mt)
#spineplot(train$dom,as.factor(train$outcome))


############## dob year
dt <- as.Date(train$DOB)
mt <- as.numeric(format(dt, '%Y') )
train$doy <- as.factor(mt)
#spineplot(train$doy,as.factor(train$outcome))

############## has license
lic <- train$License.Date
levels(lic)[1:1585] <- "yes"  #has license
levels(lic)[2] <- "no"
lic[is.na(lic)] <- "no" #no license
train$has.license <- lic

######################### TEST ON TESTING DATA ###############################


########### 1 = dog, 0 = cat #for calculating waittime
dogcat <- test$Species
dogcat[dogcat == 'DOG'] <- 1
dogcat[dogcat == 'CAT'] <- 0
test$dog.or.cat <- as.numeric(dogcat)

########### has name? 1 = yes, 0 = no
name <- as.factor(test$NAME)
name <- as.factor(is.na(name))
name <- as.numeric(name) 
name[name == 2] <- 0 #change no name to 0
test$has.name <- as.factor(name)


########### microchip status, 1 = yes, 0 = no
microchip <- as.factor(test$Microchip.Status)
levels(microchip)
levels(microchip)[1] <- 1
test$has.microchip <- as.numeric(microchip) 
test$has.microchip[is.na(test$has.microchip)] <- 0 #no microchip 
test$has.microchip <- as.factor(test$has.microchip)


########### spayed or neutered? 1 = yes #for calculting waittime
sn <- test$Sex
sn[sn == 'F'] <- 0
sn[sn == 'M'] <- 0
sn[sn == 'U'] <- 0
sn[sn == 'N'] <- 1
sn[sn == 'S'] <- 1
test$sn <- as.numeric(sn)

########### age at intake in months
intakeage <- as.Date(as.character(test$Intake.Date), format="%Y-%m-%d") - as.Date(as.character(test$DOB), format="%Y-%m-%d")
intakeage <- as.numeric(intakeage)
#convert to months
test$age.at.intake <- 12*floor(intakeage/365) + floor((intakeage %% 365)/30)
#dont forget to deal with missing data...


########## WAIT TIME in DAYS
waitdays <- as.Date(as.character(test$Outcome.Date), format="%Y-%m-%d") - as.Date(as.character(test$Intake.Date), format="%Y-%m-%d")
waitdays <- as.numeric(waitdays)
test$waittime <- waitdays


########## first 3 zip code, 0 = no zipcode recorded
first.zip2 <- as.numeric(str_extract(test$Intake.Zip.Code, "[0-9]{3}+"))
first.zip2[is.na(first.zip2)] <- 0 #no 
test$zip <- first.zip2 #keep numeric since xgboost is numeric

############## outcome month
dt <- as.Date(test$Outcome.Date)
mt <- as.numeric(format(dt, '%m') )
test$month.out <- mt

############## outcome year
dt <- as.Date(test$Outcome.Date)
mt <- as.numeric(format(dt, '%Y') )
test$year.out <- mt

############## outcome dow
dt <- as.Date(test$Outcome.Date)
day <- weekdays(dt)
test$out.dow <- as.factor(day)


############## intake month (good predictor)
dt <- as.Date(test$Intake.Date)
mt <- as.numeric(format(dt, '%Y') )
test$month.in <- as.factor(mt)
#spineplot(train$month.in,as.factor(train$outcome))

############## dob month (good predictor)
dt <- as.Date(test$DOB)
mt <- as.numeric(format(dt, '%m') )
test$dom <- as.factor(mt)
#spineplot(train$dom,as.factor(train$outcome))


############## dob year
dt <- as.Date(test$DOB)
mt <- as.numeric(format(dt, '%Y') )
test$doy <- as.factor(mt)
#spineplot(train$doy,as.factor(train$outcome))

############## has license
lic <- test$License.Date
levels(lic)[1:1585] <- "yes"  #has license
levels(lic)[2] <- "no"
lic[is.na(lic)] <- "no" #no license
test$has.license <- lic

############## most/least occuring outcome by zipcode 
zipp <- as.factor(test$Intake.Zip.Code) 

mostlikely <- byzip$mostly[match(zipp,byzip$zipcodes)] 
leastlikely <- byzip$least[match(zipp,byzip$zipcodes)] 

zipagg.test <- data.frame(ARN = test$ARN, zip = test$Intake.Zip.Code, mostlikely2 = mostlikely, leastlikely2 = leastlikely)
dim(zipagg.test)
zipagg.test$middle <- NA
zipagg.test$middle[zipagg.test$mostlikely2 == 1 & zipagg.test$leastlikely2 == 2] <- 3
zipagg.test$middle[zipagg.test$mostlikely2 == 2 & zipagg.test$leastlikely2 == 1] <- 3
zipagg.test$middle[zipagg.test$mostlikely2 == 1 & zipagg.test$leastlikely2 == 3] <- 2
zipagg.test$middle[zipagg.test$mostlikely2 == 3 & zipagg.test$leastlikely2 == 1] <- 2
zipagg.test$middle[zipagg.test$mostlikely2 == 2 & zipagg.test$leastlikely2 == 3] <- 1
zipagg.test$middle[zipagg.test$mostlikely2 == 3 & zipagg.test$leastlikely2 == 2] <- 1

zipagg.test$division <- NA
zipagg.test$division[zipagg.test$mostlikely2 == 1 & zipagg.test$leastlikely2 == 2 & zipagg.test$middle == 3] <- 1
zipagg.test$division[zipagg.test$mostlikely2 == 1 & zipagg.test$leastlikely2 == 3 & zipagg.test$middle == 2] <- 2
zipagg.test$division[zipagg.test$mostlikely2 == 2 & zipagg.test$leastlikely2 == 1 & zipagg.test$middle == 3] <- 3
zipagg.test$division[zipagg.test$mostlikely2 == 2 & zipagg.test$leastlikely2 == 3 & zipagg.test$middle == 2] <- 4
zipagg.test$division[zipagg.test$mostlikely2 == 3 & zipagg.test$leastlikely2 == 1 & zipagg.test$middle == 2] <- 5
zipagg.test$division[zipagg.test$mostlikely2 == 3 & zipagg.test$leastlikely2 == 2 & zipagg.test$middle == 1] <- 6

write.csv(zipagg.test, file = "outbyziptest", row.names = FALSE)

####################################################################


#############reformat outcatg
outcat <- train$OutCatg
table(outcat)
levels(outcat)[3] <- 'OTHER'
levels(outcat)[4] <- 'OTHER'
levels(outcat)[4] <- 'OTHER'
levels(outcat)[4] <- 'OTHER'
levels(outcat)[4] <- 'OTHER'
train$out.category <- outcat



######################## handling missing values ###############################################
######## Expectation maximazation USING MICE PACKAGE 
######## install.packages("mice")
library(mice)
################# TRAIN : mice pmm ###############
t.mice <- train[,c(23,22,21,20,19,18,17)] #17 = dog or cat, 23 = outcome

#chi <- table(train$waittime,train$doy)
#chisq.test(chi)

imp0 <- mice(as.matrix(t.mice), maxit=1)
predM <- imp0$predictorMatrix
impM <- imp0$method

complete.mice <- complete(imp0,5)
any(is.na(complete.mice$waittime)) #check missing data
train$mice.wait <-complete.mice$waittime
train$mice.age <- complete.mice$age.at.intake

#compare distribution
par(mfrow=c(1,2))
hist(train$age.at.intake, breaks = 50)
hist(train$mice.age, breaks = 50)
par(mfrow=c(1,1))

#check significance of age
under <- which(train$age.at.intake < -2)
train[under,c(6,8,16)]
birthyear <- as.numeric(str_extract(train$DOB, "[0-9]{4}+")) 
m <- table(as.factor(train$outcome),as.factor(birthyear))
barplot(m)
cdplot(as.factor(outcome)~mice.age,data = train) ####***********#######
#chiage <- table(train$mice.age,train$outcome)
#chisq.test(chiage)

################### export result #############
train.na <- data.frame(ARN = finaltrain$ARN, waittime =train$mice.wait, age.intake = train$mice.age)
write.csv(train.na, file = "train_na")
############################################################################
##te.mice <- test[,c(23,22,21,20,19,18,17)] #remove outcatg

imp1 <- mice(as.matrix(te.mice), maxit=1)
predM1 <- imp1$predictorMatrix
impM1 <- imp1$method

complete.mice1 <- complete(imp1,5)
any(is.na(complete.mice1$waittime)) #check missing data
test$mice.wait <-complete.mice1$waittime
test$mice.age <- complete.mice1$age.at.intake

#compare distr
par(mfrow=c(1,2))
hist(test$age.at.intake, breaks = 50)
hist(test$mice.age, breaks = 50)
par(mfrow=c(1,1))

################### export result #############
test.na <- data.frame(ARN = (finaltest$ARN), waittime =test$mice.wait, age.intake = test$mice.age)
write.csv(test.na, file = "test_na")
################################################

#########add cimple color (a new variable)
te.color <- read.csv("~/Downloads/test2.csv")
tr.color <- read.csv("~/Downloads/train2.csv")

train$SimpleColor <- tr.color$SimpleColor
test$SimpleColor <- te.color$SimpleColor



################################ Modeling ################################ 
################################ PREDICTION: xgboost #######################
str(train)
#BOOST
#create matrix
tt <- cbind(train$Species, train$Sex, train$Intake.Type, train$Shelter, train$has.name,
            train$has.microchip, train$waittime, train$age, train$month.out,
            train$year.out, train$out.dow, train$month.in, train$dom, train$doy,
            train$has.license, train$SimpleColor, train$pure) 

tt[tt < 0] <- 0 #negative age and waittime to 0

#adoption = 0, other = 2, euth = 1
train_targets <-as.matrix(as.numeric(train$out.category) - 1)

###################### predictor model -boosted
library("xgboost")
set.seed(100)
cv.res = xgb.cv(data = tt, nfold=3, label = train_targets, nrounds = 180, verbose =1, 
                objective ="multi:softprob", num_class = 3, eval_metric="mlogloss",
                missing=NaN, earlearly.stop.round = 4)
bestRound = which.min(as.matrix(cv.res)[,3]-as.matrix(cv.res[,4])) #best iteration
bestRound # 177

m9 = xgboost(data=tt, label=train_targets, nrounds = 180, verbose=2, eta=0.3,
             max_depth=8, subsample=0.9, colsample_bytree=0.9, objective="multi:softprob",
             eval_metric="mlogloss", num_class=3, missing = NaN, early.stop.round = 3)

####  predict outcome on test 
te <- cbind(test$Species, test$Sex, test$Intake.Type, test$Shelter, test$has.name, 
            test$has.microchip, test$waittime, test$age,test$month.out, 
            test$year.out, test$out.dow, test$dom, test$doy, 
            test$has.license, test$pure)

###################### Lets predict!!
pred1 <- predict(m9, newdata = te, missing = NaN)

###################### reformat outcome into submission mode ###################### 
test_preds_frame <- data.frame(matrix(pred1, ncol = 3, byrow=TRUE))
colnames(test_preds_frame) <- c("ADOPTION","EUTHANASIA","OTHER")
submission22 <- cbind(data.frame('ARN' = test$ARN), test_preds_frame)

submission22[20:25,] #same but with zip variable
write.csv(submission22, file = "June2_2", row.names = FALSE)



