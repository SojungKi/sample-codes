############################ Description ##############################
######## This code rescales nominal data(1~5) into binary data and then
######## conducts factor analysis and item response theory to extract
######## principal components and learning curves
######## 
#######################################################################


############### preprocessing: recode all items to binary ################
data<- read.csv("~/Dropbox/m154/mobile_report4.csv")
str(data)

#extract rows with yes to mobile bank and mobile pay
mobile1 <- data[data$mobilebank == "No", ]
mobile <- mobile1[mobile1$mobilepay == "No", ]
dim(mobile)
#2315 cases, 33 variables
View(mobile)

#rekey variables (refused is -1, so sum only number greater than 0)
attach(mobile)

############### preprocessing: recode all items to binary ################
levels(Q24)
levels(Q24)[2] <- 0 #not confident 
levels(Q24)[3] <- "" #refused to answer
levels(Q24)[3] <- 1 #somewhat confident
levels(Q24)[4] <- 1 #very confident 

levels(Q27)
levels(Q27)[2]<- 1 #internet is always available
levels(Q27)[3]<- 1 #available at certain locations
levels(Q27)[3]<- 0 #internet is not always available
levels(Q27)[4]<- 0 #access is not available
levels(Q27)[4]<- "" #i dont use internet
levels(Q27)[4]<- "" #refused

#no is 1, yes is 0 (reverse coding)
levels(Q45_1)
levels(Q45_1)[2] <- 1
levels(Q45_1)[3] <- ""
levels(Q45_1)[3] <- 0
  
#no is 1, yes is 0 (reverse coding)
levels(Q45_2)
levels(Q45_2)[2] <- 1
levels(Q45_2)[3] <- ""
levels(Q45_2)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q45_3)
levels(Q45_3)[2] <- 1
levels(Q45_3)[3] <- ""
levels(Q45_3)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q45_4)
levels(Q45_4)[2] <- 1
levels(Q45_4)[3] <- ""
levels(Q45_4)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q45_5)
levels(Q45_5)[2] <- 1
levels(Q45_5)[3] <- ""
levels(Q45_5)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q45_6)
levels(Q45_6)[2] <- 1
levels(Q45_6)[3] <- ""
levels(Q45_6)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q45_7)
levels(Q45_7)[2] <- 1
levels(Q45_7)[3] <- ""
levels(Q45_7)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q45_8)
levels(Q45_8)[2] <- 1
levels(Q45_8)[3] <- ""
levels(Q45_8)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q45_9)
levels(Q45_9)[2] <- 1
levels(Q45_9)[3] <- ""
levels(Q45_9)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_1)
levels(Q49_1)[2] <- 1
levels(Q49_1)[3] <- ""
levels(Q49_1)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_2)
levels(Q49_2)[2] <- 1
levels(Q49_2)[3] <- ""
levels(Q49_2)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_3)
levels(Q49_3)[2] <- 1
levels(Q49_3)[3] <- ""
levels(Q49_3)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_4)
levels(Q49_4)[2] <- 1
levels(Q49_4)[3] <- ""
levels(Q49_4)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_5)
levels(Q49_5)[2] <- 1
levels(Q49_5)[3] <- ""
levels(Q49_5)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_6)
levels(Q49_6)[2] <- 1
levels(Q49_6)[3] <- ""
levels(Q49_6)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_7)
levels(Q49_7)[2] <- 1
levels(Q49_7)[3] <- ""
levels(Q49_7)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_8)
levels(Q49_8)[2] <- 1
levels(Q49_8)[3] <- ""
levels(Q49_8)[3] <- 0

#no is 1, yes is 0 (reverse coding)
levels(Q49_9)
levels(Q49_9)[2] <- 1
levels(Q49_9)[3] <- ""
levels(Q49_9)[3] <- 0

levels(Q53)
levels(Q53)[2]<- "" #dont know
levels(Q53)[2]<- "" #refused
levels(Q53)[2]<- 1 #somewhat safe
levels(Q53)[3]<- 0 #somewhat unsafe
levels(Q53)[4]<- 1 #very safe
levels(Q53)[4]<- 0 #very unsafe


levels(Q54)
levels(Q54)[2]<- "" #dont know
levels(Q54)[2]<- "" #refused
levels(Q54)[2]<- 1 #somewhat safe
levels(Q54)[3]<- 0 #somewhat unsafe
levels(Q54)[4]<- 1 #very safe
levels(Q54)[4]<- 0 #very unsafe

levels(Q56)
levels(Q56)[2]<- 1 # agree
levels(Q56)[3]<- 0 # disagree
levels(Q56)[4]<- "" # refused
levels(Q56)[4]<- 1 #strongly agree
levels(Q56)[4]<- 0 #strongly disagree

levels(Q57)
levels(Q57)[2]<- 1 # agree
levels(Q57)[3]<- 0 # disagree
levels(Q57)[4]<- "" # refused
levels(Q57)[4]<- 1 #strongly agree
levels(Q57)[4]<- 0 #strongly disagree



################ recoded data ###################
df <- data.frame(Q24, Q45_1, Q45_2, Q45_3, Q45_4, Q45_5, Q45_6, Q45_7,
                 Q45_8, Q45_9, Q49_1, Q49_2, Q49_3, Q49_4, Q49_5, Q49_6, Q49_7,
                 Q49_8, Q49_9)#, Q53, Q54, Q56, Q57)

df1 <- data.frame(Q24, Q45_1, Q45_2, Q45_3, Q45_4, Q45_5, Q45_6, Q45_7,
                  Q45_8, Q45_9, Q49_1, Q49_2, Q49_3, Q49_4, Q49_5, Q49_6, Q49_7,
                  Q49_8, Q49_9, Q53, Q54, Q56, Q57)
colnames(df1) <- c("2-7", "1-1", "1-5","1-6","1-8","2-8","2-11","2-6","1-4","2-3","1-2","1-7",
                   "2-9","2-10","2-5","1-3","2-2","2-4","2-1","53","54","56","57")

write.csv(df1, file = "final data_ sojung ki") ##### save for future use

###########change factor into numeric - functional properties
indx <- sapply(df, is.factor)
df[indx] <- lapply(df[indx], function(x) as.numeric(as.character(x)))

indx1 <- sapply(df1, is.factor)
df1[indx1] <- lapply(df1[indx1], function(x) as.numeric(as.character(x)))

str(df)
sum(cor(df[sapply(df, is.numeric)], use = "pairwise.complete"))#revise data to get better var, cor
(77.80451-19)/(19*19-19)

sapply(df, mean, na.rm = TRUE) 
sapply(df, nobs, na.rm = TRUE)



################################## Factor analysis ################################## 
library(gdata)
#polychoric FA
library(polycor)
library(psych) 
het.mat <- polychoric(df,smooth=TRUE,global=TRUE)

fa.11<- fa(het.mat$rho,fm="ml", nfactors=2,n.obs = NA, rotate="varimax")

fa.11$loadings
fa.11$e.values
fa.11$score.cor
fa.11$scores

p <- print(fa.11)$Vaccounted
fa.diagram(fa.11, main="Input from a matrix")




library(ltm)
keys.list <- list(factor1=c("Q45_1", "Q49_1", "Q49_6", "Q45_8", "Q45_2", 
                          "Q49_2", "Q45_4"),
                  factor2=c("Q49_9", "Q49_7", "Q45_9", "Q49_8", "Q49_5", "Q45_7", "Q24", "Q45_5",
                            "Q49_3", "Q49_4","Q45_6"))

keys <- make.keys(df,keys.list)
scores <- scoreItems(keys,df, totals = FALSE)
summary(scores) #diagnoal is chronbach alpha
scores$alpha
scores$cor
scores$item.cor
scores$item.corrected



################################## IRT ################################## 
library(ltm)
descript(df1)
fit_2pl <- ltm(df1~z1)
fit_2pl
summary(fit_2pl, IRT.param = TRUE)



#A list or a matrix of the estimated parameters for b and alpha for the fitted model
coef(fit_2pl, TRUE, TRUE)

## ICC on all items (-3,3)
plot(fit_2pl,legend = TRUE, zrange = c(-3, 3),cx = "bottomright", xlab = "θ", main = "Figure 1. Item Characteristic Curves")
#ICC on selected
plot(fit_2pl,items = c(2,11,16,9,3,17,10,20,21), legend = TRUE, zrange = c(-3, 3),cx = "bottomright", xlab = "θ", main = "Figure 1. Item Characteristic Curves")

#information
plot(fit_2pl, type = "IIC", items = 0, zrange = c(-3, 3), col ="blue", main = "Figure 3. Total Information",
     xlab = "Skepticism on Mobile Financial Platforms", ylab = "Information")
info <- information(fit_2pl, c(-3, 3))
text(x = -1, y = 13, labels = paste("Total Information:", round(info$InfoTotal, 3),
                                      "\n\nInformation in (-3, 3):", round(info$InfoRange, 3),
                                      paste("(", round(100 * info$PropRange, 2), "%)", sep = "")), cex = 1)

vals <- plot(fit_2pl, type = "IIC", items = 0, plot = FALSE)
plot(vals[, "z"], 1/vals[, "info"], type = "l", col = "dark red", xlab = "Skepticism on Mobile Financial Platforms", ylab = "SEM", main = "Figure 4. Standard Error of Measurement")
text(x = -1, y = 13, labels = paste("SEM", round(info$InfoTotal, 3),
                                    "\n\n in (-3, 3):", round(info$InfoRange, 3),
                                    paste("(", round(100 * info$PropRange, 2), "%)", sep = "")), cex = 1)

#information curve on all items (-3,3)
plot(fit_2pl, type = "IIC", items = c(2,11,16,9,3,17,10,20,21),legend = TRUE, zrange = c(-3, 3),  main = "Figure 2. Item Information Curves for Selected Items",
     xlab = "Skepticism on Mobile Financial Services")

info.s <- information(fit_2pl, c(-3, 3), items = c(2,11,16,9,3,17,10,20,21))$InfoTotal
plot(vals[, "z"], info.s[, "info"], type = "l", lwd = 2)
