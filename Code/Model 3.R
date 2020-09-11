# loading packages
setwd("D:/R Projects/Genpro/MPA/Linear Mix Model")
library(sqldf)
library(reshape2)

# removing unwanted spaces

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

opioids <- read.csv("./data/opioids.csv")
overdoses <- read.csv("./data//overdoses.csv")
prescriber_info <- read.csv("./data//prescriber-info.csv")
Accidental_Death <- read.csv("./data//ds3.csv")

Accidental_Death_sub <- Accidental_Death[c("Age","Sex","COD"
                                           ,"Fentanyl","Heroin","Cocaine","FentanylAnalogue"
                                           ,"Oxycodone","Oxymorphone","Hydrocodone","Methadone","Ethanol","Benzodiazepine"
                                           ,"Amphet","Tramad"
                                           ,"Morphine_NotHeroin","Hydromorphone","OpiateNOS","AnyOpioid" )]

# Accidental_Death_1 <- sqldf("select Sex,count(*) from Accidental_Death_sub
#                             Group by Sex")
# 
# Accidental_Death_2 <- sqldf("select * from Accidental_Death_sub
#                             where AnyOpioid!='Y' AND Heroin='Y'
#                             ")
# 
# Accidental_Death_3 <- sqldf("select * from Accidental_Death_sub
#                             where AnyOpioid!='Y' AND OpiateNOS='Y'
#                             ")

Accidental_Death_sub$Fentanyl <- trim(Accidental_Death_sub$Fentanyl)
Accidental_Death_sub$Oxycodone <- trim(Accidental_Death_sub$Oxycodone)
Accidental_Death_sub$Oxymorphone <- trim(Accidental_Death_sub$Oxymorphone)
Accidental_Death_sub$Hydrocodone <- trim(Accidental_Death_sub$Hydrocodone)
Accidental_Death_sub$Methadone <- trim(Accidental_Death_sub$Methadone)
Accidental_Death_sub$Morphine_NotHeroin <- trim(Accidental_Death_sub$Morphine_NotHeroin)
Accidental_Death_sub$Hydromorphone <- trim(Accidental_Death_sub$Hydromorphone)
Accidental_Death_sub$OpiateNOS <- trim(Accidental_Death_sub$OpiateNOS)
Accidental_Death_sub$AnyOpioid <- trim(Accidental_Death_sub$AnyOpioid)
Accidental_Death_sub$Heroin <- trim(Accidental_Death_sub$Heroin)
Accidental_Death_sub$Cocaine <- trim(Accidental_Death_sub$Cocaine)
Accidental_Death_sub$FentanylAnalogue <- trim(Accidental_Death_sub$FentanylAnalogue)
Accidental_Death_sub$Ethanol <- trim(Accidental_Death_sub$Ethanol)
Accidental_Death_sub$Benzodiazepine <- trim(Accidental_Death_sub$Benzodiazepine)
Accidental_Death_sub$Amphet <- trim(Accidental_Death_sub$Amphet)
Accidental_Death_sub$Tramad <- trim(Accidental_Death_sub$Tramad)


Accidental_Death_sub$Fentanyl[Accidental_Death_sub$Fentanyl=='Y'] <- 1
Accidental_Death_sub$Oxycodone[Accidental_Death_sub$Oxycodone=='Y'] <- 1
Accidental_Death_sub$Oxymorphone[Accidental_Death_sub$Oxymorphone=='Y'] <- 1
Accidental_Death_sub$Hydrocodone[Accidental_Death_sub$Hydrocodone=='Y'] <- 1
Accidental_Death_sub$Methadone[Accidental_Death_sub$Methadone=='Y'] <- 1
Accidental_Death_sub$Morphine_NotHeroin[Accidental_Death_sub$Morphine_NotHeroin=='Y'] <- 1
Accidental_Death_sub$Hydromorphone[Accidental_Death_sub$Hydromorphone=='Y'] <- 1
Accidental_Death_sub$OpiateNOS[Accidental_Death_sub$OpiateNOS=='Y'] <- 1
Accidental_Death_sub$AnyOpioid[Accidental_Death_sub$AnyOpioid=='Y'] <- 1
Accidental_Death_sub$Heroin[Accidental_Death_sub$Heroin=='Y'] <- 1
Accidental_Death_sub$Cocaine[Accidental_Death_sub$Cocaine=='Y'] <- 1
Accidental_Death_sub$FentanylAnalogue[Accidental_Death_sub$FentanylAnalogue=='Y'] <- 1
Accidental_Death_sub$Ethanol[Accidental_Death_sub$Ethanol=='Y'] <- 1
Accidental_Death_sub$Benzodiazepine[Accidental_Death_sub$Benzodiazepine=='Y'] <- 1
Accidental_Death_sub$Amphet[Accidental_Death_sub$Amphet=='Y'] <- 1
Accidental_Death_sub$Tramad[Accidental_Death_sub$Tramad=='Y'] <- 1

# Numeric

Accidental_Death_sub$Fentanyl <- as.numeric(Accidental_Death_sub$Fentanyl)
Accidental_Death_sub$Oxycodone <- as.numeric(Accidental_Death_sub$Oxycodone)
Accidental_Death_sub$Oxymorphone <- as.numeric(Accidental_Death_sub$Oxymorphone)
Accidental_Death_sub$Hydrocodone <- as.numeric(Accidental_Death_sub$Hydrocodone)
Accidental_Death_sub$Methadone <- as.numeric(Accidental_Death_sub$Methadone)
Accidental_Death_sub$Morphine_NotHeroin <- as.numeric(Accidental_Death_sub$Morphine_NotHeroin)
Accidental_Death_sub$Hydromorphone <- as.numeric(Accidental_Death_sub$Hydromorphone)
Accidental_Death_sub$OpiateNOS <- as.numeric(Accidental_Death_sub$OpiateNOS)
Accidental_Death_sub$AnyOpioid <- as.numeric(Accidental_Death_sub$AnyOpioid)
Accidental_Death_sub$Heroin <- as.numeric(Accidental_Death_sub$Heroin)
Accidental_Death_sub$Cocaine <- as.numeric(Accidental_Death_sub$Cocaine)
Accidental_Death_sub$FentanylAnalogue <- as.numeric(Accidental_Death_sub$FentanylAnalogue)
Accidental_Death_sub$Ethanol <- as.numeric(Accidental_Death_sub$Ethanol)
Accidental_Death_sub$Benzodiazepine <- as.numeric(Accidental_Death_sub$Benzodiazepine)
Accidental_Death_sub$Amphet <- as.numeric(Accidental_Death_sub$Amphet)
Accidental_Death_sub$Tramad <- as.numeric(Accidental_Death_sub$Tramad)


Accidental_Death_sub$Fentanyl[is.na(Accidental_Death_sub$Fentanyl)] <- 0

Accidental_Death_sub$Oxycodone[is.na(Accidental_Death_sub$Oxycodone)] <- 0
Accidental_Death_sub$Oxymorphone[is.na(Accidental_Death_sub$Oxymorphone)] <- 0
Accidental_Death_sub$Hydrocodone[is.na(Accidental_Death_sub$Hydrocodone)] <- 0
Accidental_Death_sub$Methadone[is.na(Accidental_Death_sub$Methadone)] <- 0
Accidental_Death_sub$Morphine_NotHeroin[is.na(Accidental_Death_sub$Morphine_NotHeroin)] <- 0
Accidental_Death_sub$Hydromorphone[is.na(Accidental_Death_sub$Hydromorphone)] <- 0
Accidental_Death_sub$OpiateNOS[is.na(Accidental_Death_sub$OpiateNOS)] <- 0
Accidental_Death_sub$AnyOpioid[is.na(Accidental_Death_sub$AnyOpioid)] <- 0
Accidental_Death_sub$Heroin[is.na(Accidental_Death_sub$Heroin)] <- 0
Accidental_Death_sub$Cocaine[is.na(Accidental_Death_sub$Cocaine)] <- 0
Accidental_Death_sub$FentanylAnalogue[is.na(Accidental_Death_sub$FentanylAnalogue)] <- 0
Accidental_Death_sub$Ethanol[is.na(Accidental_Death_sub$Ethanol)] <- 0
Accidental_Death_sub$Benzodiazepine[is.na(Accidental_Death_sub$Benzodiazepine)] <- 0
Accidental_Death_sub$Amphet[is.na(Accidental_Death_sub$Amphet)] <- 0
Accidental_Death_sub$Tramad[is.na(Accidental_Death_sub$Tramad)] <- 0

#Accidental_Death_sub$OpioidFlag<-Accidental_Death_sub$Fentanyl+Accidental_Death_sub$Oxycodone+Accidental_Death_sub$Oxymorphone+Accidental_Death_sub$Hydrocodone+Accidental_Death_sub$Methadone+Accidental_Death_sub$Morphine_NotHeroin+Accidental_Death_sub$Hydromorphone+Accidental_Death_sub$OpiateNOS+Accidental_Death_sub$AnyOpioid

#Accidental_Death_sub$OpioidFlag[Accidental_Death_sub$OpioidFlag>=1]<- 1

# categorising cod

Accidental_Death_cod <- sqldf("select COD from Accidental_Death_sub
                              where AnyOpioid=0 
                    and (COD LIKE '%OPIOID%' OR COD LIKE '%OPIATE%'
                     OR  COD LIKE '%Fentanyl%' OR COD LIKE '%Oxycodone%'
                     OR  COD LIKE '%Oxymorphone%' OR COD LIKE '%Hydrocodone%'
                     OR  COD LIKE '%Hydromorphone%' OR COD LIKE '%Morphine_NotHeroin%'
                     OR  COD LIKE '%OpiateNOS%' OR COD LIKE '%AnyOpioid%' )
                     GROUP BY COD ")

Accidental_Death_cod$Flag <- 1

Accidental_Death_sub <- merge(Accidental_Death_sub,Accidental_Death_cod
                              ,by='COD',all.x='T')

Accidental_Death_sub$Flag <- as.numeric(Accidental_Death_sub$Flag)
Accidental_Death_sub$Flag[is.na(Accidental_Death_sub$Flag)] <- 0

Accidental_Death_sub$OpioidFlag<-Accidental_Death_sub$AnyOpioid+Accidental_Death_sub$Flag
Accidental_Death_sub$OpioidFlag[Accidental_Death_sub$OpioidFlag>=1]<- 1

#######################################################################################

Accidental_Death_sub$Age <- trim(Accidental_Death_sub$Age)

Accidental_Death_sub <- sqldf("select * from Accidental_Death_sub
                              where Sex!='' and Sex!='Unknown'")

# Accidental_Death_sub$AgeCat[Accidental_Death_sub$Age>=65]<- 'Seniors'
# Accidental_Death_sub$AgeCat[Accidental_Death_sub$Age>=25 & Accidental_Death_sub$Age<65]<- 'Adult'
# Accidental_Death_sub$AgeCat[Accidental_Death_sub$Age>=15 & Accidental_Death_sub$Age<25]<- 'Young'
# Accidental_Death_sub$AgeCat[Accidental_Death_sub$Age<15]<- 'Children'

# write.csv(Accidental_Death_sub,file="D:/Medical_Analytics/Complete_Data/Accidental_Death_sub.csv", row.names = F)

# conver gender factor to numeric

Accidental_Death_sub$Sex = as.factor(Accidental_Death_sub$Sex)
Accidental_Death_sub$Sex = as.numeric(Accidental_Death_sub$Sex)
Accidental_Death_sub$Sex = Accidental_Death_sub$Sex - 1 # 0-F, 1-M

#Accidental_Death_sub$Age = as.factor(Accidental_Death_sub$Age)
Accidental_Death_sub$Age = as.numeric(Accidental_Death_sub$Age)

Accidental_Death_log <- sqldf("select *
                              from Accidental_Death_sub ")

Accidental_Death_log$Sex <- as.factor(Accidental_Death_log$Sex)
#Accidental_Death_log$Age <- as.numeric(Accidental_Death_log$Age)
Accidental_Death_log$Age[is.na(Accidental_Death_log$Age)] <-median(Accidental_Death_log$Age,na.rm=T)

Accidental_Death_log <- subset(Accidental_Death_log,select = -c(1,19,20))
names(Accidental_Death_log)[names(Accidental_Death_log) == "OpioidFlag"] <- "AnyOpioid"
Accidental_Death_log$AnyOpioid <- as.factor(Accidental_Death_log$AnyOpioid)
# split training and testing
index = sample(1:nrow(Accidental_Death_log)
               , floor(0.8*nrow(Accidental_Death_log)), replace = FALSE, prob = NULL)
traindata = Accidental_Death_log[index,]
testdata = Accidental_Death_log[-index,]



summary(Accidental_Death$ResidenceState)
summary(Accidental_Death$Age)
summary(Accidental_Death$Sex)
summary(Accidental_Death$AnyOpioid)
summary(Accidental_Death_log$AnyOpioid)

# Model

logit <- glm(AnyOpioid~.,family=binomial(link='logit'),data=traindata)

# logit <- glm(OpioidFlag ~ Age+Sex+Heroin+OpiateNOS+AnyOpioid
#              ,data=Accidental_Death_log,family="binomial")


summary(logit)


car::vif(logit)

corr = cor(Accidental_Death_sub[,2:21])
corrplot::corrplot(corr, method="circle")


# Anova
anova(logit, test="Chisq")

# test data
p <- predict(logit,testdata,type='response')
p <- ifelse(p > 0.5,1,0)
misClasificError <- mean(p != testdata$AnyOpioid)
print(paste('Accuracy',1-misClasificError))

# # random forest
# 
# rf <- randomForest(AnyOpioid~.,data=traindata)
# predrf = predict(rf, newdata=testdata[-18])
# s <- rf$predicted
# s <- ifelse(s > 0.5,1,0)
# misClasificError <- mean(s != testdata$AnyOpioid)
# print(paste('Accuracy',1-misClasificError))

# curve
testdata$p=p
xtab <- table(testdata$AnyOpioid , testdata$p)

caret::confusionMatrix(xtab)

library(pROC)
g <- roc(AnyOpioid ~ p, data = testdata)
plot(g)
roc(AnyOpioid ~ p, data = testdata, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
#roc(AnyOpioid ~ p, data = testdata, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

# train data
q <- predict(logit,traindata,type='response')
q <- ifelse(q > 0.5,1,0)
misClasificError <- mean(q != traindata$AnyOpioid)
print(paste('Accuracy',1-misClasificError))

q <- as.data.frame(q)

# curve
traindata$q=q
library(pROC)
g <- roc(AnyOpioid ~ q, data = traindata)
plot(g)

# improving accuracy

traindata$AnyOpioid <- as.factor(traindata$AnyOpioid)

library(caret)

lasso_reg = train(AnyOpioid~.,data=traindata,
                  method='glmnet',trControl = trainControl(method="none"),
                  tuneGrid=expand.grid(alpha=1,lambda=0.01))

pred_lasso = predict(lasso_reg,newdata = subset(testdata,select = c(1:17)))


logit2 <- glm(AnyOpioid~Age+Fentanyl+Cocaine+Oxycodone+Oxymorphone+Hydrocodone+Methadone+Benzodiazepine+Morphine_NotHeroin+Hydromorphone+OpiateNOS,family=binomial(link='logit'),data=traindata)
summary(logit2)

anova(logit,logit2,test = "Chisq")
exp(OR = coef(logit))
data.frame(oR=exp(coef(logit)))