library(dplyr)
library(reshape)
library(lme4)
library(ggplot2)
library(corrplot)
library(car)
library(moments)
library(psych)
library(merTools)
library(MuMIn)




oddata<-read.csv("./data/Drug Overdose Mortality by State 2017.csv")
ds7<-read.csv("./data//ds7.csv")
ds71<-ds7%>%filter(State != "United States",TimeFrame == 2017,Data > 1)
ds71<-dplyr::select(ds71,1,2,4)

ds72<-reshape(ds71,direction = "wide",idvar = "State",timevar = "Education")
colnames(ds72)<-c("State","Not_a_high_school_graduate","High_school_diploma_or_GED","Associates_degree","Bachelors_degree","Graduate_degree")
ds2<-read.csv("./data//ds2.csv")
ds2<-dplyr::select(ds2,2,5)
ds73<-merge(ds2,ds72)
oddata1<-dplyr::select(oddata,1,2,4)
oddata1$DEATHS<-as.numeric(gsub(",","",oddata1$DEATHS))
colnames(oddata1)<-c("Year","State","Death")
oddata2<-oddata1%>%filter(Year %in% c(2014,2015,2016,2017,2018))
oddata3<-merge(oddata2,ds73,by.x = "State",by.y = "Abbrev")
oddata4<-dplyr::select(oddata3,4,1,2,3,5:9)
colnames(oddata4)<-c("State","Abbrev","Year","Death","Not_a_high_school_graduate","High_school_diploma_or_GED","Associates_degree","Bachelors_degree","Graduate_degree")

eco<-read.csv("./data//ds4.csv")
eco1<-subset(eco,select = -c(CountyId,County,IncomeErr,IncomePerCapErr,Drive,Carpool,Transit,Walk,OtherTransp,MeanCommute,ChildPoverty))
eco1[2:25]<-lapply(eco1[2:25],as.numeric)
eco2<-aggregate(eco1,by= list(eco1$State),FUN = median)
eco2 <- subset(eco2,select = -State)
names(eco2)[1] = "State"
ds2$State_name<-as.numeric(ds2$State)
eco3<-merge(ds2,eco2,by.x = "State",by.y ="State" )
eco4<-dplyr::select(eco3,1:2,4:28)
data<-merge(oddata4,eco4)
data1<-dplyr::select(data,1,3:34)
data1$Death

data1$Deathratio<-data1$Death/data1$TotalPop

#data1
outt = subset(data1,Deathratio <=0.08 )





X = data1 %>% dplyr::select(Not_a_high_school_graduate,High_school_diploma_or_GED,Associates_degree,Bachelors_degree,Graduate_degree,Men,Women,Hispanic,White,Black,Native,Asian,Pacific,VotingAgeCitizen,IncomePerCap,Poverty,Professional,Service,Office,Construction,Production,WorkAtHome,Employed,PrivateWork,PublicWork,SelfEmployed,FamilyWork,Unemployment)%>% data.matrix()
y = data1$Deathratio



hist(data1$Deathratio)
boxplot(Deathratio~State,data = data1)

ggplot(data1,aes(x=State,y=Deathratio)) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(size=10, angle=90))



model_lm = lm(Deathratio~Not_a_high_school_graduate+High_school_diploma_or_GED+Associates_degree+Bachelors_degree+Graduate_degree+Men+Women+Hispanic+White+Black+Native+Asian+Pacific+VotingAgeCitizen+IncomePerCap+Poverty+Professional+Service+Office+Construction+Production+WorkAtHome+Employed+PrivateWork+PublicWork+SelfEmployed+FamilyWork+Unemployment,data = data1)
summary(model_lm)
as.data.frame(vif(model_lm))



model_mix1 = lmerTest::lmer(Deathratio~Not_a_high_school_graduate+High_school_diploma_or_GED+Associates_degree+Bachelors_degree+Graduate_degree+Men+Women+Hispanic+White+Black+Native+Asian+Pacific+VotingAgeCitizen+IncomePerCap+Poverty+Professional+Service+Office+Construction+Production+WorkAtHome+Employed+PrivateWork+PublicWork+SelfEmployed+FamilyWork+Unemployment+(1|State),data = data1,control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model_mix<-lmer(Deathratio~Not_a_high_school_graduate+High_school_diploma_or_GED+Associates_degree+Bachelors_degree+Graduate_degree+Men+Hispanic+White+Black+Native+Asian+Pacific+IncomePerCap+Poverty+Professional+Service+Office+Construction+Production+WorkAtHome+Employed+PrivateWork+PublicWork+SelfEmployed+FamilyWork+Unemployment+(1|State),data = data1,control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(model_mix)
summary(model_mix1)
anova(model_mix)
coefs <- data.frame(coef(summary(model_mix)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


model_mix11 = lmerTest::lmer(Deathratio~Not_a_high_school_graduate+High_school_diploma_or_GED+Associates_degree+Bachelors_degree+Graduate_degree+Hispanic+White+Black+Native+Asian+Pacific+IncomePerCap+Poverty+Professional+Service+Office+Construction+Production+WorkAtHome+Employed+PrivateWork+PublicWork+SelfEmployed+FamilyWork+Unemployment+(1|State),data = data1,control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(model_mix11)
as.data.frame(vif(model_mix11))


coefs <- data.frame(coef(summary(model_mix)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

r.squaredGLMM(model_mix)


confint(model_mix)
ranef(model_mix)$State %>% head(5)
coef(model_mix)$State %>% head(5)


predictInterval(model_mix)   # for various model predictions, possibly with new data
REsim(model_mix)             # mean, median and sd of the random effect estimates
plotREsim(REsim(model_mix))  # plot the interval estimates

predict_no_re = predict(model_mix, re.form=NA)
predict_lm = predict(model_mix)
predict_with_re = predict(model_mix)
plot(predict_no_re)


#ridge
model_ridg <- glmnet(X, y, alpha = 0)
predict(model_ridg, exact = TRUE, type = 'coefficients')

#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(X, y, alpha = 0)
cv.out

bestlam <- cv.out$lambda.min
bestlam


#ridge
model_ridg <- glmnet(X, y, alpha = 0, lambda = bestlam)

#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(X, y, alpha = 0)
cv.out


summary(model_ridg)
#make predictions
ridge_pred <- predict(model_ridg, lambda = bestlam, newx = X)
s_pred <- predict(model_lm, newdata = data1)
#check MSE
mean((ridge_pred-y)^2)

mean((ridge_pred-y)^2)

cv.glmnet(X, y, alpha = 0, lambda = bestlam)

library(ridge)
model_rid <-  linearRidge(Deathratio~Not_a_high_school_graduate+High_school_diploma_or_GED+Associates_degree+Bachelors_degree+Graduate_degree+Men+Women+Hispanic+White+Black+Native+Asian+Pacific+VotingAgeCitizen+IncomePerCap+Poverty+Professional+Service+Office+Construction+Production+WorkAtHome+Employed+PrivateWork+PublicWork+SelfEmployed+FamilyWork+Unemployment,data = data1)
summary(model_rid)
vif(model_rid)


model_rid1 <-  linearRidge(Deathratio~Not_a_high_school_graduate+High_school_diploma_or_GED+Associates_degree+Bachelors_degree+Graduate_degree+Men+Hispanic+White+Black+Native+Asian+Pacific+IncomePerCap+Poverty+Professional+Service+Office+Construction+Production+WorkAtHome+Employed+PrivateWork+PublicWork+SelfEmployed+FamilyWork+Unemployment,data = data1)
summary(model_rid1)
vif(model_rid1)



model_Mrid <- glmnet(X, y, alpha = 0, lambda = bestlam,method = c("glmnet", "lme4"))
summary(model_Mrid)




AIC(model_mix)
AIC(model_ridg)
AIC(model_lm)


