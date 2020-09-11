library(tidyverse)
library(stringr)
library(dplyr)
library(gbm)
library(magrittr)
library(ggplot2)
library(maps)
library(data.table)
library(lme4)
library(caret)
library(cvAUC)

setwd("D:/R Projects/Genpro/MPA/Linear Mix Model")
limit.rows <- 25000
df <- data.frame(fread("./data/prescriber-info.csv",nrows=limit.rows))
#head(df,1)

head(df$Credentials,10)

for (i in 1:length(df$Credentials)) {
  df$Credentials[i] = sub(' .*', '', df$Credentials[i])
  df$Credentials[i] = gsub('([A-z]+) .*', '\\1', df$Credentials[i])
  df$Credentials[i] = gsub(' [A-z ]*', '' ,df$Credentials[i])
  df$Credentials[i] = gsub("[[:punct:]]","",df$Credentials[i])
  
}

write.csv2(df,"cleaned Data.csv")
unique(df$Credentials)

opioids <- read.csv("./data/opioids.csv")
opioids <- as.character(opioids[,1]) # First column contains the names of the opiates
opioids <- gsub("\ |-",".",opioids) # replace hyphens and spaces with periods to match the dataset
df <- df[, !names(df) %in% opioids]

char_cols <- c("NPI",names(df)[vapply(df,is.character,TRUE)])
df[,char_cols] <- lapply(df[,char_cols],as.factor)

str(df[,1:6])

df %>%
  group_by(State) %>%
  dplyr::summarise(state.counts = n()) %>%
  arrange(state.counts)

rare.abbrev <- df %>%
  group_by(State) %>%
  dplyr::summarise(state.counts = n()) %>%
  arrange(state.counts) %>%
  filter(state.counts < 10) %>%
  select(State)

levels(df$State) <- c(levels(df$State),"other")
df$State[df$State %in% rare.abbrev$State] <- "other"
df$State <- droplevels(df$State)

df <- cbind(df[names(df)!="State"],dummy(df$State))

df %>%
  group_by(Credentials) %>%
  dplyr::summarise(credential.counts = n()) %>%
  arrange(credential.counts) %>% 
  data.frame() %>% 
  head(n=25)

df %<>%
  select(-Credentials)

df %>%
  group_by(Specialty) %>%
  dplyr::summarise(specialty.counts = n()) %>%
  arrange(desc(specialty.counts)) %>% 
  data.frame() %>% 
  glimpse()

common.specialties <- df %>%
  group_by(Specialty) %>%
  dplyr::summarise(specialty.counts = n()) %>%
  arrange(desc(specialty.counts)) %>% 
  filter(specialty.counts > 50) %>%
  select(Specialty)
common.specialties <- levels(droplevels(common.specialties$Specialty))


# Default to "other", then fill in. I'll make special levels for surgeons and collapse any category containing the word pain
new.specialties <- factor(x=rep("other",nrow(df)),levels=c(common.specialties,"Surgeon","other","Pain.Management"))
new.specialties[df$Specialty %in% common.specialties] <- df$Specialty[df$Specialty %in% common.specialties]
new.specialties[grepl("surg",df$Specialty,ignore.case=TRUE)] <- "Surgeon"
new.specialties[grepl("pain",df$Specialty,ignore.case=TRUE)] <- "Pain.Management"
new.specialties <- droplevels(new.specialties)
df$Specialty <- new.specialties


df %>%
  group_by(Specialty) %>%
  dplyr::summarise(specialty.counts = n()) %>%
  arrange(desc(specialty.counts)) %>% 
  data.frame() %>% 
  head(n=25)

df <- df[!is.na(df$Specialty),]
df <- cbind(df[,names(df)!="Specialty"],dummy(df$Specialty))


df <- df[vapply(df,function(x) if (is.numeric(x)){sum(x)>0}else{TRUE},FUN.VALUE=TRUE)]

train_faction <- 0.8
train_ind <- sample(nrow(df),round(train_faction*nrow(df)))

df %<>% select(-NPI)
df$Opioid.Prescriber <- as.factor(ifelse(df$Opioid.Prescriber==1,"yes","no"))
train_set <- df[train_ind,]
test_set <- df[-train_ind,]

set.seed(42)
#Logestic Regression

model_glm <- glm( Opioid.Prescriber~.,family=binomial(link='logit'),data=train_set)

coef(model_glm)

head(predict(model_glm))
head(predict(model_glm, type = "link"))
head(predict(model_glm, type = "response"))
model_glm_pred = ifelse(predict(model_glm, type = "link") > 0, "Yes", "No")

calc_class_err = function(actual, predicted) { mean(actual != predicted)}

#classification error rate.
calc_class_err(actual = test_set$Opioid.Prescriber, predicted = model_glm_pred)

train_tab = table(predicted = model_glm_pred, actual = train_set$Opioid.Prescriber)
library(caret)
train_con_mat = confusionMatrix(train_tab, positive = "Yes")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])













Lmodel = train(train_set %>% select(-Opioid.Prescriber),train_set$Opioid.Prescriber, 
                method='vglmAdjCat', 
                metric = "ROC",
                trControl=objControl)
predictions <- predict(Lmodel,test_set%>% select(-Opioid.Prescriber),type="prob")
confusionMatrix(predictions,test_set$Opioid.Prescriber,positive="yes")
library(pROC)

test_prob = predict(model_glm, newdata = test_set, type = "response")
test_roc = roc(test_set$Opioid.Prescriber ~ test_prob, plot = TRUE, print.auc = TRUE)

train_prob = predict(model_glm, newdata = train_set, type = "response")
train_roc = roc(train_set$Opioid.Prescriber ~ train_prob, plot = TRUE, print.auc = TRUE)









#Gredient Boosting
objControl <- trainControl(method='cv', number=5, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
model <- train(train_set %>% select(-Opioid.Prescriber),train_set$Opioid.Prescriber, 
               method='gbm', 
               metric = "ROC",
               trControl=objControl)

predictions <- predict(model,test_set%>% select(-Opioid.Prescriber),type="raw")
confusionMatrix(predictions,test_set$Opioid.Prescriber,positive="yes")


train_prob = predict(model, train_set, type = "prob")
test_prob = predict(model, test_set, type = "prob")

gbm.ROC <- roc(predictor=train_prob$yes,
               response=train_set$Opioid.Prescriber,
               levels=rev(levels(train_set$Opioid.Prescriber)))

result.roc <- roc(test_set$Opioid.Prescriber, test_prob$versicolor) # Draw ROC curve.

test_roc = roc(test_set$Opioid.Prescriber ~ test_prob, plot = TRUE, print.auc = TRUE)






importance <- as.data.frame(varImp(model)[1])
importance <- cbind(row.names(importance), Importance=importance)
row.names(importance)<-NULL
names(importance) <- c("Feature","Importance")
importance %>% arrange(desc(Importance)) %>%
  mutate(Feature=factor(Feature,levels=as.character(Feature))) %>%
  slice(1:15) %>%
  ggplot() + geom_bar(aes(x=Feature,y=(Importance)),stat="identity",fill="blue") + 
  theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1),axis.ticks.x = element_blank()) +ylab("Importance") +ggtitle("Feature Importance for Detecting Opioid Prescription")


