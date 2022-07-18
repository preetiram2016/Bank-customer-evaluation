setwd("C:/Users/Admin/Desktop/Preeti IIT Kanpur/R language/Projects")

bank_train = read.csv("bank-full_train.csv")
bank_test  = read.csv("bank-full_test.csv")

View(bank_train)
View(bank_test)

bank_test$y = NA

bank_train$data = "train"
bank_test$data = "test"

bank_all = rbind(bank_train,bank_test)
View(bank_all)

library(dplyr)

glimpse(bank_all)

## job marital education  default housing loan contact month poutcome
table(bank_all$job)

table(bank_all$marital)

table(bank_all$education)

table(bank_all$default)

table(bank_all$housing)

table(bank_all$loan)

table(bank_all$contact)

table(bank_all$month)

table(bank_all$poutcome)

## create dummies for above char columns

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(bank_all)

names(bank_all)[sapply(bank_all,function(x)is.character(x))]

cat_col = c("job","marital","education","default","housing",
             "loan","contact","month","poutcome")

for (col in cat_col)
{
  bank_all = CreateDummies(bank_all,col,50)
}

View(bank_all)

## Findout missing values

lapply(bank_all, function(x) sum(is.na(x)))


## filter train and test data sets
bank_train = bank_all %>% 
           filter(data == "train") %>%
           select(-data)

bank_test = bank_all %>% 
  filter(data == "test") %>%
  select(-data,-y)

## Converting outcome column "y" into numeric column
bank_train = bank_train %>% 
  mutate(y = ifelse(y=="yes",1,0))

View(bank_train)

set.seed(2)
s = sample(1:nrow(bank_train),0.7*nrow(bank_train))
bank_train1 = bank_train[s,]
bank_train2 = bank_train[-s,]


table(bank_train1$y)

library(dplyr)  
library(car)

## Checking VIF to remove features

vif_check = lm(y~.-ID,data = bank_train1)

sort(vif(vif_check),decreasing = T)[1:3]

vif_check = lm(y~.-ID
                  -month_may
                  -job_blue_collar,data = bank_train1)

train1_fit = glm(y~.-ID
                 -month_may
                 -job_blue_collar
                 -job_technician
                 -previous
                 -age
                 -job_unemployed 
                 -job_entrepreneur 
                 -job_self_employed
                 -job_services
                 -job_admin.
                 -job_management
                 -marital_single
                 -education_primary
                 -education_secondary
                 -default_no 
                 -contact_cellular,data = bank_train1)

summary(train1_fit)

train1_fit = step(train1_fit)

formula(train1_fit)

train1_fit = glm(y ~ balance + day + duration + campaign + pdays  + 
                     job_student + job_housemaid + 
                     job_retired  + marital_married  + education_tertiary + 
                     housing_yes + loan_no + contact_unknown  + month_mar + month_sep + 
                     month_oct + month_jan + month_feb + month_apr + month_nov + 
                     month_jun + month_aug + month_jul + poutcome_other + 
                     poutcome_failure + poutcome_unknown,data = bank_train1,
                     family = "binomial")

## performance of score model on validation data

library(pROC)

train2_val = predict(train1_fit,newdata = bank_train2,type = "response")

summary(train2_val)

auc(roc(bank_train2$y,train2_val))

### Build model on train data

vif_check = lm(y~.-ID,data = bank_train)

sort(vif(vif_check),decreasing = T)

vif_check = lm(y~.-ID
               -month_may
               -job_blue_collar,data = bank_train)

fit_train = glm(y~.-ID
                   -month_may
                   -job_blue_collar,data = bank_train)
summary(fit_train)

formula(fit_train)

fit_train = glm(y~ balance + day + duration + campaign + 
                   job_student + job_housemaid  + 
                   job_retired + job_admin. + 
                  job_technician + job_management  + 
                  marital_married  + education_tertiary + 
                  education_secondary  + housing_yes + loan_no + 
                  contact_unknown  + month_mar + month_sep + 
                  month_oct + month_jan + month_feb + month_apr + month_nov + 
                  month_jun + month_aug + month_jul + poutcome_other + 
                  poutcome_failure + poutcome_unknown,data = bank_train,family = "binomial")


## performance of probability score model on test data

library(pROC)

prob_test_score = predict(fit_train,newdata = bank_test,type = "response")

summary(prob_test_score)

## however if we need to submit hard classes, we'll need to determine cutoff score

train_pred = predict(fit_train,newdata = bank_train,type = "response")

real = bank_train$y

cutoffs = seq(0.001,0.999,0.001)
cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs)
{
  predicted = as.numeric(train_pred>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=KS))+geom_line()

library(tidyr)
cutoff_long = cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x = cutoff,y = Value,color = Measure))+geom_line()

## find out cutoff based on maximum KS score

cutoff_val = cutoff_data$cutoff[which.max(cutoff_data$KS)]

# now that we have our cutoff we can convert score to hard classes

test_pred = as.numeric(prob_test_score>cutoff_val)

## save project into folder
write.csv(test_pred,"U_Preeti_P5_Part2",row.names = F)

getwd()


##### Project 5 part(1) Q

##
round(mean(bank_train$age),2)

##
var(bank_train$balance)

##
boxplot(bank_train$balance)

quantile(bank_train$balance,na.rm = T)
IQR(bank_train$balance,na.rm = T)

Q1 = 72-1.5*1342

Q3 = 1414+1.5*1342

below_q1 = bank_train$balance[which(bank_train$balance<Q1)]

above_q3 = bank_train$balance[which(bank_train$balance>Q3)]
sum(length(below_q1)+length(above_q3))

###