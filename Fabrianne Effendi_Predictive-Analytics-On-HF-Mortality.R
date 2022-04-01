# =========================  Download Libraries  ===============================
library(caTools)            # Train-Test Split
library(randomForest)       # Random Forest
library(corrplot)
library(nnet)
library(ggplot2)
library(rpart)
library(rpart.plot)	
library(ggcorrplot)

# ================================= Q1a =========================================
#Import csv 
setwd("/Users/FabrianneEffendi/Documents/01 UNI/BC2407 Analytics II/AY21S2 BC2407 CBA")
data1 <- read.csv("data01.csv")

#Convert categorical data to categories
data1$group<-as.factor(data1$group)
data1$outcome<-as.factor(data1$outcome)
data1$gendera<-as.factor(data1$gendera)
data1$hypertensive<-as.factor(data1$hypertensive)
data1$atrialfibrillation<-as.factor(data1$atrialfibrillation)
data1$CHD.with.no.MI<-as.factor(data1$CHD.with.no.MI)
data1$diabetes<-as.factor(data1$diabetes)
data1$deficiencyanemias<-as.factor(data1$deficiencyanemias)
data1$depression<-as.factor(data1$depression)
data1$Hyperlipemia<-as.factor(data1$Hyperlipemia)
data1$Renal.failure<-as.factor(data1$Renal.failure)
data1$COPD<-as.factor(data1$COPD)

str(data1)

# ================================= Q1c =========================================
sum(is.na(data1)) #1929
summary(data1)

#Show table of missing value counts
NA.Count <- sapply(data1, function(x) sum(is.na(x)))
na_count <- data.frame(NA.Count)
na_count$Data.Type <- sapply(data1, function(x) class(x))
na_count <- na_count[na_count$NA.Count != 0, ]
na_count <- na_count[,c(Data.Type, NA.Count)]
View(na_count)



# ================================= Q1d =========================================
#Explore data1. Produce charts, tables or/and statistics to explain 3 interesting findings.
#Mortality against BMI
ggplot(data1[!(is.na(data1$outcome)),], aes(x = outcome, y = BMI, fill = outcome)) +
  geom_boxplot() + 
  theme_minimal() +
  labs(title = "Incidence of In-Hospital Mortality of ICU Patients with Heart Failure by BMI")+
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("0"="Survived","1"="Died"))

#Mortality against LVEF
ggplot(data1[!(is.na(data1$outcome)),], aes(x = outcome, y = EF, fill = outcome)) +
  geom_boxplot() + 
  theme_minimal() +
  labs(title = "Incidence of In-Hospital Mortality of ICU Patients with Heart Failure by LVEF")+
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("0"="Survived","1"="Died"))

#Mortality against age
ggplot(data1[!(is.na(data1$outcome)),], aes(x = outcome, y = age, fill = outcome)) +
  geom_boxplot() + 
  theme_minimal() +
  labs(title = "Incidence of In-Hospital Mortality of ICU Patients with Heart Failure by age")+
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("0"="Survived","1"="Died"))


#Mortality against COPD
COPD.table <- table(mortality = data1$outcome, COPD = data1$COPD, deparse.level = 2)
COPD.table



atrialfibrillation.table <- table(mortality = data1$outcome, atrialfibrillation = data1$atrialfibrillation, deparse.level = 2)
atrialfibrillation.table
 
#Mortality against hypertension
hypertension.table <- table(mortality = data1$outcome, hypertension = data1$hypertensive, deparse.level = 2)
hypertension.table

cfmatrix <- hypertension.table
DeadAndHypertension <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
AliveAnd <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)


ggplot(data1[!(is.na(data1$outcome)),], aes(x = outcome, y = hypertensive, fill = outcome)) +
  geom_boxplot() + 
  theme_minimal() +
  labs(title = "Incidence of In-Hospital Mortality of ICU Patients with Heart Failure by BMI")+
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("0"="Survived","1"="Died"))

ggplot(data1[!(is.na(data1$outcome)),], aes(x = outcome, y = age, fill = outcome)) +
  geom_boxplot() + 
  theme_minimal() +
  labs(title = "Incidence of In-Hospital Mortality of ICU Patients with Heart Failure by BMI")+
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("0"="Survived","1"="Died"))


# ================================= Q2a =========================================
#Create a copy of data1 and named it data2.
data2 <- data1

#Replace all missing values by median if continuous variable or the mode if categorical variable.
#Replace missing values of continuous variables with median
data2$BMI[is.na(data2$BMI)] <- median(data2$BMI, na.rm = TRUE)
data2$heart.rate[is.na(data2$heart.rate)] <- median(data2$heart.rate, na.rm = TRUE)
data2$Systolic.blood.pressure[is.na(data2$Systolic.blood.pressure)] <- median(data2$Systolic.blood.pressure, na.rm = TRUE)
data2$Diastolic.blood.pressure[is.na(data2$Diastolic.blood.pressure)] <- median(data2$Diastolic.blood.pressure, na.rm = TRUE)
data2$Respiratory.rate[is.na(data2$Respiratory.rate)] <- median(data2$Respiratory.rate, na.rm = TRUE)
data2$temperature[is.na(data2$temperature)] <- median(data2$temperature, na.rm = TRUE)
data2$SP.O2[is.na(data2$SP.O2)] <- median(data2$SP.O2, na.rm = TRUE)
data2$Urine.output[is.na(data2$Urine.output)] <- median(data2$Urine.output, na.rm = TRUE)
data2$Neutrophils[is.na(data2$Neutrophils)] <- median(data2$Neutrophils, na.rm = TRUE)
data2$Basophils[is.na(data2$Basophils)] <- median(data2$Basophils, na.rm = TRUE)
data2$Lymphocyte[is.na(data2$Lymphocyte)] <- median(data2$Lymphocyte, na.rm = TRUE)
data2$PT[is.na(data2$PT)] <- median(data2$PT, na.rm = TRUE)
data2$INR[is.na(data2$INR)] <- median(data2$INR, na.rm = TRUE)
data2$Creatine.kinase[is.na(data2$Creatine.kinase)] <- median(data2$Creatine.kinase, na.rm = TRUE)
data2$glucose[is.na(data2$glucose)] <- median(data2$glucose, na.rm = TRUE)
data2$Blood.calcium[is.na(data2$Blood.calcium)] <- median(data2$Blood.calcium, na.rm = TRUE)
data2$PH[is.na(data2$PH)] <- median(data2$PH, na.rm = TRUE)
data2$Lactic.acid[is.na(data2$Lactic.acid)] <- median(data2$Lactic.acid, na.rm = TRUE)
data2$PCO2[is.na(data2$PCO2)] <- median(data2$PCO2, na.rm = TRUE)

#Mode function
getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

#Replace missing values of categorical variable with mode
data2$outcome[is.na(data2$outcome)] <- getMode(data2$outcome)

#Check that data2 has no missing values
sum(is.na(data2))
sum(is.null(data2))

str(data2)

# ============================= Q2b =====================================
trainset <- subset(data2, group==1)
trainset = subset(trainset,select=-c(group,ID))
#died: outcome=1; alive: outcome=0
table(trainset$outcome)


# ============================= Q2c =====================================
testset <- subset(data2, group==2)
testset = subset(testset,select=-c(group,ID))
#died: outcome=1; alive: outcome=0
table(testset$outcome)

# ============================= Q5 & 6 =====================================
# ====================== Logistic Regression ===========================
set.seed(22)
m.logreg.1 <- glm(outcome ~ . , family = binomial, data = trainset)
step(m.logreg.1)

set.seed(22)
m.logreg.2 <- glm(outcome ~ age + gendera + hypertensive + atrialfibrillation + 
                    diabetes + deficiencyanemias + Renal.failure + COPD + heart.rate + 
                    Systolic.blood.pressure + Diastolic.blood.pressure + SP.O2 + 
                    MCV + RDW + Platelets + Lymphocyte + Creatinine + Urea.nitrogen + 
                    Blood.sodium + Blood.calcium + Chloride + Anion.gap + PH + 
                    Lactic.acid + PCO2, family = binomial, data = trainset)

# Train set prediction
logreg.train.prob <- predict(m.logreg.2, type = 'response')

# Set the threshold for predicting Y = 1 based on probability.
threshold <- 0.5

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat.train <- ifelse(logreg.train.prob > threshold, 1, 0)

# confusion matrix for train set
logreg.traintable <- table(Trainset.Actual = trainset$outcome, y.hat.train, deparse.level = 2)
logreg.traintable

cfmatrix <- logreg.traintable
logreg.train.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
logreg.train.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
logreg.train.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)
logreg.train.error

#Test
logreg.test.prob <- predict(m.logreg.2, newdata = testset, type = 'response')

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat.test <- ifelse(logreg.test.prob > threshold, 1, 0)

# confusion matrix for test set
logreg.testtable <- table(Testset.Actual = testset$outcome, y.hat.test, deparse.level = 2)
logreg.testtable

cfmatrix <- logreg.testtable
logreg.test.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
logreg.test.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
logreg.test.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)

# ========================== Random Forest =============================
set.seed(22)
m.RF.1 <- randomForest(outcome ~ . , data = trainset, na.action = na.omit, importance = T)
m.RF.1
var.impt <- importance(m.RF.1)

# Train set prediction
prob.train <- predict(m.RF.1, type = 'class')

# confusion matrix for train set
RF.traintable <- table(Trainset.Actual = trainset$outcome, prob.train, deparse.level = 2)
RF.traintable

cfmatrix <- RF.traintable
RF.train.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
RF.train.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
RF.train.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)

prob.test <- predict(m.RF.1, newdata = testset, type = 'class')

# confusion matrix for test set
RF.testtable <- table(Testset.Actual = testset$outcome, prob.test, deparse.level = 2)
RF.testtable

cfmatrix <- RF.testtable
RF.test.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
RF.test.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
RF.test.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)
  
# ============================ GWTG ====================================
#Create GWTG
gwtg.data<-subset(data2,select=c(group,outcome,Systolic.blood.pressure,Urea.nitrogen,Blood.sodium,age,heart.rate,COPD))

for (x in 1:1177){
  SBP=gwtg.data$Systolic.blood.pressure[x]
  BUN=gwtg.data$Urea.nitrogen[x]
  sodium=gwtg.data$Blood.sodium[x]
  age=gwtg.data$age[x]
  heart.rate=gwtg.data$heart.rate[x]
  COPD=gwtg.data$COPD[x]
  
  SBP<-round(SBP)
  if ((50<=SBP) && (SBP<=59)){
    points=28
  }else if ((60<=SBP) && (SBP<=69)){
    points=26
  }else if ((70<=SBP) && (SBP<=79)){
    points=24
  }else if ((80<=SBP) && (SBP<=89)){
    points=23
  }else if ((90<=SBP) && (SBP<=99)){
    points=21
  }else if ((100<=SBP) && (SBP<=109)){
    points=19
  }else if ((110<=SBP) && (SBP<=119)){
    points=17
  }else if ((120<=SBP) && (SBP<=129)){
    points=15
  }else if ((130<=SBP) && (SBP<=139)){
    points=13
  }else if ((140<=SBP) && (SBP<=149)){
    points=11
  }else if ((150<=SBP) && (SBP<=159)){
    points=9
  }else if ((160<=SBP) && (SBP<=169)){
    points=8
  }else if ((170<=SBP) && (SBP<=179)){
    points=6
  }else if ((180<=SBP) && (SBP<=189)){
    points=4
  }else if ((190<=SBP) && (SBP<=199)){
    points=2
  }else{
    points=0
  }
  gwtg.data$points.SBP[x]=points
  
  BUN<-round(BUN)
  if ((BUN<=9)){
    points=0
  }else if ((10<=BUN) && (BUN<=19)){
    points=2
  }else if ((20<=BUN) && (BUN<=29)){
    points=4
  }else if ((30<=BUN) && (BUN<=39)){
    points=6
  }else if ((40<=BUN) && (BUN<=49)){
    points=8
  }else if ((50<=BUN) && (BUN<=59)){
    points=9
  }else if ((60<=BUN) && (BUN<=69)){
    points=11
  }else if ((70<=BUN) && (BUN<=79)){
    points=13
  }else if ((80<=BUN) && (BUN<=89)){
    points=15
  }else if ((90<=BUN) && (BUN<=99)){
    points=17
  }else if ((100<=BUN) && (BUN<=109)){
    points=19
  }else if ((110<=BUN) && (BUN<=119)){
    points=21
  }else if ((120<=BUN) && (BUN<=129)){
    points=23
  }else if ((130<=BUN) && (BUN<=139)){
    points=25
  }else if ((140<=BUN) && (BUN<=149)){
    points=27
  }else{
    points=28
  }
  gwtg.data$points.BUN[x]=points
  
  sodium<-round(sodium)
  if (sodium<=130){
    points=4
  }else if ((sodium>=131) && (sodium<=133)){
    points=3
  }else if ((sodium>=134) && (sodium<=136)){
    points=2
  }else if ((sodium>=137) && (sodium<=138)){
    points=1
  }else{
    points=0
  }
  gwtg.data$points.sodium[x]=points
  
  age<-round(age)
  if (age<=19){
    points=0
  }else if ((age>=20) && (29>=age)){
    points=3
  }else if ((age>=30) && (39>=age)){
    points=6
  }else if ((age>=40) && (49>=age)){
    points=8
  }else if ((age>=50) && (59>=age)){
    points=0
  }else if ((age>=60) && (69>=age)){
    points=14
  }else if ((age>=70) && (79>=age)){
    points=17
  }else if ((age>=80) && (89>=age)){
    points=19
  }else if ((age>=90) && (99>=age)){
    points=22
  }else if ((age>=100) && (109>=age)){
    points=25
  }else{
    points=28
  }
  gwtg.data$points.age[x]=points
  
  heart.rate<-round(heart.rate)
  if (heart.rate<=79){
    points=0
  }else if ((heart.rate>=80) && (84>=heart.rate)){
    points=1
  }else if ((heart.rate>=85) && (89>=heart.rate)){
    points=3
  }else if ((heart.rate>=90) && (94>=heart.rate)){
    points=4
  }else if ((heart.rate>=95) && (99>=heart.rate)){
    points=5
  }else if ((heart.rate>=100) && (104>=heart.rate)){
    points=6
  }else{
    points=8
  }
  gwtg.data$points.heart.rate[x]=points
  
  if (COPD==1){
    points=2
  }else{
    points=0
  }
  gwtg.data$points.COPD[x]=points
  
  gwtg.data$total.score[x]=3+gwtg.data$points.SBP[x]+gwtg.data$points.BUN[x]+gwtg.data$points.sodium[x]+gwtg.data$points.age[x]+gwtg.data$points.heart.rate[x]+gwtg.data$points.COPD[x]
  
  if (gwtg.data$total.score[x] >= 79){
    gwtg.data$pred.outcome[x] = 1
  }else{
    gwtg.data$pred.outcome[x] = 0
  }
  
}
gwtg.data$pred.outcome <- as.factor(factor(gwtg.data$pred.outcome, levels = c("0", "1")))
gwtg.data$pred.outcome = as.factor(gwtg.data$pred.outcome)



#Split train and test sets
gwtg.trainset <- subset(gwtg.data, group==1)
gwtg.trainset = subset(gwtg.trainset,select=-c(group))

gwtg.testset <- subset(gwtg.data, group==2)
gwtg.testset = subset(gwtg.testset,select=-c(group))


# confusion matrix for train set
gwtg.traintable <- table(Trainset.Actual = gwtg.trainset$outcome, gwtg.trainset$pred.outcome, deparse.level = 2)

View(gwtg.data)
cfmatrix <- gwtg.traintable
gwtg.train.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
gwtg.train.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
gwtg.train.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)
cfmatrix
gwtg.train.FNR
gwtg.train.FPR
gwtg.train.error

# confusion matrix for test set
gwtg.testtable <- table(Testset.Actual = gwtg.testset$outcome, gwtg.testset$pred.outcome, deparse.level = 2)
gwtg.testtable

cfmatrix <- gwtg.testtable
gwtg.test.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
gwtg.test.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
gwtg.test.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)

#gwtg.test.FNR <- sum(gwtg.testset$outcome==1 && gwtg.testset$pred.outcome==0)/sum(gwtg.testset$pred.outcome==1)
#gwtg.test.FPR <- sum(gwtg.testset$outcome==0 && gwtg.testset$pred.outcome==1)/sum(gwtg.testset$pred.outcome==0)
#gwtg.test.error <- gwtg.test.FNR + gwtg.test.FPR
summary(gwtg.trainset$outcome)
summary(gwtg.trainset$pred.outcome)

# ============================ Nomogram ================================
#Create Nomogram
nomogram.data <- subset(data2,select=c(group,outcome,Anion.gap,Lactic.acid,Blood.calcium,
                                       Urea.nitrogen, Renal.failure, Diastolic.blood.pressure))

nomogram.data$points=0
nomogram.data$pred.outcome=0
sum(is.na(nomogram.data))
for (x in 1:1177) {
  aniongap<-nomogram.data$Anion.gap[x]
  lactate<-nomogram.data$Lactic.acid[x]
  calcium<-nomogram.data$Blood.calcium[x]
  bun<-nomogram.data$Urea.nitrogen[x]
  rf<-nomogram.data$Renal.failure[x]
  Diastolic.blood.pressure<-nomogram.data$Diastolic.blood.pressure[x]
  points=0
  
  if (aniongap>26){
    points=53.25+points
  }
  else if (aniongap>=14){
    points=(aniongap-14)*(53.25/(26-14))+points
  }
  
  if (lactate>7.5){
    points=67.5+points
  }else if (lactate>=1.5){
    points=(lactate-1.5)*(67.5/(7.5-1.5))+points
  }
  
  if (calcium<6.5){
    points=92.5+points
  }else if (calcium<=11){
    points=((calcium-11)*92.5/(6.5-11))+points
  }
  
  #rf = 0: No; rf=1:yes
  if (rf==1){
    points=35+points
  }else if (rf==0){
    points=0
  }
  
  if (bun>180){
    points=points+95
  }else if (bun>0){
    points=((bun-0)*95/(180-0))+points
  }
  
  if (Diastolic.blood.pressure<20){
    points=points+100
  }else if (Diastolic.blood.pressure<110){
    points=((Diastolic.blood.pressure-110)*100/(20-110))+points
  }
  
  nomogram.data$points[x]=points
  if (points>=205){
    nomogram.data$pred.outcome[x]=1
  }else{
    nomogram.data$pred.outcome[x]=0
  }
}
nomogram.data$pred.outcome <- as.factor(factor(nomogram.data$pred.outcome, levels = c("0", "1")))
nomogram.data$pred.outcome=as.factor(nomogram.data$pred.outcome)

#Split train and test sets
nomogram.trainset <- subset(nomogram.data, group==1)
nomogram.trainset = subset(nomogram.trainset,select=-c(group))

nomogram.testset <- subset(nomogram.data, group==2)
nomogram.testset = subset(nomogram.testset,select=-c(group))

# confusion matrix for train set
nomogram.traintable <- table(Trainset.Actual = nomogram.trainset$outcome, nomogram.trainset$pred.outcome, deparse.level = 2)
nomogram.traintable

cfmatrix <- nomogram.traintable
nomogram.train.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
nomogram.train.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
nomogram.train.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)

# confusion matrix for test set
nomogram.testtable <- table(Testset.Actual = nomogram.testset$outcome, nomogram.testset$pred.outcome, deparse.level = 2)
nomogram.testtable

cfmatrix <- nomogram.testtable
nomogram.test.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
nomogram.test.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
nomogram.test.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)


# ============================ Create train table for Q5 ================================
Model <- c("Logistic Reg (BE)", "Random Forest", "GWTG", "Nomogram")
FPR <- c(logreg.train.FPR, RF.train.FPR, gwtg.train.FPR, nomogram.train.FPR)
FNR <- c(logreg.train.FNR, RF.train.FNR, gwtg.train.FNR, nomogram.train.FNR)
Err <- c(logreg.train.error, RF.train.error, gwtg.train.error,nomogram.train.error)
trainresults <- data.frame(Model, FPR, FNR, Err)
trainresults[is.na(trainresults)] = 0
View(trainresults)

# ============================ Create test table for Q6 ================================
Model <- c("Logistic Reg (BE)", "Random Forest", "GWTG", "Nomogram")
FPR <- c(logreg.test.FPR, RF.test.FPR, gwtg.test.FPR, nomogram.test.FPR)
FNR <- c(logreg.test.FNR, RF.test.FNR, gwtg.test.FNR, nomogram.test.FNR)
Err <- c(logreg.test.error, RF.test.error, gwtg.test.error,nomogram.test.error)
testresults <- data.frame(Model, FPR, FNR, Err)
testresults[is.na(testresults)] = 0
View(testresults)

RF.traintable
RF.testtable
logreg.traintable
logreg.testtable
gwtg.traintable
gwtg.testtable
nomogram.traintable
nomogram.testtable

# ============================= Q7 =====================================
View(trainset)

# Random sample from majority class and combine with minority to form new trainset
majority <- subset(trainset, outcome == 0)
minority <- subset(trainset, outcome == 1)

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases.
set.seed(22)
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers.
majority.chosen <- majority[chosen,]

# Combine two data tables by appending the rows
trainset.bal <- rbind(majority.chosen, minority)

## Check trainset is balanced.
summary(trainset.bal)

View(trainset.bal)

# ====================== Logistic Regression for Q7 ===========================
set.seed(22)
m.logreg.1 <- glm(outcome ~ . , family = binomial, data = trainset.bal)
step(m.logreg.1)

set.seed(22)
m.logreg.2 <- glm(outcome ~ age + hypertensive + atrialfibrillation + 
                    deficiencyanemias + Renal.failure + COPD + heart.rate + Respiratory.rate + 
                    SP.O2 + MCV + RDW + Leucocyte + Platelets + Neutrophils + 
                    Blood.calcium + Anion.gap + PCO2, family = binomial, data = trainset.bal)

# Train set prediction
logreg.train.prob <- predict(m.logreg.2, type = 'response')

# Set the threshold for predicting Y = 1 based on probability.
threshold <- 0.5

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat.train <- ifelse(logreg.train.prob > threshold, 1, 0)

# confusion matrix for train set
logreg.traintable <- table(trainset.bal$outcome, y.hat.train, deparse.level = 2)
logreg.traintable

cfmatrix <- logreg.traintable
logreg.train.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
logreg.train.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
logreg.train.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)
logreg.train.error

#Test
logreg.test.prob <- predict(m.logreg.2, newdata = testset, type = 'response')

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat.test <- ifelse(logreg.test.prob > threshold, 1, 0)

# confusion matrix for test set
logreg.testtable <- table(Testset.Actual = testset$outcome, y.hat.test, deparse.level = 2)

cfmatrix <- logreg.testtable
logreg.test.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
logreg.test.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
logreg.test.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)

# ========================== Random Forest =============================

set.seed(22)
m.RF.bal <- randomForest(outcome ~ . , data = trainset.bal, na.action = na.omit, importance = T)
m.RF.bal


# Train set prediction
prob.train <- predict(m.RF.bal, type = 'class')
prob.train

# confusion matrix for train set
RF.traintable <- table(trainset.bal$outcome, prob.train, deparse.level = 2)
RF.traintable

cfmatrix <- RF.traintable
RF.train.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
RF.train.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
RF.train.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)

prob.test <- predict(m.RF.bal, newdata = testset, type = 'class')

# confusion matrix for test set
RF.testtable <- table(Testset.Actual = testset$outcome, prob.test, deparse.level = 2)
RF.testtable

cfmatrix <- RF.testtable
RF.test.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
RF.test.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
RF.test.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)


# =================== Create test table for Q7 =====================
Model <- c("Logistic Reg (BE)", "Random Forest")
FPR <- c(logreg.test.FPR, RF.test.FPR)
FNR <- c(logreg.test.FNR, RF.test.FNR)
Err <- c(logreg.test.error, RF.test.error)
testresults.bal <- data.frame(Model, FPR, FNR, Err)
testresults.bal[is.na(testresults.bal)] = 0
View(testresults.bal)

# ============================= Q8 =====================================
#Extract top 20 variable importance
var.impt <- importance(m.RF.bal)
var.impt
varImpPlot(m.RF.bal, sort=TRUE, n.var=20)


#Fit variables as predictors into logreg
set.seed(22)
m.logreg.2 <- glm(outcome ~ Blood.calcium + Bicarbonate + Lymphocyte + heart.rate + Anion.gap + 
                  Leucocyte + Respiratory.rate + Creatine.kinase + Lactic.acid + Basophils + RDW +
                  Blood.sodium + BMI + Neutrophils + NT.proBNP + age + Platelets + INR + MCV + glucose,
                  family = binomial, data = trainset.bal)

#Test
logreg.test.prob <- predict(m.logreg.2, newdata = testset, type = 'response')

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat.test <- ifelse(logreg.test.prob > threshold, 1, 0)

# confusion matrix for test set
logreg.testtable <- table(Testset.Actual = testset$outcome, y.hat.test, deparse.level = 2)

cfmatrix <- logreg.testtable
logreg.test.FNR <- round((cfmatrix[2]) / (cfmatrix[2] + cfmatrix[4]),2)
logreg.test.FPR <- round((cfmatrix[3]) / (cfmatrix[1] + cfmatrix[3]),2)
logreg.test.error <- round((cfmatrix[2] + cfmatrix[3]) / (cfmatrix[1] + cfmatrix[2] + cfmatrix[3] + cfmatrix[4]),2)

#Append to table
testresults.bal[nrow(testresults.bal) + 1,] <- c("RF VarImpt into Logistic Reg",logreg.test.FPR, 
                                                 logreg.test.FNR, logreg.test.error)
View(testresults.bal)


