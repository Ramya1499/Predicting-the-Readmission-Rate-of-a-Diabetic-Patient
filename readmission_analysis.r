# Predicting the Readmission Rate of Diabetic Patients 
library(dplyr)
library(corrplot)
library(caret)
library(psych)
library(stats)
library(rpart)
library(randomForest)
library(e1071)
library(naivebayes)

diabeties <- read.csv("diabeties.csv", header=TRUE,na.strings = "?")
str(diabeties)
summary(diabeties)

# As we see that encounter_id and patient_nbr are meta data and do not help towards 

# We see that readmitted column is the target feature over here. 
# It is of 3 levels <30 - the patient was hospitalized within 30 days.
#                   >30 - the patient was admitted after 30 days
#                   No -The patient was not admitted.

# Another important column is discharge disposition id which tells us where the patient went after being dischared from the hospital.
# By observing the id mappings we can see that 11,13,14,19,20,21 are related  to death or hospise and hence should be removed while constructing the predictive model.
# Also we see columns examide and citoglipton have only 1 level and hence can be removed from the analysis.
# The maximum time spent in a hospital by a patient is 14 days. Range is 1-14 days
# The number of lab tests performed during an encounter ranged from 1- 132.
# The number of procedures during an encounter ranges from 1- 6 other than lab tests.
# The number of medications resulted from 1-81 distinct values.
# The number of diagnoses are between 3-16.

# ------ Data Preprocessing / Data Cleaning and Understanding the data-----------

table(diabeties$weight)

# We see that 96% of the data is missing in the weight column so lets drop the value.
# Also, payer code and medical spec can be dropped as more than 53% of the data is missing.
# Dropped certain drugs where there were no more than 2 levels and where 1 level was very high than the other level.
diabeties1 <- subset(diabeties,select=-c(encounter_id, patient_nbr, examide,citoglipton,weight, payer_code,medical_specialty)) 

str(diabeties1)
summary(diabeties1)

# Replaced unknown Race with the value Other
diabeties1$race[is.na(diabeties1$race)] <- "Other"

# Dropped the unknown level in Gender variable.
diabeties1 <- droplevels(diabeties1[!diabeties1$gender == 'Unknown/Invalid',])

# Removed rows having missing values in Diagnosis 1,2,3 and converted to resective values via ICD Codes (Wikipedia Reference)
diabeties1<- diabeties1[!is.na(diabeties1$diag_1),]
diabeties1<- diabeties1[!is.na(diabeties1$diag_2),]
diabeties1<- diabeties1[!is.na(diabeties1$diag_3),]

#------------ Changing categorical Variables--------------------
# Changing the diagnosis 1, 2 and 3 into ICD coding

data2 <- diabeties1
data2$diag_1 <- as.numeric(levels(data2$diag_1)[data2$diag_1])
data2$diag_2 <- as.numeric(levels(data2$diag_2)[data2$diag_2])
data2$diag_3 <- as.numeric(levels(data2$diag_3)[data2$diag_3])
# diagnosis1
data2$diagnosis1 <- factor( rep("other",nrow(data2)),ordered = F, 
                            levels = c("Circulatory","Respiratory","Digestive","Diabetes","Injury",
                                       "Musculoskeletal","Genitourinary","Neoplasms","other"))
data2$diagnosis1[data2$diag_1>=390 & data2$diag_1 <= 459 | data2$diag_1==785] <- "Circulatory"
data2$diagnosis1[data2$diag_1>=460 & data2$diag_1 <= 519 | data2$diag_1==786] <- "Respiratory"
data2$diagnosis1[data2$diag_1>=520 & data2$diag_1 <= 579 | data2$diag_1==787] <- "Digestive"
data2$diagnosis1[data2$diag_1>=250 & data2$diag_1 < 251] <- "Diabetes"
data2$diagnosis1[data2$diag_1>800 & data2$diag_1 <= 999] <- "Injury"
data2$diagnosis1[data2$diag_1>=710 & data2$diag_1 <= 739] <- "Musculoskeletal"
data2$diagnosis1[data2$diag_1>=580 & data2$diag_1 <= 629 | data2$diag_1==788] <- "Genitourinary"
data2$diagnosis1[data2$diag_1>=140 & data2$diag_1 <= 239 | data2$diag_1>=790 & data2$diag_1 <= 799 | data2$diag_1==780 | data2$diag_1>=240 & data2$diag_1 < 250 | data2$diag_1>=251 & data2$diag_1 <= 279 | data2$diag_1>=680 & data2$diag_1 <= 709 |data2$diag_1>=001 & data2$diag_1 <= 139 | data2$diag_1==781 |data2$diag_1==782 | data2$diag_1==784] <- "Neoplasms"
# diagnosis_2
data2$diagnosis2 <- factor( rep("other",nrow(data2)),ordered = F, levels = c("Circulatory","Respiratory","Digestive","Diabetes","Injury","Musculoskeletal","Genitourinary","Neoplasms","other"))
data2$diagnosis2[data2$diag_2>=390 & data2$diag_2 <= 459 | data2$diag_2==785] <- "Circulatory"
data2$diagnosis2[data2$diag_2>=460 & data2$diag_2 <= 519 | data2$diag_2==786] <- "Respiratory"
data2$diagnosis2[data2$diag_2>=520 & data2$diag_2 <= 579 | data2$diag_2==787] <- "Digestive"
data2$diagnosis2[data2$diag_2>=250 & data2$diag_2 < 251] <- "Diabetes"
data2$diagnosis2[data2$diag_2>800 & data2$diag_2 <= 999] <- "Injury"
data2$diagnosis2[data2$diag_2>=710 & data2$diag_2 <= 739] <- "Musculoskeletal"
data2$diagnosis2[data2$diag_2>=580 & data2$diag_2 <= 629 | data2$diag_2==788] <- "Genitourinary"
data2$diagnosis2[data2$diag_2>=140 & data2$diag_2 <= 239 | data2$diag_2>=790 & data2$diag_2 <= 799 | data2$diag_2==780 | data2$diag_2>=240 & data2$diag_2 < 250 |data2$diag_2>=251 & data2$diag_2 <= 279 | data2$diag_2>=680 & data2$diag_2 <= 709 |data2$diag_2>=001 & data2$diag_2 <= 139 | data2$diag_2==781 |data2$diag_2==782 | data2$diag_2==784] <- "Neoplasms"

# diagnosis_3
data2$diagnosis3 <- factor( rep("other",nrow(data2)),ordered = F, 
                            levels = c("Circulatory","Respiratory","Digestive","Diabetes","Injury",
                                       "Musculoskeletal","Genitourinary","Neoplasms","other"))
data2$diagnosis3[data2$diag_3>=390 & data2$diag_3 <= 459 | data2$diag_3==785] <- "Circulatory"
data2$diagnosis3[data2$diag_3>=460 & data2$diag_3 <= 519 | data2$diag_3==786] <- "Respiratory"
data2$diagnosis3[data2$diag_3>=520 & data2$diag_3 <= 579 | data2$diag_3==787] <- "Digestive"
data2$diagnosis3[data2$diag_3>=250 & data2$diag_3 < 251] <- "Diabetes"
data2$diagnosis3[data2$diag_3>800 & data2$diag_3 <= 999] <- "Injury"
data2$diagnosis3[data2$diag_3>=710 & data2$diag_3 <= 739] <- "Musculoskeletal"
data2$diagnosis3[data2$diag_3>=580 & data2$diag_3 <= 629 | data2$diag_3==788] <- "Genitourinary"
data2$diagnosis3[data2$diag_3>=140 & data2$diag_3 <= 239 | data2$diag_3>=790 & data2$diag_3 <= 799 | data2$diag_3==780 | data2$diag_3>=240 & data2$diag_3 < 250 |data2$diag_3>=251 & data2$diag_3 <= 279 | data2$diag_3>=680 & data2$diag_3 <= 709 |data2$diag_3>=001 & data2$diag_3 <= 139 | data2$diag_3==781 |data2$diag_3==782 | data2$diag_3==784] <- "Neoplasms"


diabeties1<- data2
# Removing specific colums related to death from the discharge disposition id column

# As a patient who is expired cannot help towards readmission prediction subsetted columns 11,19,20,21
#-------------------------------------------------
diabeties1 <- subset(diabeties1, discharge_disposition_id != 11 & discharge_disposition_id != 19 & discharge_disposition_id != 20 & discharge_disposition_id != 21)

diabeties1$discharged_to <- factor( rep("transferred",nrow(diabeties1)),ordered = F, 
                                    levels = c("Discharged", "transferred","left_AMA"))
diabeties1$discharged_to[diabeties1$discharge_disposition_id==c(1,6,8)]<- "Discharged"
diabeties1$discharged_to[diabeties1$discharge_disposition_id==7]<- "left_AMA"

diabeties1$discharged_to<- as.factor(diabeties1$discharged_to)

#diabeties1$discharged_to <- factor( rep("other",nrow(diabeties1)),ordered = F, 
# levels = c("Discharged", "Transferred","left_AMA","Hospice"))
#diabeties1$discharged_to[diabeties1$discharge_disposition_id==c(1,6,8)]<- "Discharged"
#diabeties1$discharged_to[diabeties1$discharge_disposition_id==7]<- "left_AMA"
#diabeties1$discharged_to[diabeties1$discharge_disposition_id==c(2,3,4,5,15,16,17,22,23,24,30,27,28,29,10)]<- "Transferred"
#diabeties1$discharged_to[diabeties1$discharge_disposition_id==c(13,14)]<- "Hospice"

diabeties1$discharged_to <- as.factor(diabeties1$discharged_to)

# I have converted the readmit target feature into 2 levels for convenience 1 if  the patient was admitted within 30 days 0 if the patiennt was admitted after 30 days or was not admitted.
#The 30 day distinction is of high importance to hospitals because federal regulations penalize hospitals for an excessive proportion of readmissions as such. 

diabeties1$readmitbin <- as.factor(ifelse(diabeties1$readmitted == "<30",1,0) )

summary(diabeties1)

#Now I wanted  To categorize the admission_type_id, discarge_disposition_id,admission_source_id with their respective labelling in the ID_Mappings.csv sheet given.
# Considering the number of levels in each column as very high I have made some assumptions on classifying the patients as Emergency (ICU) and Non-Emergency(Non-ICU) patients.
# This would help decrease the number of dimensions and also if performing classification/ clustering alalysis would help in decreasing the number of dummy variables.

# I have considered the admission types 1,2,7 as Emergency cases i.e levels Emergency, Urgent and Trauma Center.
# I have considered levels 4,7,10,12,26 from the admission source id i.e transfer from a hospital,emergency room, transfer from critical access hospital, premture delivery, transfer from hospise.

#data3 <- diabeties1
#diabeties1$admission_type_id[diabeties1$admission_type_id==1 | diabeties1$admission_type_id==2 | diabeties1$admission_type_id==7] <- "Emergency Patient"
#diabeties1$admission_type_id <- as.factor(diabeties1$admission_type_id)

diabeties1 <- diabeties1%>%
  mutate(admission_type_id = ifelse(diabeties1$admission_type_id %in% c(1,2,7),"Emergency Patient","Non Emergency Patient"))%>%
  mutate(admission_source_id=ifelse(admission_source_id %in% c(4,7,10,12,26),"Emergency Patient","Non Emergency Patient"))

diabeties1$admission_source_id <- as.factor(diabeties1$admission_source_id)
diabeties1$admission_type_id<- as.factor(diabeties1$admission_type_id)


#diabeties1$admission_type_id <- as.numeric(as.factor(diabeties1$admission_type_id))
#diabeties1$discharge_disposition_id <- as.numeric(as.factor(diabeties1$discharge_disposition_id))
#diabeties1$admission_source_id <- as.numeric(as.factor(diabeties1$admission_source_id))

#summary(diabeties1)

diabeties1 <- select(diabeties1, -diag_1, -diag_2, -diag_3,-discharge_disposition_id)
summary(diabeties1)

# Now lets normalize the numerical features and add them back to our dataset

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

diabeties1_n <- as.data.frame(lapply(diabeties1[c(6:13)],normalize))
diabeties1<- cbind(diabeties1[c(1:5)],diabeties1_n,diabeties1[c(43,40,40,42)],diabeties1[c(14:39,44)])

# Removing the columns of drugs which have maximum values of 1 level and very very few in the other
diabeties2 <- diabeties1
diabeties2 <- select(diabeties2,-acetohexamide,-troglitazone,-glimepiride.pioglitazone,-metformin.rosiglitazone,-metformin.pioglitazone,-glipizide.metformin,-tolbutamide,-miglitol,-tolazamide,-chlorpropamide,-glyburide.metformin,-acarbose)
diabeties2 <- select(diabeties2, -readmitted)

summary(diabeties2)
str(diabeties2)
# From the readmittedbin variable has more number of patients that are readmitted again i.e 86987 patients where the readmitted patients are only 11066 patients this means that our dataset is totally unbalanced!

#---------- Understanding the data -----------
# Now let us understand our data wrt race, gender,age.
byrace <- table(diabeties2$readmitbin,diabeties2$race)
byrace
# We can see that most of the patients belonged to the Caucasian and AfricanAmerican population 
# where most of Caucasian population have been readmitted followed by AfricanAmerican 
bygender<- table(diabeties2$readmitbin,diabeties2$gender)
bygender
# Most of the patients were females
byage <- table(diabeties2$readmitbin,diabeties2$age)
byage


# From the distribution we can understand that most of the patients belonged to the age group 50-90 years.
# It is a right skewed distribution.
#The age group 70-80 had higher chances of being readmitted.

plot(diabeties2$age, main = "age distribution") # age: mode 70-80yrs normal distribution, right skewed
plot(diabeties2$gender, main = "gender distribution") # gender: female 53% male 47%
plot(diabeties2$A1Cresult, main = "A1C") # A1Cresult: 84% no A1c results, 8% >8
plot(diabeties2$admission_source_id, main = "admission source") # emergency 60%

#----------------- Correlation Analysis--------------------
#data<-select_if(diabeties2,is.numeric)
numtf <- as.numeric(diabeties2$readmitbin)
data <- cbind(diabeties2[c(6:13)],numtf)
c <- cor(data, use= "pairwise.complete.obs")
corrplot(c)


#-------------PCA Analysis---------------
# Performing PCA to determine the significant numerical features towards the target feature
# Before performing PCA we have to normalize the data
#Using princomp function 
# Anyways it is not required to perform PCA analysis as the number of numerical columns are less.
# However to determine the columns which provide us the maximum variablity we can perform the PCA analysis to find the same.
y <- select(diabeties2, readmitbin)
numeric_data<-diabeties2[6:13]
pca_n <- princomp(numeric_data, cor = TRUE, scores = TRUE)
summary(pca_n)
unclass(pca_n$loadings)

# Using principal function
numeric_data_1 <- numeric_data
y <- select(diabeties2, readmitbin)
X <- numeric_data_1

# no rotation
pca <- principal(X, nfactors = 5, rotate = "none")
rotat <- data.frame(cbind(pca$score, y))
head(rotat)
pca$loadings

# From both the types of PCA methods the components can be selected as follows.
# pc1 is number of medications and time in hosptial
# pc2 is number of in-patient visits and emergency
# pc3 is number of procedures
# pc4 is number of out-patient visits
# pc5 is number of diagnoses

summary(diabeties2)
str(diabeties2)
diabeties3<- diabeties2


write.csv(diabeties3,"preprossed_db_data.csv", row.names = FALSE)
#--------------------------------------------------
#----------- Data Partitioning---------------------------------------
# --------Unblanced Negative Class Problem--------------
#We have used synthetic minority over-sampling technique (SMOTE) to oversample our underrepresented class of readmissions and obtain equal representation of our overrepresented and underrepresented classes.
# Overcoming the problem via SMOTE ANALYSIS

# First lets partition the data
library(caret)
set.seed(1499)
# Due to a large amount of data let me create a data partition of 50% of the data i.e. 50% of 98591 is 49295 records
par_data <- createDataPartition(diabeties3$readmitbin, p=.5, list=FALSE)
str(par_data)
train <-diabeties3[1:36972,]
val <- diabeties3[36973:49296,]

table(train$readmitbin)

prop.table(table(train$readmitbin))
prop.table(table(val$readmitbin))

#fit_all <- glm(readmitbin ~., data=train, family="binomial")
#summary(fit_all)

library(ROSE)
library(DMwR)


# Applying Smote analysis on the training data to create artificial data and use on the unseen data to see how it classifies it.

# Smoting
data_balanced_smote <- SMOTE(readmitbin ~ ., train, perc.over = 300, perc.under=100)

# Let us customize the tuning parameter to perform 10 fold cross validation on all the values
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE,selectionFunction = "oneSE",verboseIter=T)

#----- Logistic Regression ------
# Using smote
mod_lr_smote <- train(readmitbin~.,data=data_balanced_smote, method="glm", family="binomial",
                       trControl = ctrl, tuneLength = 2)
summary(mod_lr_smote)
mod_lr_smote

## Predicting the test data

pred_lr_smote = predict(mod_lr_smote, subset(val,select=-c(readmitbin)))
table(pred_lr_smote,val$readmitbin)
confusionMatrix(data=pred_lr_smote, val$readmitbin,positive = "1")

library(pROC)
roc(as.numeric(val$readmitbin),as.numeric(pred_lr_smote))

#-----Naive Bayes-------

## Smote
y=data_balanced_smote$readmitbin
x=subset(data_balanced_smote,select=-c(readmitbin))

grad1<- expand.grid(fL=1,usekernel=FALSE,adjust=0)

model_nb_smote = train(x,y,method='nb',metric="Accuracy",trControl=ctrl,tuneGrid=grad1)
model_nb_smote
#trControl=trainControl(method='cv',number=3,verboseIter = T)

### Predicting on test data
pred_nb_smote=predict(model_nb_smote,subset(val,select=-c(readmitbin)),type = "raw")
confusionMatrix(val$readmitbin,pred_nb_smote,positive = "1")

roc(as.numeric(val$readmitbin),as.numeric(pred_nb_smote))

#--- Decision Tree------
# Build a Decision Tree Classification model 
grad2<- expand.grid(trials=c(1,3,5,7,11), model="tree",winnow=TRUE)
dt<-train(readmitbin~.,data=train, method="C5.0",metric="Accuracy",trControl=ctrl,tuneGrid=grad2)
dt

# Predict on  Test data
pred_dt_smote <- predict(dt, subset(val,select=-c(readmitbin)), type="raw")
confusionMatrix(pred_dt_smote,val$readmitbin,positive = "1")

roc(as.numeric(val$readmitbin), as.numeric(pred_dt_smote))


#-----KNN---------------
#Applying knn algol
grad <- expand.grid(k=c(5,7))

c<- train(readmitbin~.,data=data_balanced_smote, method="knn",metric="Accuracy",trControl=ctrl,tuneGrid=grad)
c
predTest_knn_smote <- predict(c, subset(val,select=-c(readmitbin)), type="raw")
confusionMatrix(predTest_knn_smote,val$readmitbin,positive = "1")

roc(as.numeric(val$readmitbin), as.numeric(predTest_knn_smote))

#----Random Forest-------
grid3 <- expand.grid(mtry= 6)
h <- train(readmitbin~., data=data_balanced_smote,method="rf",metric="Accuracy",trControl=ctrl,tuneGrid=grid3)
h
predTest_rf_smote <- predict(h, subset(val,select=-c(readmitbin)), type="raw")
confusionMatrix(predTest_rf_smote,val$readmitbin,positive = "1")

roc(as.numeric(val$readmitbin), as.numeric(predTest_rf_smote ))

#-------------------------------------------
