#Q2

#Load the data: read.csv() 
#Check the data structure: str() to clarify the types and structure 
kidneydata=read.csv("KidneyData.csv")
attach(kidneydata)
head(kidneydata)
dim(kidneydata)
str(kidneydata)

#Remove irrelevant variables such as  PatientID which is an index variable and need to be removed for modelling
kidneydata=kidneydata[,-1]
#Convert categorical variables: Convert categorical variables into factors using as.factor().
kidneydata$Gender=as.factor(kidneydata$Gender)
kidneydata$SmokingStatus=as.factor(kidneydata$SmokingStatus)
kidneydata$Gender=as.factor(kidneydata$Gender)
kidneydata$KidneyDisease=as.factor(kidneydata$KidneyDisease)
str(kidneydata)

# split the data: 80% for training and 20% for testing.
set.seed(2)
tr.id = sample(1:nrow(kidneydata),nrow(kidneydata)*0.8)
train=kidneydata[tr.id, ]
test=kidneydata[-tr.id,]
str(train)
str(test)

#visulization 

boxplot(Age~KidneyDisease, data = kidneydata)
#The median age for individuals without or with kidney disease has no big difference,
#but There is a slightly wider range of younger people ages among people with kidney disease.


gender_table <- table(kidneydata$Gender, kidneydata$KidneyDisease)
gender_prop <- prop.table(gender_table, 1)
barplot(gender_prop, beside = TRUE,
        legend=TRUE,
        xlab = "Gender",
        ylab = "Proportion",
        main = "Proportion of Kidney Disease by Gender")
  #We can see that there is no notable difference in gender proportions with regard to kidney disease.
boxplot(BloodPressure~KidneyDisease, data = kidneydata)
#those who have KidneyDiseases tend to have higher blood pressure median and lower range

boxplot(BloodSugar~KidneyDisease, data = kidneydata)
#those who have KidneyDiseases tend to have higher BloodSugar median and wider range ,also more oulier on the higher bloodsugar side.

boxplot(Cholesterol~KidneyDisease, data = kidneydata)
#We can see that there is almost no difference in Cholesterol median with regard to kidney disease.
boxplot(BMI~KidneyDisease, data = kidneydata)
#There appears to be no significant difference in kidney disease between people with low BMI and those with higher BMI.


smoking_table <- table(kidneydata$SmokingStatus, kidneydata$KidneyDisease)
smoking_prop <- prop.table(smoking_table, 1)
barplot(smoking_prop, beside = TRUE,
        legend=TRUE,
        xlab = "smoking status",
        ylab = "Proportion",
        main = "Proportion of Kidney Disease by smoking status")

#People who have never smoked and those who currently smoke have similar percentages of developing or not developing kidney disease.Therefore smoking status has no significant relationship with kidneydiseas

boxplot(ElectricConductivity~KidneyDisease, data = kidneydata)
#those who have KidneyDiseases tend to have significantly lower ElectricConductivity median and wider range, those who does not have kidneay diseas higher median of ElectricConductivity and lower range.


boxplot(pH~KidneyDisease, data = kidneydata)
#there is no significant difference with pH median with  kidneyKidneyDiseases status

boxplot(DissolvedOxygen~KidneyDisease, data = kidneydata)
#those who have KidneyDiseases tend to have lower DissolvedOxygen median and more outlier

boxplot(Turbidity ~KidneyDisease, data = kidneydata)
#those who have KidneyDiseases tend to have higher Turbidity median and wider range,those who don'n have KidneyDiseases have lower median and more outlier.

boxplot(TotalDissolvedSolids ~KidneyDisease, data = kidneydata)
#those who have KidneyDiseases tend to have lower TotalDissolvedSolids median and wider range

boxplot(NitriteLevel ~KidneyDisease, data = kidneydata)
#there is no significant difference with NitriteLevel median with  kidneyKidneyDiseases status

boxplot(LeadConcentration ~KidneyDisease, data = kidneydata)
#there is no significant difference with LeadConcentration median with  kidneyKidneyDiseases status

boxplot(ArsenicConcentration ~KidneyDisease, data = kidneydata)
#there is no significant difference with ArsenicConcentration median with  kidneyKidneyDiseases status

boxplot(Humidity ~KidneyDisease, data = kidneydata)
#there is no significant difference with Humidity median with  kidneyKidneyDiseases status

#4.	Use logistic regression to answer the research question. Clearly explain the process or all the steps involved 
  
#Model Building:
 model1 <- glm(KidneyDisease ~ ., data = train, family = binomial)
 summary(model1)
#Using the hypothesis testing, it can be seen clearly that BloodPressure,ElectricConductivity,pH,DissolvedOxygen,Turbidity,TotalDissolvedSolids have significant relationship with kidneydiseas .

#model improvement
model2=glm(KidneyDisease~BloodPressure+ElectricConductivity+pH+DissolvedOxygen+Turbidity+TotalDissolvedSolids,  data = train,family =binomial)

#Use summary(model) to evaluate significance.
summary(model2) 



#To predict KidneyDisease probability by model2;
testdata=test[-18]
glm_prob =predict(model2, type="response",newdata=testdata)

#Evaluate model performance on the test set using confusion matrix .
glm_pred=rep("0",100)
glm_pred[glm_prob>0.5]="1"
table(glm_pred,test$KidneyDisease)
misclassification_rate=(5+7)/100
misclassification_rate


#Give your resultant model
coef(model2)
#logit(P(KidneyDisease))= 31.12855285 + 0.02955996 *BloodPressure -0.03318803 *ElectricConductivity -0.96993682 *pH− -0.52863860 *DissolvedOxygen+ 3.41677763 *Turbidity -0.03773373 *TotalDissolvedSolids
