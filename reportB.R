# load the dataset
kidneydata=read.csv("KidneyData.csv")
attach(kidneydata)
#check data structures
head(kidneydata)
str(kidneydata)
#There are 500 observations and 19 variables in the data set.KidneyDisease is int .It should be converted as a factor variable.
#And other categorical variables converted into factor variables.
kidneydata$KidneyDisease = as.factor(kidneydata$KidneyDisease)
kidneydata$Gender =as.factor(kidneydata$Gender)
kidneydata$SmokingStatus=as.factor(kidneydata$SmokingStatus)

#remove patientid
kidneydata=kidneydata[,-1]


# from partA my best logistic regression model is as below
#model2=glm(KidneyDisease~BloodPressure+ElectricConductivity+pH+DissolvedOxygen+Turbidity+TotalDissolvedSolids,  data = train,family =binomial)
#I choose 10-Fold Cross-Validation because it requires only 10 model fits, making it computationally less expensive than LOOCV
library(boot)

#split the data: 80% for training and 20% for testing with same setseed value and ratio with PartA.
set.seed(2)
tr1 = sample(1:nrow(kidneydata), round(nrow(kidneydata)*0.8,0))
train = kidneydata[tr1, ] #defining training dataset
test=kidneydata[-tr1,]

set.seed(2)
poly_order = c(1:10)
cv_error_10= rep (0,10)
for (i in 1:10) {
model_10 = glm(KidneyDisease ~ poly(BloodPressure, i) + 
                    poly(ElectricConductivity, i) + 
                    poly(pH, i) + 
                    poly(DissolvedOxygen, i) + 
                    poly(Turbidity, i) + 
                    poly(TotalDissolvedSolids, i), 
                  data = train, family = binomial)
  
  # Perform 10-fold cross-validation and store the error
  cv_error_10[i] = cv.glm(train, model_10, K = 10)$delta[1]
}
cv_error_10
plot(poly_order, cv_error_10, type = "b", xlab = "Order of polynomials", ylab = "10-Fold CV - MSE")

#The lowest cross-validation error is at the third degree (cv_error_10[3]), 
#which indicates that the third-degree polynomial provides the best fit with minimal error.

#check both train data and test just in case
dim(train)
dim(test)
#Fit a polynomial logistic regression model on train data set
model_1=glm(KidneyDisease~poly(BloodPressure,3)+
                   poly(ElectricConductivity,3)+
                   poly(pH,3)+
                   poly(DissolvedOxygen,3)+
                   poly(Turbidity,3)+
                   poly(TotalDissolvedSolids,3),
                 data = train,family = binomial
                 )

summary(model_1)
## Make predictions on the test set
predict=predict(model_1,newdata = test,type = "response")
predict_class=ifelse(predict>0.5,1,0)

##calculate the misclassification rate
classification_table=table(predict_class,test$KidneyDisease)
misclassificationrate1=(classification_table[1,2]+classification_table[2,1])/sum(classification_table)
misclassificationrate1
library(dplyr)

# Fit Model 2 with the specified all significant predictors
model_2 = glm(KidneyDisease ~ 
                poly(ElectricConductivity, 3)[, 1] + 
                poly(BloodPressure, 3)[,2] +
                poly(pH, 3)[,1]+
                poly(DissolvedOxygen, 3)[, 1] + 
                poly(TotalDissolvedSolids, 3)[,1]+
                poly(Turbidity, 3)[, 1] + 
                poly(Turbidity, 3)[, 3],
              data = train, family = binomial)
summary(model_2)

## Make predictions on the test set
predict=predict(model_2,newdata = test,type = "response")
predict_class=ifelse(predict>0.5,1,0)

##calculate the misclassification rate
classification_table=table(predict_class,test$KidneyDisease)
misclassificationrate2=(classification_table[1,2]+classification_table[2,1])/sum(classification_table)
misclassificationrate2

# Fit Model 3 with the specified significant predictors with p value less than 0.05 from model 1
model_3 = glm(KidneyDisease ~ 
                poly(ElectricConductivity, 3)[, 1] + 
                poly(DissolvedOxygen, 3)[, 1] + 
                poly(Turbidity, 3)[, 1] + 
                poly(Turbidity, 3)[, 3],  
              data = train, family = binomial)

# Display the summary of Model 3
summary(model_3)
## Make predictions on the test set
predict=predict(model_3,newdata = test,type = "response")
predict_class=ifelse(predict>0.5,1,0)

##calculate the misclassification rate
classification_table=table(predict_class,test$KidneyDisease)
misclassificationrate3=(classification_table[1,2]+classification_table[2,1])/sum(classification_table)
misclassificationrate3

##equation 
#logit(E(KidneyDisease))=2.449−45.783⋅poly(ElectricConductivity,3)[, 1]−9.039⋅poly(DissolvedOxygen,3) [,1]+10.910⋅poly(Turbidity,3)[,1]−12.139⋅poly(Turbidity,3) [,3]



#decision tree library and check the data structor
library(tree)
str(train)
#The tree() function expects categorical variables like the Gender and SmokingStatus  to be factors,I already  converted them from the beginning
tree_kidneydata_train<-tree(KidneyDisease~.,train)
plot(tree_kidneydata_train)
text(tree_kidneydata_train, pretty=0)
summary(tree_kidneydata_train)

#cross validation
par(mfrow=c(1,1))
set.seed(1)
cv_kidneydata=cv.tree(tree_kidneydata_train, FUN = prune.misclass)
names(cv_kidneydata)
plot(cv_kidneydata$size, cv_kidneydata$dev, type = "b")

# Select the best size of the tree model
#According the plot , best size of the tree model is 4.
cv_kidneydata=cv.tree(tree_kidneydata_train, FUN = prune.misclass)
pruned_kidneydata=prune.misclass(tree_kidneydata_train, best = 4)
pruned_kidneydata
summary(pruned_kidneydata)
plot(pruned_kidneydata)
text(pruned_kidneydata, pretty=0)
summary(pruned_kidneydata)
#The three key variables (ElectricConductivity, Turbidity, and BloodPressure) 
#played significant roles in determining the likelihood of kidney disease.


#check the model accuracy
tree_predicted<-predict(pruned_kidneydata, test, type="class")
tab=table(tree_predicted,test$KidneyDisease)
misrate=((tab[1,2]+tab[2,1])/sum(tab))
paste("Misclassification error rate is ",misrate)


#9.	Apply an unsupervised learning technique of your choice to identify any interesting or hidden patterns in the dataset.
#Provide a clear explanation of the technique used and thoroughly describe your findings
options(scipen = 999)
str(kidneydata)
#since we use only numerical variables for Principal Component Analysis and unsupervised learning is for dataset that has no target variaable , 
#I will select only numerical variables,so that there is no factor and categorical variables 
numerickidney_data <- kidneydata %>%select_if(is.numeric)
str(numerickidney_data)


#Perform Principal Component Analysis for the numeric kidney_data dataset.
pr.out=prcomp(numerickidney_data,scale. = T)

#check the Cumulative Proportion of Variance Explained
summary(pr.out)
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve
#plot the Proportion of Variance Explained and Cumulative Proportion of Variance Explained
plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')

#This gives the standard deviances of the original variables.
sd_rank=sort(pr.out$scale,decreasing = T)
sd_rank

#check the loading of pc1 and pc2
pr.out$rotation[,1:2]
pr.out

#Draw the biplot and interpret it
biplot(pr.out,scale = 0,cex=0.4)

