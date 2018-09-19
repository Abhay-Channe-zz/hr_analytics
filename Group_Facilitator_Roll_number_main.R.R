## Loading all the required libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(stringr)

# Read all the dataset
emp_surv <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
str(emp_surv)

emp_data <- read.csv("general_data.csv", stringsAsFactors = F)
str(emp_data)

emp_in <- read.csv("in_time.csv", stringsAsFactors = F)
str(emp_in)

emp_out <- read.csv("out_time.csv", stringsAsFactors = F)
str(emp_out)

emp_mngr_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
str(emp_mngr_data)

# Varifying the datatables for the rows and integrity
length(unique(emp_surv$EmployeeID))
length(unique(emp_data$EmployeeID))
length(unique(emp_in$X))
length(unique(emp_out$X))
length(unique(emp_mngr_data$EmployeeID))

setdiff(emp_data$EmployeeID, emp_surv$EmployeeID)
setdiff(emp_data$EmployeeID, emp_mngr_data$EmployeeID)
setdiff(emp_data$EmployeeID, emp_in$X)
setdiff(emp_data$EmployeeID, emp_out$X)

#Merging the dataset into the master Employee table
emp_master <- merge(emp_surv, emp_data, by = "EmployeeID")
emp_master <- merge(emp_master, emp_mngr_data, by = "EmployeeID")


#Converting categorical variables into the Factors
emp_master$Attrition <- as.factor(emp_master$Attrition)
emp_master$BusinessTravel <- as.factor(emp_master$BusinessTravel)
emp_master$Department <- as.factor(emp_master$Department)
emp_master$JobRole <- as.factor(emp_master$JobRole)
emp_master$EducationField <- as.factor(emp_master$EducationField)
emp_master$Gender <- as.factor(emp_master$Gender)
emp_master$Over18 <- as.factor(emp_master$Over18)
emp_master$MaritalStatus <- as.factor(emp_master$MaritalStatus)
emp_master$JobLevel <- as.factor(emp_master$JobLevel)

#Converting levels to meaningful labels
emp_master$Education[which(emp_master$Education == 1)] <- "Below Collage"
emp_master$Education[which(emp_master$Education == 2)] <- "Collage"
emp_master$Education[which(emp_master$Education == 3)] <- "Bachelor"
emp_master$Education[which(emp_master$Education == 4)] <- "Master"
emp_master$Education[which(emp_master$Education == 5)] <- "Doctor"

emp_master$Education <- as.factor(emp_master$Education)

emp_master$EnvironmentSatisfaction[which(emp_master$EnvironmentSatisfaction == 1)] <- "Low"
emp_master$EnvironmentSatisfaction[which(emp_master$EnvironmentSatisfaction == 2)] <- "Medium"
emp_master$EnvironmentSatisfaction[which(emp_master$EnvironmentSatisfaction == 3)] <- "High"
emp_master$EnvironmentSatisfaction[which(emp_master$EnvironmentSatisfaction == 4)] <- "Very High"

emp_master$EnvironmentSatisfaction <- as.factor(emp_master$EnvironmentSatisfaction)

emp_master$JobInvolvement[which(emp_master$JobInvolvement == 1)] <- "Low"
emp_master$JobInvolvement[which(emp_master$JobInvolvement == 2)] <- "Medium"
emp_master$JobInvolvement[which(emp_master$JobInvolvement == 3)] <- "High"
emp_master$JobInvolvement[which(emp_master$JobInvolvement == 4)] <- "Very High"

emp_master$JobInvolvement <- as.factor(emp_master$JobInvolvement)

emp_master$JobSatisfaction[which(emp_master$JobSatisfaction == 1)] <- "Low"
emp_master$JobSatisfaction[which(emp_master$JobSatisfaction == 2)] <- "Medium"
emp_master$JobSatisfaction[which(emp_master$JobSatisfaction == 3)] <- "High"
emp_master$JobSatisfaction[which(emp_master$JobSatisfaction == 4)] <- "Very High"

emp_master$JobSatisfaction <- as.factor(emp_master$JobSatisfaction)

emp_master$PerformanceRating[which(emp_master$PerformanceRating == 1)] <- "Low"
emp_master$PerformanceRating[which(emp_master$PerformanceRating == 2)] <- "Good"
emp_master$PerformanceRating[which(emp_master$PerformanceRating == 3)] <- "Excellent"
emp_master$PerformanceRating[which(emp_master$PerformanceRating == 4)] <- "Outstanding"

emp_master$PerformanceRating <- as.factor(emp_master$PerformanceRating)

emp_master$WorkLifeBalance[which(emp_master$WorkLifeBalance == 1)] <- "Bad"
emp_master$WorkLifeBalance[which(emp_master$WorkLifeBalance == 2)] <- "Good"
emp_master$WorkLifeBalance[which(emp_master$WorkLifeBalance == 3)] <- "Better"
emp_master$WorkLifeBalance[which(emp_master$WorkLifeBalance == 4)] <- "Best"

emp_master$WorkLifeBalance <- as.factor(emp_master$WorkLifeBalance)

# Verifying the structure, summary and NA values
summary(emp_master)
sum(is.na(emp_master))
emp_master <- na.omit(emp_master)
str(emp_master)

# Outlier treatments by checking the quantiles 
quantile(emp_master$Age, seq(0,1,.01), na.rm = T)
quantile(emp_master$DistanceFromHome, seq(0,1,.01), na.rm = T)
quantile(emp_master$MonthlyIncome, seq(0,1,.01), na.rm = T)
emp_master$MonthlyIncome[which(emp_master$MonthlyIncome > 116754)] <- 116754

quantile(emp_master$NumCompaniesWorked, seq(0,1,.01), na.rm = T)
quantile(emp_master$StockOptionLevel, seq(0,1,.01), na.rm = T)

quantile(emp_master$YearsAtCompany, seq(0,1,.01), na.rm = T)
emp_master$YearsAtCompany[which(emp_master$YearsAtCompany > 22)] <- 22

quantile(emp_master$YearsSinceLastPromotion, seq(0,1,.01), na.rm = T)
emp_master$YearsSinceLastPromotion[which(emp_master$YearsSinceLastPromotion > 9)] <- 9

quantile(emp_master$YearsWithCurrManager, seq(0,1,.01), na.rm = T)
emp_master$YearsWithCurrManager[which(emp_master$YearsWithCurrManager > 10)] <- 10

# check duplicated
which(duplicated(emp_master$EmployeeID))
# no duplicated

# What is the normal distribution of all employees
p1 <- ggplot(emp_master, aes(x=EnvironmentSatisfaction,fill=Attrition))+geom_bar(stat = "count") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p2 <- ggplot(emp_master, aes(x=JobSatisfaction,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p3 <- ggplot(emp_master, aes(x=WorkLifeBalance,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p4 <- ggplot(emp_master, aes(x=BusinessTravel,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p5 <- ggplot(emp_master, aes(x=Department,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p6 <- ggplot(emp_master, aes(x=Education,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))

plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)

p1 <- ggplot(emp_master, aes(x=EducationField,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p2 <- ggplot(emp_master, aes(x=JobRole,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p3 <- ggplot(emp_master, aes(x=MaritalStatus,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p4 <- ggplot(emp_master, aes(x=JobInvolvement,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p5 <- ggplot(emp_master, aes(x=PerformanceRating,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))
p6 <- ggplot(emp_master, aes(x=JobLevel,fill=Attrition))+geom_bar() +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1))

plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)

#Removing Categorical & un-necessery columns
emp_master <- emp_master[ ,-c(12,19,21)]
str(emp_master)
#write.csv(emp_master, "emp_master.csv")


# Correlation between numeric variables
library(GGally)
ggpairs(emp_master[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike","StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager")])
emp_original <- emp_master

# Normalising continuous features 
emp_master$Age <- scale(emp_master$Age)
emp_master$DistanceFromHome <- scale(emp_master$DistanceFromHome)
emp_master$MonthlyIncome <- scale(emp_master$MonthlyIncome)
emp_master$NumCompaniesWorked <- scale(emp_master$NumCompaniesWorked)
emp_master$PercentSalaryHike <- scale(emp_master$PercentSalaryHike)
emp_master$StockOptionLevel <- scale(emp_master$StockOptionLevel)
emp_master$TotalWorkingYears <- scale(emp_master$TotalWorkingYears)
emp_master$TrainingTimesLastYear <- scale(emp_master$TrainingTimesLastYear)
emp_master$YearsAtCompany <- scale(emp_master$YearsAtCompany)
emp_master$YearsSinceLastPromotion <- scale(emp_master$YearsSinceLastPromotion)
emp_master$YearsWithCurrManager <- scale(emp_master$YearsWithCurrManager)

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
emp_master$Attrition<- as.numeric(ifelse(emp_master$Attrition=="Yes",1,0))
emp_master$Gender<- as.numeric(ifelse(emp_master$Gender=="Male",1,0))


# Checking Attrition rate of employee

Attrn <- sum(emp_master$Attrition)/nrow(emp_master)
Attrn # 16.16% churn rate. 

# Create the dummy variables

# For EnvironmentSatisfaction
dummy_1 <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-2]

# For JobSatisfaction
dummy_1 <- data.frame(model.matrix( ~JobSatisfaction, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-2]


# For WorkLifeBalance
dummy_1 <- data.frame(model.matrix( ~WorkLifeBalance, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-2]


# For BusinessTravel
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-4]

# For Department
dummy_1 <- data.frame(model.matrix( ~Department, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-4]

# For Education
dummy_1 <- data.frame(model.matrix( ~Education, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-5]

# For EducationField
dummy_1 <- data.frame(model.matrix( ~EducationField, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-5]

# For JobLevel
dummy_1 <- data.frame(model.matrix( ~JobLevel, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-6]

# For JobRole
dummy_1 <- data.frame(model.matrix( ~JobRole, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-6]

# For JobInvolvement
dummy_1 <- data.frame(model.matrix( ~JobInvolvement, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-16]

# For MaritalStatus
dummy_1 <- data.frame(model.matrix( ~MaritalStatus, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-6]

# For PerformanceRating
dummy_1 <- data.frame(model.matrix( ~PerformanceRating, data = emp_master))
dummy_1 <-dummy_1[,-1]
emp_master <- cbind(emp_master, dummy_1)
emp_master <- emp_master[ ,-15]

emp_master <- emp_master[ , -1]
# For variables having only two levels, "yes" is 1,
#gender "male" is 1 and SeniorCitizen "1" is 1 

# Final dataset
View(emp_master) #7032 obs. of  31 variables
str(emp_master)

# Verifying the NA values again
sum(is.na(emp_master))

########    Creating Train and Test dataset   #########
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(emp_master), 0.7*nrow(emp_master)) # 70% data for training and rest for test
train = emp_master[trainindices,]
test = emp_master[-trainindices,]


#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2237.2 37 coeff NA nullDev 2624.8...resDev 2130.0

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
#AIC = 2201.1

library(car)
vif(model_2)

# VIF does not eliminate any of the parameter
# based on the P-Value, JobInvolvementLow is removed from model
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                 WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationCollage + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + 
                 MaritalStatusSingle, family = "binomial", data = train)
summary(model_3)
#AIC = 2201.4

# based on the P-Value, JobRoleSales.Executive is removed from model
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                 WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationCollage + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director +  
                 MaritalStatusSingle, family = "binomial", data = train)
summary(model_4)
#AIC = 2202

# based on the P-Value, JobRoleSales.Executive is removed from model
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                 WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationCollage + EducationFieldOther + 
                 JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director +  
                 MaritalStatusSingle, family = "binomial", data = train)
summary(model_5)
#AIC = 2202.9

# based on the P-Value, JobRoleManufacturing.Director is removed from model
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                 WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationCollage + EducationFieldOther + 
                 JobLevel5 + JobRoleResearch.Director +  
                 MaritalStatusSingle, family = "binomial", data = train)
summary(model_6)
#AIC = 2207.8

# based on the P-Value, EducationCollage is removed from model
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                 WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldOther + JobLevel5 + JobRoleResearch.Director +  
                 MaritalStatusSingle, family = "binomial", data = train)
summary(model_7)
#AIC = 2210

# based on the P-Value, JobLevel5 is removed from model
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                 WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldOther + JobRoleResearch.Director +  
                 MaritalStatusSingle, family = "binomial", data = train)
summary(model_8)
#AIC = 2212.8

# based on the P-Value, JobRoleResearch.Director is removed from model
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                 WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldOther +   
                 MaritalStatusSingle, family = "binomial", data = train)
summary(model_9)
#AIC = 2214.3

# based on the P-Value, PercentSalaryHike is removed from model
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                 WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldOther +   
                 MaritalStatusSingle, family = "binomial", data = train)
summary(model_10)
#AIC = 2217.1

# based on the P-Value, EducationFieldOther is removed from model
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfactionLow + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                  WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales +    
                  MaritalStatusSingle, family = "binomial", data = train)
summary(model_11)
#AIC = 2220.8

# based on the P-Value, EnvironmentSatisfactionVery.High is removed from model
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfactionLow +  
                  JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                  WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                  DepartmentSales +    
                  MaritalStatusSingle, family = "binomial", data = train)
summary(model_12)
#AIC = 2226.5

# based on the P-Value, BusinessTravelTravel_Rarely is removed from model
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfactionLow +  
                  JobSatisfactionLow + JobSatisfactionVery.High + WorkLifeBalanceBest + 
                  WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + 
                  DepartmentSales +    
                  MaritalStatusSingle, family = "binomial", data = train)
summary(model_13)
#AIC = 2234

# based on the P-Value, JobSatisfactionVery.High is removed from model
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfactionLow +  
                  JobSatisfactionLow  + WorkLifeBalanceBest + 
                  WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + 
                  DepartmentSales +    
                  MaritalStatusSingle, family = "binomial", data = train)
summary(model_14)
#AIC = 2240.8


# based on the P-Value, Age is removed from model
model_15 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfactionLow +  
                  JobSatisfactionLow  + WorkLifeBalanceBest + 
                  WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + 
                  DepartmentSales +    
                  MaritalStatusSingle, family = "binomial", data = train)
summary(model_15)
#AIC = 2248.8

# based on the P-Value, Age is removed from model
model_16 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfactionLow +  
                  JobSatisfactionLow  + WorkLifeBalanceBest + 
                  WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                  DepartmentSales +    
                  MaritalStatusSingle, family = "binomial", data = train)
summary(model_16)
#AIC = 2256.4

# based on the P-Value, DepartmentSales is removed from model
model_17 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfactionLow +  
                  JobSatisfactionLow  + WorkLifeBalanceBest + 
                  WorkLifeBalanceBetter + WorkLifeBalanceGood + BusinessTravelTravel_Frequently + 
                  MaritalStatusSingle, family = "binomial", data = train)
summary(model_17)
#AIC = 2255.4

Final_model <- model_17

# Plotting variables against the attrition
p1 <- ggplot(emp_original, aes(x=emp_original$NumCompaniesWorked, fill = Attrition))+geom_histogram()
p2 <- ggplot(emp_original, aes(x=emp_original$TotalWorkingYears, fill = Attrition))+geom_histogram()
p3 <- ggplot(emp_original, aes(x=emp_original$TrainingTimesLastYear, fill = Attrition))+geom_histogram()
p4 <- ggplot(emp_original, aes(x=emp_original$YearsSinceLastPromotion, fill = Attrition))+geom_histogram()
p5 <- ggplot(emp_original, aes(x=emp_original$YearsWithCurrManager, fill = Attrition))+geom_histogram()
p6 <- ggplot(emp_original, aes(x=emp_original$TotalWorkingYears, fill = Attrition))+geom_histogram()
plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)


test_pred = predict(Final_model, type = "response", newdata = test[,-2])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrn <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrn <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrn,test_pred_attrn)
test_conf <- confusionMatrix(test_pred_attrn, test_actual_attrn, positive = "Yes")
test_conf

#######################################################################
test_pred_attrn <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

install.packages("e1071")
library(e1071)
library(caret)
test_conf <- confusionMatrix(test_pred_attrn, test_actual_attrn, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrn, test_actual_attrn, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.1616 for final model

test_cutoff_attrn <- factor(ifelse(test_pred >=0.1616, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrn, test_actual_attrn, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrn <- ifelse(test_cutoff_attrn=="Yes",1,0)
test_actual_attrn <- ifelse(test_actual_attrn=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrn, test_actual_attrn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrn_decile = lift(test_actual_attrn, test_pred, groups = 10)
ggplot(attrn_decile, aes(x = attrn_decile$bucket, y = attrn_decile$Gain, label_value()))+geom_line()
