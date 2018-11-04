#setwd("C:\\Users\\eravdiv\\Desktop\\Ravi\\PG-Data-Science\\HR-Case-Study")

install.packages("MASS")
install.packages("car")
install.packages("cowplot")
install.packages("e1071")
install.packages("caret")
install.packages("GGally")
install.packages("caTools")
install.packages("RcppRoll")
install.packages("ddalpha")
install.packages("DEoptimR")
install.packages("dimRed")
install.packages("gower")

library(MASS)
library(car)
library(ggplot2)
library(cowplot)
library(GGally)
library(caTools)
library(ROCR)
require(dplyr)
library(dplyr)
library(e1071)
library(caret)

## Problem Statement

# A large company named XYZ, employs, at any given point of time, around 4000 employees. 
# However, every year, around 15% of its employees leave the company and need to be replaced 
# with the talent pool available in the job market. The management believes that this level of 
# attrition (employees leaving, either on their own or because they got fired) is bad for the
# company, because of the following reasons -
# 1. The former employees’ projects get delayed, which makes it difficult to meet timelines, 
#    resulting in a reputation loss among consumers and partners
# 2. A sizeable department has to be maintained, for the purposes of recruiting new talent
# 3. More often than not, the new employees have to be trained for the job and/or given time 
#    to acclimatise themselves to the company

# Hence, the management has contracted an HR analytics firm to understand what factors they 
# should focus on, in order to curb attrition. In other words, they want to know what changes 
# they should make to their workplace, in order to get most of their employees to stay. Also, 
# they want to know which of these variables is most important and needs to be addressed 
# right away.

# Since you are one of the star analysts at the firm, this project has been given to you.

## Goal of the case study
# You are required to model the probability of attrition using a logistic regression. 
# The results thus obtained will be used by the management to understand what changes 
# they should make to their workplace, in order to get most of their employees to stay.

# Approach followed - 
# 1. Import files and perform data cleaning and missing value inclusion
# 2. Perform logistics regression and obtain factors that influence attrition.
# 3. Find out the optimal probalility cutoff 
# 4. Obtain KS statistic
# 5. Check KS statistic in decile for optimal gain

# Import general data
general <- read.csv("general_data.csv", stringsAsFactors = FALSE )
summary(general)
nrow(general)
length(unique(general$EmployeeID))
typeof(general$EmployeeID)

# Import employee_survey_data
employee <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
summary(employee)
nrow(employee)
length(unique(employee$EmployeeID))
typeof(employee$EmployeeID)

# Import manager_survey_data
manager <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
summary(manager)
nrow(manager)
length(unique(manager$EmployeeID))
typeof(manager$EmployeeID)

# Import in_time
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
summary(in_time)
nrow(in_time)
length(unique(in_time$X))
colnames(in_time)[1] <- "EmployeeID"
typeof(in_time$EmployeeID)

# Import out_time
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)
summary(out_time)
nrow(out_time)
length(unique(out_time$X))
colnames(out_time)[1] <- "EmployeeID"
typeof(out_time$EmployeeID)

# Combining the files into 1 common file.

setdiff(general$EmployeeID,employee$EmployeeID) # Identical EmployeeID across these datasets
general_employee <- merge(general, employee, by="EmployeeID", all = F)

setdiff(general_employee$EmployeeID, manager$EmployeeID ) # Identical EmployeeID across these datasets
manager_general_employee <- merge(general_employee, manager, by="EmployeeID", all = F)
  
setdiff(in_time$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these datasets
in_out_time <- merge(in_time, out_time, by = "EmployeeID", all = F)

setdiff(manager_general_employee$EmployeeID,in_out_time$EmployeeID) # Identical EmployeeID across these datasets
employee_details <- merge(manager_general_employee, in_out_time, by = "EmployeeID", all = F) 
nrow(employee_details)
summary(employee_details)
str(employee_details)
View(employee_details)

# Missing value operations.
colSums(is.na(employee_details))

percentage_NA <- (sum(is.na(employee_details$NumCompaniesWorked)) +
sum(is.na(employee_details$TotalWorkingYears)) +
sum(is.na(employee_details$EnvironmentSatisfaction)) +
          sum(is.na(employee_details$JobSatisfaction)) +
                    sum(is.na(employee_details$WorkLifeBalance)))/nrow(employee_details)*100
print(paste("Total number of NA in data set are :",percentage_NA, "% which is less than 3% hence deleting the rows."))

# Removing NA rows from data set

employee_details <- employee_details[!is.na(employee_details$NumCompaniesWorked),]
employee_details <- employee_details[!is.na(employee_details$TotalWorkingYears),]
employee_details <- employee_details[!is.na(employee_details$EnvironmentSatisfaction),]
employee_details <- employee_details[!is.na(employee_details$JobSatisfaction),]
employee_details <- employee_details[!is.na(employee_details$WorkLifeBalance),]

colSums(is.na(employee_details))
str(employee_details)

#Univariant Analysis
Att_yes <- length(which(employee_details$Attrition == "Yes"))
Att_no <- length(which(employee_details$Attrition == "No"))

#ggplot(employee_details, aes(x=factor(Attrition),fill=Attrition))+ geom_bar() + theme_bw() + geom_text(aes(y = employee_details$Attrition/2,label = paste("Percentage %")), colour = "white", size = 4, position = position_dodge(.9)) + theme_bw() + labs(title = "Employee Attrition", x = "Attrition", y = "Count") 
ggplot(employee_details, aes(x=factor(Attrition),fill=Attrition))+ geom_bar()

#Bivariant Analysis
# Barcharts for categorical features with stacked HR employee churn information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

#Bivariant Analysis 1
plot_grid(ggplot(employee_details, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar(), 
          ggplot(employee_details, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

#Bivariant Analysis 2
plot_grid(ggplot(employee_details, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(EducationField),fill=Attrition))+ geom_bar(), 
          align = "h")

#Bivariant Analysis 3
plot_grid(ggplot(employee_details, aes(x=factor(Gender),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

#Bivariant Analysis 4
plot_grid(ggplot(employee_details, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

#Bivariant Analysis 5
plot_grid(ggplot(employee_details, aes(x=factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(Over18),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

#Bivariant Analysis 6
plot_grid(ggplot(employee_details, aes(x=factor(StockOptionLevel),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(TrainingTimesLastYear),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

#Bivariant Analysis 7
plot_grid(ggplot(employee_details, aes(x=factor(YearsAtCompany),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(YearsSinceLastPromotion),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

#Bivariant Analysis 8
plot_grid(ggplot(employee_details, aes(x=factor(YearsWithCurrManager),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(), 
          align = "h")   

#Bivariant Analysis 9
plot_grid(ggplot(employee_details, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

#Bivariant Analysis 10
plot_grid(ggplot(employee_details, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_details, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

#Bivariant Analysis 11
plot_grid(ggplot(employee_details, aes(DistanceFromHome))+ geom_histogram(binwidth = 5),
          ggplot(employee_details, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Bivariant Analysis 12
plot_grid(ggplot(employee_details, aes(Age))+ geom_histogram(binwidth = 5),
          ggplot(employee_details, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Bivariant Analysis 13
plot_grid(ggplot(employee_details, aes(PercentSalaryHike))+ geom_histogram(binwidth = 2),
          ggplot(employee_details, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Bivariant Analysis 14
plot_grid(ggplot(employee_details, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5),
          ggplot(employee_details, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Bivariant Analysis 15
plot_grid(ggplot(employee_details, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000),
          ggplot(employee_details, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#Bivariant Analysis 16
plot_grid(ggplot(employee_details, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000),
          ggplot(employee_details, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# Boxplots of numeric variables relative to attrition status.
# Outlier Treatment 1
plot_grid(ggplot(employee_details, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_details, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_details, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Outlier Treatment 2
plot_grid(ggplot(employee_details, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_details, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Outlier Treatment for Years in company as 40 shows 100% attrition indicating retirement.
length(which( employee_details$YearsAtCompany == 40))
employee_details$Attrition[which(employee_details$YearsAtCompany == 40)]
#Removing those candidates from data to ensure no negative impact on analysis
employee_details <- employee_details[-which(employee_details$YearsAtCompany == 40), ]

# Outlier Treatment for Age as 60 shows 100% no attrition indicating retirement.
length(which( employee_details$Age == 60))
employee_details$Attrition[which(employee_details$Age >= 59)]
# Since they are not leaving the company we will keep them.

# Outlier Treatment for Total working years as 40 shows 100% attrition indicating retirement.
length(which( employee_details$TotalWorkingYears == 40))
employee_details$Attrition[which(employee_details$TotalWorkingYears == 40)]
#Removing those candidates from data to ensure no negative impact on analysis
employee_details <- employee_details[-which(employee_details$TotalWorkingYears == 40), ]

# Below tables show that there are no significant outliers.
sapply(employee_details[,c("DistanceFromHome","Age","PercentSalaryHike", "TotalWorkingYears", "MonthlyIncome")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier

ggpairs(employee_details[, c("DistanceFromHome","Age","PercentSalaryHike", "TotalWorkingYears", "MonthlyIncome")])
# As expected, Age and no. of years of experience are corelated.

################################################################
# Feature standardisation
# Normalising continuous features 

employee_details$DistanceFromHome<- scale(employee_details$DistanceFromHome)
employee_details$Age<- scale(employee_details$Age)
employee_details$PercentSalaryHike<- scale(employee_details$PercentSalaryHike)
employee_details$TotalWorkingYears<- scale(employee_details$TotalWorkingYears)
employee_details$MonthlyIncome<- scale(employee_details$MonthlyIncome)

# converting target variable attrition from No/Yes character to factorwith levels 0/1 
employee_details$Attrition<- ifelse(employee_details$Attrition=="Yes",1,0)

# Checking attrition rate of probable employee

Attrition_percentage <- sum(employee_details$Attrition)/nrow(employee_details)*100
Attrition_percentage # 16% attrition 

str(employee_details)

table(employee_details$EmployeeID)
table(employee_details$StandardHours)
table(employee_details$Over18)
# Ignoring the columns : EmployeeID, Over18 and StandardHours since they all have the same values

# creating a dataframe of categorical features
employee_categorical<- employee_details[,c("BusinessTravel", "Department", "Education", "EducationField", "Gender", "JobLevel", "JobRole", "MaritalStatus", "NumCompaniesWorked", "StockOptionLevel", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager", "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance", "JobInvolvement", "PerformanceRating")]
str(employee_categorical)

# converting categorical attributes to factor
employee_categorical<- data.frame(sapply(employee_categorical, function(x) factor(x)))
str(employee_categorical)

str(employee_categorical$YearsAtCompany)
table(employee_categorical$YearsAtCompany)
ggplot(employee_details, aes(x=factor(YearsAtCompany),fill=Attrition))+ geom_bar()
View(employee_categorical)

ggplot(employee_details, aes(x=factor(YearsSinceLastPromotion),fill=Attrition))+ geom_bar()
ggplot(employee_details, aes(x=factor(YearsWithCurrManager),fill=Attrition))+ geom_bar()

# creating dummy variables for factor attributes
employee_master12<- data.frame(sapply(employee_categorical, 
                            function(x) data.frame(model.matrix(~x,data =employee_categorical))[,-1]))

ncol(employee_master12)

employee_master <- cbind(employee_details[ , c("Attrition", "DistanceFromHome", "Age", "PercentSalaryHike", "TotalWorkingYears", "MonthlyIncome")], employee_master12)
View(employee_master)
# For variables having only two levels, PerformanceRating 3 is set to 1 and 4 is set to 0 and Male/Female is set to 1/0

# Creating a dataframe of dates
employee_hours <- employee_details[ ,c( 30:ncol(employee_details))]
View(employee_hours)


x <- ncol(employee_hours)/2
temp_time1 <- data.frame(matrix(0,nrow = nrow(employee_hours), ncol = ncol(employee_hours)/2))
for (n in 1:x) {
  temp_time1[ ,n] <- as.numeric(as.POSIXlt(employee_hours[ ,x + n], format = "%m/%d/%Y %H:%M", rm.na = TRUE)) - as.numeric(as.POSIXlt(employee_hours[,n], format = "%m/%d/%Y %H:%M", rm.na = TRUE))
}
temp_time1[is.na(temp_time1)] <- 0

for (n in 1:nrow(employee_hours)) {
  employee_master$sumhours[n] <- sum(temp_time1[ n,])
}

employee_master$sumhours <- scale(employee_master$sumhours)

# Final dataset
View(employee_master)
str(employee_master)
colSums(is.na(employee_master))

colnames(employee_master)
duplicated(colnames(employee_master))

table(employee_master$sumhours)
employee_master$sumhours
#####################################


########################################################################
# splitting the data between train and test
set.seed(100)
emp_indices = sample.split(employee_master$Attrition, SplitRatio = 0.7)
emp_train = employee_master[emp_indices,]
emp_test = employee_master[!(emp_indices),]

summary(emp_test@sumhours)

########################################################################
# Logistic Regression: 

ncol(emp_train)
duplicated(colnames(emp_train))

#Initial model
model_emp1 = glm(Attrition ~ ., data = emp_train, family = "binomial")
summary(model_emp1) #AIC 2029.9

# Stepwise selection
model_emp2<- stepAIC(model_emp1, direction="both")
summary(model_emp2)

# Removing multicollinearity through VIF check
vif(model_emp2)

#After stepAIC
model_emp3<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
  Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
  MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
  NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
  NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
  TrainingTimesLastYear.x2 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
  TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
  YearsAtCompany.x10 + YearsAtCompany.x11 + YearsAtCompany.x12 + 
  YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
  YearsAtCompany.x29 + YearsAtCompany.x3 + YearsAtCompany.x32 + 
  YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
  YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
  YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
  YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
  YearsSinceLastPromotion.x8 + YearsSinceLastPromotion.x9 + 
  YearsWithCurrManager.x1 + YearsWithCurrManager.x12 + YearsWithCurrManager.x13 + 
  YearsWithCurrManager.x14 + YearsWithCurrManager.x15 + YearsWithCurrManager.x2 + 
  YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
  YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
  WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
  YearsWithCurrManager.x9, family = "binomial", 
  data = emp_train, maxit = 100)


summary(model_emp3) 
vif(model_emp3)

#Excluding TrainingTimesLastYear.x2
model_emp4<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                   JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                   NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                   NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                   TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
                   TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                   YearsAtCompany.x10 + YearsAtCompany.x11 + YearsAtCompany.x12 + 
                   YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                   YearsAtCompany.x29 + YearsAtCompany.x3 + YearsAtCompany.x32 + 
                   YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                   YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                   YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                   YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                   YearsSinceLastPromotion.x8 + YearsSinceLastPromotion.x9 + 
                   YearsWithCurrManager.x1 + YearsWithCurrManager.x12 + YearsWithCurrManager.x13 + 
                   YearsWithCurrManager.x14 + YearsWithCurrManager.x15 + YearsWithCurrManager.x2 + 
                   YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                   YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                   YearsWithCurrManager.x9, family = "binomial", 
                 data = emp_train, maxit = 100)


summary(model_emp4) 
vif(model_emp4)

#Excluding YearsAtCompany.x29             
model_emp5<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                   JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                   NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                   NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                   TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
                   TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                   YearsAtCompany.x10 + YearsAtCompany.x11 + YearsAtCompany.x12 + 
                   YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                   YearsAtCompany.x3 + YearsAtCompany.x32 + 
                   YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                   YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                   YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                   YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                   YearsSinceLastPromotion.x8 + YearsSinceLastPromotion.x9 + 
                   YearsWithCurrManager.x1 + YearsWithCurrManager.x12 + YearsWithCurrManager.x13 + 
                   YearsWithCurrManager.x14 + YearsWithCurrManager.x15 + YearsWithCurrManager.x2 + 
                   YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                   YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                   YearsWithCurrManager.x9, family = "binomial", 
                 data = emp_train, maxit = 100)


summary(model_emp5) 


#Excluding YearsWithCurrManager.x15             
model_emp6<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                   JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                   NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                   NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                   TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
                   TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                   YearsAtCompany.x10 + YearsAtCompany.x11 + YearsAtCompany.x12 + 
                   YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                   YearsAtCompany.x3 + YearsAtCompany.x32 + 
                   YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                   YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                   YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                   YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                   YearsSinceLastPromotion.x8 + YearsSinceLastPromotion.x9 + 
                   YearsWithCurrManager.x1 + YearsWithCurrManager.x12 + YearsWithCurrManager.x13 + 
                   YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                   YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                   YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                   YearsWithCurrManager.x9, family = "binomial", 
                 data = emp_train, maxit = 100)


summary(model_emp6) 


#Excluding YearsAtCompany.x12             
model_emp7<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                   JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                   NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                   NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                   TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
                   TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                   YearsAtCompany.x10 + YearsAtCompany.x11 + 
                   YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                   YearsAtCompany.x3 + YearsAtCompany.x32 + 
                   YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                   YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                   YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                   YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                   YearsSinceLastPromotion.x8 + YearsSinceLastPromotion.x9 + 
                   YearsWithCurrManager.x1 + YearsWithCurrManager.x12 + YearsWithCurrManager.x13 + 
                   YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                   YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                   YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                   YearsWithCurrManager.x9, family = "binomial", 
                 data = emp_train, maxit = 100)


summary(model_emp7)


#Excluding YearsWithCurrManager.x13             
model_emp8<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                   JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                   NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                   NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                   TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
                   TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                   YearsAtCompany.x10 + YearsAtCompany.x11 + 
                   YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                   YearsAtCompany.x3 + YearsAtCompany.x32 + 
                   YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                   YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                   YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                   YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                   YearsSinceLastPromotion.x8 + YearsSinceLastPromotion.x9 + 
                   YearsWithCurrManager.x1 + YearsWithCurrManager.x12 + 
                   YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                   YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                   YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                   YearsWithCurrManager.x9, family = "binomial", 
                 data = emp_train, maxit = 100)


summary(model_emp8)

 
#Excluding YearsSinceLastPromotion.x8             
model_emp9<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                   JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                   NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                   NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                   TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
                   TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                   YearsAtCompany.x10 + YearsAtCompany.x11 + 
                   YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                   YearsAtCompany.x3 + YearsAtCompany.x32 + 
                   YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                   YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                   YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                   YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                   YearsSinceLastPromotion.x9 + 
                   YearsWithCurrManager.x1 + YearsWithCurrManager.x12 + 
                   YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                   YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                   YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                   YearsWithCurrManager.x9, family = "binomial", 
                 data = emp_train, maxit = 100)


summary(model_emp9)


#Excluding YearsWithCurrManager.x12             
model_emp10<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                   JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                   NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                   NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                   TrainingTimesLastYear.x3 + TrainingTimesLastYear.x4 + 
                   TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                   YearsAtCompany.x10 + YearsAtCompany.x11 + 
                   YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                   YearsAtCompany.x3 + YearsAtCompany.x32 + 
                   YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                   YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                   YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                   YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                   YearsSinceLastPromotion.x9 + 
                   YearsWithCurrManager.x1 + 
                   YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                   YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                   YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                   YearsWithCurrManager.x9, family = "binomial", 
                 data = emp_train, maxit = 100)


summary(model_emp10)


#Excluding TrainingTimesLastYear.x3             
model_emp11<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + YearsAtCompany.x11 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                    YearsWithCurrManager.x9, family = "binomial", 
                  data = emp_train, maxit = 100)


summary(model_emp11)


#Excluding TrainingTimesLastYear.x5             
model_emp12<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + YearsAtCompany.x11 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences + 
                    YearsWithCurrManager.x9, family = "binomial", 
                  data = emp_train, maxit = 100)


summary(model_emp12)


#Excluding YearsWithCurrManager.x9             
model_emp13<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + StockOptionLevel.x3 + TrainingTimesLastYear.x1 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + YearsAtCompany.x11 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences  
                    , family = "binomial", 
                  data = emp_train, maxit = 100)


summary(model_emp13)


#Excluding TrainingTimesLastYear.x1             
model_emp14<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + StockOptionLevel.x3 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + YearsAtCompany.x11 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x33 + YearsAtCompany.x4 + YearsAtCompany.x7 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)


summary(model_emp14)


#Excluding YearsAtCompany.x7    
model_emp15<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + StockOptionLevel.x3 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + YearsAtCompany.x11 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x33 + YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)


summary(model_emp15)


#Excluding YearsAtCompany.x33    
model_emp16<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + StockOptionLevel.x3 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + YearsAtCompany.x11 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)


summary(model_emp16)


#Excluding YearsAtCompany.x11    
model_emp17<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x3 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + StockOptionLevel.x3 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp17)


#Excluding JobLevel.x3     
model_emp18<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + StockOptionLevel.x3 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp18)

 
#Excluding StockOptionLevel.x3     
model_emp19<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x4 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp19)


#Excluding TrainingTimesLastYear.x4     
model_emp20<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours + EducationField.xLife.Sciences  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp20)


#Excluding EducationField.xLife.Sciences     
model_emp21<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x13 + YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp21)


#Excluding YearsAtCompany.x13     
model_emp22<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp22)


#Excluding YearsSinceLastPromotion.x4    
model_emp23<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp23)


#Excluding JobRole.xResearch.Scientist    
model_emp24<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xSales.Executive + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp24)


#Excluding JobRole.xSales.Executive    
model_emp25<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x11 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp25)


#Excluding YearsSinceLastPromotion.x11    
model_emp26<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp26)


#Excluding NumCompaniesWorked.x8    
model_emp27<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp27)


#Excluding NumCompaniesWorked.x4    
model_emp28<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp28)


#Excluding JobSatisfaction.x3    
model_emp29<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp29)


#Excluding JobSatisfaction.x2    
model_emp30<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 + YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp30)


#Excluding TrainingTimesLastYear.x6    
model_emp31<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + JobInvolvement.x3 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp31)


#Excluding JobInvolvement.x3    
model_emp32<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsAtCompany.x8 + YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp32)

#Excluding YearsAtCompany.x8   
model_emp33<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp33)


#Excluding YearsWithCurrManager.x4   
model_emp34<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x4 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp34)
 
#Excluding YearsWithCurrManager.x4   
model_emp35<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x6 + 
                    YearsWithCurrManager.x8 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp35)


#Excluding YearsWithCurrManager.x8   
model_emp36<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x3 + YearsWithCurrManager.x6 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp36)

#Excluding YearsWithCurrManager.x3   
model_emp37<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    YearsWithCurrManager.x6 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp37)

#Excluding YearsWithCurrManager.x6   
model_emp38<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp38)

#Excluding JobLevel.x2   
model_emp39<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x3 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp39)

#Excluding YearsSinceLastPromotion.x3  
model_emp40<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp40)

#Excluding JobRole.xResearch.Director  
model_emp41<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4 + sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp41)

#Excluding WorkLifeBalance.x4  
model_emp42<- glm(formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp42)


#Excluding MonthlyIncome  
model_emp43<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x15 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp43)

#Excluding YearsSinceLastPromotion.x15  
model_emp44<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x10 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp44)
 
#Excluding YearsAtCompany.x10   
model_emp45<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp45)

#Excluding JobLevel.x5    
model_emp46<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp46)
 
#Excluding WorkLifeBalance.x2    
model_emp47<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x6 + YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp47)

#Excluding YearsSinceLastPromotion.x6    
model_emp48<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp48)

#Excluding NumCompaniesWorked.x6    
model_emp49<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    JobRole.xManager + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp49)

#Excluding JobRole.xManufacturing.Director    
model_emp50<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    JobRole.xManager +  
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp50)

#Excluding JobRole.xManager    
model_emp51<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp51)

#Excluding YearsAtCompany.x32    
model_emp52<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + YearsAtCompany.x32 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp52)

#Excluding YearsAtCompany.x32    
model_emp53<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x1 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp53)

#Excluding YearsWithCurrManager.x1    
model_emp54<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x3 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp54)


#Excluding YearsAtCompany.x3   
model_emp55<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsAtCompany.x4 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp55)


#Excluding YearsAtCompany.x4   
model_emp56<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x2 + YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp56)


#Excluding YearsAtCompany.x2   
model_emp57<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x14 + YearsWithCurrManager.x2 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp57)

#Excluding YearsWithCurrManager.x2   
model_emp58<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    NumCompaniesWorked.x9 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x14 +  
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp58)

#Excluding NumCompaniesWorked.x9 
model_emp59<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x14 +  
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp59)


#Excluding WorkLifeBalance.x3 
model_emp60<- glm(formula = Attrition ~ Age + TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x14 +  
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp60)

#Excluding Age 
model_emp61<- glm(formula = Attrition ~ TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    YearsWithCurrManager.x14 +  
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp61)

#Excluding YearsWithCurrManager.x14 
model_emp62<- glm(formula = Attrition ~ TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp62)

#Excluding BusinessTravel.xTravel_Rarely 
model_emp63<- glm(formula = Attrition ~ TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    YearsSinceLastPromotion.x9 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp63)

#Excluding YearsSinceLastPromotion.x9 
model_emp64<- glm(formula = Attrition ~ TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    YearsAtCompany.x1 + 
                    YearsAtCompany.x23 + 
                    YearsSinceLastPromotion.x7 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp64)

#Excluding YearsAtCompany.x23 by comparing the P values with other P values.
model_emp65<- glm(formula = Attrition ~ TotalWorkingYears + BusinessTravel.xTravel_Frequently + 
                    Department.xResearch...Development + 
                    Department.xSales + 
                    MaritalStatus.xSingle + NumCompaniesWorked.x5 + 
                    NumCompaniesWorked.x7 + 
                    YearsAtCompany.x1 + 
                    YearsSinceLastPromotion.x7 + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 +  
                    JobSatisfaction.x4 + 
                    sumhours  
                  , family = "binomial", 
                  data = emp_train, maxit = 100)

summary(model_emp65)

#######################################################################

### Model Evaluation
### Test Data ####
#predicted probabilities of Churn 1 for test data

emp_test_pred = predict(model_emp65, type = "response", newdata = emp_test[,-1])
emp_test_pred

# Let's see the summary 
summary(emp_test_pred)

emp_test$prob <- emp_test_pred
View(emp_test)
test_actual_attrition <- factor(ifelse(emp_test$Attrition==1,"Yes","No"))
test_actual_attrition
# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(emp_test_pred >= 0.50, "Yes", "No"))
test_pred_attrition

test_conf1 <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf1

#table(test_actual_attrition,test_pred_attrition)

#######################################################################
test_pred_attrition <- factor(ifelse(emp_test_pred >= 0.40, "Yes", "No"))
test_pred_attrition

test_conf2 <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf2

#########################################################################################
# Let's Choose the cutoff value. 
# Let's find out the optimal probalility cutoff 
emp_test_pred

perform_function <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(emp_test_pred >= cutoff, "Yes", "No"))
  conf_attr <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc_attr <- conf_attr$overall[1]
  sens_attr <- conf_attr$byClass[1]
  spec_attr <- conf_attr$byClass[2]
  out_attr <- t(as.matrix(c(sens_attr, spec_attr, acc_attr))) 
  colnames(out_attr) <- c("sensitivity", "specificity", "accuracy")
  return(out_attr)
}

# predicted_attrition <- factor(ifelse(emp_test_pred >= 0.75212121, "Yes", "No"))
# conf_attr <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
# conf_attr
# acc_attr <- conf_attr$overall[1]
# acc_attr
# sens_attr <- conf_attr$byClass[1]
# sens_attr
# spec_attr <- conf_attr$byClass[2]
# spec_attr
# out_attr <- t(as.matrix(c(sens_attr, spec_attr, acc_attr))) 
# colnames(out_attr) <- c("sensitivity", "specificity", "accuracy")
# out_attr
# Creating cutoff values from 0.001587 to 0.835149 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred_attrition)
s_attr = seq(.01,.80,length=100)
s_attr
OUT_attr = matrix(0,100,3)
OUT_attr

for(i in 1:100) {
  OUT_attr[i,] = perform_function(s_attr[i])
} 
OUT_attr

plot(s_attr, OUT_attr[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_attr,OUT_attr[,2],col="darkgreen",lwd=2)
lines(s_attr,OUT_attr[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_attr <- s_attr[which(abs(OUT_attr[,1]-OUT_attr[,2])<0.01)]
print(paste("The cutoff value where accuracy, sensitivity and specificity meet is:",cutoff_attr))

# Let's choose a cutoff value of 0.169596 for final model
test_cutoff_attrition <- factor(ifelse(emp_test_pred >=0.16161, "Yes", "No"))
conf_final_attr <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc_attr <- conf_final_attr$overall[1]
sens_attr <- conf_final_attr$byClass[1]
spec_attr <- conf_final_attr$byClass[2]
acc_attr
sens_attr
spec_attr
View(emp_test)
#################################################################################################
### KS -statistic - Test Data ######
test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#on testing  data
pred_object_test_attr<- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test_attr <- performance(pred_object_test_attr, "tpr", "fpr")
ks_table_test_attr <- attr(performance_measures_test_attr, "y.values")[[1]] - (attr(performance_measures_test_attr, "x.values")[[1]])
ks_table_test_attr
max(ks_table_test_attr)
KS_value <- max(ks_table_test_attr)*100
print(paste("KS Value is:",KS_value, "%"))

####################################################################
####################################################################
# Lift & Gain Chart 
# plotting the lift chart

lift_attr <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>% summarise_at(vars(labels ), funs(total = n(), totalresp=sum(., na.rm = TRUE))) %>%
  mutate(Cumresp = cumsum(totalresp), Gain=Cumresp/sum(totalresp)*100, Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift_attr(test_actual_attrition, emp_test_pred, groups = 10)
Attrition_decile
print(paste("Gain table shows that KS value",KS_value,"% falls between 1st and 2nd decile."))
################# END PROGRAM #####################
