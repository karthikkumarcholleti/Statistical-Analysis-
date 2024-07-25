
# PROJECT TITLE : Analyzing Salary Determination in Tech Industry
# GROUP NAME : PI SMARTS

# Team Members:
# Karthik kumar Cholleti
# Shirisha Gajjela
# Hasibur Rashid Mahi


#Loading the Data Set
data <- read.csv("StatsProject.csv", header = TRUE)

nrow(data)
summary(data)



# Checking for missing values
missing_values <- sum(is.na(data))
missing_values




# Converting categorical variables to factors (if needed)
data$experience_level <- as.factor(data$ExperienceLevel)
data$job_title <- as.factor(data$JobTitle)
data$company_size <- as.factor(data$CompanySize)


#Analyzing the data about Job Title
data$JobTitle
nrow(data)

count_of_job_titles <- table(data$JobTitle)
count_of_job_titles
print(data.frame(count = count_of_job_titles))


# Applying logarithm transformation to our response variable
data$logSalary <- log(data$SalaryUsd)
summary(data)



#Data Visualizations



# Histogram of distribution of log-transformed salary
hist(data$logSalary, main = "Salary distribution of Employees", xlab = "log Salary",xlim=range(data$logSalary))



# Comparing distributions across experience levels using boxplots
plot(data$ExperienceLevel, data$logSalary)


boxplot(logSalary ~ ExperienceLevel, data = data, ylab = "log Salary (USD)", xlab = "Experience Level of Employees",main='Experience level of Employees vs Salary')


# BoxPlot between Employment Type and Salary


boxplot(logSalary ~ EmploymentType, data = data, ylab = "log Salary (USD)", xlab = "Employment Type of Employees",main='Employment Type of Employees vs Salary')


# BoxPlot between Job Title and Salary
boxplot(logSalary ~ JobTitle, data = data, ylab = "log Salary (USD)", xlab = "Job Titles of Employees",main='Job Titles of employees vs salary')


# Anova on Experience Level
anova_result <- aov(logSalary ~ ExperienceLevel, data = data)
summary(anova_result)
anova_result


# Conducting pairwise comparisons (Tukey's HSD test)
tukey_result <- TukeyHSD(anova_result)
summary(tukey_result)
tukey_result

str(data)




# Load the necessary library for linear regression
library(stats)

# Simple Linear Regression with Experience Level
model_exp <- lm(logSalary ~ ExperienceLevel, data = data)
model_exp
summary(model_exp)


#Q-Q plot
qqnorm(resid(model_exp),main='Normal Q-Q plot of Experience Level vs Salaries')
qqline(resid(model_exp),col='red')

#Residuals vs Fitted
plot(resid(model_exp)-fitted(model_exp), main = 'Residuals vs Fitted Values')
abline(h=0, lty = 'dashed', col = 'red')



# Simple Linear Regression with Employment Type
model_emp <- lm(logSalary ~ EmploymentType, data = data)
model_emp

summary(model_emp)

plot(logSalary ~ EmploymentType, data = data)

#Q-Q plot
qqnorm(resid(model_emp),main = 'Normal Q-Q plot of Employment Type vs Salary')
qqline(resid(model_emp),col='red')

plot(resid(model_emp)-fitted(model_emp), main = 'Residuals vs Fitted Values')
abline(h=0, lty = 'dashed', col = 'red')



# Simple Linear Regression with Job Title
model_job <- lm(logSalary ~ jobTitle, data = data)
model_job
summary(model_job)

#Q-Q plot
qqnorm(resid(model_job),main='Normal Q-Q plot of Job Title vs Salary')
qqline(resid(model_job),col='red')

#Residuals Vs Fitted
plot(resid(model_job)-fitted(model_job),main = 'Residuals vs Fitted Values')
abline(h=0, lty = 'dashed', col = 'red')



# Simple Linear Regression with Employee Residence
model_res <- lm(logSalary ~ EmployeeResidence, data = data)
model_res
summary(model_res)

#Q-Q plot
qqnorm(resid(model_res),main='Normal Q-Q plot of Employee Residence vs Salary')
qqline(resid(model_res),col='red')

#Residuals vs Fitted
plot(resid(model_res)-fitted(model_res),main = 'Residuals vs Fitted Values' )

abline(h=0, lty = 'dashed', col = 'red')




# Simple Linear Regression with Remote Work Ratio
model_remote <- lm(logSalary ~ RemoteRatio, data = data)
summary(model_remote)

plot(logSalary ~ RemoteRatio, data = data)


#Q-Q plot
qqnorm(resid(model_remote),main='Normal Q-Q plot of Remote ratio vs Salary')
qqline(resid(model_remote),col='red')

#Residuals vs Fitted
plot(resid(model_remote)-fitted(model_remote,main = 'Residuals vs Fitted Values'))
abline(h=0, lty = 'dashed', col = 'red')



# Simple Linear Regression with Company Location
model_loc <- lm(logSalary ~ companyLocation, data = data)
summary(model_loc)

#Q-Q plot
qqnorm(resid(model_loc))
qqline(resid(model_loc),col='red')

#Residual vs Fitted
plot(resid(model_loc)-fitted(model_loc))
abline(h=0, lty = 'dashed', col = 'red')




# Simple Linear Regression with Company Size
model_size <- lm(logSalary ~ CompanySize, data = data)
summary(model_size)

#Q-Q plot
qqnorm(resid(model_size))
qqline(resid(model_size))

#Residual vs Fitted
plot(resid(model_size)-fitted(model_size))
abline(h=0, lty = 'dashed', col = 'red')



# Employee Residence Model Hypothesis Tests
employee_residence_test <- summary(model_res)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]
employee_residence_test

# Extracting coefficient estimates and p-values
employee_residence_test <- summary(model_res)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]

#significance level
alpha <- 0.05

# Separating significant and not significant employee residences
significant_residences <- employee_residence_test[employee_residence_test[, "Pr(>|t|)"] < alpha, ]
not_significant_residences <- employee_residence_test[employee_residence_test[, "Pr(>|t|)"] >= alpha, ]

# Print significant employee residences
cat("Significant Employee Residences:\n")
print(significant_residences)

# Print not significant employee residences
cat("\nNot Significant Employee Residences:\n")
print(not_significant_residences)




# Remote Work Ratio Model Hypothesis Test
remote_ratio_test <- summary(model_remote)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]
remote_ratio_test





# Company Size Model Hypothesis Tests
company_size_test <- summary(model_size)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]
company_size_test




# Company Location Model Hypothesis Tests
company_location_test <- summary(model_loc)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]
company_location_test


# Perform ANOVA
anova_job <- aov(logSalary ~ JobTitle, data = data)
anova_job
# Summary of ANOVA
summary(anova_job)





# Perform correlation test
cor_test <- cor.test(data$WorkYear, data$logSalary)
cor_test

summary(anova_exp)





# ANOVA for employment type
anova_emp <- aov(logSalary ~ EmploymentType, data = data)
anova_emp
summary(anova_emp)


# Multiple linear regression model
multi_model <- lm(logSalary ~ WorkYear + ExperienceLevel + EmploymentType + JobTitle + EmployeeResidence + RemoteRatio + CompanyLocatioin + CompanySize, data = data)
summary(multi_model)



# Converting categorical variables to factors (if needed)
data$experience_level <- as.factor(data$ExperienceLevel)
data$job_title <- as.factor(data$JobTitle)
data$company_size <- as.factor(data$CompanySize)
str(data)



pairs(~ logSalary + WorkYear + ExperienceLevel + EmploymentType + JobTitle + EmployeeResidence + RemoteRatio + CompanyLocatioin + CompanySize, data = data)

pairs(~ logSalary + WorkYear + ExperienceLevel + EmploymentType+ JobTitle + EmployeeResidence + RemoteRatio + CompanyLocatioin + CompanySize, data = data)


model <- summary(multi_model)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]
model


alpha <- 0.05

# Separating significant and not significant employee residences
significant_residences <- model[model[, "Pr(>|t|)"] < alpha, ]
not_significant_residences <- model[model[, "Pr(>|t|)"] >= alpha, ]

#significant employee residences
cat("Significant Employee Residences:\n")
print(significant_residences)

#non significant employee residences
cat("\nNot Significant Employee Residences:\n")
print(not_significant_residences)




qqnorm(resid(multi_model),main='Normal Q-Q plot of Multi Linear Regression')
qqline(resid(multi_model),col='red')

plot(resid(multi_model)-fitted(multi_model),xla='fitted',ylab='residuaks',main = 'residuals vs fitted')
abline(h=0, lty = 'dashed', col = 'red')


plot(multi_model)
#Plotted Multi-model plots below







#Anova Test for MultiLinear regression Model
anova_test <- aov(logSalary ~  WorkYear + ExperienceLevel + EmploymentType + JobTitle + EmployeeResidence + RemoteRatio + CompanyLocatioin + CompanySize, data = data)
anova_test
summary(anova_test)



#F-test
# Fit the linear regression model
model <- lm(logSalary ~  WorkYear + ExperienceLevel + EmploymentType + JobTitle + EmployeeResidence + RemoteRatio + CompanyLocatioin + CompanySize, data = data)

# Anova test
anova_result <- anova(model)

# Display the ANOVA table
print(anova_result)


plot(multi_model)

#Enter Return see the next plots

#1st plot-Residiual vs Fitted
#2nd plot-Normal Q-Q plot
#3rd plot - scale location plot
#4th plot Residual vs Leverage plot




# Thank You.
