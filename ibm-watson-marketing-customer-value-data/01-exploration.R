# IBM Watson Marketing Customer Value Data

## Limpando espaço de trabalho
rm(list = ls())

## Carregando as bibliotecas
library(readr)
library(tidyverse) 
library(car) 
library(zoo)
library(lmtest) 
library(dplyr) 
library(stringr)
library(caret)
library(ggplot2) 
library(timeDate)
## Carregando os dados
set.seed(123)
df <- read_csv("WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv")

## Organizando os nomes dos dados
names(df) <- tolower(gsub(' ', "_", names(df)))
names(df)

## Exploratory Data Analysis (EDA)
head(df)
summary(df)
dim(df)
summary(df)
str(df)

## Tratando os dados para os tipos corretos
df$effective_to_date <- as.Date(df$effective_to_date, "%m/%d/%Y")
df$customer_lifetime_value <- as.numeric(df$customer_lifetime_value)

## Removendo o customerID do dataset
df <- df[,!(names(df) %in% c('customer'))]

## Avaliando valores nulos
na_counts <- sapply(df, function(y) sum(is.na(y)))
na_counts <- data.frame(na_counts)
na_counts

## Avaliando valores únicos em cada feature
sapply(df, data.table::uniqueN)

### Análise descritiva do customer_lifetime_value
range(df$customer_lifetime_value)
mean(df$customer_lifetime_value)
sd(df$customer_lifetime_value)
summary(df$customer_lifetime_value)

var(df$customer_lifetime_value)
skewness(df$customer_lifetime_value)
kurtosis(df$customer_lifetime_value) 

hist(df$customer_lifetime_value, col = "#FF5733", xlab = "CLV")
hist(df$customer_lifetime_value, breaks = (max(df$customer_lifetime_value) - min(df$customer_lifetime_value))/100, freq = FALSE, main = "CLV Histogram", xlab = "CLV", border = "#FF5733")

## Analise descritiva do monthly_premium_auto (MPA)

range(df$monthly_premium_auto)
mean(df$monthly_premium_auto)
sd(df$monthly_premium_auto)
summary(df$monthly_premium_auto)
var(df$monthly_premium_auto)
skewness(df$monthly_premium_auto)
kurtosis(df$monthly_premium_auto)

cor(df$monthly_premium_auto,df$customer_lifetime_value)

hist(df$monthly_premium_auto, col = "#00AFBB", xlab = "Monthly Premium Auto")
hist(df$monthly_premium_auto, breaks = (max(df$monthly_premium_auto) - min(df$monthly_premium_auto))/1, freq = FALSE, main = "Monthly Premium Histogram", xlab = "Monthly Premium", border = "#00AFBB")

plot(x=df$monthly_premium_auto, y=df$customer_lifetime_value, col="#00AFBB", cex=1, xlab="MonthlyPremiumAuto", ylab="CustomerLifetimeValue",
     main="Scatterplot of MPA vs CLV")


### Analise descritiva de Total Claim Amount (TCA)
range(df$total_claim_amount)
mean(df$total_claim_amount)
sd(df$total_claim_amount)
summary(df$total_claim_amount)
var(df$total_claim_amount)
skewness(df$total_claim_amount)
kurtosis(df$total_claim_amount) 
cor(df$total_claim_amount,df$customer_lifetime_value)

hist(df$total_claim_amount, col = "#FC4E07", xlab = "Total Claim Amount")
hist(df$total_claim_amount, breaks = (max(df$total_claim_amount) - min(df$total_claim_amount))/10, freq = FALSE, main = "Total Claim Amount Histogram", xlab = "Total Claim Amount", border = "#FC4E07")

plot(x=df$total_claim_amount, y=df$customer_lifetime_value, col="#FC4E07", cex=1, xlab="TotalClaimAmount", ylab="CustomerLifetimeValue",
     main="Scatterplot of TCA vs CLV")

###  CLV > MPA > TCA

## OUTRAS VAIRÁVEIS

cor(df$income,df$customer_lifetime_value)
plot(x=df$income, y=df$customer_lifetime_value, col="#FC4E07", cex=1, xlab="Income", ylab="CustomerLifetimeValue",main="Scatterplot of Income vs CLV")

cor(df$months_since_last_claim,df$customer_lifetime_value)
plot(x=df$months_since_last_claim, y=df$customer_lifetime_value, col="#FC4E07", cex=1, xlab="MonthsSinceLastClaim", ylab="CustomerLifetimeValue",main="Scatterplot of MonthsSinceLastClaim vs CLV")

cor(df$months_since_policy_inception,df$customer_lifetime_value)
plot(x=df$months_since_policy_inception, y=df$customer_lifetime_value, col="#FC4E07", cex=1, xlab="MonthsSinceLastClaim", ylab="CustomerLifetimeValue",main="Scatterplot of MonthsSincePolicyInception vs CLV")

cor(df$number_of_open_complaints,df$customer_lifetime_value)
plot(x=df$number_of_open_complaints, y=df$customer_lifetime_value, col="#FC4E07", cex=1, xlab="NumberofOpenComplaints", ylab="CustomerLifetimeValue",main="Scatterplot of NumberofOpenComplaints vs CLV")

cor(df$number_of_policies,df$customer_lifetime_value)
plot(x=df$number_of_policies, y=df$customer_lifetime_value, col="#FC4E07", cex=1, xlab="NumberofPolicies", ylab="CustomerLifetimeValue",main="Scatterplot of NumberofPolicies vs CLV")





# Effect of Insurance Coverage on Customer Life Time Value (CLV)



ggplot(df, aes(x=coverage, y= customer_lifetime_value, fill = coverage)) + 
  geom_boxplot() + 
  labs(x="Coverage",y = "Customer Life Time Value", fill="Coverage") + 
  ggtitle("Visualization of CLV wrt Coverage")

aggData <- aggregate(x = df$customer_lifetime_value, by=list(coverage = df$coverage), FUN = sum)
aggData
ggplot(data = aggData, aes(x = coverage, y = prop.table(stat(aggData$x)), fill = coverage, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Coverage', y = 'CLV in Percentage', fill = 'Coverage') + 
  ggtitle("CLV Distribution by Coverage")


# Effect of Education on Customer Life Time Value (CLV)

ggplot(df, aes(x=education, y= customer_lifetime_value, fill = education)) + 
  geom_boxplot() + 
  labs(x="Education",y = "Customer Life Time Value", fill="Education") + 
  ggtitle("Visualization of CLV wrt Education")

aggData <- aggregate(x = df$customer_lifetime_value, by=list(education = df$education), FUN = sum)

ggplot(data = aggData, aes(x = education, y = prop.table(stat(aggData$x)), fill = education, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Education', y = 'CLV in Percentage', fill = 'Education') + 
  ggtitle("CLV Distribution by Education")




dataContinous <- dplyr::select_if(df, ~!is.factor(.))
str(dataContinous)

dim(dataContinous)

trainIndex <- createDataPartition(dataContinous$customer_lifetime_value, p=0.80, list = FALSE)
print(trainIndex)
# 80% Train dataset for regression analysis
insurncTrain <- dataContinous[trainIndex,]
# Remaining 30% Test dataset for testing
insurncTest <- dataContinous[-trainIndex,]

#Regression
#lm is used to fit linear models. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance.

# Creating Linear Regression Model using all the continues indepedent variables.
fit <- lm(insurncTrain$customer_lifetime_value ~., data = insurncTrain) 
summary(fit)

new_fit <- lm(insurncTrain$customer_lifetime_value ~ 
                monthly_premium_auto + number_of_open_complaints + number_of_policies + total_claim_amount, 
              data = insurncTrain) 
summary(new_fit)


predictedCLV <- predict(new_fit)  
#print predicted CLV.
print(predictedCLV[1:10])

#print actual CLV to compare it with above calculated predicted CLV.
print(insurncTrain$customer_lifetime_value[1:10])


residualsCLV <- residuals(new_fit)
print(residualsCLV[1:10])

predicatedTestData=predict(new_fit,insurncTest)
print(predicatedTestData[1:10])


InsuranceTrainData <- cbind(insurncTrain,predictedCLV,residualsCLV)
head(InsuranceTrainData)

ErrorRate <- abs((InsuranceTrainData$customer_lifetime_value - InsuranceTrainData$predictedCLV)/(InsuranceTrainData$customer_lifetime_value)*100)
print(ErrorRate[1:10])

InsuranceTrainData <- cbind(InsuranceTrainData, ErrorRate)
head(InsuranceTrainData)

mean(InsuranceTrainData$ErrorRate, na.rm = TRUE)

hist(ErrorRate, col = "blue")
boxplot(ErrorRate)

shapiro.test(residualsCLV[0:5000])

hist(residualsCLV,col = "green")

plot(new_fit, which=1, col=c("blue"))

cor(InsuranceTrainData) 

# Variance Inflation Factors
car::vif(new_fit)


bptest(new_fit)

dwt(new_fit)


ErrorRate <- mean(abs((InsuranceTrainData$CustomerLifetimeValue - InsuranceTrainData$predictedCLV)/InsuranceTrainData$CustomerLifetimeValue) *100 )
print(ErrorRate)



ggplot(InsuranceTrainData, aes(x = monthly_premium_auto, y = customer_lifetime_value)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +     # regression line  
  geom_segment(aes(xend = monthly_premium_auto, yend = predictedCLV), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residualsCLV), size = abs(residualsCLV))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predictedCLV), shape = 1) +
  theme_bw()












#df_chars <- names(df[, sapply(df, class) == 'character'])
#df_dummy <- fastDummies::dummy_cols(df, select_columns = names(df_chars))
#names(df_dummy)
#df_dummy$response_Yes
#View(cor(df_dummy[, sapply(df_dummy, class) == 'numeric']))
# rm(df_dummy)





# Referências
#- https://rstudio-pubs-static.s3.amazonaws.com/538579_204ebd513ebf4094b7e61897794848b5.html
# - https://www.kaggle.com/pankajjsh06/ibm-watson-marketing-customer-value-data
# - https://www.kaggle.com/dktalaicha/predict-customer-life-time-value-clv
# https://www.qualtrics.com/experience-management/customer/customer-lifetime-value/#:~:text=Customer%20lifetime%20value%20is%20the,great%20way%20to%20drive%20growth.