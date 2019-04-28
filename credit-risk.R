library(readxl)
CreditRiskDataSet <- read_excel("CreditRiskDataSet.xlsx", range = "A3:L428")
summary(CreditRiskDataSet)
#View(CreditRiskDataSet)

library(e1071)
#attach(CreditRiskDataSet)

CreditRiskDataSet$Gender <- factor(CreditRiskDataSet$Gender)
CreditRiskDataSet$`Loan Purpose` <- factor(CreditRiskDataSet$`Loan Purpose`)
CreditRiskDataSet$`Marital Status` <- factor(CreditRiskDataSet$`Marital Status`)
CreditRiskDataSet$Housing <- factor(CreditRiskDataSet$Housing)
CreditRiskDataSet$`Credit Risk` <- factor(CreditRiskDataSet$`Credit Risk`)

#set.seed(4)

library(Rcmdr)
reliability(cov(CreditRiskDataSet[,c("Checking", "Savings", "Months Customer", "Months Employed", "Age", "Years")]))

library(corrplot)
corrplot(cor(CreditRiskDataSet[,c("Checking", "Savings", "Months Customer", "Months Employed", "Age", "Years")]), method = "ellipse")

maxValue <- apply(CreditRiskDataSet[,c("Checking", "Savings", "Months Customer", "Months Employed", "Age", "Years")], 2, max)
minValue <- apply(CreditRiskDataSet[,c("Checking", "Savings", "Months Customer", "Months Employed", "Age", "Years")], 2, min)
CreditRiskDataSet[,c("Checking", "Savings", "Months Customer", "Months Employed", "Age", "Years")] <- as.data.frame(scale(CreditRiskDataSet[,c("Checking", "Savings", "Months Customer", "Months Employed", "Age", "Years")], center = minValue, scale = maxValue - minValue))

ind <- sample(2, nrow(CreditRiskDataSet), replace = TRUE, prob = c(0.7, 0.3))
train.data <- CreditRiskDataSet[ind == 1, ]
test.data <- CreditRiskDataSet[ind == 2, ]

log.model<-glm(`Credit Risk` ~ `Loan Purpose` + Checking + Savings + `Months Customer` + `Months Employed` + Gender + `Marital Status` + Age + Housing + Years + Job, data = train.data, family = "binomial")
summary(log.model)

ypred <- predict(log.model, test.data)
str(ypred)

ypredbin <- ifelse(ypred > 0.5, 1,0)

library(gmodels)
CrossTable(test.data$`Credit Risk`, ypredbin)
tab <- table(Predicted = ypredbin, Actual = test.data$`Credit Risk`)

library(MASS)

step <- stepAIC(log.model, direction = "backward")
step$anova

log.model <- glm(`Credit Risk` ~ Checking + Savings + `Months Customer`  + `Marital Status`+ Job, data = train.data, family = "binomial")
summary(log.model)

ypred <- predict(log.model, test.data)
str(ypred)
ypredbin <- ifelse(ypred > 0.5, 1,0)
CrossTable(test.data$`Credit Risk`, ypredbin)
tab <- table(Predicted = ypredbin, Actual = test.data$`Credit Risk`)
1-sum(diag(tab))/sum(tab)

#confusion matrix
table(test.data$`Credit Risk`, ypred > 0.5)
