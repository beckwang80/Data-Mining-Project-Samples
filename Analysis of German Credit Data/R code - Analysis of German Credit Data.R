German.Credit <- read.csv("german_credit.csv", header = TRUE, sep = ",")

# Creating marginal proportional tables from a K1 x K2 x K3 x … x Kl contingency table.
# marginal proportions from a 8-level contingency table

margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),1)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),2)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),3)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),4)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),5)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),6)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),7)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),8)

# creating K1 x K2 contingency table.

CrossTable(Creditability, Account.Balance, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(Creditability, Payment.Status.of.Previous.Credit, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(Creditability, Purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

# Descriptive Statistics.

attach(German.Credit) # If the data frame is attached then the column names may be directly called
summary(Duration.of.Credit..Month) # Summary statistics are printed for this variable
brksCredit <- seq(0, 80, 10) # Bins for a nice looking histogram
hist(Duration.of.Credit.Month., breaks=brksCredit, xlab = "Credit Month", ylab = "Frequency", main = " ", cex=0.4) # produces nice looking histogram
boxplot(Duration.of.Credit.Month., bty="n",xlab = "Credit Month", cex=0.4) # For boxplot

# Discriminant Analysis.

library(MASS)
ldafit <- lda(Creditability ~ Value.Savings.Stocks + Length.of.current.employment + Duration.of.Credit..month.+ Credit.Amount + Age..years., data = Train50)
ldafit
plot(ldafit)
lda.pred <- predict(ldafit, data=Test50)
ldaclass <- lda.pred$class
table(ldaclass, Test50$Creditability)

qdafit <- qda(Creditability ~ Value.Savings.Stocks + Length.of.current.employment + Duration.of.Credit..month.+ Credit.Amount + Age..years., data = Train50)
qdafit
qda.pred <- predict(qdafit, data=Test50)
qdaclass <- qda.pred$class
table(qdaclass, Test50$Creditability)

# Tree method.
library(tree)
Train50_tree <- tree(Creditability ~ Account.Balance+Duration.of.Credit..month.+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Guarantors+Duration.in.Current.address+Most.valuable.available.asset+Age..years.+Concurrent.Credits+Type.of.apartment+No.of.Credits.at.this.Bank+Occupation+No.of.dependents+Telephone, data=Train50, method="class")
summary(Train50_tree)
plot(Train50_tree)
text(Train50_tree, pretty=0,cex=0.6)
Test50_pred <- predict(Train50_tree, Test50, type="class")
table(Test50_pred, Test50$Creditability)

Train50_prune8 <- prune.misclass(Train50_tree, best=8)
Test50_prune8_pred <- predict(Train50_prune8, Test50, type="class")
table(Test50_prune8_pred, Test50$Creditability))

# Random Forest.
library(randomForest)
rf50 <- randomForest(Creditability ~., data = Train50, ntree=200, importance=T, proximity=T)
plot(rf50, main="")
rf50
Test50_rf_pred <- predict(rf50, Test50, type="class")
table(Test50_rf_pred, Test50$Creditability)
importance(rf50)
varImpPlot(rf50,  main="", cex=0.8)

# 50:50 cross-validation data creation.
indexes = sample(1:nrow(German.Credit), size=0.5*nrow(German.Credit)) # Random sample of 50% of row numbers created
Train50 <- German.Credit[indexes,] # Training data contains created indices
Test50 <- German.Credit[-indexes,] # Test data contains the rest
# Using any proportion, other than 0.5 above and size Training and Test data can be constructed

# Logistic Model building with Training data and assessing for Test data

LogisticModel50 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Value.Savings.Stocks + Length.of.current.employment + Sex...Marital.Status + Most.valuable.available.asset + Type.of.apartment + Concurrent.Credits + Duration.of.Credit..month.+ Credit.Amount + Age..years., family=binomial, data = Train50)
LogisticModel50final <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family=binomial, data = Train50)
fit50 <- fitted.values(LogisticModel50S1)
Threshold50 <- rep(0,500)
for (i in 1:500)
  if(fit50[i] >= 0.5) Threshold50[i] <- 1

CrossTable(Train50$Creditability, Threshold50, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=F, data=Train50)

perf <- performance(pred, "tpr", "fpr")
plot(perf)