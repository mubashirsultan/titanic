# titanic
library(openintro)
library(tidyr)
library(ggplot2)
library(caret)
library(dplyr)
library(plotly)
library(stringr)


titanic <- read.csv("/Users/mubashirsultan/Documents/R/Datasets/train.csv")

#Explore data
str(titanic)
summary(titanic)

#Lets begin to explore Gender
table(titanic$Sex)

ggplot(titanic, aes(x = Sex, fill=Sex)) +
  geom_bar(position = "dodge") +
  ggtitle("Number of men and women on the ship")

"Out of 891 passengers, 577 passengers were males and 314 were females."

#Number of people survived

table(titanic$Survived)

# Change variable Survived values to yes and no.
titanic$Survived <- factor(titanic$Survived, levels = c(0,1), labels = c("No", "Yes"))

ggplot(titanic, aes(x=Survived, fill=Survived)) +
  geom_bar(position = "dodge") +
  ggtitle("Number of People Survived")

"Out of 891 passengers, 342 survived and the others couldn't make it."

########################################################################################################

# Survived vs Dead by Gender
table(titanic$Sex, titanic$Survived)

ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge")

ggplot(titanic, aes(x = Sex, fill=Survived)) +
  geom_bar(position = "fill")

"It looks like females were the being rescued first. More females were rescued than males. 
Out of 342 survived passengers, 233 passengers were females."

########################################################################################################

# Survived men and women
tab1 <- titanic %>%
  group_by(Sex) %>%
  filter(Survived =="Yes") %>%
  summarize(Total = n()) %>%
  mutate(pct = Total/sum(Total))


plot_ly(tab1, labels = ~Sex, values = ~pct, type = 'pie', textposition = 'inside', textinfo = 'label+percent') %>%
  layout(title = 'Percentage of men and women survived',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

"68% of survived passengers were females."

########################################################################################################

#Survived vs Dead by Class
table(titanic$Pclass)

# Change variable Pclass labels names because why not! And change it to facotr variable
titanic$Pclass <- factor(titanic$Pclass, levels = c(1,2,3), labels = c("First", "Second", "Third"))

ggplot(titanic, aes(x = Pclass)) +
  geom_bar(position = "dodge")

"There were more passengers in Third class compared to First and Sceond class combined."

table(titanic$Pclass, titanic$Survived)

ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge")

ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill")

"More than 60% of First class passengers were rescued."

tab2 <- titanic %>%
  group_by(Pclass) %>%
  filter(Survived == "Yes") %>%
  summarize(Total = n()) %>%
  mutate(pct = Total/sum(Total))

plot_ly(tab2, labels = ~Pclass, values = ~pct, type = 'pie', textposition = 'inside', textinfo = 'label+percent') %>%
  layout(title = 'Percentage of passengers survived by class',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

"Almost 40% of survived passengers were from First class." 
"Does this mean passengers from 1st class are given the first priority? Lets find out!"

########################################################################################################

#Class vs Gender
table(titanic$Pclass, titanic$Sex)

ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Sex)

"It doesn't matter what Pclass the passengers were from, females were given the first priority."

########################################################################################################

# There are missing values in variables Age, Embarked and Cabin. Lets fill the missing values!

# Extract Title from names to fill missing values in variable Age with the median of Title
titanic$Title <- str_extract(titanic$Name, "([A-Z][a-z]++)\\.")
titanic$Title <- factor(titanic$Title)
table(titanic$Title, exclude=NULL)

# Turns out there are 17 different prefixes. Values Mlle. and Mme. look like typos.
# Lets cut down prefixes from 17 to simple 6 categories (Mr., Mrs., Miss., Master., Royalty, and Others)

Others <- c("Capt.", "Col.", "Don.", "Dr.", "Major.", "Rev.")

Royalty <- c("Countess.", "Jonkheer.", "Lady.", "Sir.")

#Before we fix Title, lets unfacotrize the variable to character variable
titanic$Title <- as.character(titanic$Title)

# Lets fix the typos first
titanic$Title[titanic$Title == 'Mlle.'] <- 'Miss.' 
titanic$Title[titanic$Title == 'Mme.'] <- 'Mrs.' 
titanic$Title[titanic$Title == 'Ms.'] <- 'Mrs.' 


#Lets change other values
titanic$Title[titanic$Title %in% Royalty] <- 'Royalty' 
titanic$Title[titanic$Title %in% Others] <- 'Others' 

# Lets factorize Title variable
titanic$Title <- factor(titanic$Title)

table(titanic$Title)

#Impute missing age values with subset of Title median
impute.median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
titanic <- titanic %>%
  group_by(Title) %>%
  mutate(Age = impute.median(Age))


#Title vs Survived
table(titanic$Title, titanic$Survived)

ggplot(titanic, aes(x = Title, fill = Survived)) +
  geom_bar(position = "dodge")

"This graph confirms that women and children were rescued first."

#Survived vs Dead by Age

ggplot(titanic, aes(x=Age)) +
  geom_histogram(binwidth = 1) 

ggplot(titanic, aes(x=Age, fill= Survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Sex)

ggplot(titanic, aes(x = Age, fill=Survived)) +
  geom_density(alpha = .3) +
  facet_wrap(~Sex)

"As all ages of females were given the first priority, males below the age of 18 were alos given first priority.
30 years old men were the unlukiest group out of everybody. Less than 25 were rescued and more than 125 died."

########################################################################################################

# Fill missing values of variable Embarked with S since a lot of passengers boarded from Port S
titanic$Embarked <- sub("^$", "S", titanic$Embarked)

#Convert the variable to factor variable
titanic$Embarked <- factor(titanic$Embarked)

table(titanic$Embarked)

ggplot(titanic, aes(x = Embarked)) +
  geom_bar(position = "dodge")

"Out of 891 passengers, more than 600 boarded from Southampton."

#Survived vs Dead by Embarkment
table(titanic$Embarked, titanic$Survived)

ggplot(titanic, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge")

ggplot(titanic, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "fill")

"2/3 of survived passenger boarded from Southhampton. Does this mean majority of women and children on the ship boarded from Southampton? Lets find out."

#Sex by Embarkment
table(titanic$Embarked, titanic$Sex)

ggplot(titanic, aes(x = Embarked, fill = Sex)) +
  geom_bar(position = "dodge")

ggplot(titanic, aes(x = Embarked, fill = Sex)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Survived)

ggplot(titanic, aes(x = Embarked, fill = Sex)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Survived + Title)

"Yes! Majority of women and children boarded from Southampton."

########################################################################################################

#Feature Enginnering
#Add family size

titanic$FamilySize <- titanic$SibSp + titanic$Parch + 1

# Add new variable called Family1
titanic$Family1 <- NA

titanic$Family1[titanic$FamilySize>1 & titanic$FamilySize<5] <- "SmallFamily"
titanic$Family1[titanic$FamilySize>=5 ] <- "BigFamily"
titanic$Family1[titanic$FamilySize==1] <- "Single"

table(titanic$Family1)
table(titanic$Family1, titanic$Survived)

ggplot(titanic, aes(x = Family1, fill = Survived)) +
  geom_bar(position = "dodge") 

ggplot(titanic, aes(x = Family1, fill = Survived)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Sex)

"Out of 3 family groups, small families and single people had the higher chance to survive."
"This makes sense as bigger families dont want to leave out their loved once behind."

########################################################################################################

# Add a new variable called mother

titanic$Mother <- NA

titanic$Mother[titanic$Title == "Mrs." & titanic$Parch>0] <- "Mother"
titanic$Mother[titanic$Title != "Mrs." | titanic$Parch==0] <- "NotMother"

table(titanic$Mother)
table(titanic$Mother, titanic$Survived)

ggplot(titanic, aes(x = Mother, fill = Survived)) +
  geom_bar(position = "dodge") 

"Most of the mothers were rescued."

# Add a new variable called Child1

titanic$Child1 <- NA
titanic$Child1[titanic$Age < 18] <- "Child"
titanic$Child1[titanic$Age >= 18] <- "Adult"

table(titanic$Child1)
table(titanic$Child1, titanic$Survived)

ggplot(titanic, aes(x = Child1, fill = Survived)) +
  geom_bar(position = "dodge")

"Out of 117 children, only 63 were rescued. Lets find out why."

table(titanic$Family1, titanic$Child1, titanic$Survived)

ggplot(titanic, aes(x = Child1, fill = Survived)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Family1)

"Looks like children from larger families didnt want to leave their siblings behind."
"50 of the rescued children were from smaller families."

# Lets factorize Family variable
titanic$Family1 <- factor(titanic$Family1)

# Lets factorize Mother variable
titanic$Mother <- factor(titanic$Mother)

# Lets factorize Child1 variable
titanic$Child1 <- factor(titanic$Child1)

########################################################################################################

# Four different models were developed for this data set: 
    # basic classification tree (rpart)
    # bagged classification tree (treebag)
    # random forest (rf)
    # stochastic gradient boosting model (gbm).

# Create dummy variable for factor variables
titanic <- data.frame(titanic)
Survived <- factor(titanic[,2])
titanic1 <- titanic[,-c(1,4,9:11,14)]
dummyInfo <- dummyVars(~ . , data=titanic1[,-1])
titanic1 <- predict(dummyInfo, titanic1[,-1])
titanic1 <- data.frame(titanic1)
titanic1$Survived <- Survived

set.seed(49)
trainIndex <- createDataPartition(titanic1$Survived, p=0.8, list=FALSE)
head(trainIndex)

trainData <- titanic1[trainIndex,]
testData  <- titanic1[-trainIndex,]

########################################################################################################

library(snow)
library(doSNOW)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

########################################################################################################

##  CART
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

set.seed(50)
rpartOut <- train(trainData[,-grep('Survived', names(trainData))],
                  trainData[,grep('Survived', names(trainData))],
                  method = "rpart", tuneLength = 10, metric='ROC',
                  trControl = fitControl)
stopCluster(cl)

rpartOut

rpartPred <- predict(rpartOut, testData)

confusionMatrix(data = rpartPred, 
                reference = testData$Survived)

########################################################################################################

##  Bagged CART
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

set.seed(50)
treeBagOut <- train(trainData[,-grep('Survived', names(trainData))],
                    trainData[,grep('Survived', names(trainData))],
                    method = "treebag", metric='ROC',
                    trControl = fitControl)
stopCluster(cl)

treeBagOut

save(treeBagOut, file='treeBagOut.RData')
load('treeBagOut.RData')

treeBagPred <- predict(treeBagOut, testData)

confusionMatrix(data = treeBagPred, 
                reference = testData$Survived)

########################################################################################################

#  Random Forest
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

set.seed(50)
rfOut <- train(trainData[,-grep('Survived', names(trainData))],
               trainData[,grep('Survived', names(trainData))],
               method = "rf", tuneLength = 9, metric='ROC',
               trControl = fitControl)
stopCluster(cl)

rfOut

save(rfOut, file='rfOut.RData')
load('rfOut.RData')
rfPred <- predict(rfOut, testData)

confusionMatrix(data = rfPred, 
                reference = testData$Survived)

########################################################################################################

cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

gbmGrid <- expand.grid(interaction.depth = c(1,5,9),
                       n.trees = seq(100, 1500, by = 100),
                       shrinkage = c(0.1),
                       n.minobsinnode=c(10))

set.seed(50)
gbmOut <- train(trainData[,-grep('Survived', names(trainData))],
                trainData[,grep('Survived', names(trainData))],
                method = "gbm", metric='ROC', tuneGrid = gbmGrid,
                trControl = fitControl)

stopCluster(cl)

gbmOut

save(gbmOut, file='gbmOut.RData')
load('gbmOut.RData')
gbmPred <- predict(gbmOut, testData)

confusionMatrix(data = gbmPred, 
                reference = testData$Survived)

########################################################################################################

postResample(rpartPred, testData$Survived)
postResample(treeBagPred, testData$Survived)
postResample(rfPred, testData$Survived)
postResample(gbmPred, testData$Survived)

rpartProb <- predict(rpartOut, testData, type='prob')
treeBagProb <- predict(treeBagOut, testData, type='prob')
rfProb <- predict(rfOut, testData, type='prob')
gbmProb <- predict(gbmOut, testData, type='prob')

library(AUC)
rpartAuc <- auc(roc(rpartProb$Yes, testData$Survived))
treeBagAuc <- auc(roc(treeBagProb$Yes, testData$Survived))
rfAuc <- auc(roc(rfProb$Yes, testData$Survived))
gbmAuc <- auc(roc(gbmProb$Yes, testData$Survived))

barchart(c(rpart=rpartAuc, 
           treeBag=treeBagAuc,
           rf=rfAuc,
           gbm=gbmAuc),
         xlim=c(0,1))

plot(rpartOut)
plot(treeBagOut)
plot(rfOut)
plot(gbmOut)

"Out of all 4 models, Random Forest is the best model with an accuracy 81.92%."

