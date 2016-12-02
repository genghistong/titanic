setwd("C:/Users/Jonny/titanic")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

test$Survived <- NA
combi <- rbind(train,test)

combi$Name <- as.character(combi$Name)
combi$Sex <- factor(combi$Sex)

#Ticket
combi$TicketNum <- gsub("[[:digit:]]","",combi$Cabin)
combi$TicketNum[combi$TicketNum ==''] = "FALSE"
combi$TicketNum[combi$TicketNum != 'FALSE'] = "TRUE"
combi$TicketNum <- factor(combi$TicketNum)

#Cabin
combi$CabinLetter <- gsub("[[:digit:]]","",combi$Cabin)
combi$CabinLetter[combi$CabinLetter ==''] = "No Cabin"
combi$CabinLetter <- factor(combi$CabinLetter)

combi$CabinNumber <- gsub("[[:alpha:]]","",combi$Cabin)

combi$NumOfCabin <- sapply(gregexpr("[[:digit:]]+", combi$CabinNumber), function(x) sum(x > 0))
combi$NumOfCabin <- factor(combi$NumOfCabin)

combi$Embarked[combi$Embarked == ''] = "S"

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)


Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
			data = combi[!is.na(combi$Age),],
			method= "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Embarked[combi$Embarked == ''] = "S"
combi$Embarked <- factor(combi$Embarked)

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]

write.csv(train, file = "transformed_train.csv", row.names = FALSE)

call_random_forests <- function(factor, variables, train, test, importance = TRUE, ntree = 2000, 
		seed = 415)
	{set.seed(seed)	 
	 fit <- randomForest(as.factor(factor) ~ variables, train = train, importance = importance, ntree = tree)
	 varImpPlot(fit)
	 prediction <- predict(fit, test)
	}

call_random_forests(factor = "Survived", variables = "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2",
		train = train, test = test)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                       Embarked + Title + FamilySize + FamilyID + NumOfCabin + TicketNum,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
varImpPlot(fit)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = 'submit.csv', row.names = FALSE)

for(i in check){
 pasted <- paste(i, "+", sep = " ")
 print(pasted)}


