library(knitr)
library(ggplot2)
library(summarytools)
library(GGally)
library(corrplot)
library(WVPlots)
library(psych)
library(mltools)
library(caret)
library(glmnet)
library(rpart)
library(rfUtilities)
options(warn=-1) #Disable warnings

### Loading required libraries
#Loading data

groups <- read.csv("C://Users/gvega/Downloads/JNotebooks/R_Notebooks/Data/groups.csv", stringsAsFactors = F)
voters <- read.csv("C://Users/gvega/Downloads/JNotebooks/R_Notebooks/Data/voters.csv", stringsAsFactors = F)

groupsId_To_VoterId = function(groupsId,votersDataframe){
  #Check if the groupId has the proper format (A 'V' before the numbers)
  if(substr(groupsId,0,1) != 'V'){
    print("groupsId value is not properly formatter")
  } else{
    #Cutting the group id to have the same format as voterId
    groupsCutID = substr(groupsId,2,100000)
  }
  isGroupIdInVoterId = dim(votersDataframe[which(votersDataframe$userid == groupsCutID),])[1]
  
  return(isGroupIdInVoterId)
  
}

#For the first version I will 'bruteforce' my way through this question
groupVotersInRecord = 0
groupVotersInRecord_NOT = 0
pb <- txtProgressBar(min = 0, max = length(groups$voterid), style = 3)
for(i in seq(1,length(groups$voterid))){
  #Check if each voter in the groups list is in the voters list
  is_voter_in_list = groupsId_To_VoterId(toString(groups$voterid[i]),voters)
  #If it is, add 1 to our count of voters in record
  groupVotersInRecord = groupVotersInRecord + is_voter_in_list
  
  #If the voter is not in the list, we will append to an
  # index list so that we can remove them later
  if(is_voter_in_list == 0){
    groupVotersInRecord_NOT = groupVotersInRecord_NOT + 1
  }
  setTxtProgressBar(pb, i)
}

print(groupVotersInRecord)
print(groupVotersInRecord_NOT)


#Transform the voters id into a zero padded number 
pb <- txtProgressBar(min = 0, max = length(voters$userid), style = 3)
for(i in seq(1,length(voters$userid))){
  voters$userid[i] = formatC(as.numeric(voters$userid[i]), digits = 5, flag = "0")
  setTxtProgressBar(pb, i)
}

#Renaming the dataframes so as to merge them
colnames(groups)[1] <- "ID"
colnames(voters)[1] <- "ID"

completeData = merge(groups, voters, by ="ID")

percentage_of_2016_voters = sum(completeData$voting2006)/length(completeData$voting2006)
print(percentage_of_2016_voters)


pb <- txtProgressBar(min = 0, max = length(completeData$control), style = 3)
groups_with_more_than_1_value = list()
for (i in seq(1,length(completeData$control))){
  civicduty_val = completeData$civicduty[i]
  hawthorne_val = completeData$hawthorne[i]
  self_val = completeData$self[i]
  neighbors_val = completeData$neighbors[i]
  control_val = completeData$control[i]
  
  if(sum(civicduty_val,hawthorne_val,self_val,neighbors_val,control_val) > 1){
    groups_with_more_than_1_value = append(groups_with_more_than_1_value, i)
  }
  setTxtProgressBar(pb, i)
}

#Checking if our function worked
completeData[as.numeric(groups_with_more_than_1_value[1]),]

completeData <- completeData[-c(as.numeric(groups_with_more_than_1_value)),] 

pb <- txtProgressBar(min = 0, max = length(completeData$control), style = 3)

newAgeColumn = list()
for (i in seq(1,length(completeData$control))){
  age = 2006 - completeData$yob[i]
  newAgeColumn = append(newAgeColumn,age)    
  setTxtProgressBar(pb, i)
}

#get the new values into the data
completeData$age = newAgeColumn


#This code is a bit "hacky", this list will be used to 
# calculate the percentages of each ageGroup
Age_18_30 = list()
Age_31_40 = list()
Age_41_50 = list()
Age_51_65 = list()
Age_65_all = list()

pb <- txtProgressBar(min = 0, max = length(completeData$control), style = 3)
for (i in seq(1,length(completeData$control))){
  age = completeData$age[i]
  #This can definetly be automated
  if(age < 31){
    Age_18_30 = append(Age_18_30,1)
    Age_31_40 = append(Age_31_40,0)
    Age_41_50 = append(Age_41_50,0)
    Age_51_65 = append(Age_51_65,0)
    Age_65_all = append(Age_65_all,0)
  }else if (age < 41){
    Age_18_30 = append(Age_18_30,0)
    Age_31_40 = append(Age_31_40,1)
    Age_41_50 = append(Age_41_50,0)
    Age_51_65 = append(Age_51_65,0)
    Age_65_all = append(Age_65_all,0)
  }else if (age < 51){
    Age_18_30 = append(Age_18_30,0)
    Age_31_40 = append(Age_31_40,0)
    Age_41_50 = append(Age_41_50,1)
    Age_51_65 = append(Age_51_65,0)
    Age_65_all = append(Age_65_all,0)
  }else if (age < 65){
    Age_18_30 = append(Age_18_30,0)
    Age_31_40 = append(Age_31_40,0)
    Age_41_50 = append(Age_41_50,0)
    Age_51_65 = append(Age_51_65,1)
    Age_65_all = append(Age_65_all,0)
  }else{
    Age_18_30 = append(Age_18_30,0)
    Age_31_40 = append(Age_31_40,0)
    Age_41_50 = append(Age_41_50,0)
    Age_51_65 = append(Age_51_65,0)
    Age_65_all = append(Age_65_all,1)
  }
  
  setTxtProgressBar(pb, i)
}


percentage_18 = sum(as.numeric(Age_18_30)) / length(completeData$control)
percentage_31 = sum(as.numeric(Age_31_40)) / length(completeData$control)
percentage_41 = sum(as.numeric(Age_41_50)) / length(completeData$control)
percentage_51 = sum(as.numeric(Age_51_65)) / length(completeData$control)
percentage_65 = sum(as.numeric(Age_65_all)) / length(completeData$control)

print(percentage_18)
print(percentage_31)
print(percentage_41)
print(percentage_51)
print(percentage_65)


ageGroup = list()

pb <- txtProgressBar(min = 0, max = length(completeData$control), style = 3)
for (i in seq(1,length(completeData$age))){
  age = completeData$age[i]
  #This can definetly be automated
  if(age < 31){
    ageGroup = append(ageGroup,"Age_18_30")
  } else if (age < 41){
    ageGroup = append(ageGroup,"Age_31_40")
  } else if (age < 51){
    ageGroup = append(ageGroup,"Age_41_50")
  } else if (age < 65){
    ageGroup = append(ageGroup,"Age_51_65")
  }else{
    ageGroup = append(ageGroup,"Age_65_all")
  }
  setTxtProgressBar(pb, i)
}


completeData$age_group = ageGroup


one_hot_encoded_data <- completeData
one_hot_encoded_data$Age_18_30 <- Age_18_30[1:length(Age_18_30)]
one_hot_encoded_data$Age_31_40 <- Age_31_40[1:length(Age_31_40)]
one_hot_encoded_data$Age_41_50 <- Age_41_50[1:length(Age_41_50)]
one_hot_encoded_data$Age_51_65 <- Age_51_65[1:length(Age_51_65)]
one_hot_encoded_data$Age_65_all <- Age_65_all[1:length(Age_65_all)]


#Creating a dataframe to group age_groups (one_hot) and if they voted
AgeGroupsAndVotingDataFrame <- completeData
AgeGroupsAndVotingDataFrame$Age_18_30 <- Age_18_30[1:length(Age_18_30)]
AgeGroupsAndVotingDataFrame$Age_31_40 <- Age_31_40[1:length(Age_31_40)]
AgeGroupsAndVotingDataFrame$Age_41_50 <- Age_41_50[1:length(Age_41_50)]
AgeGroupsAndVotingDataFrame$Age_51_65 <- Age_51_65[1:length(Age_51_65)]
AgeGroupsAndVotingDataFrame$Age_65_all <- Age_65_all[1:length(Age_65_all)]

AgeGroupsAndVotingDataFrame$civicduty <- completeData$civicduty
AgeGroupsAndVotingDataFrame$hawthorne <- completeData$hawthorne
AgeGroupsAndVotingDataFrame$self <- completeData$self
AgeGroupsAndVotingDataFrame$neighbors <- completeData$neighbors
AgeGroupsAndVotingDataFrame$control <- completeData$control
AgeGroupsAndVotingDataFrame$voted2006 <- completeData$voting2006


voters_18 <- as.numeric(AgeGroupsAndVotingDataFrame$Age_18_30) * as.numeric(AgeGroupsAndVotingDataFrame$voted2006)
voters_31 <- as.numeric(AgeGroupsAndVotingDataFrame$Age_31_40) * as.numeric(AgeGroupsAndVotingDataFrame$voted2006)
voters_41 <- as.numeric(AgeGroupsAndVotingDataFrame$Age_41_50) * as.numeric(AgeGroupsAndVotingDataFrame$voted2006)
voters_51 <- as.numeric(AgeGroupsAndVotingDataFrame$Age_51_65) * as.numeric(AgeGroupsAndVotingDataFrame$voted2006)
voters_65 <- as.numeric(AgeGroupsAndVotingDataFrame$Age_65_all) * as.numeric(AgeGroupsAndVotingDataFrame$voted2006)

percentage_18 = sum(as.numeric(voters_18)) / length(completeData$control)
percentage_31 = sum(as.numeric(voters_31)) / length(completeData$control)
percentage_41 = sum(as.numeric(voters_41)) / length(completeData$control)
percentage_51 = sum(as.numeric(voters_51)) / length(completeData$control)
percentage_65 = sum(as.numeric(voters_65)) / length(completeData$control)

print(percentage_18)
print(percentage_31)
print(percentage_41)
print(percentage_51)
print(percentage_65)


#Creating a dataframe to group age_groups (one_hot) and if they voted
group_civic <- AgeGroupsAndVotingDataFrame$civicduty * AgeGroupsAndVotingDataFrame$voted2006
group_hawthorne <- AgeGroupsAndVotingDataFrame$hawthorne * AgeGroupsAndVotingDataFrame$voted2006
group_self <- AgeGroupsAndVotingDataFrame$self * AgeGroupsAndVotingDataFrame$voted2006
group_neighbors <- AgeGroupsAndVotingDataFrame$neighbors * AgeGroupsAndVotingDataFrame$voted2006
group_control <- AgeGroupsAndVotingDataFrame$control * AgeGroupsAndVotingDataFrame$voted2006

percentage_civic = sum(as.numeric(group_civic)) / length(completeData$control)
percentage_hawthorne = sum(as.numeric(group_hawthorne)) / length(completeData$control)
percentage_self = sum(as.numeric(group_self)) / length(completeData$control)
percentage_neighbors = sum(as.numeric(group_neighbors)) / length(completeData$control)
percentage_control = sum(as.numeric(group_control)) / length(completeData$control)

print(percentage_civic)
print(percentage_hawthorne)
print(percentage_self)
print(percentage_neighbors)
print(percentage_control)


sum_vote_18 <- sum(voters_18)
total_18 <- sum(as.numeric(AgeGroupsAndVotingDataFrame$Age_18_30))
sum_vote_31 <- sum(voters_31)
total_31 <- sum(as.numeric(AgeGroupsAndVotingDataFrame$Age_31_40))
sum_vote_41 <- sum(voters_41)
total_41 <- sum(as.numeric(AgeGroupsAndVotingDataFrame$Age_41_50))
sum_vote_51 <- sum(voters_51)
total_51 <- sum(as.numeric(AgeGroupsAndVotingDataFrame$Age_51_65))
sum_vote_65 <- sum(voters_65)
total_65 <- sum(as.numeric(AgeGroupsAndVotingDataFrame$Age_65_all))

stackedValues = list(sum_vote_18, total_18, sum_vote_31, total_31,
                     sum_vote_41, total_41,
                     sum_vote_51, total_51,
                     sum_vote_65, total_65)

ages <- c( rep("18",2), rep("31",2), rep("41",2), rep("51",2),rep("65",2))
condition <- rep(c("voted", "did not vote"),5)
value <- abs(as.numeric(stackedValues))

ggplot_data_stacked_1 <- data.frame(ages,condition,value)

ggplot(ggplot_data_stacked_1, aes(fill=condition, y=value, x=ages)) + 
  geom_bar(position="stack", stat="identity")


sum_civic <- sum(group_civic)
total_civic <- sum(AgeGroupsAndVotingDataFrame$civicduty)
sum_hawthorne <- sum(group_hawthorne)
total_hawthorne <- sum(AgeGroupsAndVotingDataFrame$hawthorne)
sum_self <- sum(group_self)
total_self <- sum(AgeGroupsAndVotingDataFrame$self)
sum_neighbors <- sum(group_neighbors)
total_neighbors <- sum(AgeGroupsAndVotingDataFrame$neighbors)
sum_control <- sum(group_control)
total_control <- sum(AgeGroupsAndVotingDataFrame$control)

stackedValues = list(sum_civic, total_civic,sum_hawthorne, total_hawthorne,
                     sum_self, total_self,
                     sum_neighbors, total_neighbors,
                     sum_control, total_control)

ages <- c( rep("civic",2), rep("hawthorn",2), rep("self",2), rep("neighbors",2),rep("control",2))
condition <- rep(c("voted", "did not vote"),5)
value <- abs(as.numeric(stackedValues))

ggplot_data_stacked_1 <- data.frame(ages,condition,value)

ggplot(ggplot_data_stacked_1, aes(fill=condition, y=value, x=ages)) + 
  geom_bar(position="fill", stat="identity")


unlisted_age_group = unlist(ageGroup)

completeData$unlisted_age_group = unlisted_age_group

LogisticRegression <- glm(voting2006 ~ civicduty + hawthorne + self + neighbors + control + unlisted_age_group, data = completeData, family = "binomial")

summary(LogisticRegression)

confint(LogisticRegression)

LogisticRegression_probabilities = predict(LogisticRegression,newdata = completeData)

count = 0
for(i in seq(1,length(LogisticRegression_probabilities))){
  if(LogisticRegression_probabilities[i] > 0.5){
    if(completeData$voting2006[i] == 1){
      count = count + 1
    }
  } else{
    if(completeData$voting2006[i] == 0){
      count = count + 1
    }
  }
}
print(count / length(LogisticRegression_probabilities))

confMat <- table(completeData$voting2006,LogisticRegression_probabilities)
accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy) # This value 


library(pROC)
roc_obj <- roc(completeData$voting2006, LogisticRegression_probabilities)
auc(roc_obj)

pROC_obj <- roc(completeData$voting2006,LogisticRegression_probabilities,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")

## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.

plot(sens.ci, type="bars")


#Grow the tree
Tree_model = rpart(voting2006 ~ civicduty + hawthorne + self + neighbors + control + unlisted_age_group, data = completeData, method="class")

printcp(Tree_model) # display the results
plotcp(Tree_model) # visualize cross-validation results
summary(Tree_model) # detailed summary of splits

# plot tree
plot(Tree_model, uniform=TRUE,
     main="Classification Tree")
text(Tree_model, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(Tree_model, file = "/tree.ps",
     title = "Classification Tree")


tree_model_preds = predict(Tree_model,newdata = completeData, type = "class")

accuracy(table(completeData$voting2006,tree_model_preds))

confMat <- table(completeData$voting2006,tree_model_preds)
accuracy_tree <- sum(diag(confMat))/sum(confMat)
print(accuracy_tree)


