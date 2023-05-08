plyoff <- read.csv("C:\\Users\\colli\\OneDrive\\Documents\\game.csv")
game_team_stats <- read.csv("C:\\Users\\colli\\OneDrive\\Documents\\game_teams_stats.csv")

############Use nhl_data to extract whether a game was in regular season or playoff#
plyoff$RP <- ifelse(grepl("R", plyoff$type, ignore.case = TRUE), 1, 0)

###Look at frequency of regular season games and playoff games#
library(ggplot2)
ggplot(plyoff, aes(x = type)) + 
  geom_bar(fill = "orange", color = "white") +
  ggtitle("Frequency of Game Types") +
  xlab("Game Type") +
  ylab("Count")

#Keep only game ID and RP#
plyoff <- plyoff[,c(1,16)]

#Remove coach variable, convert rinkside and HoA and settled_in variables to categorical

game_team_stats$HoA <- ifelse(grepl("home", game_team_stats$HoA, ignore.case = TRUE), 1, 0)
game_team_stats$settled_in <- ifelse(grepl("REG", game_team_stats$settled_in, ignore.case = TRUE), 1, 0)
game_team_stats$startRinkSide <- ifelse(grepl("left", game_team_stats$startRinkSide, ignore.case = TRUE), 1, 0)

game_team_stats <- game_team_stats[,-6]

#Merge the two datasets together and remove game_id#
library(dplyr)
nhl_data <- left_join(game_team_stats, plyoff, by = "game_id")
nhl_data <- nhl_data[,-1]



library(tidyverse)
library(lubridate)
# summary statistics
summary(nhl_data)

# visualize distribution of variables
ggplot(nhl_data, aes(x = goals)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  ggtitle("Distribution of Goals") +
  xlab("Goals") +
  ylab("Count")

ggplot(nhl_data, aes(x = shots)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  ggtitle("Distribution of Shots") +
  xlab("Shots") +
  ylab("Count")


#########Investigate missing variables#############

colSums(is.na(nhl_data))


#######Most missing variables come from faceOffWinPercentage#######
complete_rows <- complete.cases(nhl_data$faceOffWinPercentage)

# subset the data frame to include only complete rows for variable 'y'
fowpct_complete <- nhl_data[complete_rows, ]
#find mean of faceOffWinPercentage#
mean(fowpct_complete$faceOffWinPercentage)
#Set missing observations for face off win percentage to mean of non-missing observations#
nhl_data$faceOffWinPercentage <- ifelse(is.na(nhl_data$faceOffWinPercentage), mean(fowpct_complete$faceOffWinPercentage), nhl_data$faceOffWinPercentage)

##### hits, giveaways, blocked, and takeaways are all missing the same 4928 observations, these observations will be removed#
# Subset the data to include only the rows that have complete data for the variable of interest
nhl_data <- nhl_data[complete.cases(nhl_data[, "hits"]), ]

######Rechecking missing values, all are not missing now#######
colSums(is.na(nhl_data))

# investigate correlations between variables
cor(nhl_data[,c("goals", "won", "settled_in", "shots", "hits", "pim", 
                "powerPlayOpportunities", "powerPlayGoals", "faceOffWinPercentage", "giveaways",
                "takeaways", "blocked", "startRinkSide", "RP")])

library(corrplot)

# Create a correlation matrix of your data
corr_matrix <- cor(nhl_data, use = "pairwise.complete.obs")

# Plot the correlation matrix using corrplot
corrplot(corr_matrix, method = "color")
######StartRinkSide has no correlation with won, will be dropped from model#
nhl_data <- nhl_data[, -15]



# Split data into training and testing sets

nhl_data$won <- as.numeric(nhl_data$won)


set.seed(941)
filterds <- sample(
  x = c("train","test"),
  size = nrow(nhl_data),
  replace = TRUE,
  prob = c(0.7,0.3)
)
part <- split(
  x = nhl_data,
  f = filterds
)
nhl_train <- as.data.frame(part$train)
nhl_test <- as.data.frame(part$test)


# Perform XGBoost on data
install.packages("xgboost")
library(xgboost)

X <- nhl_train[, c(1:2,4:15)]
Y <- as.numeric(nhl_train$won)
X <- as.matrix(X)
set.seed(941)

xgb_model <- xgboost(data = X, label = Y, nrounds = 100, objective = "binary:logistic")
library(caret)
test_X <- as.matrix(nhl_test[, c(1:2,4:15)])
test_Y <- as.numeric(nhl_test$won)

xgb_pred <- predict(xgb_model, newdata = test_X)


# Random Forest
library(randomForest)
rf_model <- randomForest(won ~ ., data = nhl_train, ntree = 500)
rf_pred <- predict(rf_model, newdata = nhl_test)

# SVM
library(e1071)
svm_model <- svm(won ~ ., data = nhl_train, kernel = "linear")
svm_pred <- predict(svm_model, newdata = nhl_test)

# Calculate RMSE
svm_rmse <- sqrt(mean((nhl_test$won - svm_pred)^2))
rf_rmse <- sqrt(mean((nhl_test$won - rf_pred)^2))
xgb_rmse <- sqrt(mean((nhl_test$won - xgb_pred)^2))
