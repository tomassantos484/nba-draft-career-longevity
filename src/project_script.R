#NBA Draft Dataset Project
#Date: Dec 19th, 2025
#Author: Tomas Santos Yciano

#Installing and importing necessary libraries for analysis
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")

library(arules)
library(arulesViz)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

#Loading NBA Draft dataset, in my downloads folder for simplicity
nbaDraftOriginal <- read.csv(file="/Users/themi/Downloads/nbaplayersdraft.csv")

#Review of dataset
head(nbaDraftOriginal)
summary(nbaDraftOriginal)
str(nbaDraftOriginal)

#Cleaning Section

#Removing ID and rank column
nbaDraftCleaned <- nbaDraftOriginal %>%
  select(-id, -rank)

#Removing all NAs in years active column (players who never played in NBA)
nbaDraftCleaned <- nbaDraftCleaned[!is.na(nbaDraftCleaned$years_active), ]

#Checking NAs in college column
sum(is.na(nbaDraftCleaned$college)) # Returns 0

#Checking empty values in college column (players who didn't attend college in the US)
sum(nbaDraftCleaned$college == "") # Returns 245

#Converting empty strings to NAs in college column
nbaDraftCleaned$college[trimws(nbaDraftCleaned$college) == ""] <- NA

#Checking NAs in college column again
sum(is.na(nbaDraftCleaned$college)) # Returns 245, same as empty values before, coversion complete

#Renaming columns for standardization and ease of use for incoming analysis
nbaDraftCleaned <- nbaDraftCleaned %>%
  rename(
    draft_year = year,
    draft_team = team,
    player_name = player,
    total_games_played = games,
    total_minutes_played = minutes_played,
    total_points = points,
    total_assists = assists,
    fg_pct = field_goal_percentage,
    fg3_pct = X3_point_percentage,
    ft_pct = free_throw_percentage,
    mpg = average_minutes_played,
    ppg = points_per_game,
    rpg = average_total_rebounds,
    apg = average_assists,
    ws_per_48 = win_shares_per_48_minutes,
    bpm = box_plus_minus,
    vorp = value_over_replacement
  )

## Standardizing team abbreviations for consistency with present-day franchises
team_map <- c(
  #New Jersey Nets (NJN) to Brooklyn Nets (BRK)
  "NJN" = "BRK",
  "BRK" = "BRK",
  
  #Washington Bullets (WSB) to Washington Wizards (WAS)
  "WSB" = "WAS",
  "WAS" = "WAS",
  
  #Vancouver Grizzlies (VAN) to Memphis Grizzlies (MEM)
  "VAN" = "MEM",
  "MEM" = "MEM",
  
  #Seattle SuperSonics (SEA) to Oklahoma City Thunder (OKC)
  "SEA" = "OKC",
  "OKC" = "OKC",
  
  #Old Charlotte Hornets 1988-2002 (CHH) + New Orlearns Hornets 2002-2005, 2007-2013 (NOH) + New Orleans/OKC Hornets (NOK) 2005-2007 to New Orleans Pelicans 2013-present (NOP)
  "CHH" = "NOP",
  "NOH" = "NOP",
  "NOK" = "NOP",
  "NOP" = "NOP",
  
  #Charlotte Bobcats 2004-14 (CHA) to Charlotte Hornets 2014 - present (CHO)
  "CHA" = "CHO",
  "CHO" = "CHO"
)

#Mapping present-day abbreviations across dataset
rowsToChange <- nbaDraftCleaned$draft_team %in% names(team_map)
nbaDraftCleaned$draft_team[rowsToChange] <- unname(team_map[nbaDraftCleaned$draft_team[rowsToChange]])

# Mapping NBA Teams by conference (Eastern or Western)
nba_eastern_conference <- nba_eastern_conference <- c(
  "ATL", "BOS", "BRK", "CHO", "CHI",
  "CLE", "DET", "IND", "MIA", "MIL",
  "NYK", "ORL", "PHI", "TOR", "WAS"
)

nba_western_conference <- nba_western_conference <- c(
  "DAL", "DEN", "GSW", "HOU", "LAC",
  "LAL", "MEM", "MIN", "NOP", "OKC",
  "PHO", "POR", "SAC", "SAS", "UTA"
)
  
#Creating educational prestige identifier for how prestigious a player's pathway is

#Approach:

#PowerSchool =" Power Schools" in US (Top 10 College Basketball Programs historically per CBS article)
#OtherCollege = Other college basketball programs in the US
#NonCollegiate = All other educational pathways (High school, international, G League)

#List of "Power Schools" in US (Top 10 College Basketball Programs historically)
power_schools <- c("Kentucky", "North Carolina", "Duke", "UCLA", "Kansas",
                   "Louisville", "Indiana", "UConn", "Villanova", "Cincinnati")

#Using dplyr library to map educational prestige across all draft picks
nbaDraftCleaned$educational_prestige <- dplyr::case_when(
  nbaDraftCleaned$college %in% power_schools ~ "PowerSchool",
  is.na(nbaDraftCleaned$college) ~ "NonCollegiate",
  TRUE ~ "OtherCollege"
)


#Bin draft picks in buckets by positioning 
nbaDraftCleaned$pick_bucket <- cut(
  nbaDraftCleaned$overall_pick,
  breaks = c(0, 5, 14, 30, 60),
  labels = c("Top5", "Lottery", "MidtoLateFirst", "SecondRound")
)

#Bin career lengths in buckets by number of games played
nbaDraftCleaned$career_bucket <- cut(
  nbaDraftCleaned$total_games_played,
  breaks = c(-Inf, 199, 449, Inf), #Infs included to ensure capture of lowest and highest games played in dataset
  labels = c("ShortCareer", "AverageCareer", "LongCareer")
)

#Mapping teams to their respective conference
nbaDraftCleaned$team_conference <- dplyr::case_when(
  nbaDraftCleaned$draft_team %in% nba_eastern_conference ~ "Eastern",
  nbaDraftCleaned$draft_team %in% nba_western_conference ~ "Western",
  TRUE ~ NA_character_
)

####

#Analysis 1: Apriori

#New dataframe for Apriori Analysis
nbaDraftApriori <- nbaDraftCleaned


#Remove useless columns for Apriori (keep overall pick, total games played, and conference of team that drafted them)
nbaDraftApriori <- nbaDraftApriori %>%
  select(-player_name, -draft_year, -college, -years_active, -total_minutes_played, -total_points, 
         -total_rebounds, -total_assists, -fg_pct, -fg3_pct, -ft_pct, -mpg, 
         -ppg, -rpg, -apg,-win_shares, -ws_per_48, -bpm, -vorp)

#Preparing dataset for Apriori

#Saving copy of dataset before factorizing
nbaDraftAprioriTEST <- nbaDraftApriori

#Converting dataset items into factors as a list
nbaDraftApriori <- lapply(nbaDraftApriori, as.factor)

#Forcing dataset back into a data frame post factorization
nbaDraftApriori <- as.data.frame(nbaDraftApriori)

#Creating Apriori transactions
transactions <- as(nbaDraftApriori, "transactions")


#Running Apriori

####

#Iteration 1: 5% support/40% confidence/min length of rule = 2
rules <- apriori(transactions, parameter = list(supp = 0.05, conf = 0.4, minlen = 2))

#Checking the summary stats of rules
summary(rules) #Set of 95 rules

#Inspecting top 10 rules by confidence
inspect(sort(rules, by = "confidence")[1:10])

#Inspecting top 10 rules by lift
inspect(sort(rules, by = "lift")[1:10])

#Inspect by RHS = long career by creating a subset of rules
rules_long_career1 <- subset(rules, subset = rhs %pin% "career_bucket=LongCareer")

#Check length of long career subset
length(rules_long_career1) #Returns 3

#Inspect top rules confidence
inspect(sort(rules_long_career1, by = "confidence")[1:3])

#Inspect by RHS = average career by creating subset
rules_avg_career1 <- subset(rules, subset = rhs %pin% "career_bucket=AverageCareer")

#Check length of avg career subset
length(rules_avg_career1) # returns 0, min support too high

#Inspect RHS = short career through subset
rules_short_career1 <- subset(rules, subset = rhs %pin% "career_bucket=ShortCareer")

length(rules_short_career1) #Returns 16

#Inspect new top 10 rules by confidence
inspect(sort(rules_short_career1, by = "confidence")[1:10])

####

#Iteration 2: 10% support/40% confidence/min length of rule = 2 (upping support)
rules <- apriori(transactions, parameter = list(supp = 0.1, conf = 0.4, minlen = 2))

#Checking the summary stats of rules
summary(rules) #Set of 65 rules

#Inspecting top 10 rules by confidence
inspect(sort(rules, by = "confidence")[1:10])

#Inspecting top 10 rules by lift
inspect(sort(rules, by = "lift")[1:10])

#Inspect by RHS = long career by creating a subset of rules
rules_long_career2 <- subset(rules, subset = rhs %pin% "career_bucket=LongCareer")

length(rules_long_career2) # Returns 0, min support too high

#Inspect by RHS = avg by creating a subset of rules
rules_avg_career2 <- subset(rules, subset = rhs %pin% "career_bucket=AverageCareer")

length(rules_avg_career2) # Returns 0, min support too high

#Inspect by RHS = avg by creating a subset of rules
rules_short_career2 <- subset(rules, subset = rhs %pin% "career_bucket=ShortCareer")

length(rules_short_career2) # Returns 12

#Inspecting top 10 rules by confidence
inspect(sort(rules_short_career2, by = "confidence")[1:10])

####

#Iteration 3: 20% support/50% confidence/min length of rule = 2 (upping support & support)
rules <- apriori(transactions, parameter = list(supp = 0.2, conf = 0.5, minlen = 2))


summary(rules) #Set of 14 rules

#Inspecting top 10 rules by confidence
inspect(sort(rules, by = "confidence")[1:10])

#Inspecting top 10 rules by lift
inspect(sort(rules, by = "lift")[1:10])

#Inspect by RHS = long career by creating a subset of rules
rules_long_career3 <- subset(rules, subset = rhs %pin% "career_bucket=LongCareer")

length(rules_long_career3) # Returns 0, min support too high

#Inspect by RHS = avg by creating a subset of rules
rules_avg_career3 <- subset(rules, subset = rhs %pin% "career_bucket=AverageCareer")

length(rules_avg_career3) # Returns 0, min support too high

#Inspect by RHS = avg by creating a subset of rules
rules_short_career3 <- subset(rules, subset = rhs %pin% "career_bucket=ShortCareer")

length(rules_short_career3) # Returns 2, min support becoming high for short career

#Inspecting the 2 rules present
inspect(sort(rules_short_career3, by = "confidence")[1:2])


####

#Analysis 2: Correlation

#Creating copy of dataset for correlation
nbaDraftCorrelation <- nbaDraftCleaned

#Clearing all NAs
nbaDraftCorrelation <- na.omit(nbaDraftCorrelation)


#Removing useless columns (keeping overall pick, totals, per game averages, and advanced metrics)
nbaDraftCorrelation <- nbaDraftCorrelation %>%
  select(
    overall_pick, total_games_played, total_minutes_played, total_points,
    total_rebounds, total_assists, mpg, ppg, rpg, apg, win_shares, ws_per_48,
    bpm, vorp
  )


#Running correlation matrix with Pearson's for relationships between variables
cor_matrix_pearson <- cor(nbaDraftCorrelation, method = "pearson")

#Running correlation matrix with Spearman's
cor_matrix_spearman <- cor(nbaDraftCorrelation, method = "spearman")


#Plotting overall pick by career games played
plot(nbaDraftCorrelation$overall_pick, nbaDraftCorrelation$total_games_played, main = "Draft Position vs Career Games Played",
     xlab = "Overall Draft Pick",
     ylab = "Total Games Played",
     pch = 16
     )

#Plotting line of best fit
abline(lm(nbaDraftCorrelation$total_games_played~nbaDraftCorrelation$overall_pick),
       col = "red",
       lwd = 3)

#Plotting overall pick by win shares
plot(nbaDraftCorrelation$overall_pick, nbaDraftCorrelation$win_shares, main = "Draft Position vs Career Win Shares",
     xlab = "Overall Draft Pick",
     ylab = "Career Win Shares",
     pch = 16
)

#Plotting line of best fit
abline(
  lm(nbaDraftCorrelation$win_shares~nbaDraftCorrelation$overall_pick),
  col = "red",
  lwd = 3)

####

#Analysis 3: Multiple Linear Regression

#Creating copy of dataset for Regression
nbaDraftRegression <- nbaDraftCleaned


#Model 1: Total games played as dependent variable (keeping draft attributes + total games played)
nbaDraftRegressionGamesPlayed <- nbaDraftRegression %>%
  select(total_games_played, overall_pick, pick_bucket, educational_prestige, team_conference)

model_total_games <- lm(total_games_played ~ overall_pick + pick_bucket + educational_prestige + team_conference,
                        data = nbaDraftRegressionGamesPlayed)

summary(model_total_games)


#Model 2: Total minutes played as dependent variable
nbaDraftRegressionMinPlayed <- nbaDraftRegression %>%
  select(total_minutes_played, overall_pick, pick_bucket, educational_prestige, team_conference)

model_total_minutes <- lm(total_minutes_played ~ overall_pick + pick_bucket + educational_prestige + team_conference,
                          data = nbaDraftRegressionMinPlayed)

summary(model_total_minutes)


#Model 3: MPG as the dependent variable
nbaDraftRegressionMPG <- nbaDraftRegression %>%
  select(mpg, overall_pick, pick_bucket, educational_prestige, team_conference)

model_MPG <- lm(mpg ~ overall_pick + pick_bucket + educational_prestige + team_conference,
                          data = nbaDraftRegressionMPG)

summary(model_MPG)


####

#Analysis 4: Classification (Decision Tree and Random Forest)
#Creating copy of dataset for 
nbaDraftClassification <- nbaDraftCleaned

#Removing unnecessary columns (keeping draft identifiers for good, keeping career length attributes for now)
nbaDraftClassification <- nbaDraftClassification %>%
  select(overall_pick, pick_bucket, educational_prestige, team_conference, total_games_played, career_bucket)


#Binary classification: Long Career vs Not Long Career (Avg Career & Short Career)
nbaDraftClassification <- nbaDraftClassification %>%
  mutate(long_career_label = ifelse(career_bucket == "LongCareer", "LongCareer", "NotLongCareer"))

#Factoring needed for the long career label
nbaDraftClassification$long_career_label <- factor(nbaDraftClassification$long_career_label)

#Deleting career length attributes after using them to create long vs not long career labels
nbaDraftClassification <- nbaDraftClassification %>%
  select(-total_games_played, -career_bucket)


#Reviewing dataset after factorization and dataset column removal
str(nbaDraftClassification) #Everything checks out, factorization done properly
table(nbaDraftClassification$long_career_label) #Returns 556 long careers, 1113 not long careers


#Decision Tree

#Training/Test split = 70/30
set.seed(1) #standard seed

train_index70 <- createDataPartition(
  nbaDraftClassification$long_career_label, p = 0.70, list = FALSE)

train_70 <- nbaDraftClassification[train_index70, ]
test_30 <- nbaDraftClassification[-train_index70, ]

#Fitting 70/30 decision tree
decision_tree_model_70_30 <- rpart(
  long_career_label ~ overall_pick + pick_bucket + educational_prestige + team_conference,
  data = train_70,
  method = "class",
  control = rpart.control(
    cp = 0.01,
  insplit = 20,
    maxdepth = 5
  )
)

printcp(decision_tree_model_70_30)

#Prediction using 30% of dataset for testing, generating confusion matrix after
prediction_30_dt <- predict(decision_tree_model_70_30, newdata = test_30, type = "class")

confusionMatrix(prediction_30_dt, test_30$long_career_label)

#Plotting results of 70/30
rpart.plot(
  decision_tree_model_70_30,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Decision Tree #1: Long Careers vs. Not Long Careers"
)

#Training/Test split = 80/20
set.seed(1) #standard seed

train_index80 <- createDataPartition(
  nbaDraftClassification$long_career_label, p = 0.80, list = FALSE)

train_80 <- nbaDraftClassification[train_index80, ]
test_20 <- nbaDraftClassification[-train_index80, ]

#Fitting 80/20 decision tree
decision_tree_model_80_20 <- rpart(
  long_career_label ~ overall_pick + pick_bucket + educational_prestige + team_conference,
  data = train_80,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    minsplit = 20,
    maxdepth = 5
  )
)

#Prediction using 20% of dataset for testing
prediction_20_dt<- predict(decision_tree_model_80_20, newdata = test_20, type = "class")
confusionMatrix(prediction_20_dt, test_20$long_career_label)

#Plotting results of 80/20
rpart.plot(
  decision_tree_model_80_20,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Decision Tree #2: Long Careers vs. Not Long Careers"
)

#Random Forest

#Training/Test split = 70/30, 200 trees

set.seed(1) #standard seed

#Fitting the model
random_forest_70_30 <- randomForest(
  long_career_label ~ overall_pick + pick_bucket + educational_prestige + team_conference,
  data = train_70
)

#Prediction using 30% of dataset for testing
prediction_30_rf <- predict(random_forest_70_30, newdata = test_30, type = "class")
confusionMatrix(prediction_30_rf, test_30$long_career_label)

# Visualizing with a variable importance plot to see which draft features drive the model the most
varImpPlot(random_forest_70_30) #Draft position is the biggest driver of RF model


#Training/Test split = 80/20, 200 trees

#Fitting the model
random_forest_80_20 <- randomForest(
  long_career_label ~ overall_pick + pick_bucket + educational_prestige + team_conference,
  data = train_80
)

#Prediction using 30% of dataset for testing
prediction_20_rf <- predict(random_forest_80_20, newdata = test_20, type = "class")
confusionMatrix(prediction_20_rf, test_20$long_career_label)

# Visualizing with a variable importance plot to see which draft features drive the model the most
varImpPlot(random_forest_80_20) #Little to no change from 70/30 split, draft position is the biggest driver

####
