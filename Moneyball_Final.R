########################################################################################
## A2 Team Assignment: Moneyball Capstone Project (Story of Oakland Athletics) ######### 
## Visualizing & Analyzing Data with R: Methods & Tools - DAT-5323 - FMBANDD1 ##########
## Hult International Business School ##################################################
## Team 6: Abhishek Rathi, Charlynne Santos, Patrick Nielsen, Sakshi Naik, Sho Enokida #
########################################################################################

###   ###   ###   ###   INTRODUCTION AND BUSINESS PROBLEM  ###   ###   ###   ###  
# # # # # # # # # # # # # STEP 1 - PROBLEM IDENTIFICATION  # # # # # # # # # # # 

# Situation: 
# In 2001, Oakland Athletics (OA) lost three major players Jason Giambi, Johnny 
# Damon, and Jason Isringhausen. 

# Complication: 
# The MLB team budget range is very large 24,130,000 (min) - 112,287,143 (max), 
# with OA ranking as the second lowest team budget (33,810,750) (TheBaseballCube, 2001).

# Question:
# How can OA substitute the three players with the most competitive alternatives 
# within the same budget? 

# Answer: 
# In 2001 MLB salaries were primarily driven by batting matrices such as Batting 
# Average (BA). However, evaluating the winning drivers of baseball, we see the 
# following relationship: 

# Wins <-(caused by)- Rounds <-(caused by)- Bases <-(caused by)- Hitting+Reaching base

# Looking at this relationship, we see that winning a game of baseball comes down 
# to reaching more bases than your opponent. 
# While Batting Average (BA) does explain a number of bases to an extent, it does not cover a player's 
# overall average base success. However, the metric On Base Plus Slugging (OPS)
# which is the sum of On Base Percentage (OBP) and Slugging Average (SA) captures 
# the overall ability of a player to get on base and hit for power (MLB, 2024)

# To solve this, the team applies an integer linear programming model to maximize the
# On Base Plus Slugging saber metric subject to budget constraints, and 
# a set of practical constraints described in detail in the model section. 

# Based on the model, the team recommends Oakland Athletics to invest 12,950,000 $ in players -
# ID: michach01, Name: Chris Michalak, Team: Toronto Blue Jays (TOR), Salary: 200000 OPS: 1.333
# ID: bondsba01, Name: Barry Bonds, Team: San Francisco Giants (SFG), Salary: 10300000 OPS: 1.243
# ID: stantmi02, Name: Mike Stanton, Team: New York Yankess (NYA), Salary: 2450000 OPS: 1.2  

# ------------------------------  REFERENCES  -------------------------------  # 

# MLB. (2024). On-base Plus Slugging (OPS). Major League Baseball. 
# https://www.mlb.com/glossary/standard-stats/on-base-plus-slugging

# The Baseball Cube. (2001). Payroll. The Baseball Cube. 
# https://www.thebaseballcube.com/content/payroll_year/2001/

# Cipriano, C. (2009, June 17). How Boston broke the curse.
# Bleacher Report. https://bleacherreport.com/articles/201349-how-boston-broke-the-curse

###   ###   ###   ###   ###   ###  INSTALLING PACKAGES ###   ###   ###   ###   ###
install.packages("ggplot2")
install.packages("purrr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("tibble")
install.packages("lpSolve")
install.packages("lpSolveAPI")
install.packages("scales")

library(ggplot2)
library(purrr)
library(tidyr)
library(dplyr)
library(tibble)
library(lpSolve)
library(lpSolveAPI)
library(scales)

###   ###   ###   ###   ###   ###  CLEANING DATA  ###   ###   ###   ###   ###
setwd("/Users/abhishekrathi/Desktop/MBAN/R programming/Moneyball")

#Loading the datasets
batting <- read.csv('Batting.csv')
salaries <- read.csv('Salaries.csv')

head(batting)
str(batting)

#Merging the 2 datasets as we not only need the best statistics/best players based on 
#batting records but also need the undervalued players, which will be determined by the salaries. Also, using teamID as a filter to make
#sure that we include players who played for multiple teams within the same season. 
merged_data <- merge(batting, salaries, by = c("playerID", "yearID", "teamID"))
head(merged_data)
str(merged_data)
nrow(merged_data)

#Filtering Criteria - 
# 1. Filter the dataset to include all players who played in the 2001 season.
# 2. From this subset, we keep the records for these players if they also have entries for the 1999 and 2000 
#    seasons, but do not exclude players who only have records for 2001.

# Create a subset of data for players who played in the 2001 season
players_2001 <- subset(merged_data, yearID == 2001)

# Filter the subset dataset for records of these players from 1999 to 2001
filtered_data <- merged_data[merged_data$playerID %in% players_2001$playerID & 
                               merged_data$yearID %in% c(1999, 2000, 2001), ]

# Checking the no. of rows after the dataset has been filtered for years
nrow(filtered_data)

# Checking for missing values in the dataset
any(is.na(filtered_data))

#Now, filtering for the 3 players which left the team during the 2001-02 offseason.
# Jason Giambi - New York Yankees
# Johnny Damon - Boston Red Sox
# Jason Isringhausen - St. Louis Cardinals

player_ids <- c("damonjo01", "isrinja01", "giambja01")

# Filtering the dataset for the specific players
players_data <- subset(filtered_data, playerID %in% player_ids)

# Filtering for the specific players and the year 2001
players_2001 <- subset(players_data, yearID == 2001)

# Summing their salaries for the year 2001
total_salary_2001 <- sum(players_2001$salary)
total_salary_2001

# Now, the total salary for these players combined is 14503333 $
# We will use this as the budget constraint and need to make sure that the 3 new players
# we pick not exceed this salary cap.

#Counting the no. of rows with missing values
num_rows_with_missing_values <- sum(!complete.cases(filtered_data))
num_rows_with_missing_values

str(filtered_data)

# Columns with missing values
columns_with_missing_values <- c("AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "SO", "IBB", "HBP", "SH", "SF", "GIDP")

# Looping through each column to print summary statistics
for (col in columns_with_missing_values) {
  # Extracting the column data
  column_data <- filtered_data[[col]]
  
  # Calculating mean and median (excluding NA values)
  mean_value <- mean(column_data, na.rm = TRUE)
  median_value <- median(column_data, na.rm = TRUE)
  
  # Printing mean and median
  print(paste("Mean for", col, ":", mean_value))
  print(paste("Median for", col, ":", median_value))
}

# Checking for the mean and median values across all columns, we found that there is 
# a lot of variation in these values across the columns in the dataset. This shows that
# the data is not normally distributed and contains skewness, due to the presence of outliers. 
# So, we decided to impute the missing values using Median for the league and the respective
# year. By doing this, we are maintaining the integrity of the data's distribution and 
# avoiding biases that could come from using the overall median values. 

# # Calculate median values for each column by year and league
# medians <- filtered_data %>%
#   group_by(yearID, lgID.x) %>%
#   summarize(across(c(AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP), median, na.rm = TRUE)) %>%
#   ungroup()
# medians
# 
# # Join median values back to the original dataset
# # This will directly impute the median values where the original values are NA
# data_with_medians <- filtered_data %>%
#   left_join(medians, by = c("yearID", "lgID.x"))

# Intially, we were imputing median values for the missing data. However, on checking our results
# we found that some players are benefitting from the median At Bats which was 77. They had very
# low no. of At Bats and were getting 77 additional At Bats which isn't justifiable. So, decided to 
# omit the rows with null values. 

# The median imputation, while statistically valid in many contexts, does not account for the nuances of baseball
# statistics and the significant impact that playing time (as approximated by At Bats) has on a player's overall statistics.
# Players with very few At Bats are often in that position due to specific reasons such as injuries, being a late-season
# call-up, or serving in a limited role (e.g., pinch hitter, defensive replacement). Therefore, imputing a median value
# of At Bats could distort their performance metrics and the overall analysis.

# Given these considerations, we decided to shift our strategy from imputing missing values to omitting rows with
# NA values entirely. This decision was based on the desire to ensure that our analysis and subsequent model building
# or player evaluation are based on the most accurate and representative data available.

filtered_data <- na.omit(filtered_data)
filtered_data

any(is.na(filtered_data))
#No missing values now remain in the dataset. 

# Checking summary statistics
summary_stats <- filtered_data %>%
  select(AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP) %>%
  summary()
print(summary_stats)

###   ###   ###   ###   ###   ###  FEATURE ENGINEERING  ###   ###   ###   ###   ###

# Feature Engineering
# To select the most effective and efficient players, we considered several metrics, 
# including on-base percentage (OBP), which measures how frequently a batter reaches base, 
# slugging percentage (SLG), which reflects a hitter's batting productivity, 
# and on-base plus slugging (OPS), which combines both the ability to get on base and hit for power.

# In addition, these metrics were calculated as weighted averages, 
# considering the scores in the past three years.
# The weighting scheme favored more recent scores, 
# reflecting the greater importance of the current performances.

# Calculate the 1base runs needed for SLG
filtered_data$`1B` <- filtered_data$H - filtered_data$`X2B` - filtered_data$`X3B` - filtered_data$HR

# Calculate the batting average
filtered_data$BA <- round(ifelse(filtered_data$H == 0 | filtered_data$AB == 0, 0, 
                                filtered_data$H / filtered_data$AB), 3)


# Move 1B values next to X2B
filtered_data = filtered_data %>% 
  relocate(`1B`,.before = `X2B`)

# Move BA values next to 1B 
filtered_data = filtered_data %>% 
  relocate(BA ,.before = `1B`)

# Define original weights for years
original_weights <- c('1999' = 0.20, '2000' = 0.30, '2001' = 0.50)

# Ensure 'yearID' is numeric in 'filtered_data'
filtered_data <- filtered_data %>%
  mutate(yearID = as.numeric(as.character(yearID)))

# Identify which players were active in each year
active_years <- filtered_data %>%
  group_by(playerID) %>%
  summarize(active_years = list(unique(yearID)), .groups = 'drop')

# Function to dynamically adjust weights based on active years
adjust_weights <- function(active_years, original_weights) {
  weights <- c('1999' = 0, '2000' = 0, '2001' = 1) # Default to only 2001 being active
  if('2000' %in% active_years && '1999' %in% active_years) {
    weights <- original_weights
  } else if('2000' %in% active_years) {
    weights <- c('2000' = 0.40, '2001' = 0.60)
  } else if('1999' %in% active_years) {
    weights <- c('1999' = 0.40, '2001' = 0.60)
  }
  weights
}

# Adjust weights for each player based on the active years they played
active_years$weights <- map(active_years$active_years, ~adjust_weights(.x, original_weights))

# Create a dataframe to unnest and expand weights properly
weights_expanded <- active_years %>%
  unnest(weights, names_repair = "minimal") %>%
  mutate(yearID = as.numeric(names(weights))) %>%
  select(playerID, yearID, weights) %>%
  rename(weight = weights)

# Join the expanded weights back to the original data frame
filtered_data_adjusted <- filtered_data %>%
  left_join(weights_expanded, by = c("playerID", "yearID"))

# Calculate hits that are singles
filtered_data_adjusted <- filtered_data_adjusted %>%
  mutate(Singles = H - X2B - X3B - HR)

# Apply the dynamically adjusted weights to statistics
filtered_data_adjusted <- filtered_data_adjusted %>%
  mutate(
    weighted_AB = AB * weight,
    weighted_H = H * weight,
    weighted_BB = BB * weight,
    weighted_HBP = HBP * weight,
    weighted_SF = SF * weight,
    weighted_Singles = Singles * weight,
    weighted_X2B = X2B * weight,
    weighted_X3B = X3B * weight,
    weighted_HR = HR * weight
  )

# Group by playerID and calculate the weighted averages for OBP, SLG, and OPS
weighted_stats <- filtered_data_adjusted %>%
  group_by(playerID) %>%
  summarize(
    weighted_OBP = sum(weighted_H + weighted_BB + weighted_HBP, na.rm = TRUE) / sum(weighted_AB + weighted_BB + weighted_HBP + weighted_SF, na.rm = TRUE),
    weighted_SLG = (sum(weighted_Singles + (2 * weighted_X2B) + (3 * weighted_X3B) + (4 * weighted_HR), na.rm = TRUE) / sum(weighted_AB, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  mutate(
    weighted_OPS = weighted_OBP + weighted_SLG
  )

# Round the calculated metrics for presentation
weighted_stats <- weighted_stats %>%
  mutate(
    weighted_OBP = round(weighted_OBP, 3),
    weighted_SLG = round(weighted_SLG, 3),
    weighted_OPS = round(weighted_OPS, 3)
  )
head(weighted_stats)

# Display the top performers based on weighted OPS
top_performers <- weighted_stats %>%
  arrange(desc(weighted_OPS))

top_performers
summary(top_performers)

# Check for NaN values
# is.na is used to check for both NA and NaN values
na_check <- any(is.na(top_performers))
na_check

top_performers_cleaned <- na.omit(top_performers)

# Identifying rows where weighted_OBP, weighted_SLG, and weighted_OPS are all 0
rows_with_all_zeros <- with(top_performers_cleaned, weighted_OBP == 0 & weighted_SLG == 0 & weighted_OPS == 0)
rows_with_all_zeros

# Removing these rows from the dataframe
top_performers_filtered <- top_performers_cleaned[!rows_with_all_zeros,]

# Optionally, check the number of rows before and after to see how many rows were removed
cat("Number of rows before removing rows with all zeros: ", nrow(top_performers_cleaned), "\n")
cat("Number of rows after removing rows with all zeros: ", nrow(top_performers_filtered), "\n")

# Now, we are left with 648 observations. All NaN values and rows containing 0's for all the 3 columns
# have been removed
head(top_performers_filtered)

salaries_2001 <- subset(salaries, yearID == 2001)
top_performers_with_salary <- merge(top_performers_filtered, salaries_2001, by = "playerID")

head(top_performers_with_salary)

# Removing rows where teamID is 'OAK' from the dataframe
top_performers_no_oak <- top_performers_with_salary[top_performers_with_salary$teamID != "OAK", ]
head(top_performers_no_oak)

# Arrange in descending order of Weighted_OPS
# weighted_OPS = 1.368 is the highest OPS for the 2001 season. Taking this as a cut-off and filtering players
# below this OPS
top_performers_filtered <- top_performers_no_oak %>%
  arrange(desc(weighted_OPS)) %>%
  filter(weighted_OPS < 1.368)

head(top_performers_filtered)

###   ###   ###   ###   ### Visualizing Data  ###   ###   ###   ###   ###

ggplot(weighted_stats, aes(x = weighted_OPS)) + 
  geom_histogram(
    binwidth = 0.1,
    fill = "dodgerblue", color = "black"
  ) +
  labs(
    title = "Distribution of Weighted OPS",
    x = "Weighted OPS",
    y = "Frequency",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
  ) +
  scale_x_continuous(breaks = seq(0, max(weighted_stats$weighted_OPS, na.rm = TRUE), by = 1)) +  
  xlim(0, 2.5) 

# The majority of players have OPS between 0.5 and 1. We would like to take the players with OPS > 1 
# and who are under-valued in the market, as this shows the players who are performing well and also come 
# at a cheaper price compared to others in the market.

# SALARY vs OPS graph
# Create salary bins
top_performers_filtered <- top_performers_filtered %>%
  mutate(salary_bin = cut(salary,
                          breaks = c(0, 1000000, 8000000, 16000000, 22000000),
                          labels = c("Below 1M", "1M-8M", "8M-16M", "16M-22M"),
                          include.lowest = TRUE))

# Checking the distribution
table(top_performers_filtered$salary_bin)

# Calculate the average OPS (or any other performance metric you're interested in) for each salary bin
# Assuming there's a column named 'OPS' in your dataframe
average_OPS_by_salary_bin <- top_performers_filtered %>%
  group_by(salary_bin) %>%
  summarise(Average_OPS = mean(weighted_OPS, na.rm = TRUE))

ggplot(average_OPS_by_salary_bin, aes(x = salary_bin, y = Average_OPS, fill = salary_bin)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Average_OPS, 3)), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Average OPS by Salary",
       x = "Salary Bin",
       y = "Average OPS") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) # Center align the title

# From this graph, it can be observed that the average OPS for players increases with increase in the 
# salary range. This shows that higher valued players in the market tend to have a high OPS in general.



###   ###   ###   INTEGER LINEAR PROGRAMMING OPTIMIZATION MODEL  ###   ###   ###

# ---------------------------------------------------------------------------- #
# LP Model: 

# Obj. Max(OPS) = OPS(1)player_option(1) + OPS(2)player_option(2) + OPS(n..)player_option(n..)

# st. 
# Constraint 1: The average OPS of the alternative needs to be greater than the original players average.
# (OPS(choice_1)+OPS(choice_2)+OPS(choice_3))/3 ≥ ((OPS Giambi)+(OPS Damon)+(OPS Isringhausen))/3

# Constraint 2: The total budget of the alternative needs to be less than or equal to the original player's budget (14.5M).
# Salary(choice_1)+Salary(choice_2)+Salary(choice_3) ≤ Salary(Giambi)+Salary(Damon)+Salary(Isringhausen)

# Constraint 3: the number of players needs to equal 3
# player_option(1..n) = 3

# Constraint 4-n: No player can be selected more than once
# player_option(n) ≤ 1 

# Constraint 5: Players cannot come from Oakland Athletics (solved through filtering)
# player_option(n)[team] != Oakland Athletics 

# Constraint 5: Players needs  to be available in 2001 (solved through filtering)
# player_option(n)[year] =  2001

# ---------------------------------------------------------------------------- #

# Subsetting dataset
# df_lp <- weighted_metrics_merge_data[!(weighted_metrics_merge_data$teamID == "OAK" & weighted_metrics_merge_data$yearID != 2001), ]
df_lp <- top_performers_filtered
df_lp$no_of_picks <- 1
# Creating a vector containing the objective coeffecients 
obj <- df_lp$weighted_OPS

# Creating constraint matrix for the left hand side of the equation (continuous numbers)
lhs_ineq <- matrix(c(
  df_lp$weighted_OPS,  # 1 OPS of new players need to be greater than or equal to the OPS of the old players
  df_lp$salary,        # 2 Salary of new players need to be less than or equal to the salary of the old players
  df_lp$no_of_picks    # 3 Only three players can be picked, and each player counts for 1 pick 
), nrow = 3, byrow = TRUE)
# 4 No player can be chosen more than one time, and if picked needs to be a whole number/integer
# Extending the constraint matrix, with all the constraints, saying each player can only be picked once. 
no_decision_variable <- length(obj) # identifying the number of decision variables by fniding the lenght of the objective function
# creating a new matrix with 0 values for the coefficients and the dimension (integers): 
# number of decision variables * number of decisions variables. 
lhs_decision_ineq <- matrix(data = 0, 
                            nrow = no_decision_variable, 
                            ncol = no_decision_variable)
# identifying the diagonal indexes so the coefficient of 1 can be inserted 
# for each constraint where it matches the decision variable
diagonal_indices <- 1:ncol(lhs_decision_ineq)   
lhs_decision_ineq[cbind(diagonal_indices, diagonal_indices)] <- 1
# Combining the two lhs constraint matrices into 1
lhs_ineq_final <- rbind(lhs_ineq, lhs_decision_ineq)

# Creating a constraint vector for the right hand side of the equation
rhs_ineq <- c(2.489,    # 1 OPS of new players need to be greater than or equal to the OPS of the old players (avg OPS 0.8296)
              14503333, # 2 Salary of new players need to be less than or to the salary of the old players # 14503333
              3)        # 3 Only three players can be picked, and each player counts for 1 pick 
# 4 No player can be chosen more than one time:                        
# creating a vector with 1 values for the coefficients and the length n decision variables. 
rhs_decision_ineq <- rep(1, length(obj))
# Combining the two vectors
rhs_ineq_final <- c(rhs_ineq, rhs_decision_ineq)

# Creating a vector containing the different inequalities for each constraint
inequality_signs <- c(">",    # 1 OPS of new players need to be greater than or equal to the OPS of the old players
                      "<=",   # 2 Salary of new players need to be less than or to the salary of the old players
                      "=")    # 3 Only three players can be picked, and each player counts for 1 pick 
# 4 No player can be chosen more than one time:                        
# creating a vector with "<=" values for the inequalities and the length n decision variables.
inequality_decision_signs <- rep("<=", length(obj))
# Combining the two vectors
inequality_signs_final <- c(inequality_signs, inequality_decision_signs)

# Specifying the decision variables data type (continuous for decimals, binary for integers)
var_types <- c(rep("C", length(df_lp$weighted_OPS)), rep("B", length(obj)))

# Solving the objective function: 
lp_result <- lp("max", 
                obj,
                lhs_ineq_final, 
                inequality_signs_final,
                rhs_ineq_final, 
                compute.sens = TRUE, all.int = TRUE)

# Display the results
cat("Optimal solution:\n")
for (i in 1:length(obj)) {
  cat(paste("x", i, "=", lp_result$solution[i], "\n"))
}

###   ###   ###   MODEL EVALUATION AND SENSITIVITY ANALYSIS  ###   ###   ###   

# Showing Summary of Players that were chosen
cat("Optimal solution:\n")
for (i in 1:length(obj)) {
  if (lp_result$solution[i] == 1) {  # Only display selected players
    cat(paste("Player ID:", df_lp$playerID[i], 
              "Salary:", df_lp$salary[i], 
              "OPS:", obj[i], "\n"))
  }
}

# Objective Function Sensitivity Analysis
# This section analyzes the sensitivity of the objective function (players' OPS Average) in our linear programming model.
objective_sens <- data.frame(
  Player_Choice = seq_along(lp_result$solution),
  Final_Values = lp_result$solution,
  Objective_Coefficient = obj,
  Coef_Upper_Limit = lp_result$sens.coef.to,
  Coef_Lower_Limit = lp_result$sens.coef.from)

# Displaying the top 10 results from the sensitivity analysis for a focused review.
head(objective_sens, n = 10)

# Key Insights:
  # The analysis shows that the OPS averages of selected players are closely spaced.
  # This close spacing implies that even minor variations in a player's OPS can lead to significant changes in the model's output.
  # The wide range in the upper and lower limits of the coefficients highlights this sensitivity.
  # Importance of OPS: This sensitivity underscores the reliance of our model on the OPS metric, 
  # emphasizing how player performance fluctuations within seasons can pivot our optimal team composition.
  # Strategic Consideration: Given this sensitivity, it's crucial to monitor player performances closely and 
  # perhaps consider additional metrics that might provide a more stable basis for decision-making.

# This information is vital for understanding the robustness of our model and guiding future decisions 
# on player selection, emphasizing the need for a dynamic approach in response to player performance changes.

# Constraint Sensitivity Analysis
# This section performs sensitivity analysis on the linear programming model's constraints.
# It helps in understanding the impact of each constraint on the final decision-making, 
# especially in player selection based on OPS and budget constraints.
constraint_sens <- data.frame(
  Constraint = seq_along(lp_result$duals),
  Dual_Values = lp_result$duals,
  Dual_Upper_Limit = lp_result$duals.to,
  Dual_Lower_Limit = lp_result$duals.from
)

# Extracting the top 10 constraints for analysis
head(constraint_sens, n = 10)
# This displays the first 10 constraints from the sensitivity analysis,
# focusing on the most impactful constraints in the decision-making process.

# Key Insights and Strategic Actions:
  # Budget and Performance (Constraints 1 and 2): These are not limiting factors, indicating budget flexibility and confidence in player performance.
  # Player Priority (Constraint 3): Player michach01 emerges as a key figure for meeting our OPS target, suggesting a focus on securing his contract.
  # Balanced Contributions (Constraints 4-5): Other players contribute to our OPS goal but to a lesser extent compared to michach01.
  # Potential Undervalued Asset (Constraint 6): Player graveda01, despite not being selected, shows promise. This signals an opportunity to reassess his potential contribution.
  # Recommendations: Consider scouting for similar talent at a lower cost or reallocating budget for player development initiatives.

# These insights enable a more nuanced approach to team building, blending quantitative analysis with strategic planning.

# Generating model summary for alternative results with different budget constraint
cat("Optimal objective value (Avg BPS for the 3 new players) =", (lp_result$objval)/3, "\n")

# Max avg BPS at $14.5M (Full Budget) constraint = 1.258667 at a cost of: $12,950,000
   # Summary Results:
          # Player ID: michach01 Salary: 200000 OPS: 1.333
          # Player ID: bondsba01 Salary: 10300000 OPS: 1.243
          # Player ID: stantmi02 Salary: 2450000 OPS: 1.2 
# Max avg BPS at $10M (2/3 of Budget) constraint = 1.234667  at a cost of: $4,750,000
  # Summary Results:
          # Player ID: michach01 Salary: 200000 OPS: 1.333 
          # Player ID: stantmi02 Salary: 2450000 OPS: 1.2 
          # Player ID: graveda01 Salary: 2100000 OPS: 1.171 
# Max avg BPS at 4M (Less then 1/3 of Budget) constraint = 1.233333  at a cost of: $2,880,000
  # Summary Results:
          # Player ID: michach01 Salary: 200000 OPS: 1.333 
          # Player ID: stantmi02 Salary: 2450000 OPS: 1.2 
          # Player ID: padilvi01 Salary: 230000 OPS: 1.167 

# Visual Representation - Budget Ranges Vs Weighted_OPS

# Create the dataset
data <- data.frame(
  BudgetRange = rep(c("$14.5M", "$10M", "$4M"), each = 3),
  PlayerID = c("michach01", "bondsba01", "stantmi02", "michach01", "stantmi02", "graveda01", "michach01", "stantmi02", "padilvi01"),
  OPS = c(1.333, 1.243, 1.2, 1.333, 1.2, 1.171, 1.333, 1.2, 1.167)
)

# Plotting the graph
ggplot(data, aes(x = PlayerID, y = OPS, fill = BudgetRange)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black") +
  geom_text(aes(label = sprintf("%.3f", OPS)), position = position_dodge(width = 0.7), vjust = -0.25, size = 2.5) +
  facet_wrap(~BudgetRange, scales = "free_x") +
  labs(title = "Player OPS by Budget Range",
       x = "Player ID",
       y = "OPS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
  scale_fill_brewer(palette = "Set1")

# Key Insights on Different Budget Options:
  # Player michach01 (Chris Michalak) consistently appears across all budget scenarios, indicating the model considers him a high-value player for his cost.
  # Player bondsba01 (Barry Bonds), despite having a lower OPS than Chris Michalak, commands the highest salary, reflecting his historical performance, reputation, and possibly
  # marketability factors that the model does not explicitly account for.
  # This implies that the salary amount also correlates to popularity and tenure at the field, which can be a factor that can be looked upon, provided that a change in budget is needed

# The Moneyball strategy has been successfully implemented by other teams in the MLB, starting 
# with Boston Red Sox in 2004. The most important hire they made that off-season was for their assistant general manager position which went to Theo Epstein. 
# Epstein was a huge fan of Bill James and his sabermetrics approach and Billy Beane, general manager of the Oakland Athletics. Inspired by Oakland Atheltic's success in 2001, Boston Red Sox implemented 
# Moneyball strategy in the 2004 season and went on to have one of the most efficient seasons in Baseball history.

# Similarly, other teams in MLB are constantly using the Sabermetrics approach developed in Moneyball.
# Also, our Linear Programming model can be used by other teams by changing the constraints, objective function depending on their requirements 
# to enhance their performance in the league. 
# Other sporting leagues like Premier League, Bundesliga, La Liga, etc. can also benefit from the Linear Programming approach and use it
# for the benefit of their team and in scouting under-valued talent. 

# Display Selected Players and Their Stats
selected_player_ids <- c()
selected_player_salaries <- c()
selected_player_ops <- c()

cat("Selected Players:\n")
for (i in 1:length(obj)) {
  if (lp_result$solution[i] == 1) {
    cat(paste("Player ID:", df_lp$playerID[i], 
              "Salary:", df_lp$salary[i], 
              "OPS:", obj[i], "\n"))
    
    selected_player_ids <- c(selected_player_ids, df_lp$playerID[i])
    selected_player_salaries <- c(selected_player_salaries, df_lp$salary[i])
    selected_player_ops <- c(selected_player_ops, obj[i])
  }
}

# Calculate Summary for Chosen Players
total_salary_new = sum(selected_player_salaries)
average_ops_new = mean(selected_player_ops)

# Assigning Data from Previous Players
total_salary_previous = 14503333
average_ops_previous = 0.8297

# Displaying Comparison
cat("\nComparison:\n")
cat("New Players - Total Salary:", total_salary_new, "Average OPS:", average_ops_new, "\n")
cat("Previous Players - Total Salary:", total_salary_previous, "Average OPS:", average_ops_previous, "\n")

# The comparison reveals key insights if the three players are chosen:
  # Enhanced Performance, Lower Expense: By selecting players with a higher average OPS while spending less overall, 
  # the team stands to gain a competitive edge on the field without burdening the budget.
  # Intelligent Investment in Talent: Allocating funds based on OPS data signifies a strategic investment in player talent, 
  # ensuring that each dollar spent is likely to yield a higher return in game performance.
  # Smart Spending for Better Players: This player choice strategy shows smart spending - we get better players for less money, boosting performance without spending more.

  






