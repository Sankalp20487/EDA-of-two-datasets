cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse)
#Assignment-1
#Q1----
data_2015 <- read.csv("2015.csv")
head(data_2015)

#Q2----
names(data_2015)

#Q3----
View(data_2015)

#Q4----
glimpse(data_2015)

#Q5----
p_load(janitor)
data_2015 <- clean_names(data_2015)
data_2015

#Q6----
happy_df <- select(data_2015,country,region,happiness_score,freedom)

#Q7----

top_ten_df <- slice(happy_df, 1:10)

#Q8-----
no_freedom_df <- filter(happy_df, freedom < 0.20)

#Q9-----
best_freedom_df <- arrange(happy_df, desc(freedom))

#Q10----
data_2015 <- mutate(data_2015, gff_stat = family + freedom + generosity)

#Q11----
happy_summary <- data.frame(
  happy_df %>%
    summarise(
      mean_happiness = mean(happiness_score),
      max_happiness      = max(happiness_score),
      mean_freedom = mean(freedom),
      max_freedom  = max(freedom)
    ))

#Q12----
regional_stats_df <- data.frame(
  happy_df %>%
    group_by(region) %>%
    summarise(
      country_count = n(),
      mean_happiness = mean(happiness_score),
      mean_freedom = mean(freedom)
    ))

#Q13----
#Ten Least happy Western European Countries
western_europe_region <- filter(data_2015, region== "Western Europe")
western_europe_region
least_happy_western<- arrange(western_europe_region, happiness_score)
ten_least_happy_western <-slice(least_happy_western, 1:10)

#Ten happiest Sub Saharan African Countries
Sub_Sahara <- filter(data_2015, region== "Sub-Saharan Africa")
Sub_Sahara
happy_sahara <- arrange(Sub_Sahara, desc(happiness_score))
ten_happy_sahara <-slice(happy_sahara, 1:10)

#Avg. GDP of 10 least happy European Countires
europe_gdp <- data.frame(
  ten_least_happy_western %>%
    summarise(
      europe_gdp = mean(economy_gdp_per_capita)
    ))

#Avg. GDP of 10 happiest Sub Saharan African Countires
africa_gdp <- data.frame(
  ten_happy_sahara %>%
    summarise(
      africa_gdp = mean(economy_gdp_per_capita)
    ))
#Avg. GDP of 10 least happy European countries vs 10 Happiest Sub Saharan Countries
gdp_df <- mutate(europe_gdp,africa_gdp)
view(gdp_df)

####Q14----
ggplot(data = regional_stats_df, aes(x = mean_happiness, y = mean_freedom, col = region)) +
  geom_point() + geom_segment(aes(x = min(mean_happiness), y = min(mean_freedom), xend = max(mean_happiness), yend = max(mean_freedom), 
                           col = "black"   )) 
  labs(title = "Mean Happiness vs Mean Freedom",
       x = "mean_happiness",
       y = "mean_freedom")

  
####Assignment-2----

#Q1----
baseball <- read.csv("baseball.csv")

#Q2----
  
#Q3----
#str has been used to display structure of the cols
names(baseball)
str(baseball)

#Q4----
#summarise, calculates summary statistics for variables such as "HR," "H," and "R" grouped by the "Age" column, resulting in the age_stats_df data frame.
age_stats_df <- data.frame(
  baseball %>%
    group_by(Age) %>%
    summarise(
      Count = n(),
      HR = mean(HR),
      H = mean(H),
      R = mean(R)
      
    )) 
#Q5----
#keeps the values in AB>0 and stores in baseball
baseball<- filter(baseball, AB>0)

#Q6----
#Adds another col "BA"
baseball<- mutate(baseball, BA = H/AB)

#Q7----
#rounds off the values in col "BA" to third decimal
baseball$BA <- round(baseball$BA,3)

#Q8----
#Adds col "OBP"
baseball<- mutate(baseball, OBP = (H + BB) / (AB + BB))

#Q9----
#rounds off the values in col "OBP" to third decimal
baseball$OBP <- round(baseball$OBP,3)

#Q10----
#Gets the top 10 players with highest Strickout counts
strikeout_artist<- arrange(baseball, desc(SO))
strikeout_artist<- slice(strikeout_artist,1:10)

#Q11----
#Plots a scatterplot for HR vs RBI
ggplot(data = baseball, aes(x = HR, y = RBI)) +
  geom_point() + 
labs(title = "HR vs RBI",
     x = "HR",
     y = "RBI")

#Q12----
#Filters out AB>=300 or G>=100 and stores in eligible_df
eligible_df <- filter(baseball, AB>=300 | G>=100)

#Q13----
#Plots a histogram
ggplot(eligible_df,aes(x = BA))+
  geom_histogram(binwidth = 0.025,fill = "green",colour = "blue")+
  labs(title = "Batting Average Histogram for Eligible Players",
       x = "Batting Average",
       y = "Count")

#Q14----
#uses rank function to rank players and adds a col RankHR using mutate 

eligible_df <- eligible_df |>
  mutate(RankHR =rank(-1 * HR, ties.method = "min"))
eligible_df

#Q15----
#uses rank function to rank players and adds a col RankRBI using mutate

eligible_df <- eligible_df |>
  mutate(RankRBI =rank(-1*RBI, ties.method = "min"))
eligible_df

#Q16----
#uses rank function to rank players and adds a col RankOBP using mutate
eligible_df <- eligible_df |>
  mutate(RankOBP =rank(-1*OBP, ties.method = "min"))
eligible_df
eligible_df <- mutate(eligible_df,TotalRank = RankHR + RankRBI + RankOBP)

#Q17----
#selected the top 20 MVP candidates based on the total rank
mvp_candidates <- arrange(eligible_df,TotalRank)
mvp_candidates <- slice(mvp_candidates, 1:20)

#Q18----
#created a summary table with information about the top MVP candidates
mvp_candidates_abbreviated <- select(mvp_candidates,First, Last, RankHR,
                                     RankRBI,RankOBP,TotalRank )

#Q19----
#Displayed the player with least total rank
sorted_data <- mvp_candidates[order(mvp_candidates$TotalRank), ]


mvp <- sorted_data[1, ]


cat("League MVP:\n")
cat("First Name: ", mvp$First, "\n")
cat("Last Name: ", mvp$Last, "\n")
cat("TotalRank: ", mvp$TotalRank, "\n")
cat("HR: ", mvp$HR, "\n")
cat("RBI: ", mvp$RBI, "\n")
cat("OBP: ", mvp$OBP, "\n")

p_load(testthat)
test_file("project2_tests.R")


