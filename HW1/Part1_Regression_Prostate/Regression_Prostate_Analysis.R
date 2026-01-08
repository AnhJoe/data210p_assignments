# Part A: Load dataset, verify dimensions, column names, missing values, data types
df <- read.table("./HW1/Part1_Regression_Prostate/prostate.csv")

dim(df)
nrow(df)
ncol(df)
names(df)
str(df)
summary(df)
colSums(is.na(df))

train_df <- df %>% filter(train)
test_df <- df %>% filter(!train)

# Part B: Full exploratory data analysis

library(tidyverse) 
library(GGally)

ggplot(df, aes(x = lpsa)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of lpsa")

predictors <- c("lcavol", "lweight", "age", "lbph")

df %>%
  pivot_longer(all_of(predictors)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ name, scales = "free") +
  labs(title = "Distributions of Selected Predictors")

GGally::ggpairs(
  df %>% 
    select(lpsa, lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45)
)

# Part D: Train indicator

train_df <- df %>% filter(train)
test_df  <- df %>% filter(!train)

n_train <- nrow(train_df)
n_test  <- nrow(test_df)

cat(sprintf("Training set size: %d\n", n_train))
cat(sprintf("Test set size: %d\n", n_test))