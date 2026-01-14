# library loading + wd
library(tidyverse)
library(ggplot2)
library(corrplot)
library(gridExtra)
setwd("D:/data210p_assignments/HW1/Part2_AI_Data_JobMarket/Data")

# 1a
df <- read.csv("ai_jobs.csv", stringsAsFactors = FALSE)

# 5 fold train-test split indicator
set.seed(210)
train_indices <- sample(1:nrow(df), size = floor(0.8 * nrow(df)))
df$train <- FALSE
df$train[train_indices] <- TRUE

cat("Dataset Dimensions:\n")
cat(sprintf("Rows: %d\n", nrow(df)))
cat(sprintf("Columns: %d\n", ncol(df)))

cat("\nColumn Names:\n")
print(colnames(df))

cat("\nData Types:\n")
print(sapply(df, class))

cat("\nHead:\n")
print(head(df))

cat("\nSummary Statistics:\n")
print(summary(df))

cat("\nMissing Values by Column:\n")
missing_counts <- colSums(is.na(df))
missing_percentages <- round(100 * missing_counts / nrow(df), 2)
missing_df <- data.frame(
  Column = names(missing_counts),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentages
)
print(missing_df)

cat("\nTotal Missing Values: ", sum(is.na(df)), "\n")

# 1b

cat("\nJob Title Distribution:\n")
print(head(sort(table(df$job_title), decreasing = TRUE)))

cat("\nCompany Type Distribution:\n")
print(table(df$company_type))

cat("\nIndustry Distribution:\n")
print(table(df$industry))

cat("\nCountry Distribution:\n")
print(head(sort(table(df$country), decreasing = TRUE)))

cat("\nRemote Type Distribution:\n")
print(table(df$remote_type))

cat("\nExperience Level Distribution:\n")
print(table(df$experience_level))

cat("\nEmployment Type Distribution:\n")
print(table(df$employment_type))

cat("\nCompany Size Distribution:\n")
print(table(df$company_size))

cat("\nPosted Year Distribution:\n")
print(table(df$posted_year))

cat("\nMinimum Experience Years:\n")
print(summary(df$min_experience_years))

cat("\nSalary Statistics:\n")
cat("Salary Min USD:\n")
print(summary(df$salary_min_usd))
cat("\nSalary Max USD:\n")
print(summary(df$salary_max_usd))

cat("\nAverage Salary Statistics:\n")
df$salary_avg <- (df$salary_min_usd + df$salary_max_usd) / 2
print(summary(df$salary_avg))

cat("\nSalary Range Statistics:\n")
df$salary_range <- df$salary_max_usd - df$salary_min_usd
print(summary(df$salary_range))

cat("\nAverage Salary by Experience Level:\n")
salary_by_exp <- df %>%
  group_by(experience_level) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_exp)

cat("\nAverage Salary by Company Type:\n")
salary_by_company <- df %>%
  group_by(company_type) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_company)

cat("\nAverage Salary by Remote Type:\n")
salary_by_remote <- df %>%
  group_by(remote_type) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_remote)

cat("\nAverage Salary by Industry:\n")
salary_by_industry <- df %>%
  group_by(industry) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_industry)

cat("\nAverage Salary by Company Size:\n")
salary_by_size <- df %>%
  group_by(company_size) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_size)

cat("\nAverage Salary by Employment Type:\n")
salary_by_employment <- df %>%
  group_by(employment_type) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_employment)

cat("\nAverage Salary by Posted Year:\n")
salary_by_year <- df %>%
  group_by(posted_year) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(posted_year)
print(salary_by_year)

cat("\nAverage Salary by Country:\n")
salary_by_country_top <- df %>%
  group_by(country) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_country_top)

cat("\nAverage Salary by Job Title:\n")
salary_by_title <- df %>%
  group_by(job_title) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_title)

cat("\nAverage Salary by Top 10 Cities:\n")
city_counts_summary <- sort(table(df$city), decreasing = TRUE)
top_n_cities_summary <- min(10, length(city_counts_summary))
top_cities_summary <- names(city_counts_summary)[1:top_n_cities_summary]
salary_by_city <- df %>%
  filter(city %in% top_cities_summary) %>%
  group_by(city) %>%
  summarize(
    count = n(),
    avg_salary = mean(salary_avg, na.rm = TRUE),
    median_salary = median(salary_avg, na.rm = TRUE),
    sd_salary = sd(salary_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_salary))
print(salary_by_city)

# !!!! PLOTS CODE STARTS HERE !!!!
p1 <- ggplot(df, aes(x = salary_avg, fill = experience_level)) +
  geom_histogram(bins = 50, color = "black", alpha = 0.6, position = "identity") +
  labs(title = "Distribution of Average Salary by Experience Level",
       x = "Average Salary (USD)",
       y = "Frequency",
       fill = "Experience Level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(df, aes(x = log(salary_avg), fill = experience_level)) +
  geom_histogram(bins = 50, color = "black", alpha = 0.6, position = "identity") +
  labs(title = "Distribution of Log(Average Salary) by Experience Level",
       x = "Log(Average Salary)",
       y = "Frequency",
       fill = "Experience Level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p3 <- ggplot(df, aes(x = experience_level, y = salary_avg, fill = experience_level)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               aes(fill = experience_level), color = "black", stroke = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, linewidth = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0("$", format(round(after_stat(y), 0), big.mark = ",", scientific = FALSE))),
               hjust = -0.2, vjust = 0.5, size = 3.5, fontface = "bold") +
  labs(title = "Salary by Experience Level (with Mean and 95% CI)",
       x = "Experience Level",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

p4 <- ggplot(df, aes(x = company_type, y = salary_avg, fill = company_type)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               aes(fill = company_type), color = "black", stroke = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, linewidth = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0("$", format(round(after_stat(y), 0), big.mark = ",", scientific = FALSE))),
               hjust = -0.2, vjust = 0.5, size = 3.5, fontface = "bold") +
  labs(title = "Salary by Company Type (with Mean and 95% CI)",
       x = "Company Type",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

p5 <- ggplot(df, aes(x = remote_type, y = salary_avg, fill = remote_type)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               aes(fill = remote_type), color = "black", stroke = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, linewidth = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0("$", format(round(after_stat(y), 0), big.mark = ",", scientific = FALSE))),
               hjust = -0.2, vjust = 0.5, size = 3.5, fontface = "bold") +
  labs(title = "Salary by Remote Type (with Mean and 95% CI)",
       x = "Remote Type",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

p6 <- ggplot(df, aes(x = factor(posted_year))) +
  geom_bar(fill = "darkgreen", alpha = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(title = "Number of Jobs Posted by Year",
       x = "Year",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

country_counts <- sort(table(df$country), decreasing = TRUE)
top_n_countries <- min(10, length(country_counts))
top_countries <- names(country_counts)[1:top_n_countries]
df_top_countries <- df[df$country %in% top_countries, ]
p7 <- ggplot(df_top_countries, aes(x = reorder(country, country, function(x) -length(x)))) +
  geom_bar(fill = "purple", alpha = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(title = paste0("Job Postings by Country"),
       x = "Country",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

salary_by_country <- df_top_countries %>%
  group_by(country) %>%
  summarize(avg_salary = mean(salary_avg, na.rm = TRUE)) %>%
  arrange(desc(avg_salary))
p8 <- ggplot(salary_by_country, aes(x = reorder(country, avg_salary), y = avg_salary)) +
  geom_col(fill = "orange", alpha = 0.7) +
  geom_text(aes(label = paste0("$", format(round(avg_salary, 0), big.mark = ",", scientific = FALSE))), 
            hjust = 1.1, size = 3.5, fontface = "bold", color = "black") +
  coord_flip() +
  labs(title = paste0("Average Salary by Country"),
       x = "Country",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

lm_model <- lm(salary_avg ~ min_experience_years, data = df)
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]
r_squared <- summary(lm_model)$r.squared
formula_label <- paste0("y = ", format(round(slope, 2), big.mark = ","), "x + ", 
                        format(round(intercept, 0), big.mark = ","),
                        "\nR² = ", round(r_squared, 3))
p9 <- ggplot(df, aes(x = min_experience_years, y = salary_avg)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  annotate("text", x = max(df$min_experience_years, na.rm = TRUE) * 0.7, 
           y = max(df$salary_avg, na.rm = TRUE) * 0.95, 
           label = formula_label, size = 4, fontface = "bold", hjust = 0) +
  labs(title = "Salary vs. Minimum Experience Years",
       x = "Minimum Experience Years",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p10 <- ggplot(df, aes(x = reorder(job_title, salary_avg, FUN = mean), 
                      y = salary_avg, fill = job_title)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               aes(fill = job_title), color = "black", stroke = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, linewidth = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0("$", format(round(after_stat(y), 0), big.mark = ",", scientific = FALSE))),
               hjust = -0.2, vjust = 0.5, size = 3.5, fontface = "bold") +
  labs(title = "Salary by Job Title (with Mean and 95% CI)",
       x = "Job Title",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

p11 <- ggplot(df, aes(x = reorder(industry, salary_avg, FUN = mean), 
                      y = salary_avg, fill = industry)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               aes(fill = industry), color = "black", stroke = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, linewidth = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0("$", format(round(after_stat(y), 0), big.mark = ",", scientific = FALSE))),
               hjust = -0.2, vjust = 0.5, size = 3.5, fontface = "bold") +
  labs(title = "Salary by Industry (with Mean and 95% CI)",
       x = "Industry",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

p12 <- ggplot(df, aes(x = company_size, y = salary_avg, fill = company_size)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               aes(fill = company_size), color = "black", stroke = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, linewidth = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0("$", format(round(after_stat(y), 0), big.mark = ",", scientific = FALSE))),
               hjust = -0.2, vjust = 0.5, size = 3.5, fontface = "bold") +
  labs(title = "Salary by Company Size (with Mean and 95% CI)",
       x = "Company Size",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

p13 <- ggplot(df, aes(x = factor(posted_year), y = salary_avg, fill = factor(posted_year))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               aes(fill = factor(posted_year)), color = "black", stroke = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, linewidth = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0("$", format(round(after_stat(y), 0), big.mark = ",", scientific = FALSE))),
               hjust = -0.2, vjust = 0.5, size = 3.5, fontface = "bold") +
  labs(title = "Salary Trends by Posted Year (with Mean and 95% CI)",
       x = "Posted Year",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

salary_by_city_all <- df %>%
  group_by(city) %>%
  summarize(avg_salary = mean(salary_avg, na.rm = TRUE),
            count = n()) %>%
  filter(count >= 5) %>%
  arrange(avg_salary)
n_cities <- nrow(salary_by_city_all)
decile_cities <- salary_by_city_all[floor(seq(1, n_cities, length.out = 10)), ]
selected_cities <- decile_cities$city
df_decile_cities <- df[df$city %in% selected_cities, ]
p14 <- ggplot(df_decile_cities, aes(x = reorder(city, salary_avg, FUN = mean), 
                                     y = salary_avg, fill = city)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               aes(fill = city), color = "black", stroke = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, linewidth = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste0("$", format(round(after_stat(y), 0), big.mark = ",", scientific = FALSE))),
               hjust = -0.2, vjust = 0.5, size = 3.5, fontface = "bold") +
  labs(title = "Salary by City (One City per Decile with Mean and 95% CI)",
       x = "City",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

numerical_vars <- df[, c("min_experience_years", "salary_min_usd", 
                         "salary_max_usd", "salary_avg", "salary_range")]
cor_matrix <- cor(numerical_vars, use = "complete.obs")

# print
print(p1)   # freq graph avg sal by exp
print(p2)   # freq graph log sal by exp
print(p3)   # sal by exp
print(p9)   # regression for sal by min exp
print(p4)   # sal by company
print(p5)   # sal by remote
print(p6)   # num jobs by year
print(p7)   # job posting by country
print(p8)   # avg sal by country
print(p10)  # sal by job title
print(p11)  # sal by industry
print(p12)  # sal by company size
print(p13)  # sal trends by year
print(p14)  # sal by city ***decile***

cat("\nCorrelation Matrix\n")
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numerical Variables",
         mar = c(0, 0, 2, 0))

# 1c
# this bit goes in the report

# 1d
cat("\n5N Test Split:\n")
train_set <- df[df$train == TRUE, ]
test_set <- df[df$train == FALSE, ]

n_train <- nrow(train_set)
n_test <- nrow(test_set)
n_total <- nrow(df)

cat("Dataset Split Summary:\n")
cat(sprintf("Total observations: %d\n", n_total))
cat(sprintf("Training set size (n_train): %d (%.1f%%)\n", n_train, 100 * n_train / n_total))
cat(sprintf("Test set size (n_test): %d (%.1f%%)\n", n_test, 100 * n_test / n_total))

cat("\nExperience Level Distribution:\n")
cat("Training Set:\n")
print(prop.table(table(train_set$experience_level)))
cat("\nTest Set:\n")
print(prop.table(table(test_set$experience_level)))

cat("\nChi-Square Test for Experience Level:\n")
exp_table <- table(df$train, df$experience_level)
chi_exp <- chisq.test(exp_table)
print(chi_exp)
cat(sprintf("p-value: %.4f\n", chi_exp$p.value))
cramers_v_exp <- sqrt(chi_exp$statistic / (sum(exp_table) * (min(nrow(exp_table), ncol(exp_table)) - 1)))
cat(sprintf("Cramer's V (effect size): %.4f\n", cramers_v_exp))
if (cramers_v_exp < 0.1) {
  cat("Negligible Effect - good split despite p-value\n")
} else {
  cat("Effect size indicates meaningful difference if bad p-value\n")
}

cat("\nCompany Type Distribution:\n")
cat("Training Set:\n")
print(prop.table(table(train_set$company_type)))
cat("\nTest Set:\n")
print(prop.table(table(test_set$company_type)))

cat("\nChi-Square Test for Company Type:\n")
company_table <- table(df$train, df$company_type)
chi_company <- chisq.test(company_table)
print(chi_company)
cat(sprintf("p-value: %.4f\n", chi_company$p.value))
cramers_v_company <- sqrt(chi_company$statistic / (sum(company_table) * (min(nrow(company_table), ncol(company_table)) - 1)))
cat(sprintf("Cramer's V (effect size): %.4f\n", cramers_v_company))
if (cramers_v_company < 0.1) {
  cat("Negligible Effect - good split despite p-value\n")
} else {
  cat("Effect size indicates meaningful difference if bad p-value\n")
}

cat("\nSalary Statistics Comparison:\n")
cat("Training Set - Average Salary:\n")
print(summary(train_set$salary_avg))
cat("\nTest Set - Average Salary:\n")
print(summary(test_set$salary_avg))

cat("\nTwo-Sample t-test for Salary:\n")
t_test_salary <- t.test(train_set$salary_avg, test_set$salary_avg)
print(t_test_salary)
cat(sprintf("p-value: %.4f\n", t_test_salary$p.value))
pooled_sd <- sqrt(((n_train - 1) * sd(train_set$salary_avg)^2 + (n_test - 1) * sd(test_set$salary_avg)^2) / (n_train + n_test - 2))
cohens_d <- (mean(train_set$salary_avg) - mean(test_set$salary_avg)) / pooled_sd
cat(sprintf("Cohen's d (effect size): %.4f\n", cohens_d))
if (abs(cohens_d) < 0.2) {
  cat("Negligible Effect - good split despite p-value\n")
} else {
  cat("Effect size indicates meaningful difference if bad p-value\n")
}

cat("\nKolmogorov-Smirnov Test for Salary Distribution:\n")
ks_test_salary <- ks.test(train_set$salary_avg, test_set$salary_avg)
print(ks_test_salary)
cat(sprintf("p-value: %.4f\n", ks_test_salary$p.value))
cat(sprintf("D statistic (max difference): %.4f\n", ks_test_salary$statistic))
if (ks_test_salary$statistic < 0.1) {
  cat("Negligible Effect - good split despite p-value\n")
} else {
  cat("Effect size indicates meaningful difference if bad p-value\n")
}

# all p-values are above 0.5 so added effect size interpretations
# with 50,000 rows small differences can be significant
# effect size calcs show good split on all tests *****


# 2a
# log transform outcome variable (not sure if required)
df$log_salary_avg <- log(df$salary_avg)

cat("Response Variable: log(salary_avg)\n")
cat("\nSummary of log_salary_avg:\n")
print(summary(df$log_salary_avg))

cat("\nComparison: Original vs Log-Transformed:\n")
comparison_df <- data.frame(
  Variable = c("salary_avg", "log_salary_avg"),
  Min = c(min(df$salary_avg, na.rm = TRUE), min(df$log_salary_avg, na.rm = TRUE)),
  Median = c(median(df$salary_avg, na.rm = TRUE), median(df$log_salary_avg, na.rm = TRUE)),
  Mean = c(mean(df$salary_avg, na.rm = TRUE), mean(df$log_salary_avg, na.rm = TRUE)),
  Max = c(max(df$salary_avg, na.rm = TRUE), max(df$log_salary_avg, na.rm = TRUE)),
  SD = c(sd(df$salary_avg, na.rm = TRUE), sd(df$log_salary_avg, na.rm = TRUE))
)
print(comparison_df)

# 2b
p2b_1 <- ggplot(df, aes(x = experience_level, y = log_salary_avg, fill = experience_level)) +
  geom_boxplot() +
  labs(title = "Log(Salary) by Experience Level", x = "Experience Level", y = "Log(Salary)") +
  theme_minimal() +
  theme(legend.position = "none")

p2b_2 <- ggplot(df, aes(x = company_type, y = log_salary_avg, fill = company_type)) +
  geom_boxplot() +
  labs(title = "Log(Salary) by Company Type", x = "Company Type", y = "Log(Salary)") +
  theme_minimal() +
  theme(legend.position = "none")

p2b_3 <- ggplot(df, aes(x = remote_type, y = log_salary_avg, fill = remote_type)) +
  geom_boxplot() +
  labs(title = "Log(Salary) by Remote Type", x = "Remote Type", y = "Log(Salary)") +
  theme_minimal() +
  theme(legend.position = "none")

p2b_4 <- ggplot(df, aes(x = industry, y = log_salary_avg, fill = industry)) +
  geom_boxplot() +
  labs(title = "Log(Salary) by Industry", x = "Industry", y = "Log(Salary)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p2b_5 <- ggplot(df, aes(x = company_size, y = log_salary_avg, fill = company_size)) +
  geom_boxplot() +
  labs(title = "Log(Salary) by Company Size", x = "Company Size", y = "Log(Salary)") +
  theme_minimal() +
  theme(legend.position = "none")

p2b_6 <- ggplot(df, aes(x = employment_type, y = log_salary_avg, fill = employment_type)) +
  geom_boxplot() +
  labs(title = "Log(Salary) by Employment Type", x = "Employment Type", y = "Log(Salary)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p2b_7 <- ggplot(df, aes(x = min_experience_years, y = log_salary_avg)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Log(Salary) vs Experience Years", x = "Min Experience Years", y = "Log(Salary)") +
  theme_minimal()

p2b_8 <- ggplot(df, aes(x = factor(posted_year), y = log_salary_avg, fill = factor(posted_year))) +
  geom_boxplot() +
  labs(title = "Log(Salary) by Posted Year", x = "Posted Year", y = "Log(Salary)") +
  theme_minimal() +
  theme(legend.position = "none")

cat("Correlation of log_salary_avg with numeric features:\n")
numeric_features <- df[, c("min_experience_years", "salary_min_usd", "salary_max_usd", "log_salary_avg")]
cor_with_outcome <- cor(numeric_features, use = "complete.obs")
print(cor_with_outcome[, "log_salary_avg"])

# print plots
print(p2b_1)
print(p2b_2)
print(p2b_3)
print(p2b_4)
print(p2b_5)
print(p2b_6)
print(p2b_7)
print(p2b_8)

# 2c
# **** missing values was done in 1a ****

cat("\nIQR Outlier :\n")
Q1_salary <- quantile(df$salary_avg, 0.25, na.rm = TRUE)
Q3_salary <- quantile(df$salary_avg, 0.75, na.rm = TRUE)
IQR_salary <- Q3_salary - Q1_salary
lower_bound <- Q1_salary - 1.5 * IQR_salary
upper_bound <- Q3_salary + 1.5 * IQR_salary

outliers_salary <- df[df$salary_avg < lower_bound | df$salary_avg > upper_bound, ]
cat(sprintf("Number of salary outliers: %d (%.2f%%)\n", nrow(outliers_salary), 100 * nrow(outliers_salary) / nrow(df)))
cat(sprintf("Outlier bounds: $%,.0f to $%,.0f\n", lower_bound, upper_bound))

if (nrow(outliers_salary) > 0) {
  cat("\nSummary of outlier salaries:\n")
  print(summary(outliers_salary$salary_avg))
}

p2c_1 <- ggplot(df, aes(y = salary_avg)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = lower_bound, color = "red", linetype = "dashed") +
  geom_hline(yintercept = upper_bound, color = "red", linetype = "dashed") +
  labs(title = "Salary Distribution with Outlier Bounds", y = "Average Salary (USD)") +
  theme_minimal()

p2c_2 <- ggplot(df, aes(x = salary_avg)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = lower_bound, color = "red", linetype = "dashed") +
  geom_vline(xintercept = upper_bound, color = "red", linetype = "dashed") +
  labs(title = "Salary Distribution with Outlier Bounds", x = "Average Salary (USD)", y = "Frequency") +
  theme_minimal()

print(p2c_1)
print(p2c_2)

cat("\nBAD DATA CHECKS:\n")
cat("Jobs with salary_min > salary_max:\n")
anomaly_salary <- df[df$salary_min_usd > df$salary_max_usd, ]
cat(sprintf("Count: %d\n", nrow(anomaly_salary)))

cat("\nJobs with zero or negative salaries:\n")
anomaly_zero <- df[df$salary_min_usd <= 0 | df$salary_max_usd <= 0, ]
cat(sprintf("Count: %d\n", nrow(anomaly_zero)))

cat("\nJobs with extreme salary ranges:\n")
extreme_range <- df[df$salary_range > quantile(df$salary_range, 0.99, na.rm = TRUE), ]
cat(sprintf("Count (>99th percentile): %d\n", nrow(extreme_range)))
cat(sprintf("99th percentile salary range: $%,.0f\n", quantile(df$salary_range, 0.99, na.rm = TRUE)))

cat("\nPATTERN ANALYSIS:\n")
cat("\nSalary distribution by experience level:\n")
exp_pattern <- df %>%
  group_by(experience_level) %>%
  summarize(
    count = n(),
    min_sal = min(salary_avg, na.rm = TRUE),
    q25_sal = quantile(salary_avg, 0.25, na.rm = TRUE),
    median_sal = median(salary_avg, na.rm = TRUE),
    q75_sal = quantile(salary_avg, 0.75, na.rm = TRUE),
    max_sal = max(salary_avg, na.rm = TRUE),
    outliers = sum(salary_avg < (quantile(salary_avg, 0.25) - 1.5*IQR(salary_avg)) | 
                   salary_avg > (quantile(salary_avg, 0.75) + 1.5*IQR(salary_avg)))
  )
print(exp_pattern)

cat("\nSalary distribution by company type:\n")
company_pattern <- df %>%
  group_by(company_type) %>%
  summarize(
    count = n(),
    min_sal = min(salary_avg, na.rm = TRUE),
    median_sal = median(salary_avg, na.rm = TRUE),
    max_sal = max(salary_avg, na.rm = TRUE),
    cv = sd(salary_avg, na.rm = TRUE) / mean(salary_avg, na.rm = TRUE)
  )
print(company_pattern)

cat("\nTemporal patterns (by year):\n")
year_pattern <- df %>%
  group_by(posted_year) %>%
  summarize(
    count = n(),
    avg_sal = mean(salary_avg, na.rm = TRUE),
    median_sal = median(salary_avg, na.rm = TRUE),
    sd_sal = sd(salary_avg, na.rm = TRUE),
    cv = sd(salary_avg, na.rm = TRUE) / mean(salary_avg, na.rm = TRUE)
  )
print(year_pattern)

# 2d
cat("1. Response Variable:\n")
cat(sprintf("   - Log-transformed salary created: log_salary_avg\n"))
cat(sprintf("   - Range: %.3f to %.3f\n", min(df$log_salary_avg, na.rm = TRUE), max(df$log_salary_avg, na.rm = TRUE)))
cat(sprintf("   - Mean: %.3f, Median: %.3f\n", mean(df$log_salary_avg, na.rm = TRUE), median(df$log_salary_avg, na.rm = TRUE)))

cat("\n2. Data Quality:\n")
cat(sprintf("   - Total observations: %d\n", nrow(df)))
cat(sprintf("   - Missing values: %d (%.2f%%)\n", sum(is.na(df)), 100 * sum(is.na(df)) / (nrow(df) * ncol(df))))
cat(sprintf("   - Salary outliers (IQR): %d (%.2f%%)\n", nrow(outliers_salary), 100 * nrow(outliers_salary) / nrow(df)))
cat(sprintf("   - Salary anomalies: %d\n", nrow(anomaly_salary) + nrow(anomaly_zero)))

cat("\n3. Feature Correlations with Log(Salary):\n")
cat(sprintf("   - min_experience_years: %.3f\n", cor(df$min_experience_years, df$log_salary_avg, use = "complete.obs")))
cat(sprintf("   - salary_min_usd: %.3f\n", cor(df$salary_min_usd, df$log_salary_avg, use = "complete.obs")))
cat(sprintf("   - salary_max_usd: %.3f\n", cor(df$salary_max_usd, df$log_salary_avg, use = "complete.obs")))

cat("\n4. Categorical Patterns:\n")
cat(sprintf("   - Experience levels: %d\n", length(unique(df$experience_level))))
cat(sprintf("   - Company types: %d\n", length(unique(df$company_type))))
cat(sprintf("   - Industries: %d\n", length(unique(df$industry))))
cat(sprintf("   - Countries: %d\n", length(unique(df$country))))
cat(sprintf("   - Years: %d (%d to %d)\n", length(unique(df$posted_year)), min(df$posted_year), max(df$posted_year)))

cat("\n5. Temporal Trends:\n")
year_trend_summary <- df %>%
  group_by(posted_year) %>%
  summarize(avg_salary = mean(salary_avg, na.rm = TRUE))
cat(sprintf("   - Salary change over time: $%,.0f (%d) to $%,.0f (%d)\n", 
            year_trend_summary$avg_salary[1], year_trend_summary$posted_year[1],
            year_trend_summary$avg_salary[nrow(year_trend_summary)], 
            year_trend_summary$posted_year[nrow(year_trend_summary)]))