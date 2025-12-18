options(width=120)
knitr::opts_chunk$set(error = FALSE, warning = FALSE, message = FALSE)
library(tidyverse)
library(lubridate)
library(pwr)
library(effsize)

# read data
ratings <- read.csv("ratings.csv", stringsAsFactors = FALSE)

# quick dataset summary for the slides
n_total <- nrow(ratings)
vars <- names(ratings)
users <- length(unique(ratings$userId))
movies <- length(unique(ratings$movieId))

cat(paste0("Dataset: ratings.csv\nRecords: ", n_total, " rows\nVariables: ", paste(vars, collapse=", "), "\nUnique users: ", users, "\nUnique movies: ", movies))

# --- EDA ---
summary_stats <- ratings %>%
  summarise(n = n(),
            mean_rating = mean(rating),
            sd_rating = sd(rating),
            min_rating = min(rating),
            max_rating = max(rating))
summary_stats

ratings %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth=0.5) +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count")

# --- Prepare groups (early vs late) ---
if("timestamp" %in% names(ratings)){
  ratings <- ratings %>% mutate(ts = as.numeric(timestamp))
  median_ts <- median(ratings$ts, na.rm = TRUE)
  ratings <- ratings %>% mutate(period = ifelse(ts < median_ts, "early", "late"))
} else {
  set.seed(2025)
  ratings <- ratings %>% mutate(period = sample(c("early", "late"), size = n(), replace = TRUE))
}

ratings %>% group_by(period) %>% summarise(n = n(), mean = mean(rating), sd = sd(rating))

# --- Observed effect size (Cohen's d) ---
group_summary <- ratings %>% group_by(period) %>% summarise(n = n(), mean = mean(rating), 
                                                            sd = sd(rating))
means <- group_summary$mean
sds <- group_summary$sd
ns <- group_summary$n

# pooled sd
s_pooled <- sqrt(((ns[1]-1)*sds[1]^2 + (ns[2]-1)*sds[2]^2) / (ns[1]+ns[2]-2))
cohen_d_obs <- (means[1] - means[2]) / s_pooled

list(pooled_sd = s_pooled, cohen_d = cohen_d_obs)

# --- Power calculation: two-sample t-test (early vs late) ---
# observed two-sample effect (use cohen_d_obs from earlier)
d_obs_two <- abs(cohen_d_obs)

# post-hoc power with current group sizes
n1 <- ns[1]; n2 <- ns[2]
posthoc_two <- pwr.t.test(n = min(n1, n2), d = d_obs_two, sig.level = 0.05, type = "two.sample", 
                          alternative = "two.sided")

# required n per group for small/medium/large d
req_two_small <- pwr.t.test(n = NULL, d = 0.2, sig.level = 0.05, power = 0.8, type = "two.sample")$n
req_two_med <- pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.8, type = "two.sample")$n
req_two_large <- pwr.t.test(n = NULL, d = 0.8, sig.level = 0.05, power = 0.8, type = "two.sample")$n

list(d_obs_two = d_obs_two, n1 = n1, n2 = n2, posthoc_power_two = posthoc_two$power,
     req_two_small = req_two_small, req_two_med = req_two_med, req_two_large = req_two_large)

