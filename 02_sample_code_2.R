rm(list = ls())
set.seed(0112358)
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here)
pacman::p_load(tidymodels, timetk, modeltime, tictoc)

# set paths ---------------------------------------------------
source(here("paths.R"))

# load data ----------------
df_train_test <- read_fst(here(path_processed_data, "1.6-df-train-test-sf.fst"))
min(df_train_test$date)
max(df_train_test$date)

# split data into training and test sets -------------------------------------
set.seed(0112358)
splits_resp <- df_train_test |>
  time_series_split(
    assess = "24 months",
    cumulative = TRUE,
    date_var = date
  )

min(training(splits_resp)$date)
max(training(splits_resp)$date)
min(testing(splits_resp)$date)
max(testing(splits_resp)$date)

# resampling strategies ----
## Strategy 0

set.seed(0112358999)

resamples_kfold_resp_og <- training(splits_resp) |> 
    time_series_cv(
    assess = "12 months",     # Length of each assessment period
    initial = "10 years",     # Initial training period
    slice_limit = 10,        # Number of slices to create
    cumulative = TRUE       # Use expanding window
  )

## Strategy 1
set.seed(0112358)
resamples_kfold_resp_s1 <- training(splits_resp) |>
  time_series_cv(
    assess = "6 months",     # fixed length of each assessment period
    initial = "5 years",     # Initial training period, will expand over folds
    slice_limit = 5,        # Max number of slices to create
    skip = "6 months",      # same as the assess period to avoid overlapping
    cumulative = TRUE       # Use expanding window
  )

## Strategy 2
set.seed(0112358)
resamples_kfold_resp_s2 <- training(splits_resp) |>
  time_series_cv(
    assess = "1 month",     # fixed length of each assessment period
    initial = "7 years",     # Initial training period, will expand over folds
    slice_limit = 10,        # Max number of slices to create
    skip = "1 month",      # same as the assess period to avoid overlapping
    cumulative = TRUE       # Use expanding window
  )

## Strategy 3
set.seed(0112358)
resamples_kfold_resp_s3 <- training(splits_resp) |>
  time_series_cv(
    assess = "12 months",     # fixed length of each assessment period
    initial = "5 years",     # Initial training period, will expand over folds
    slice_limit = 10,        # Max number of slices to create
    skip = "1 month",      # same as the assess period to avoid overlapping
    cumulative = TRUE       # Use expanding window
  )

## Strategy 4: Closely resembles original strategy without extending training periods
resamples_kfold_resp_s4 <- training(splits_resp) |>
  time_series_cv(
    assess = "12 months",     # Same validation period length as original
    initial = "5 years",     # Initial training period
    slice_limit = 6,         # Number of slices
    skip = "1 week",       # Same as assess period to avoid overlapping
    cumulative = TRUE       # Use fixed window instead of expanding
  )

## Strategy 5: Closely resembles original strategy without extending training periods
resamples_kfold_resp_s5 <- training(splits_resp) |>
  time_series_cv(
    assess = "12 months",     # same validation period length as original
    initial = "2 years",     # same initial training period as original
    slice_limit = 5,         # 5 vs 10 slices
    skip = "12 months",       # 1 year skip vs 1 day skip
    cumulative = TRUE       # Use expanding window similar to original
  )

# Strategy 6: New strategy with 10 year training period and 12 month validation period
resamples_kfold_resp_s6 <- training(splits_resp) |>
  time_series_cv(
    assess = "12 months",     # same validation period length as original
    initial = "10 years",     # same initial training period as original
    cumulative = TRUE       # Use expanding window similar to original
  )

# check the resampled data ----

## Function to get the dates of the training and validation periods for each fold
get_cv_dates <- function(resampling_object) {
  # Input validation
  if (!inherits(resampling_object, "time_series_cv")) {
    stop("Input must be a time_series_cv object")
  }
  
  # Extract fold information
  fold_info <- resampling_object$splits |>
    map_dfr(function(split) {
      # Get training data dates
      train_dates <- analysis(split) |>
        summarise(
          train_start = min(date),
          train_end = max(date)
        )
      
      # Get validation data dates
      val_dates <- assessment(split) |>
        summarise(
          val_start = min(date),
          val_end = max(date)
        )
      
      # Combine into one row
      bind_cols(
        fold = split$id,
        train_dates,
        val_dates
      )
    })
  
  # Format the output
  fold_info |>
    mutate(
      training_period = paste(format(train_start, "%Y-%m-%d"), "to", format(train_end, "%Y-%m-%d")),
      validation_period = paste(format(val_start, "%Y-%m-%d"), "to", format(val_end, "%Y-%m-%d"))
    ) |>
    select(id, training_period, validation_period)
}

# get the dates of the training and validation periods for each fold
cv_dates_og <- get_cv_dates(resamples_kfold_resp_og)
cv_dates_s1 <- get_cv_dates(resamples_kfold_resp_s1)
cv_dates_s2 <- get_cv_dates(resamples_kfold_resp_s2)
cv_dates_s3 <- get_cv_dates(resamples_kfold_resp_s3)
cv_dates_s4 <- get_cv_dates(resamples_kfold_resp_s4)
cv_dates_s5 <- get_cv_dates(resamples_kfold_resp_s5)
# Display the results
print(cv_dates_og, n = Inf)
# print(cv_dates_s1, n = Inf)
print(cv_dates_s2, n = Inf)
# print(cv_dates_s3, n = Inf)
print(cv_dates_s4, n = Inf)
print(cv_dates_s5, n = Inf)
