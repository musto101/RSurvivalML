#' preprocessing test and train for KNN imputation
#'
#' This function takes train and test dfs and replaces NA values with KNN imputation
#' with a user specified value for K
#'
#' @param exp_dat is a list of the train and test dataframes, an external dataset can also be included.
#' @param time_var is the column in the df corresponding to time.
#' @param target_var is the column in the df corresponding to the target variable.
#' @return It returns a list containing train and test.
#' @export
#'
surv_imputation <- function(exp_dat, time_var, target_var) {

  library(tidyverse)
  library(caret)

  train_stime <- exp_dat[[1]] %>%
    select(all_of(target_var), all_of(time_var))

  train_wo_time <- exp_dat[[1]] %>%
    select(-all_of(target_var), -all_of(time_var))

  train_na <- preProcess(as.data.frame(train_wo_time[, -1]),
                         method = "knnImpute")

  dat_train <- predict(train_na, train_wo_time)
  dat_train <- cbind(dat_train, train_stime)

  dat_train$X <- NULL

  test_stime <- exp_dat[[2]] %>%
    select(all_of(target_var), all_of(time_var))

  test_wo_time <- exp_dat[[2]] %>%
    select(-all_of(target_var), -all_of(time_var))

  test_na <- preProcess(as.data.frame(rbind(train_wo_time[, -1],
                                             test_wo_time[, -1]),
                                       method = "knnImpute"))

  test_dat <- predict(test_na, test_wo_time)

  test_dat <-  cbind(test_dat, test_stime)

  test_dat$X <- NULL

  ext_stime <- exp_dat[[3]] %>%
    select(all_of(target_var), all_of(time_var))

  ext_wo_time <- exp_dat[[3]] %>%
    select(-all_of(target_var), -all_of(time_var))

  ext_wo_time$X.1 <- NULL
  ext_wo_time$X <- NULL

  ext_na <- preProcess(as.data.frame(ext_wo_time),
                         method = "knnImpute")

  dat_ext <- predict(ext_na, ext_wo_time)
  dat_ext <- cbind(dat_ext, ext_stime)

  imput_dat <- list(dat_train, test_dat, dat_ext)

  return(imput_dat)
}
