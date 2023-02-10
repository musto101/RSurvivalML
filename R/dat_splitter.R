#' Reading in dataframe and returning a list containing train and test dfs
#'
#' This function ...
#'
#' @param dat is the full dataset which will be split intp train/test.
#' @param ext_date is external data to be used for generalisation error estimates. Default is NULL for no external data.
#' @param perc is the percentage of the full dataset to be used for train.
#' @param target is the outcome value to be used in data partitioning.
#' @return it returns a list containing two dataframes, one train, one test.
#' @export
#'
dat_splitter <- function(dat, ext_dat = NULL, perc, target) {

  library(caret)

  dat[,target] <- as.factor(dat[,target])

  dat_split <- createDataPartition(y = dat[,target], p = perc, list = F)

  training <- dat[dat_split,]
  test <- dat[-dat_split,]

  exp_dat <- list(training, test, ext_dat)

  return(exp_dat)

}
