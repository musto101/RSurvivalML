#' Reading in dataframe and returning a list containing train and test dfs
#'
#' This function ...
#'
#' @param dat is ...
#' @param perc is ...
#' @param target is ...
#'
#' @return it returns a list containing two dataframes, one train, one test
#' @export
#'
dat_splitter <- function(dat, perc, target) {

  library(caret)

  dat[,target] <- as.factor(dat[,target])

  dat_split <- createDataPartition(y = dat[,target], p = perc, list = F)

  training <- dat[dat_split,]
  test <- dat[-dat_split,]

  exp_dat <- list(training, test)

  return(exp_dat)

}
