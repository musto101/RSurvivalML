#' Survival ML modelling
#'
#' This function does the actual survival based ML modelling.
#'
#' @param exp_dat is the list of training and test dfs
#' @param modType is the type of model required
#' @param time is the time column in the df
#' @param target is the target column in the df
#' @param ids is a vector of two strings used for the model ids.
#' @param grid is the hyperparameter tuning grid in a list.
#' @return it saves a model and returns a vector of C-Indexes for train and test.
#' @export
#'
surv_model <- function(exp_dat, modType, time, target, ids, grid) {

  library(mlr3)
  library(mlr3learners)
  library(mlr3extralearners)
  library(mlr3tuning)
  library(mlr3proba)

  train <- exp_dat[[1]]
  train[,target] <- as.integer(train[,target])

  test <- exp_dat[[1]]

  test[,target] <- as.integer(test[,target])

  train_mlr3 <- as_task_surv(na.omit(train), id = ids[1],
                             time = time, event = target) # change train to
  # type that is compatible with package

  test_mlr3 <- as_task_surv(na.omit(test), id = ids[2],
                            time = time, event = target) # change test to
  # type that is compatible with package

  mlr_learners$get(modType)

  if (modType == 'surv.ranger') {

    learner <- lrn(modType, mtry = to_tune(grid$mtry),
        min.node.size = to_tune(grid$min.node.size),
        importance = grid$importance)

  } else if (modType == "surv.svm") {

    learner <- lrn(modType, kernel = grid$kernel,
                  gamma.mu = to_tune(grid$gamm.mu), maxiter	= grid$maxiter)


  } else if (modType =='surv.deephit') {

    learner <- lrn(modType, num_nodes = grid$num_nodes,
                   activation = to_tune(grid$activation),
                   epochs	= grid$epochs, batch_size = grid$batch_size)

  } else if (modType == 'surv.xgboost'){

    learner <- lrn(modType, nrounds = grid$nrounds,
                   max_bin = to_tune(grid$max_bin),
                   max_depth	= grid$max_depth,
                   min_child_weight = grid$min_child_weight)

} else {

  stop("'I am sorry we currently don't support that modType")

}

instance = mlr3tuning::tune(
  method = "grid_search",
  task = train_mlr3,
  learner = learner,
  resampling = rsmp("cv", folds = 5),
  measure = msr("surv.cindex"))

learner$param_set$values = instance$result_learner_param_vals # set the learner param values
# with the found optimal values

measure <- msr("surv.cindex") # define the performance metric

learner$train(train_mlr3)

save(learner, file = paste0('model/', sub("\\.", "", modType), '.RData'))

train_score <- measure$score(learner$predict(train_mlr3))

test_score <- measure$score(learner$predict(test_mlr3))

results <- list(train_score, test_score)

return(results)

}
