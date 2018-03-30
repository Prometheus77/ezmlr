#' @title autoxgb strategy
#' @description
#' The `autoxgb` strategy creates an XGBoost learner to fit a specific task. It performs
#' imputation if any variables are missing, and one-hot encoding if any factor variables
#' are present. By default, it uses the following hyperparameters:
#' \itemize{
#'   \item{`nrounds = 200`}
#'   \item{`eta = 2^-6`}
#'   \item{`max_depth = 3`}
#'   \item{`colsample_bytree = 0.6`}
#'   \item{`subsample = 0.6`}
#' }
#'
#' Available parameters for use in in `strategy.pars`:
#' \itemize{
#'   \item{`par.vals` (`list`)} {List of hyperparameter values to override defaults.}
#'   \item{`impute.pars` (`list`)} {List of parameters to pass to `makeImputeWrapper`}
#'   \item{`dummy.pars` (`list`)} {List of parameters to pass to `makeDummyFeaturesWrapper`}
#' }
#' @name autoxgb
#' @rdname autoxgb
NULL

#' @export
makeStrategy.autoxgb = function() {

  fun = function(task, pars = list()) {

    par.vals = list(nrounds = 200, eta = 2^-6, max_depth = 3, colsample_bytree = 0.6, subsample = 0.6)

    if (getTaskType(task) == "classif") {

      wl = makeLearner("classif.xgboost", par.vals = par.vals)
      wl = do.call(autoImpute, args = c(list(wl = wl, task = task), pars$impute.pars))
      wl = do.call(autoDummy, args = c(list(wl = wl, task = task), pars$dummy.pars))

    } else if (getTaskType(task) == "regr") {

      wl = makeLearner("regr.xgboost", par.vals = par.vals)
      wl = do.call(autoImpute, args = c(list(wl = wl, task = task), pars$impute.pars))
      wl = do.call(autoDummy, args = c(list(wl = wl, task = task), pars$dummy.pars))

    } else {
      stop("Supported task types are 'classif' and 'regr'")
    }

  }

  makeStrategy(id = "autoxgb",
               name = "XGboost with one-hot encoding and NA imputation",
               task.type = c("classif", "regr"),
               properties = unique(c(getLearnerProperties("classif.xgboost"),
                                     getLearnerProperties("regr.xgboost"),
                                     "missings")),
               base.learner = c("classif.xgboost", "regr.xgboost"),
               wrappers = c("makeImputeWrapper", "makeDummyFeaturesWrapper"),
               fun = fun)
}
