#' @export
makeStrategy.autoxgb = function() {

  fun = function(task, pars) {

    if (getTaskType(task) == "classif") {

      wl = makeLearner("classif.xgboost")
      wl = autoImpute(td, wl)
      wl = autoDummy(td, wl)

    } else if (getTaskType(task) == "regr") {

      wl = makeLearner("regr.xgboost")
      wl = autoImpute(td, wl)
      wl = autoDummy(td, wl)

    } else {
      stop("Supported task types are 'classif' and 'regr'")
    }

  }

  makeStrategy(id = "autoxgb", name = "Automatically tuned XGboost with one-hot encoding and NA imputation",
               task.type = c("classif", "regr"),
               properties = unique(c(getLearnerProperties("classif.xgboost"),
                                     getLearnerProperties("regr.xgboost"),
                                     "missings")),
               fun = fun)
}
