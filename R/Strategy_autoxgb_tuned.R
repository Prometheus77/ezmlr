#' @title autoxgb_tuned strategy
#' @description
#' The `autoxgb_tuned` strategy creates an XGBoost learner to fit a specific
#' task and tunes it for improved performance.
#'
#' Available parameters for use in in `strategy.pars`:
#' \itemize{
#'   \item{`impute.pars` (`list`)} {List of parameters to pass to `makeImputeWrapper`}
#'   \item{`dummy.pars` (`list`)} {List of parameters to pass to `makeDummyFeaturesWrapper`}
#'   \item{`tune.pars` (`list`)} {list of parameters to pass to `makeTuneWrapper`}
#' }
#' @name autoxgb
#' @rdname autoxgb
NULL

#' @export
makeStrategy.autoxgb_tuned = function() {

  fun = function(task, pars = list()) {

    cols = getTaskNFeats(task)
    rows = getTaskSize(task)

    if (!exists("tune.pars", where = pars)) pars$tune.pars = list()

    if (exists("par.set", where = pars$tune.pars)) {
      par.set = pars$tune.pars$par.set
    } else {
      par.set = makeParamSet(
        makeIntegerParam("nrounds", lower = 100, upper = 2500),
        makeNumericParam("eta", lower = -7, upper = -3, trafo = function(x) 2^x),
        makeIntegerParam("max_depth", lower = 3, upper = 15),
        makeNumericParam("colsample_bytree", lower = ceiling(cols / 10) / cols, upper = 1),
        makeNumericParam("subsample", lower = ceiling(rows / 100) / rows, upper = 1))
    }

    if (exists("control", where = pars$tune.pars)) {
      control = pars$tune.pars$control
    } else {
      mbo.control = makeMBOControl(propose.points = parallel::detectCores())
      control = makeTuneControlMBO(budget = 50, mbo.control = mbo.control)
    }

    if (exists("resampling", where = pars$tune.pars)) {
      resampling = pars$tune.pars$resampling
    } else {
      resampling = makeResampleDesc(method = "CV", iters = min(parallel::detectCores(), 5))
    }

    if (exists("measures", where = pars$tune.pars)) {
      measures = pars$tune.pars$measures
    } else {
      measures = NULL
    }

    pars$tune.pars = list(resampling = resampling, measures = measures, par.set = par.set, control = control)

    if (getTaskType(task) == "classif") {

      wl = makeLearner("classif.xgboost")
      wl = do.call(autoImpute, args = c(list(wl = wl, task = task), pars$impute.pars))
      wl = do.call(autoDummy, args = c(list(wl = wl, task = task), pars$dummy.pars))
      wl = do.call(makeTuneWrapper, args = c(list(learner = wl), pars$tune.pars))

    } else if (getTaskType(task) == "regr") {

      wl = makeLearner("regr.xgboost")
      wl = do.call(autoImpute, args = c(list(wl = wl, task = task), pars$impute.pars))
      wl = do.call(autoDummy, args = c(list(wl = wl, task = task), pars$dummy.pars))
      wl = do.call(makeTuneWrapper, args = c(list(learner = wl), pars$tune.pars))

    } else {
      stop("Supported task types are 'classif' and 'regr'")
    }

  }

  makeStrategy(id = "autoxgb_tuned",
               name = "XGboost with one-hot encoding and NA imputation, tuned using MBO",
               task.type = c("classif", "regr"),
               properties = unique(c(getLearnerProperties("classif.xgboost"),
                                     getLearnerProperties("regr.xgboost"),
                                     "missings")),
               base.learner = c("classif.xgboost", "regr.xgboost"),
               wrappers = c("makeImputeWrapper", "makeDummyFeaturesWrapper", "makeTuneWrapper"),
               fun = fun)
}
