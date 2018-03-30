#' @title Build a wrapped learner using a specified learning strategy.
#'
#' @description
#' Follows a specified learning strategy (`Strategy`) to create a wrapped learner
#' customized to a specific task. May include things such as:
#' \itemize{
#'   \item Preprocessing
#'   \item Imputation of missing values
#'   \item Feature selection
#'   \item Feature filtering
#'   \item Upsampling/downsampling
#'   \item Hyperparameter tuning
#' }
#'
#' @param strategy (`character(1)`)\cr
#'   Name (short name) of the strategy to use for training.
#' @param id (`character(1)`)\cr
#'   Learner ID. Default is `strategy`.
#' @param task (\code{\link[mlr]{Task}})\cr
#'   Task to which the strategy should be applied.
#' @param strategy.pars (`list`)\cr
#'   Optional parameters to pass to the strategy.
#'   These will vary strategy by strategy.
#' @param learner.pars (`list`)\cr
#'   Optional parameters to pass to the wrapped
#'   learner. These are the standard parameters used by each learner in `mlr`.
#' @param ...
#' @param predict.type
#' @param predict.threshold
#' @param fix.factors.prediction
#' @param config
#' @export
makeWrappedLearner = function(strategy, id = strategy, task, strategy.pars = list(),
                              learner.pars = list(), ..., predict.type = NULL,
                              predict.threshold = NULL, fix.factors.prediction = TRUE,
                              config = NULL) {
  assertString(strategy)
  assertString(id)
  assertList(strategy.pars, names = "unique")
  assertList(learner.pars, names = "unique")

  stg = do.call(paste("makeStrategy", strategy, sep = "."), args = list())
  wl = stg$fun(task = task, pars = strategy.pars)

  if (!hasArg("predict.type")) {
    if (getTaskType(task) == "classif") predict.type = "prob" else predict.type = "response"
  }
  wl = setPredictType(learner = wl, predict.type = predict.type)
  wl = setHyperPars(wl, ..., par.vals = learner.pars)
  if (!is.null(predict.threshold))
    wl = setPredictThreshold(wl, predict.threshold)
  wl$fix.factors.prediction = fix.factors.prediction
  wl$config = config

  return(wl)
}
