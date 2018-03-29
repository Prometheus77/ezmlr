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
#'   Name of the strategy to use for tuning.
#' @param name (`character(1)`)\cr
#'   Long name of the strategy. Default is `id`.
#' @param properties ([character])\cr
#'   Set of strategy properties.
#'   \describe{
#'     \item{prop1}{Property1}
#'     \item{prop2}{Property2}
#'   }
#' @param fun (`function(task, pars)`)\cr
#'   Takes a `Task` and returns a wrapped `Learner` customized to the task.
#'   `pars` may be added optionally to override default strategy settings.
#'   \describe{
#'     \item{`task` ([Task])}{The task around which to build the learning strategy.}
#'     \item{`pars` (`list`)}{An optional list of parameters to override the defaults.}
#'   }
#' @seealso [strategies]
#' @return ([Strategy]).
#' @examples
#' # TBD
#' @export
makeWrappedLearner = function(strategy, id = strategy, task, predict.type = "response",
                              predict.threshold = NULL, fix.factors.prediction = FALSE, ...,
                              strategy.pars = list(), learner.pars = list(),
                              config = list()) {
  assertString(strategy)
  assertString(id)
  assertList(strategy.pars, names = "unique")

  fun = get(strategy)[["fun"]]
  wl = fun(task = task, pars = strategy.pars)

  wl = setPredictType(learner = wl, predict.type = predict.type)
  wl = setHyperPars(wl, ..., par.vals = learner.pars)
  if (!is.null(predict.threshold))
    wl = setPredictThreshold(wl, predict.threshold)
  wl$fix.factors.prediction = fix.factors.prediction
  wl$config = config

  return(wl)
}
