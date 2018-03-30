#' @title Train a strategy.
#'
#' @description
#' Given a `Strategy` and a `task`, creates an appropriate `Wrapped Learner` and then
#' trains a model on that wrapped learner. If passed a base mlr learner, will simply
#' train that learner directly.
#'
#' @export
train = function(strategy, task, max.cores = NULL, ...) {
  cl <- try(checkLearner(strategy), silent = TRUE)
  cpus = min(max.cores, parallel::detectCores())
  if (class(cl) == "try-error") {
    lrn = makeWrappedLearner(strategy, task = task, ...)
    parallelMap::parallelStartSocket(cpus = cpus, logging = TRUE, storagedir = getwd())
    message(paste0("Logging results in ", getwd()))
    mdl = mlr::train(lrn, task = task)
    parallelMap::parallelStop()
    return(mdl)
  } else {
    parallelMap::parallelStartSocket(cpus = cpus, logging = TRUE, storagedir = getwd())
    message(paste0("Logging results in ", getwd()))
    mdl = mlr::train(learner = strategy, task = task, ...)
    parallelMap::parallelStop()
    return(mdl)
  }
}
