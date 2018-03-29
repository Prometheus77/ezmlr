#' @title Automatically replace factors with dummy features.
#'
#' @description
#' Checks a task to see if it contains factors or ordered factors, then checks a
#' learner to see if it is unable to handle factors. If both are true, returns
#' the learner wrapped with \code{\link[mlr]{makeDummyFeaturesWrapper}} which
#' perfors one-hot encoding, i.e. creates a separate column for each factor
#' level with a 1 indicating that observation contained the factor level in
#' question, and a zero indicating otherwise.
#'
#' @param task (`Task`) An mlr Task.
#' @param wl (`WrappedLearner`) An ezmlr WrappedLearner object.
#' @param ... Additional arguments to pass to \code{\link[mlr]{makeDummyFeaturesWrapper}}
#'
#' @export
autoDummy = function(task, wl, ...) {

  td = getTaskDesc(task)

  if ((td$n.feat[2] > 0 & !("factors" %in% getLearnerProperties(wl))) |
      (td$n.feat[3] > 0 & !("ordered" %in% getLearnerProperties(wl)))) {
    wl = makeDummyFeaturesWrapper(wl, ...)
  }

  return(wl)
}
