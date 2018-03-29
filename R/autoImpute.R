#' @title Automatic imputation of missing values
#'
#' @description
#' Checks a task to see if it has missing values, then checks a learner to see
#' if it is unable to handle missing values. If both are true, returns the
#' learner wrapped with \code{\link[mlr]{makeImputeWrapper}} which sets missing
#' values to zero and creates a 0/1 dummy column for each column on which
#' missings were present performed, where 1 indicates that the observation was
#' missing in the original column.
#'
#' @param task (`Task`) An mlr Task.
#' @param wl (`WrappedLearner`) An ezmlr WrappedLearner object.
#' @param classes (`list`) Named list containing imputation techniques for
#'   classes of columns. Default is `list(numeric = imputeConstant(0), integer =
#'   imputeConstant(0), factor = imputeConstant(0))`.
#' @param dummy.classes (`character`) Feature types to consider for imputation.
#'   Default is `c("numeric", "integer", "factor")`. See
#'   \code{\link[mlr]{makeImputeWrapper}}.
#' @param dummy.type (`character(1)`) How to encode dummy variables. Default is
#'   "numeric". See \code{\link[mlr]{makeImputeWrapper}}.
#' @param ... Additional arguments to pass to \code{\link[mlr]{makeImputeWrapper}}
#'
#' @export
autoImpute = function(wl, task, classes = NULL, dummy.classes = c("numeric", "integer", "factor"),
                      dummy.type = "numeric", ...) {

  td = getTaskDesc(task)

  if (!hasArg("classes")) {
    classes = list(numeric = imputeConstant(0),
                   integer = imputeConstant(0),
                   factor = imputeConstant(0))
  }

  if (td$has.missings == TRUE &
      "missings" %in% getLearnerProperties(wl)) {
    wl = makeImputeWrapper(wl, classes = classes, dummy.classes = dummy.classes, dummy.type = dummy.type, ...)
  }

  return(wl)
}
