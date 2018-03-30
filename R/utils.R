addMissing = function(task, pct = 0.1) {
  taskdata = getTaskData(task)
  featcols = (names(taskdata) %in% getTaskFeatureNames(task))
  noisify = function(x, pct) {
    x[sample(1:length(x), size = length(x) * pct)] <- NA
    x}
  taskdata[, featcols] = lapply(taskdata[, featcols], FUN = noisify, pct = pct)

  if (getTaskType(task) == "classif") {
    return(makeClassifTask(id = getTaskId(task), data = taskdata, target = getTaskTargetNames(task),
                           weights = task$weights, blocking = task$blocking, coordinates = task$coordinates,
                           positive = task$task.desc$positive))
  } else if (getTaskType(task) == "regr") {
    return(makeRegrTask(id = getTaskId(task), data = taskdata, target = getTaskTargetNames(task),
                        weights = task$weights, blocking = task$blocking, coordinates = task$coordinates))
  } else {
    stop("Task type must be 'classif' or 'regr'")
  }
}

isRegrTask = function(task) {
  x = try(getTaskType(task), silent = TRUE)
  (x == "regr")
}

isClassifTask = function(task) {
  x = try(getTaskType(task), silent = TRUE)
  (x == "classif")
}

isBinaryClassTask = function(task) {
  n = try(length(getTaskClassLevels(task)), silent = TRUE)
  (isClassifTask(task) & n == 2)
}

isMultiClassTask = function(task) {
  n = try(length(getTaskClassLevels(task)), silent = TRUE)
  (isClassifTask(task) & n > 2)
}

`%==%` <- function(x, y)
{
  if ((is_empty(x) || is_empty(y)) && !is.null(x) && !is.null(y))
  {
    return(is_empty(x) == is_empty(y))
  } else if (is.null(x) || is.null(y)) {
    return(is.null(x) == is.null(y))
  } else if (is.na(x) || is.na(y)) {
    return(is.na(x) == is.na(y))
  } else {
    return(x == y)
  }
}

is_empty <- function(x) {
  length(x) == 0
}
