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
