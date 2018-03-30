#' @title Diagnose a learner, strategy, or model with respect to a task.
#'
#' @description
#' Given a `task` and either a `strategy` or `learner`, or else given a trained `model`,
#' create diagnostic plots to provide useful information about model performance and
#' the inner workings of the model.
#'
#' @param obj (`learner`, `strategy`, or `model`)\cr
#'   The object to diagnose
#' @param task (`task`)\cr
#'   If `obj` is a `learner` or a `strategy`, a `task` must be specified on which
#'   to evaluate performance.
#' @param plots (`list`)\cr
#'   List of plots to produce. If not specified, the function will choose a set
#'   of appropriate plots based on the object and task. Will be overridden if
#'   any of the individual plot parameters is set to `TRUE` or `FALSE`.
#' @param threshVsPerf (`logical`)\cr
#'   Plot threshold vs. performance curve using `plotThreshVsPerf`? Only
#'   available for classification tasks.
#' @param ROCCurve (`logical`)\cr
#'   Plot receiver operating characteristic (ROC) curve using `plotROCCurves`?
#'   Only available for classification tasks.
#' @param residuals (`logical`)\cr
#'   Plot residuals of predictions using `plotResiduals`?
#' @param learningCurve (`logical`)\cr
#'   Plot learning curve showing learner performance vs. proportion of data
#'   used, using `plotLearningCurve`?
#' @param filterValues (`logical`)\cr
#'   Plot feature importance for a given filter method using `plotFilterValues`?
#' @param hyperParsEffect (`logical`)\cr
#'   Plot hyperparameter effect on model performance from tuning data using
#'   `plotHyperParsEffect`?
#' @param optPath (`logical`)\cr
#'   Plot optimization path from tuning data using `plotOptPath`?
#' @param tuneMultiCritResult (`logical`)\cr
#'   Plot pareto front for results of tuning to multiple performance measures
#'   using `plotTuneMultiCritResult`?
#' @param partialDependence (`logical`)\cr
#'   Plot partial dependence of model prediction over each data feature using
#'   `plotPartialDependence`?
#' @param calibration (`logical`)\cr
#'   Plot calibration of probability predictions vs. true incidence using
#'   `plotCalibration`?
#' @param ... Additional arguments to pass to each plot.
#' @export
diagnose = function(obj, task, plots = NULL, file = file.path(getwd(), "plots.pdf"), threshVsPerf = NULL,
                    ROCCurve = NULL, residuals = NULL, learningCurve = NULL, filterValues = NULL,
                    hyperParsEffect = NULL, optPath = NULL, tuneMultiCritResult = NULL,
                    PartialDependence = NULL, calibration = NULL, ...) {

  if (hasArg(plots)) warning("'plots' parameter not yet supported. Please use the individual parameters for each plot.")

  plot_out = list()

  if ("WrappedModel" %in% class(obj)) {

    obj_type = "model"
    if (!hasArg("threshVsPerf") & isBinaryClassTask(obj)) threshVsPerf = TRUE
    if (!hasArg("ROCCurve") & isBinaryClassTask(obj)) ROCCurve = TRUE
    if (!hasArg("residuals")) residuals = TRUE
    if (!hasArg("partialDependence")) partialDependence = TRUE
    if (!hasArg("calibration") & isBinaryClassTask(obj)) calibration = TRUE

    pred = predict(obj, task = task)

    if (threshVsPerf %==% TRUE) {
      tvp = generateThreshVsPerfData(pred)
      plot_out[[length(plot_out) + 1]] = plotThreshVsPerf(tvp) +
        ggplot2::labs(title = "Threshold Vs. Performance",
                      caption = "Caution: Predictions on training data and are likely to suffer from overfitting")
    }

    if (ROCCurve %==% TRUE) {
      tvp = generateThreshVsPerfData(pred, measures = list(fpr, tpr))
      plot_out[[length(plot_out) + 1]] = plotROCCurves(tvp) +
        ggplot2::labs(title = "Receiver Operating Characteristic (ROC) Curve",
                      caption = "Caution: Predictions on training data and are likely to suffer from overfitting")

    }

    if (residuals %==% TRUE) {
      plot_out[[length(plot_out) + 1]] = plotResiduals(pred)
    }

    if (partialDependence %==% TRUE) {
      pdd = generatePartialDependenceData(obj, getTaskData(task))
      plot_out[[length(plot_out) + 1]] = plotPartialDependence(pdd) +
        ggplot2::labs(title = "Partial Dependence of Target on Predictor Features")
    }

    if (calibration %==% TRUE) {
      cal = generateCalibrationData(pred)
      plot_out[[length(plot_out) + 1]] = plotCalibration(cal) +
        ggplot2::labs(title = "Calibration of Probability Predictions vs. True Incidence")
    }

  } else if ("Learner" %in% class(obj)) {

    obj_type = "learner"


  } else if ("Strategy" %in% class(obj)) {
    obj_type = "strategy"
  } else {
    stop("'obj' must be one of class 'WrappedModel', 'Learner', or 'Strategy'")
  }

  pdf(file)
  for (p in plot_out) print(p)
  dev.off()
  message(paste0("Plots saved to ", file))

  plot_out
}
