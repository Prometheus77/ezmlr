#' @title Strategy object.
#' @description
#' A learning `Strategy` builds a wrapped `Learner` for training a `Task`, which may be
#' either manually configured or (default) automatically customized to the
#' characteristics of that `Task`.
#' To see all currently implemented strategies, look at [strategies].
#'
#' Object members:
#' \describe{
#' \item{id (`character(1)`)}{Name of the strategy.}
#' \item{name (`character(1)`)}{Long name of the strategy.}
#' \item{task.type (`character`)}{Supported task types of the strategy.}
#' \item{properties (`character`)}{Supported learner properties of the strategy.}
#' \item{fun (`function(task, pars)`)}{Function that executes the strategy and returns a wrapped `Learner`.}
#' }
#' @name Strategy
#' @seealso [makeStrategy]
#' @rdname Strategy
NULL

#' @title Specify your own learning strategy.
#'
#' @description
#' Create a strategy which uses data about the task to automatically configure a
#' wrapped `Learner` to train the learner.
#'
#' @param id (`character(1)`)\cr
#'   Name of the strategy.
#' @param name (`character(1)`)\cr
#'   Long name of the strategy. Default is `id`.
#' @param task.type ([character])\cr
#'   Supported task types of the strategy, e.g. 'classif', 'regr', etc.
#' @param properties ([character])\cr
#'   Supported learner properties of the strategy, e.g. 'numerics', 'factors', 'missings', etc.
#' @param base.learner ([character])\cr
#'   Base learner or learners that strategy uses
#' @param wrappers ([character])\cr
#'   Wrapper or wrappers that strategy uses
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
makeStrategy = function(id, name = id, task.type = NULL, properties = NULL, base.learner = NULL,
                        wrappers = NULL, fun) {
  assertString(id)
  assertString(name)
  assertFunction(fun, args = c("task", "pars"))
  makeS3Obj(c("Strategy", id), id = id, name = name, task.type = task.type, properties = properties,
            base.learner = base.learner, wrappers = wrappers, fun = fun)
}

#' @export
print.Strategy = function(x, ...) {
  cat(
    "Strategy ID: ", x$id, "\n",
    "Long name: ", x$name, "\n",
    "Task types handled: ", collapse(x$task.type), "\n",
    "Properties: ", collapse(x$properties), "\n",
    "Base learner: ", collapse(x$base.learner), "\n",
    "Wrappers: ", collapse(x$wrappers), "\n",
    "Class: ", class(x)[1L], "\n",
    sep = ""
  )
}

#' @export
train.Strategy = function(.strategy, .task, ...) {
  wl = makeWrappedLearner(strategy = .strategy, task = .task, ...)
}
