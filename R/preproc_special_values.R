#' @title Handles special values in a preprocessing step
#'
#' @description
#' \code{makeSpecialValuesWrapper} is an mlr-compatible preprocessing function
#' which performs custom special value handling on data. It takes a list of
#' \code{args} which define how the preprocessing is to function.
#'
#' @param ppc List of arguments to describe the preprocessing to be done
#' \itemize{
#'   \item{ppc} List of preprocessing rules, where each top-level item
#'   represents a preprocessing rule, and each sublist defines how that rule is
#'   applied. All \code{ppc} subitems are optional.
#'   \itemize{
#'     \item{Feature criteria (multiple criteria will be evaluated as AND
#'     statements). Failure to specify feature criteria will apply the handling
#'     to all features except the \code{target}.}
#'     \itemize{
#'       \item \code{feat_begins} apply to features which begin with a supplied string
#'       \item \code{feat_contains} apply to features which contain a supplied string
#'       \item \code{feat_ends} apply to features which end with a supplied string
#'       \item \code{feat_in} apply to features which appear in a supplied vector or list
#'       \item \code{feat_class} apply to features whose class is in a supplied vector or list
#'       \item \code{feat_fun} apply to features which the specified function returns as TRUE
#'     }
#'     \item{Value criteria (multiple criteria will be evaluated as AND
#'     statements). Failure to specify any value criteria will apply the
#'     handling to all values, which is probably not useful.}
#'     \itemize{
#'       \item \code{value_in} apply to values which appear in a supplied vector or list
#'       \item \code{value_not_in} apply to values which do not appear in a supplied vector or list
#'       \item \code{value_between} apply to values which are between the
#'       maximum and minimum values in a supplied vector or list (inclusive)
#'       \item \code{value_not_between} apply to values outside of the maximum
#'       and minimum values in a supplied vector or list (exclusive)
#'       \item \code{value_gt} apply to values greater than a supplied numeric
#'       \item \code{value_ge} apply to values greater than or equal to a supplied numeric
#'       \item \code{value_lt} apply to values less than a supplied numeric
#'       \item \code{value_le} apply to values less than or equal to a supplied numeric
#'       \item \code{value_fun} apply to values which the specified function returns as TRUE
#'     }
#'     \item\code{handling} string providing handling instructions to execute
#'     when above criteria are met. If not specified, defaults to 'NA'.
#'     \itemize{
#'       \item \code{'zero'} replace value with zero
#'       \item \code{'zero_dummy'} replace value with zero and flag in a dummy column
#'       \item \code{'NA'} replace value with NA
#'     }
#'     \item\code{dummy_suffix} string to append to the name of any dummy columns created. If not specified, defaults to '.dummy'
#'   }
#' }
#'
#' @return An mlr-compatible preprocessing object consisting of:
#' \itemize{
#'   \item \code{data} the preprocessed data
#'   \item \code{control} a list which tells the prediction method how to perform the preprocessing on new data
#' }
#' @examples
#' # Preprocess data using two rules:
#' # 1) Select any value between -6 to -1 or 999 in any field beginning with 'TU.', and replace
#' # with 0, creating a dummy column for each field with a 1 for each observations which was replaced
#' # 2) Select any value of -9999999 in any field of type 'numeric' or 'integer', and replace with NA
#' lrn = makeSpecialValuesWrapper(lrn, ppc = list(
#'   list(feat_begins = "TU.", value_in = c(-6:-1,999), handling = "zero_dummy"),
#'   list(feat_class = c("numeric", "integer"), value_in = c(-9999999), handling = "NA")))
#'
#' @export
makeSpecialValuesWrapper <- function(lrn, target, ppc) {
  makePreprocWrapper(lrn = lrn, train = ppc_sv_train, predict = ppc_sv_predict,
                     par.vals = list(ppc))
}

ppc_sv_train <- function(data, target = "target", args)
{

  if (target %in% names(data)) {
    feat_names <- names(data)[-which(names(data) == target)]
  } else {
    feat_names <- names(data)
  }
  feat_classes <- sapply(data, class)[-which(names(data) == target)]
  feats_apply <- list()
  dummy_col_holder <- data[, FALSE]

  for(i in args$ppc)
  {
    # start with all features
    feats_do <- feat_names

    # narrow it down for each condition included
    if (exists("feat_begins", where = i)) feats_do <- intersect(feats_do, feat_names[which(startsWith(feat_names, i$feat_begins))])
    if (exists("feat_contains", where = i)) feats_do <- intersect(feats_do, feat_names[which(grepl(i$feat_contains, feat_names))])
    if (exists("feat_ends", where = i)) feats_do <- intersect(feats_do, feat_names[which(endsWith(feat_names, i$feat_ends))])
    if (exists("feat_in", where = i)) feats_do <- intersect(feats_do, feat_names[which(feat_names %in% i$feat_in)])
    if (exists("feat_class", where = i)) feats_do <- intersect(feats_do, feat_names[which(feat_classes %in% i$feat_class)])
    if (exists("feat_fun", where = i)) feats_do <- intersect(feats_do, feat_names[which(i$feat_fun(feat_names))])

    # keep the final result
    cols_do <- which(names(data) %in% feats_do)
    feats_apply[[length(feats_apply) + 1]] <- feats_do

    # apply operations to target features
    data_sub <- as.matrix(data[, cols_do])
    mask <- matrix(rep(1, prod(dim(data_sub))), nrow(data_sub))

    if (exists("value_in", where = i)) mask <- mask * (data_sub %in% i$value_in)
    if (exists("value_not_in", where = i)) mask <- mask * (!(data_sub %in% i$value_not_in))
    if (exists("value_between", where = i)) mask <- mask * (data_sub >= min(i$value_between) & data_sub <= max(i$value_between))
    if (exists("value_not_between", where = i)) mask <- mask * (data_sub < min(i$value_not_between) | data_sub > max(i$value_not_between))
    if (exists("value_gt", where = i)) mask <- mask * (data_sub > i$value_gt)
    if (exists("value_ge", where = i)) mask <- mask * (data_sub >= i$value_ge)
    if (exists("value_lt", where = i)) mask <- mask * (data_sub < i$value_lt)
    if (exists("value_le", where = i)) mask <- mask * (data_sub <= i$value_le)
    if (exists("value_fun", where = i)) mask <- mask * i$value_fun(data_sub)

    if (!exists("handling", where = i)) i$handling <- "NA"
    if (i$handling == "NA") data_sub[mask == 1] <- NA
    if (i$handling %in% c("zero", "zero_dummy")) data_sub[mask == 1] <- 0
    data[, cols_do] <- as.data.frame(data_sub)

    # create dummy columns
    if (i$handling %in% c("zero_dummy"))
    {
      dummy_cols <- as.data.frame(mask)
      if (!exists("dummy_suffix", where = i)) i$dummy_suffix <- ".dummy"
      names(dummy_cols) <- paste(feats_do, i$dummy_suffix, sep = "")
      dummy_cols <- dummy_cols[, colSums(dummy_cols) > 0]
      dummy_col_holder <- cbind(dummy_col_holder, dummy_cols)
    }

  }

  data <- cbind(data, dummy_col_holder)

  return(list(data = data,
              control = list(feat_names = names(data),
                             args = args)))
}

#' @export
ppc_sv_predict <- function(data, target = "target", args, control)
{
  ppc_data <- ppc_sv_train(data = data, target = target, args = control$args)$data

  data <- as.data.frame(matrix(rep(0, nrow(ppc_data) * length(control$feat_names)), nrow = nrow(ppc_data)))
  names(data) <- control$feat_names

  for(i in 1:ncol(data))
  {
    if(control$feat_names[i] %in% names(ppc_data))
    {
      data[, i] <- ppc_data[, which(names(ppc_data) == control$feat_names[i])]
    }
  }

  return(data)
}
