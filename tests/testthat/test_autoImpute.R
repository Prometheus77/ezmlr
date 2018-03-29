context("autoImpute")

irismissing.task = addMissing(iris.task)

test_that("autoImpute imputes NA for learners that do not handle missings", {
  lrn = makeLearner("classif.randomForest")
  lrn2 = autoImpute(lrn, irismissing.task)
  lrn3 = makeImputeWrapper(lrn, classes = list(numeric = imputeConstant(0),
                                               integer = imputeConstant(0),
                                               factor = imputeConstant(0)),
                           dummy.classes = c("numeric", "integer", "factor"),
                           dummy.type = "numeric")
  expect_equal(lrn2, lrn3)
})

test_that("autoImpute does not change learners that handle missings", {
  lrn = makeLearner("classif.xgboost")
  lrn2 = autoImpute(lrn, irismissing.task)
  expect_equal(lrn2, lrn)
})
