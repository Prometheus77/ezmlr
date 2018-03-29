context("autoDummy")

test_that("autoDummy makes dummy features for learners that do not handle factors", {
  lrn = makeLearner("classif.xgboost")
  lrn2 = autoDummy(lrn, bc.task)
  expect_equal(lrn2, makeDummyFeaturesWrapper(lrn))
})

test_that("autoDummy does not change learners that handle factors", {
  lrn = makeLearner("classif.randomForest")
  lrn2 = autoDummy(lrn, bc.task)
  expect_equal(lrn2, lrn)
})
