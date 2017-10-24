context("test-build-req.R")

test_that("field value is absent", {
  expect_equal(buildReqFilter("field", NULL), " ")
  expect_equal(buildReqFilter("field", NA), " ")
  expect_equal(buildReqFilter("field", NaN), " ")
})

test_that("field value is non-string", {
  expect_equal(buildReqFilter("field", 1), " ")
  expect_equal(buildReqFilter("field", c(1, 2)), " ")
  expect_equal(buildReqFilter("field", list(d="as_is", v=2)), " ")
})

test_that("field value is string", {
  expect_equal(buildReqFilter("field", "all"), " ")
  expect_equal(buildReqFilter("field", "val1"),  " AND field IN ('val1') ")
  expect_equal(buildReqFilter("field", c("val1", "val2")),  " AND field IN ('val1','val2') ")
})



