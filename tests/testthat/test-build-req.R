context("Build unite filter")

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

context("Build request limits")

test_that("begin/end must be Date", {
  expect_error(buildReqLimits(begin="2016-09-12", end=as.Date("2016-09-14")),
               "Assertion on 'begin' failed: Must be of class 'Date', not 'character'.")
  expect_error(buildReqLimits(begin=as.Date("2016-09-12"), end="2016-09-14"),
               "Assertion on 'end' failed: Must be of class 'Date', not 'character'.")
})

test_that("Serial_mask must be string (empty or not)", {
  expect_error(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"), serial_mask=4),
               "Assertion on 'serial_mask' failed. Must be of class 'string', not 'double'.")
})

test_that("Check correct behavior", {
  expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14")),
               " date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=7200 ")
  expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"), segment=5),
               " date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=7200 ")
  expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"),
                              region="1", prefix="2", segment="3", channel="4", event="5"),
               paste0(" date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=7200 ",
                      "AND region IN ('1') AND prefix IN ('2') AND segment IN ('3') AND channelId IN ('4') ",
                      "AND switchEvent IN ('5') "))
})
