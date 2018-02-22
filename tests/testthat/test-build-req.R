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
# необходимо еще проверить, что field = "", а в conds -- именованный список

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
  expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"), serial_mask="5674"),
               paste0(" date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=43200 ",
                      "AND like(serial, '%5674%') "))
  expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14")),
               " date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=43200 ")
  expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"), segment=5),
               " date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=43200 ")
  expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"),
                              region="1", prefix="2", segment="3", channel="4", event="5"),
               paste0(" date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=43200 ",
                      "AND region IN ('1') AND prefix IN ('2') AND segment IN ('3') AND channelId IN ('4') ",
                      "AND switchevent IN ('5') "))
})

test_that("Check new buildReqLimitsExt behavior", {
  expect_equal(buildReqLimitsExt(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"),
                              region="1", prefix="2", segment="3", channel="4", event="5"),
               paste0(" date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=43200 ",
                      "AND region IN ('1') AND prefix IN ('2') AND segment IN ('3') AND channel IN ('4') ",
                      "AND event IN ('5') "))
})

context("Build request limits v2")
test_that("Check correct behavior", {
  # expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"), serial_mask="5674"),
  #              paste0(" date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=43200 ",
  #                     "AND like(serial, '%5674%') "))
  # expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14")),
  #              " date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=43200 ")
  # expect_equal(buildReqLimits(begin=as.Date("2016-09-12"), end=as.Date("2016-09-14"), segment=5),
  #              " date>='2016-09-12' AND date<='2016-09-14' AND duration>=0 AND duration<=43200 ")

  dates <- list(
    date=c(as.Date("2016-09-12"), as.Date("2016-09-14")),
    activated_at=c(as.Date("2016-09-12"), as.Date("2018-02-13"))
  )
  ranges <- list(
    duration=c(rlang::expr(1*60),rlang::expr(5*60*60))
  )
  res <- glue::glue("date BETWEEN '2016-09-12' AND '2016-09-14' AND activated_at \\
              BETWEEN '2016-09-12' AND '2018-02-13' AND \\
              duration BETWEEN 1 * 60 AND 5 * 60 * 60 \\
              AND region IN ('1') AND prefix IN ('2') \\
              AND event IN ('5')") %>%
    as.character()
  expect_equal(buildReqLimitsExt2(dates=dates, ranges=ranges, region="1", prefix="2", event="5"),
               res)

  ranges <- list(
    duration=c(10, 5*60*60)
  )
  res <- glue::glue("date BETWEEN '2016-09-12' AND '2016-09-14' AND activated_at \\
              BETWEEN '2016-09-12' AND '2018-02-13' AND \\
              duration BETWEEN 10 AND 18000 \\
              AND region IN ('1') AND prefix IN ('2') \\
              AND event IN ('5')") %>%
    as.character()
  expect_equal(buildReqLimitsExt2(dates=dates, ranges=ranges, region="1", prefix="2", event="5"),
               res)
})
