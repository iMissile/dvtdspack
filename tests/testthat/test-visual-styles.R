context("Check custom theme")

test_that("Check corect input values", {
  expect_equal(class(theme_dvt()), c("theme", "gg"))
  expect_equal(class(theme_dvt(target="screen")), c("theme", "gg"))
  expect_equal(class(theme_dvt(target="word_A4")), c("theme", "gg"))
})

test_that("Check incorect input values", {
  expect_error(theme_dvt(target="air"),
               "Assertion on 'target' failed: Must be element of set \\{'screen','word_A4'\\}, but is 'air'\\.")
  expect_error(theme_dvt(target=NULL),
               "Assertion on 'target' failed: Must be a subset of \\{'screen','word_A4'\\}, not 'NULL'\\.")
})
