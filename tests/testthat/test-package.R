test_that("bundled datasets have documented dimensions", {
  expected <- list(
    BMIcsdata = c(1582L, 5L), BMIcsdataNT = c(792L, 5L),
    dailyPM10.2021 = c(3115L, 9L), dssurv = c(20L, 3L),
    motidf = c(47L, 2L), ssdsample = c(2018L, 16L),
    xylemDF = c(29L, 11L)
  )
  for (name in names(expected)) {
    env <- new.env(parent = emptyenv())
    utils::data(list = name, package = "CSUBstats", envir = env)
    expect_s3_class(env[[name]], "data.frame")
    expect_identical(dim(env[[name]]), expected[[name]])
  }
})

test_that("Q-Q plots work without attaching other packages", {
  expect_s3_class(normqqplot(~ Sepal.Length, data = iris), "trellis")
  expect_s3_class(normqqplot(Sepal.Length ~ Species, data = iris), "trellis")
})

test_that("two-sample functions accept their default alternative", {
  expect_no_error(two.mean.test(Score ~ Treatment, data = motidf,
                               first.level = "Intrinsic", printout = FALSE))
  expect_no_error(two.wilcox.test(Score ~ Treatment, data = motidf,
                                 first.level = "Intrinsic", printout = FALSE))
})

test_that("the default regression shuffle count can be used", {
  set.seed(123)
  expect_output(slr.randtest(Sepal.Length ~ Petal.Length, data = iris),
                "Randomization test")
})
