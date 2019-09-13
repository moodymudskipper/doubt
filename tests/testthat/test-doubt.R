test_that("default dubious ops work", {
  expect_equal(head(cars,2), cars ?head? 2)
  expect_equal(head(cars,2), cars +head? 2)
})

test_that("default dubious ops have the right precedence", {
  expect_equal(1:6, 1:10 +head? 2 * 3)
  expect_equal(4:5, 1:10 +head? 2 + 3)
})

test_that("n-ary dubious ops work", {
  x <- "a"
  expect_equal(paste(x,"b","c"), ?paste? x ? "b" ? "c")
  expect_equal(paste(x,"b","c"), x ?paste? "b" ? "c")
})

test_that("custom dubious ops work", {
  `?!` <- toupper
  # for it to work in test this must be done
  assign("?!",`?!`,pos = .GlobalEnv)
  expect_equal(toupper("foo"), ?!"foo")
  rm("?!", envir=.GlobalEnv)
})
