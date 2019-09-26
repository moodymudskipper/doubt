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

test_that("non existent function returns explicit error", {
  expect_error("a" +foobar? "z", "^no function named 'foobar' was found$")
  expect_error("a" ?foobar? "z", "^no function named 'foobar' was found$")
  expect_true(startsWith(capture.output(?foobar)[1], "No documentation for"))
  expect_error(foo?bar, "^no documentation of type")
})

test_that("dubious pipe works", {
  expect_equal(iris +head? 2, head(iris,2))
  expect_equal(.(iris,head(2)), head(iris,2))
})

test_that("deparsing works", {
  expect_equal(deparse_rec(quote(
    function(a,b) {code}
  )), "function(a,b) {code}")

  expect_equal(deparse_rec(quote(
    while(a){}
  )), "while(a) {}")

  expect_equal(deparse_rec(quote(
    for(a in b){}
  )), "for(a in b) {}")

  expect_equal(deparse_rec(quote(
    if (a) b else c
  )), "if(a) b else c")

  expect_equal(deparse_rec(quote(
    a[b]
  )), "a[b]")

  expect_equal(deparse_rec(quote(
    a[[b]]
  )), "a[[b]]")

  expect_equal(deparse_rec(quote(
    {a}
  )), "{a}")

  expect_equal(deparse_rec(quote(
    (a)
  )), "(a)")
  expect_equal(deparse_rec(quote(
    a %% b
  )), "a%%b")

  expect_equal(deparse_rec(quote(
    repeat a
  )), "repeat a")
})


test_that("parsing n-ary args are parsed correctly", {
  expect_equal(parse_qm_args("a"),quote(a))
  expect_equal(parse_qm_args("a?b"),list(quote(a), quote(b)))
  expect_equal(parse_qm_args("?a?b"),list(quote(a), quote(b)))
  expect_equal(parse_qm_args("?a"),quote(a))
})
