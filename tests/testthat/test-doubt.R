
test_that("binary dubious ops work", {
  expect_equal(head(cars,2), cars ^head? 2)
  expect_equal(head(cars,2), cars :head? 2)
  expect_equal(head(cars,2), cars %%head? 2)
  expect_equal(head(cars,2), cars *head? 2)
  expect_equal(head(cars,2), cars /head? 2)
  expect_equal(head(cars,2), cars +head? 2)
  expect_equal(head(cars,2), cars -head? 2)
  expect_equal(head(cars,2), cars <head? 2)
  expect_equal(head(cars,2), cars >head? 2)
  expect_equal(head(cars,2), cars <=head? 2)
  expect_equal(head(cars,2), cars >=head? 2)
  expect_equal(head(cars,2), cars ==head? 2)
  expect_equal(head(cars,2), cars !=head? 2)
  expect_equal(head(cars,2), cars &head? 2)
  expect_equal(head(cars,2), cars &&head? 2)
  expect_equal(head(cars,2), cars |head? 2)
  expect_equal(head(cars,2), cars ||head? 2)
  expect_equal(head(cars,2), cars ~head? 2)
  expect_equal(head(cars,2), cars <-head? 2)
  expect_equal(head(cars,2), cars <<-head? 2)
})

test_that("unary dubious ops work", {
  expect_equal(head(cars), +head? cars )
  expect_equal(head(cars), -head? cars )
  expect_equal(head(cars), !head? cars )
  expect_equal(head(cars), ~head? cars )
})

test_that("'+' can be used right after a binary op", {
  expect_equal(1:4 ^head? 2  + 1, 1:head(4, 2) + 1)
  expect_equal(1:4 -head? 2  + 1, head(1:4, 2) + 1)
  expect_equal(1:4 +head? 2  + 1, head(1:4, 2) + 1)
  expect_equal(1:4 ~head? 2  + 1, head(1:4, 2 + 1)) # here `+` is IN the argument
})

test_that("'-' can be used right after a binary op", {
  expect_equal(1:4 ^head? 2  - 1, 1:head(4, 2) - 1)
  expect_equal(1:4 -head? 2  - 1, head(1:4, 2) - 1)
  expect_equal(1:4 +head? 2  - 1, head(1:4, 2) - 1)
  expect_equal(1:4 ~head? 2  - 1, head(1:4, 2 - 1)) # here `-` is IN the argument
})

test_that("'+' can be used right after an unnary op", {
  expect_equal( +sqrt? 4  + 1, sqrt(4) + 1)
  expect_equal( -sqrt? 4  + 1, sqrt(4) + 1)
  expect_equal( !sqrt? 4  + 1, sqrt(4 + 1)) # here `+` is IN the argument
  expect_equal( ~sqrt? 4  + 1, sqrt(4 + 1)) # here `+` is IN the argument
})

test_that("'-' can be used right after an unnary op", {
  expect_equal( +sqrt? 4  - 1, sqrt(4) - 1)
  expect_equal( -sqrt? 4  - 1, sqrt(4) - 1)
  expect_equal( !sqrt? 4  - 1, sqrt(4 - 1)) # here `-` is IN the argument
  expect_equal( ~sqrt? 4  - 1, sqrt(4 - 1)) # here `-` is IN the argument
})

test_that("'+' can be used right after an unnary op", {
  expect_equal( +sqrt? 4  + 1, sqrt(4) + 1)
  expect_equal( -sqrt? 4  + 1, sqrt(4) + 1)
  expect_equal( !sqrt? 4  + 1, sqrt(4 + 1)) # here `+` is IN the argument
  expect_equal( ~sqrt? 4  + 1, sqrt(4 + 1)) # here `+` is IN the argument
})

test_that("'+' can be used right before an unnary op", {
  expect_equal(1 + +sqrt? 4  , 1 + sqrt(4))
  expect_equal(1 + -sqrt? 4  , 1 + sqrt(4))
  expect_equal(1 + !sqrt? 4  , 1 + sqrt(4)) # here `-` is IN the argument
  expect_equal(1 + ~sqrt? 4  , 1 + sqrt(4)) # here `-` is IN the argument
})

test_that("'-' can be used right before an unnary op", {
  expect_equal(1 - +sqrt? 4  , 1 - sqrt(4))
  expect_equal(1 - -sqrt? 4  , 1 - sqrt(4))
  expect_equal(1 - !sqrt? 4  , 1 - sqrt(4)) # here `-` is IN the argument
  expect_equal(1 - ~sqrt? 4  , 1 - sqrt(4)) # here `-` is IN the argument
})


test_that("multi argument rhs works", {
  expect_equal(head(cars,2), +head? {cars;2} )
  expect_equal(head(cars,2), +head? {x=cars;n=2} )
  expect_equal(paste("a","b", "c"), -paste? {"a";"b";"c"} )
  expect_equal(paste("a","b", "c"), "a" -paste? {"b";"c"} )
  expect_equal(list(a=NULL, b=NA), +list?{a=NULL;b=NA})
})

test_that("multi argument fails with invalid name", {
  expect_error(+head? {x=cars;+n=2}, "Invalid argument name")
})


test_that("default dubious ops have the right precedence", {
  expect_equal(1:6, 1:10 +head? 2 * 3)
  expect_equal(4:5, 1:10 +head? 2 + 3)
})

# test_that("n-ary dubious ops work", {
#   x <- "a"
#   expect_equal(paste(x,"b","c"), ?paste? x ? "b" ? "c")
#   expect_equal(paste(x,"b","c"), x ?paste? "b" ? "c")
# })
#
# test_that("custom dubious ops work", {
#   `?!` <- toupper
#   # for it to work in test this must be done
#   assign("?!",`?!`,pos = .GlobalEnv)
#   expect_equal(toupper("foo"), ?!"foo")
#   rm("?!", envir=.GlobalEnv)
# })

test_that("non existent function returns explicit error", {
  expect_error("a" +foobar? "z", "^no function named 'foobar' was found$")
  expect_true(startsWith(capture.output(?foobar)[1], "No documentation for"))
  expect_error(foo?bar, "^no documentation of type")
})

test_that("dubious pipe works", {
  expect_equal(pipe(iris,head(.,2)), head(iris,2))
  expect_equal(iris ~.? head(.,2), head(iris,2))
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

test_that("The deparsing of `< -` and `<-` works",{
  expect_equal(1:4 <-head? 2, 1:2)
  expect_equal(1:4 < -head? 2, c(T,F,F,F))
})


test_that("dubious syntaxes work",{
  "?add> {x} : {y}" <<- "{x} +  {y}" # <<- because needs to be global during tests
  expect_equal(?add> 2 : 3, 5)
})

test_that("non syntactic names are supported",{
  `+cars-` <- cars
  expect_equal(+head? `+cars-`, head(cars))
})

test_that("ambiguous sets of operators trigger failure",{
  "?add> {x}" <<- "{x}"
  expect_error(?add> 2 : 3, "Ambiguous syntax")
})

test_that("We can register ops",{
  register_dubious_syntaxes("?add> {x} : {y}")
  expect_true("?add> {x} : {y}" %in% getOption("doubt.registered_syntaxes"))
})
