library(dplyr)

# note : expect_equal does something that makes those fail so we don't use it
# devtools::test() says the test is skipped but it does go through it
# and will fail explicitly if tests don't pass
test_that("summarize(data, !!!? ....) works",{

  stopifnot(all.equal(
    iris %>% group_by(Species) %>% summarize(!!!?is.numeric := mean),
    iris %>% group_by(Species) %>% summarize_if(is.numeric, mean)))

  stopifnot(all.equal(
    iris %>% group_by(Species) %>%
      summarize(!!!?c("Petal.Length", "Petal.Width") := mean),
    iris %>% group_by(Species) %>%
      summarize_at(c("Petal.Length", "Petal.Width"), mean)))

  stopifnot(all.equal(
    iris %>% group_by(Species) %>%
      summarize(!!!?vars(Petal.Length, Petal.Width) := mean),
    iris %>% group_by(Species) %>%
      summarize_at(vars(Petal.Length, Petal.Width), mean)))

})
