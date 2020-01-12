# library(dplyr)
# library(tidyr)
# # note : expect_equal does something that makes those fail so we don't use it
# # devtools::test() says the test is skipped but it does go through it
# # and will fail explicitly if tests don't pass
# `?` <<- doubt::`?`
# test_that("summarize(data, !!!? ....) works",{
#
#   stopifnot(all.equal(
#     iris %>% group_by(Species) %>% summarize(!!!?is.numeric := mean),
#     iris %>% group_by(Species) %>% summarize_if(is.numeric, mean)))
#
#   stopifnot(all.equal(
#     iris %>% summarize(!!!?is.numeric := mean),
#     iris %>% summarize_if(is.numeric, mean)))
#
#   stopifnot(all.equal(
#     iris %>% group_by(Species) %>% summarize(!!!?is.numeric := ~mean(.)),
#     iris %>% group_by(Species) %>% summarize_if(is.numeric, mean)))
#
#   stopifnot(all.equal(
#     iris %>% group_by(Species) %>% summarize(!!!?mean),
#     iris %>% group_by(Species) %>% summarize_all(mean)))
#
#   stopifnot(all.equal(
#     iris %>% group_by(Species) %>% summarize(!!!?~mean(.)),
#     iris %>% group_by(Species) %>% summarize_all(mean)))
# })
#
# test_that("mutate(data, !!!? ....) works",{
#   stopifnot(all.equal(
#     iris %>% group_by(Species) %>% mutate(!!!?is.numeric := log),
#     iris %>% group_by(Species) %>% mutate_if(is.numeric, log)))
#
#   stopifnot(all.equal(
#     iris %>% group_by(Species) %>% mutate(!!!?is.numeric := ~log(.)),
#     iris %>% group_by(Species) %>% mutate_if(is.numeric, ~log(.))))
#
#   stopifnot(all.equal(
#     iris %>% group_by(Species) %>% mutate(!!!?is.numeric),
#     iris %>% group_by(Species) %>% mutate_all(is.numeric)))
#
#   stopifnot(all.equal(
#     iris %>% mutate(!!!?is.numeric),
#     iris %>% mutate_all(is.numeric)))
#
#   # this does nothing
#   stopifnot(all.equal(
#     iris %>% mutate(!!!?vars(starts_with("Sepal"))),
#     iris %>% mutate(Sepal.Length, Sepal.Width)))
#
#   # this selects the columns
#   stopifnot(all.equal(
#     iris %>% transmute(!!!?vars(starts_with("Sepal"))),
#     iris %>% transmute(Sepal.Length, Sepal.Width)))
#
#   stopifnot(all.equal(
#     iris %>% select(?is.numeric),
#     iris %>% select_if(is.numeric)))
#
#   stopifnot(all.equal(
#     iris %>% select(?is.numeric),
#     iris %>% select_if(is.numeric)))
#
#   stopifnot(all.equal(
#     iris %>% gather(k,v,Species),
#     iris %>% gather(k,v,?is.factor)))
#
# })
#
#
#
