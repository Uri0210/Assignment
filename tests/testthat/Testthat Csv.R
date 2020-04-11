test_that('data is available', {
    testthat::expect_equal(list.files(system.file("exdata", package = "ASSIGNMENT")),
                                             c('accident_2013.csv.bz2',
                                               'accident_2014.csv.bz2',
                                               'accident_2015.csv.bz2'))
})
