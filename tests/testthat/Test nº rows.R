test_that('Nº of rows is the same',{
    testthat::expect_equal(nrow(fars_read_years(2013)[[1]]), 30202)
})
