## - fars_read function, create a dataset based on a csv file
## - In order to run the fuction, we only need a csv file with the data
## - Once it has been executed, will return us the database as a data frame
## - The function itself has code to act in case that we don?t introduce any csv, in addition we can have problems with a wrong file type (no csv) for example
## - There ara two functions used in external packages, read_csv ( from readr) in order to read the csv and tbl_df (from dplyr) used to transform the csv in a data frama
## - As a example we can read a csv like accident_2013.csv and observe how the function converts this file in a dataframe:
##  fars_read('accident_2013.csv')
##  Output:
#   A tibble: 30,202 x 50
#   STATE ST_CASE VE_TOTAL VE_FORMS PVH_INVL  PEDS PERNOTMVIT PERMVIT PERSONS COUNTY  CITY   DAY MONTH  YEAR DAY_WEEK  HOUR MINUTE   NHS ROAD_FNC ROUTE TWAY_ID TWAY_ID2 MILEPT LATITUDE
#   <dbl>   <dbl>    <dbl>    <dbl>    <dbl> <dbl>      <dbl>   <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl> <dbl>  <dbl> <dbl>    <dbl> <dbl> <chr>   <chr>     <dbl>    <dbl>
#   1     1   10001        1        1        0     0          0       8       8    115     0     6     1  2013        1     0     55     1        1     1 I-59    NA         1589     33.8
#   2     1   10002        2        2        0     0          0       2       2     55  1670     3     1  2013        5    21     24     1        2     2 US-SR ~ MCCLAIN~   1397     34.0
#   3     1   10003        1        1        0     0          0       1       1     89  1730     6     1  2013        1    11     45     0       15     6 OAKWOO~ NA            0     34.7
#   4     1   10004        1        1        0     0          0       3       3     73   350     6     1  2013        1    12     25     0       99     6 JEFFER~ NA            0     33.5
#   5     1   10005        2        2        0     0          0       3       3    125  3050     6     1  2013        1    18     28     1       11     1 I-359   NA           10     33.2
#   6     1   10006        2        2        0     0          0       3       3     97  2100     8     1  2013        3    14     31     1       13     6 GOVERN~ PLEASAN~      0     30.7
#   7     1   10007        1        1        0     0          0       1       1     95    50    11     1  2013        6    21     30     0        3     3 SR-205  NA           95     34.3
#   8     1   10008        2        2        0     0          0       2       2     49  2904    14     1  2013        2    14     49     0        3     3 SR-75   NA          910     34.6
#   9     1   10009        1        1        0     0          0       1       1     17     0     5     1  2013        7    16     21     0        4     4 CR-267  NA            0     33.0
#  10     1   10010        2        2        0     0          0       4       4     51     0     5     1  2013        7    17     34     0        3     3 SR-14   CHEROKE~   1865     32.6
# ... with 30,192 more rows, and 26 more variables: LONGITUD <dbl>, SP_JUR <dbl>, HARM_EV <dbl>, MAN_COLL <dbl>, RELJCT1 <dbl>, RELJCT2 <dbl>, TYP_INT <dbl>, WRK_ZONE <dbl>,
#   REL_ROAD <dbl>, LGT_COND <dbl>, WEATHER1 <dbl>, WEATHER2 <dbl>, WEATHER <dbl>, SCH_BUS <dbl>, RAIL <chr>, NOT_HOUR <dbl>, NOT_MIN <dbl>, ARR_HOUR <dbl>, ARR_MIN <dbl>, HOSP_HR <dbl>,
#   HOSP_MN <dbl>, CF1 <dbl>, CF2 <dbl>, CF3 <dbl>, FATALS <dbl>, DRUNK_DR <dbl>

fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

## - make_filename function, create names for datasets in the currently working directory to use them in the functions
## - In order to run the fuction, we only need a csv file with the data, and as input introduce a year (number)
## - Once executed returns us a file name concatenated with the year
## - There are just a few situations that function could returns us NA, for example if we introduce a wrong file type
## - It doesn?t use any function of a external package
## - Examples:
##  make_filename(2013)
##  make_filename(2014)

make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

## - fars_read_years summarize year and month of two data sets showing both information together
## - As input, we only need a numeric vector with both years to compare
## - This function returns a list of year and month inlcuding both datasets
## - As is already coded inside the function, will return us an error if the year introduced doesn?t exist
## - We can find two functions from the package dplyr, mutate and select
## - Example:
##  fars_read_years(c(2013,2014))


fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) %>%
                dplyr::select(MONTH, year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

## - fars_summarize_years take the output of fars_read_years function and sumarrize the result making a count per year and month
## - To run the function, we only need fars_read_years output and the same year of this function
## - Returns a count of month and year taking fars_read_years output
## - It?s hard to imagine a possible error, because the info introduced is the same as the last function (we would already detected this mistakes)
## - From dplyr package we need bind_rows, group_by and summarize. From tidyr we will use spread
## - Example :
##  fars_summarize_years(c(2013,2014))



fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

## - fars_map_state show all the information filtering one state in both dataset, also plots a map
## - As inputs, needs the state.num and year(as in the last functions)
## - Once executed, returns all the info in the data set filtered by the state
## - As is written in the function, if you introduce a wrong state, it will return 'invalid STATE number'. Also if the year is in a wrong format it will give us an error
##  From the map packages, it uses the map fucntion, and also from the graphics package, uses the points function
##  Example:
##  fars_map_state(50,2013)

fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if(!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if(nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                  xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}
