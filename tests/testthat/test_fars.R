context("Tests the fars package")

library(dplyr)
library(maps)
library(fars)

test_that('fars_read', {
    expect_is(fars_read("accident_2015.csv.bz2"), "tbl_df")
    expect_error(fars_read("accident_2015.csv.bz2"))
  }
)

test_that('fars_map_state', {
    expect_error(fars_map_state(100, 2015))
  }
)

test_that('fars_summarize_years', {
    expect_is(fars_summarize_years(2013:2015), "tbl_df")
    expect_error(fars_summarize_years(2016))
  }
)
