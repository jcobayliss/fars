#' Reads a delimited csv file into a data frame table
#'
#' Reads a delimited csv file specified by \code{filenME}into a data frame table that wraps the local data frame.
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @param filename A character string that indicates the file to read.
#'
#' @return A data frame table.
#'
#' @examples
#' fars_read('Table.csv')
#' fars_read(filename = 'Table.csv')
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Returns a character string in a format "accident_<year>.csv.bz2" (this can later be used as a file name).
#'
#' Returns a character string in a format "accident_<year>.csv.bz2", based on the \code{year} argument.
#'
#' @param year The year to be used for creating the character string.
#'
#' @return A character string.
#'
#' @examples
#' make_file(1992)
#' make_file(year = 1992)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads month and year from accident files
#'
#' The function takes a vector of years and returns a list of data frames with month and year based on data in "accident_<year>.csv.bz2".
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param years The vector of years to be considered.
#'
#' @return A list of data frames with month and year of Returns NULL and a warning if the file does not exist.
#'
#' @examples
#' fars_read_years(1992:1996)
#'
#' @export
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

#' Counts the number of accidents per month in a given range of years.
#'
#' Returns a data frame that contains months as rows and the number of accidents years as columns.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @param years The range of years to analyze.
#'
#' @return A data frame with the number of accidents per month in a given range of years.
#'
#' @examples
#' fars_summarize_years(1992:1999)
#' fars_summarize_years(years = 1992:1996)
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots the accidents of one specific state and a range of years.
#'
#' Returns a plot of the accidents based on the \code{state.num} and \code{year} inputs. Returns an error if the state or year do not exist in the data set.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @param state.num The number of the state to analyze.
#' @param year The year to analyze.
#'
#' @return A plot with the accidents of one specific state and a year.
#'
#' @examples
#' fars_map_state(15, 1992)
#' fars_map_state(state.num = 15, year = 1992)
#'
#' @export
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
