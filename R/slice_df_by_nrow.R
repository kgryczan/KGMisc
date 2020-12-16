#' This function is slicing data frame by declarated number of rows and saving output 3 different declarative ways.
#'
#' @param df Data frame
#' @param nrows Number of rows which each segment have to had (except last which contains reminders). Default 100.
#' @param to_file Information if output should be written to csv, xlsx or pushed back as list of dataframes
#' @param name Name of output file
#'
#' @return xlsx file/csv files/list of dfs in env
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' slice_df_by_nrow(df, nrows = 190, to_file = "no", v_name = "test")
#'
#'
slice_df_by_nrow = function(df, nrows = 100, to_file = "no", v_name = NULL) {
  #Check if inputs are valid
  avail_file_types = c("no", "csv", "xlsx")

  assert_that(is.data.frame(df))
  assert_that(any(to_file == avail_file_types))
  assert_that(is.numeric(nrows))

  df_sliced = df %>%
    mutate(section = ((row_number()-1) %/% nrows) + 1)

  if (to_file == "csv") {
    split_df_to_files(df_sliced, "csv", v_name, section)
  }
  else if (to_file == "xlsx") {
    split_df_to_files(df_sliced, "xlsx", v_name, section)
  }
  else if (to_file == "no") {
    list_of_df = df_sliced %>%
      group_by(section) %>%
      group_split()

    assign(v_name, list_of_df, envir = .GlobalEnv)
  }
}
