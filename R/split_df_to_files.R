#' Dataframe is splitted by one or more variable and depending of selected mode exported to xlsx file with group named tabs or to multiple csv files.
#'
#' @param df Data frame
#' @param file_name file name
#' @param file_type export mode (csv or xlsx)
#' @param ... grouping variable
#'
#' @return file or files
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by group_split group_keys enquos
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable saveWorkbook
#' @importFrom tidyr unite
#' @importFrom utils write.csv
#' @importFrom assertthat assert_that
#'
#' @export
#'
split_df_to_files = function(df, file_type, file_name, ...) {
  #Check if inputs are valid
  assert_that(is.data.frame(df))
  assert_that(file_type %in% c("csv","xlsx"))

  #Splitting dataframe and preparing group names
  .group_vars <- enquos(...)
  df_grouped <- df %>% group_by(!!!.group_vars)
  df_splitted <- group_split(df_grouped, .keep = F)
  group_names <- group_keys(df_grouped) %>%
    unite("name", !!!.group_vars) %>%
    unlist()

  if (file_type == "xlsx") {
    #writing tabbed xlsx file.
    wb = createWorkbook()
    for (i in 1:length(df_splitted)) {
      assert_that(nchar(group_names[[i]]) < 32)

      addWorksheet(wb,
                   sheetName = group_names[[i]])
      writeDataTable(wb,
                     group_names[[i]],
                     x = df_splitted[[i]],
                     rowNames = F)
    }
    saveWorkbook(wb,
                 paste0(file_name, ".xlsx"),
                 overwrite = T)
  } else if (file_type == "csv") {
    #write csv files
    for (i in 1:length(df_splitted)) {
      write.csv(df_splitted[[i]],
                paste0(file_name, group_names[[i]], ".csv"),
                row.names = F)
    }
  }
}
