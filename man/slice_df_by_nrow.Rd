% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slice_df_by_nrow.R
\name{slice_df_by_nrow}
\alias{slice_df_by_nrow}
\title{This function is slicing data frame by declarated number of rows and saving output 3 different declarative ways.}
\usage{
slice_df_by_nrow(df, nrows = 100, to_file = "no", v_name = NULL)
}
\arguments{
\item{df}{Data frame}

\item{nrows}{Number of rows which each segment have to had (except last which contains reminders). Default 100.}

\item{to_file}{Information if output should be written to csv, xlsx or pushed back as list of dataframes}

\item{name}{Name of output file}
}
\value{
xlsx file/csv files/list of dfs in env
}
\description{
This function is slicing data frame by declarated number of rows and saving output 3 different declarative ways.
}
\examples{
slice_df_by_nrow(df, nrows = 190, to_file = "no", v_name = "test")


}
