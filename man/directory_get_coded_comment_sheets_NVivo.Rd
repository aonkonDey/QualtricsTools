% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comment_coding_NVivo.R
\name{directory_get_coded_comment_sheets_NVivo}
\alias{directory_get_coded_comment_sheets_NVivo}
\title{Turn a Directory into a list of Coded Comment Data Frames (unprocessed)}
\usage{
directory_get_coded_comment_sheets_NVivo(directory)
}
\arguments{
\item{directory}{A string path to the directory containing the coded comments
sheets, which are formatted as described in the Wiki.
https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding}
}
\value{
A list of dataframes for each sheet of coded comments.
}
\description{
This function takes as an argument a string representative of a
directory, loads the CSVs and Excel data from that directory,
looks for 'Coded' sheets, extracts the coded sheets,
and saves the coded comment table and the question ID
as a pair in the output coded_appendix_tables list. If there
are sheets which contain non-numeric data, warnings are raised.
}
