

#Read in the csv files from both data formats
#Obviously filepaths would change to where you're accessing these

# Aonkon Edit: I update the responsefile variable and then they read it

legacy_unchecked <- readr::read_csv(responsesfile)
  #We only want the first 3, so filter out the actual responses for now
legacy_unchecked <- dplyr::filter(legacy_unchecked, ! stringr::str_detect(ResponseID, "^R_"))

data_table <- readr::read_csv(responsesfile)

#'The first thing I've noticed with the data_table_full read in is that there are MANY duplicate column names.
#'This should be written down what types of questions produce this


#Now I want to create a table with the names in columns

legacy_info <- tibble::tibble("Varnames" = names(legacy_unchecked),
                              "LegacyNames" = names(legacy_unchecked),
                              "LegacyQID" = unlist(legacy_unchecked[2,]))

datatable_info <- tibble::tibble("Varnames" = names(data_table),
                                 "DatatableNames" = names(data_table),
                                 "DatatableQID" = unlist(data_table[2,]))

compare_varnames <- dplyr::full_join(legacy_info, datatable_info, by = "Varnames")
#now reorder so the names all come first
compare_varnames <- dplyr::select(compare_varnames,Varnames, LegacyNames, DatatableNames, everything())

library(xlsx)
write.csv(compare_varnames, "original_row_names_comparison.csv")
#Now do another bind except look at the cleaned QIDs
