#' Take your redcap outputs and prepare them for analysis
#'
#' @param redcap_data_file The .csv file redcap gave you.
#' @param redcap_script_file The .r redcap gave you.
#' @return The data set with better labels, etc.
#' @examples
#' examples forthcoming

redcap_to_r_data_set <- function(redcap_data_file, redcap_script_file) {
  #Via Wade R Stats https://www.waderstats.com/rstudio-files/redcap_to_r_data_set/redcap_to_r_data_set.html
  # Read in the data and script file.
  redcap_data <- read.csv(file = redcap_data_file, stringsAsFactors = FALSE)
  redcap_script <- readLines(redcap_script_file)
  # We want to remove the appended .factor, but since releveling the numerically coded data erases the labels, we need to reorder the file so that the labels are last.
  # Every line in the script file that uses the factor() function
  redcap_factor <- redcap_script[grep("factor\\(", redcap_script)]
  # Every line in the script file that uses the levels() function
  redcap_levels <- redcap_script[grep("levels\\(", redcap_script)]
  # Every line in the script file that begins with the label function
  redcap_label <- redcap_script[grep("^label\\(", redcap_script)]
  # Reorder the chunks in the script file.
  redcap_reorder <- c(redcap_factor, "", redcap_levels, "", redcap_label)
  # Remove the appended .factor.
  redcap_no_append <- gsub("\\.factor", "", redcap_reorder)
  # REDCap defaults to calling the data 'data'.  Before evaluating, we need to change this to what the data is named here.
  redcap_rename <- gsub("data\\$", "redcap_data\\$", redcap_no_append)
  # Now we can safely evaluate the script file.
  eval(parse(text = redcap_rename))
  return(redcap_data)
}

#' Take Your Long Data With Repeatable Follow Up Events and Limit to Only The Event With Highest Day of Follow Up
#'
#' @param dataset Your prepared dataset from redcap
#' @param lengthoffollowuprepeating The repeating instrument parameter from r that gives you POD of follow up.
#' @param personidentifierg The record/person level parameter from r that gives you the unique person
#' @return The data set limited to the last follow up repeatable instrument.
#' @examples
#' examples forthcoming

Last_Follow_Up_Metrics <-function(dataset, lengthoffollowuprepeating, personidentifier){

  dataset <- dataset %>% group_by(.data[[personidentifier]]) %>% slice_max(.data[[lengthoffollowuprepeating]])
  return(dataset)
}


