# ANZSRC Research 'Division' labels used in ERA data.
# See: # https://researchservices.anu.edu.au/ore/data/reference/for-codes.php
.divisionNames <- function() {
    list(
        '01' = 'Mathematical Sciences',
        '02' = 'Physical Sciences',
        '03' = 'Chemical Sciences',
        '04' = 'Earth Sciences',
        '05' = 'Environmental Sciences',
        '06' = 'Biological Sciences',
        '07' = 'Agricultural and Veterinary Sciences',
        '08' = 'Information and Computing Sciences',
        '09' = 'Engineering',
        '10' = 'Technology',
        '11' = 'Medical and Health Sciences',
        '12' = 'Built Environment and Design',
        '13' = 'Education',
        '14' = 'Economics',
        '15' = 'Commerce, Management, Tourism and Services',
        '16' = 'Studies in Human Society',
        '17' = 'Psychology and Cognitive Sciences',
        '18' = 'Law and Legal Studies',
        '19' = 'Creative Arts and Writing',
        '20' = 'Language, Communication and Culture',
        '21' = 'History and Archaeology',
        '22' = 'Philosophy and Religious Studies',
        'MD' = 'Multidisciplinary')
}

# Identify the unique division labels from a list of divisions.
getUniqueDivisions <- function(div_list) {
    div_list %>% do.call(c, .) %>% (function(x) x[!is.na(x)]) %>% unique %>% sort
}

# Find whether a specific division code occurs in a string of multiple
# division codes. Returns 0/1.
inDivision <- function(divisions, division_code) {
    str_detect(divisions, paste0('\\b', division_code, '\\b')) %>%
            as.integer
}

#' Append columns of research division dummy variables to the citations
#'  data frame.
#'
#' One column is added for each division represented by any article in
#' the citations data frame. The columns have 0/1 values, indicating which
#' articles are in that research division.
#'
#' @param cites_df The citations data frame.
#'
#' @return The original citations dataframe with the division columns added.
#'
addDivisions <- function(cites_df) {
    divnames <- .divisionNames()

    divvecs <- divnames %>%
        names %>%
        as.list %>%
        lapply(function(x) { inDivision(cites_df$divisions, x)}) %>%
        do.call(cbind, .) %>%
        set_colnames(paste0('Div. ', divnames %>% unlist)) %>%
        cbind(cites_df)

}

#' Select the FoR division data columns from a data frame
#'
#' @param df A dataframe with division columns.
#' @param pattern (default "^div\\.") A regex pattern
#' for detecting division column names.
#'
#' @return A dataframe of only the division columns.
#'
selectDivisionCols <- function(df, pattern="Div\\.") {
    df[ , str_detect(names(df), pattern)]
}
