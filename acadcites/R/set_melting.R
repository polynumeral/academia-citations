# Functions for dealing with string-set data.
#
# Many variables contain 'string' sets, which are lists attributes
# in a string of the form: '{sandwich, apple, "chocolate milk"}'
#
# These functions parse and manipulate the data w/in the string.


#' Combine columns of a dataframe into a string set representation.
#'
#' Note that the uniqueness of the set is not ensured in this
#' function. If a row of the input data frame has the same data
#' in multiple columns, the resulting set representation will
#' have non-unique elements.
#'
#' @param df A data frame whose columns will be combined into a set.
#'
#' @return A character vector of string representation forms.
#'
#' @examples
#' myDF <- data.frame(x=c(1,2,3), y=c('a', 'b', 'c'))
#' toSetStr(myDF)  # {'1', 'a'} {'2', 'b'} {'3', 'c'}
#'
toSetStr <- function(df) {
    df %>%
        mutate_each(funs(sQuote)) %>%
        do.call(function(...) paste(..., sep=','), .) %>%
        # Nix all the curly quotes
        str_replace_all('[\u2018\u2019]', '\'') %>%
        str_replace_all('[\u201C\u201D]', '\"') %>%
        str_replace_all("(,'NA')|(,'NA'$)|(,'')|(,''$)", '') %>%
        (function(x) paste0('{', x, '}'))
}


#' Parse a vectors of string representations of sets to a list of vectors
#'
#' @param set_str A character vector of string representations of sets.
#' @param cast_to The name of a data type to cast the elements of the set to.
#' @param prefix A prefix to name columns in the data frame (default 'v').
#'
#' @return A data frame with columns equal to the largest set's cardinality.
#'
#' @examples
#' setParse(c('{1, 2, 3}', '{2, 3}'), 'integer')
#' # v1 v2 v3
#' # 1  1  2  3
#' # 2  2  3 NA
#' setParse(c('{a, b}', '{}'))
#' #   v1 v2
#' # 1  a  b
#'
setParse <- function(set_str, cast_to=NULL, prefix='v') {
    set <- set_str %>% str_replace_all('^\\{|\\}$', '')
    max_els <- max(set %>% textConnection %>% count.fields(sep=', '))

    .unquote <- function(s) { s %>% str_replace_all('[\'\"]', '') %>% str_trim }

    set <- set %>%
        textConnection %>%
        read.csv(header=FALSE, colClasses='character', as.is=TRUE, encoding='utf-8',
                 quote="\"'",
                 col.names=paste0(prefix, 1:max_els)) %>%
        mutate_each(funs(.unquote))

    # Remove empty sets
    set[set == ''] <- NA

    # Cast to data type
    if (!is.null(cast_to)) set <- set %>%
        sapply(function(s) { as(s, cast_to) }) %>%
        as.data.frame
    set
}

dfToList <- function(df) {
    as.data.frame(t(df), stringsAsFactors=FALSE) %>%
        lapply(function(x) { x[!is.na(x)] }) %>%
        set_names(NULL)
}


#' Repeat rows of a dataframe for each element in a set.
#'
#' @param df A dataframe
#' @param col A column in dataframe that is a character vector containing string
#' representations of sets.
#' @param prefix A string to name the column of set elements
#' @param ... Other arguments to pass to `setParse`.
#'
#' @return A 'molten' dataframe.
#'
#' @examples
#' mydf <- data.frame(x=c(1, 2), ids=c('{1,2,3}', '{5, 10}'))
#' meltOnSets(mydf, 'ids', prefix='id', cast_to='integer')
#' #    x     ids variable id
#' # 1  1 {1,2,3}     id_1  1
#' # 2  2 {5, 10}     id_1  5
#' # 3  1 {1,2,3}     id_2  2
#' # 4  2 {5, 10}     id_2 10
#' # 5  1 {1,2,3}     id_3  3
meltOnSets <- function(df, col, prefix, ...) {
    set_df <- df[ , col] %>% setParse(., prefix=prefix, ...)

    cbind(df, set_df) %>%
        reshape2::melt(na.rm=TRUE,
                       measure.vars=names(set_df),
                       value.name=prefix, ...)
}

