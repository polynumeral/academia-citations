# Functions for importing and combining citation and journal data.

#' Import and combine paper citations, paper downloads, and journal information.
#'
#' @param cites_path A path to a file with paper citation data for papers both
#' on and off Academia,
#' @param journals_path A path to a file with data on academic journals.
#' @param ... Other arguments to import and join functions.
#'
#' @return A dataframe of citation, download, and journal data merged together.
#'
importData <- function(cites_path=NULL,
                      journals_path=NULL,
                      ...) {

    if (is.null(cites_path)) {
        cites_path <- system.file('extdata', 'papers.csv.gz', package='acadcites')
    }
    if (is.null(journals_path)) {
        journals_path <- system.file('extdata', 'era_2012_journal_list.csv.gz', package='acadcites')
    }
    cites <- importCitesFile(cites_path, ...)
    journals <- importEraFile(journals_path, ...)
    joinCitesJournals(cites, journals, ...)
}


#' Estimate the age of a paper in years.
#'
#' Since we only have data on publication year, we approximate an article's
#' publication date as June 30 of its publication year.
#'
#' @param date_collected A Date vector of dates when the articles citations
#' were recorded.
#' @param published_year An integer vector of articles' publication years.
#'
#' @return A numeric vector of years between publication and citation
#' collection for each article.
#'
#' @examples
#' colln_dates = as.Date(c('2014-06-19', '2014-05-18', '2014-07-03'))
#' pub_years = c(2010, 2011, 2012)
#' paperAgeYears(colln_dates, pub_years)
#'
paperAgeYears <- function(date_collected, published_year) {
    pubdate <- as.Date(paste0(published_year, '-06-30'))
    as.numeric(date_collected - pubdate) / 365.25
}

# Import the article citations data.
importCitesFile <- function(cites_path, online_cutoff=0) {
    read.csv(cites_path, header=TRUE, as.is=TRUE) %>%
        mutate(source = ifelse(on_set == 't', 'on', 'off'),
               on_acad = as.integer(source=='on'),
               date_collected = as.Date(date_collected),
               age = paperAgeYears(date_collected, year),
               online = as.integer(avail_online_score > online_cutoff))  %>%
        select(-on_set)
}

# Import journal data from the Excellence in Research for Australia (ERA)
# initiative.
importEraFile <- function(era_path) {
    df <- read.csv(era_path, colClasses='character', header=TRUE, as.is=TRUE)
    names(df) <- c('era_id', 'journal_title', 'foreign_title',
                  'for1', 'for1_name', 'for2', 'for2_name',
                  'for3', 'for3_name', 'issn1', 'issn2', 'issn3',
                  'issn4', 'issn5', 'issn6', 'issn7')

    df %>% mutate_each(funs(str_trim))
    issns <- df %>% select(issn1, issn2, issn3, issn4, issn5, issn6, issn7) %>% toSetStr
    topics <- df %>% select(for1_name, for2_name, for3_name) %>%
        mutate_each(funs(str_trim)) %>%
        toSetStr

    # Get the first two digits of a string number.
    # If the string is only length 1, pad w/ a leading 0.
    # '12345' -> '12'
    # '6'     -> '06'
    first2Dig <- function(s) {
        s <- ifelse(nchar(s) == 1, paste0('0', s), s)
        substring(str_trim(s), 1,2)
    }

    # Divisions are the first two digits in the
    # 'Field of Research' code
    divisions <- df %>%
        select(for1, for2, for3) %>%
        mutate_each(funs(first2Dig)) %>%
        toSetStr

    data.frame(journal_title=df$journal_title,
               issns=issns,
               topics=topics,
               divisions=divisions, stringsAsFactors=FALSE)
}


## A journal is a single row, with multiple ISSN values.
## Melt to multiple rows, one for each ISSN (with other
## journal data duplicated). Need single, unique ISSN values
## in table to merge w/ citations.
meltJournalsOnIssn <- function(journals_df) {
    meltOnSets(journals_df, 'issns', prefix='issn')
}

# Join articles to their journals.
joinCitesJournals <- function(cites_df, journals_df) {
    melted_journals <- meltJournalsOnIssn(journals_df)
    left_join(cites_df,
              melted_journals %>% select(journal_title, issn, topics, divisions),
              by='issn')
}

