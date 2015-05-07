# Functions for comparing citations within Impact Factor buckets

#' Group a variable into buckets based on its quantiles or those of another
#' variable.
#'
#' @param x_quantile The variable to calculate quantile buckets from.
#' @param x_bucket The variable to collect into the quantile buckets.
#' @param nbuckets The number of quantile buckets to use. Specify this *or*
#' `probs`, but not both.
#' @param probs The vector of probabilities for the quantile bucket cut points.
#' Specify this *or* `nbuckets`, but not both.
#' @return A factor vector corresponding to `x_bucket` with bucket ranges.
#' If an element of `x_bucket` is outside of the range of `x_quantile`, its
#' bucket will be NA.
#'
quantileBuckets <- function(x_quantile, x_bucket=x_quantile, nbuckets=10, probs=NULL) {
    if (!is.null(nbuckets) & is.null(probs)) {
        breaks <- quantile(x_quantile, probs=0:nbuckets / nbuckets)
        cut(x_bucket, breaks, include.lowest=TRUE)
    } else if (!is.null(probs) & is.null(nbuckets)) {
        cut(x_bucket, quantile(x_quantile, probs), include.lowest=TRUE)
    } else {
        stop('Only specify nbuckets or probs, not both.')
    }
}


#' Compare on- and off-Academia citations within years and quantile groups
#' of journal impact factors.
#'
#' @param cites_df A dataframe with citations and impact factors.
#' @param summarizer (default mean) A function to summarize citations within groups.
#' @param comparator (default `/`) A function with arguments (on, off), that compares
#' on and off-Academia citation summaries. The default computes the on/off ratio.
#' @param ... Extra parameters to `quantileBuckets`
#' @return A dataframe with statistic by year, impact factor group, and on/off-source.
compareByImpactFactorBuckets <- function(cites_df, summarizer=mean,
                                         comparator=`/`, ...) {

   # Find buckets based on distribution of on-Academia citations.
   bucketFactors <- function(x) {
       on_factors <- cites_df %>% filter(source=='on') %>% use_series(impact_factor)
       quantileBuckets(on_factors, x, ...)
   }

   cites_df %>%
        mutate(if_bucket = bucketFactors(impact_factor)) %>%
        filter(!is.na(if_bucket)) %>%
        group_by(if_bucket, year, source) %>%
        summarize(cites=summarizer(citations)) %>%
        reshape2::dcast(year + if_bucket ~ source, value.var='cites') %>%
        mutate(comparison = comparator(on, off))
}

#' Average results over buckets, weighting by the number of on-Academia
#' articles in the bucket.
#'
#' @param cites_df A dataframe of citations with impact factors.
#' @return A dataframe of weighted average results by year.
#'
summarizeOverBuckets <- function(cites_df) {
    cite_ratios <- compareByImpactFactorBuckets(cites_df,
                                                summarizer=mean,
                                                comparator=`/`)
    counts <- compareByImpactFactorBuckets(cites_df,
                                           summarizer=length,
                                           comparator=`+`)

    weights <- counts %>%
        group_by(year) %>%
        mutate(weight = on / sum(on)) %>%
        ungroup %>%
        select(year, if_bucket, weight)

    cite_ratios %>% left_join(., weights, by=c('year', 'if_bucket')) %>%
        group_by(year) %>% summarize(wtd_avg = sum(weight * comparison))

}


#' Boxplots on- and off-Academia citations within years and quantile groups
#' of journal impact factors.
#'
#' @param cites_df A dataframe with citations and impact factors.
#' @param ... Extra parameters to `quantileBuckets`
#'
#' @return A ggplot2 plot.
plotByImpactFactorBuckets <- function(cites_df, ...) {

    # Find buckets based on distribution of on-Academia citations.
    bucketFactors <- function(x) {
        on_factors <- cites_df %>% filter(source=='on') %>% use_series(impact_factor)
        quantileBuckets(on_factors, x, ...)
    }

    # Add bucket variable to data
    df <- cites_df %>%
        mutate(if_bucket = bucketFactors(impact_factor)) %>%
        filter(!is.na(if_bucket))

    p <- ggplot(df, aes(x=factor(year), y=citations, color=source)) +
        geom_boxplot() +
        facet_wrap(~if_bucket, ncol=2) +
        labs(x='Year', y='Citations (log scale)',
            title='Citations of On- and Off-Academia Articles By Year and Journal Impact Factor') +
        theme_bw()
    plotLogScale(p, xy='y')
}
