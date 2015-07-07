# Reproduce figures
#
# Figures:
# --------
# 1. Histograms over citations counts by off-/on-Academia
# 2. Citations boxplots by impact factor bucket and year of publication
# 3. Scatterplot of cites against impact factor
# 4. Scatterplot of cites against impact factor by off-/on-Academia and year.

## Function names that produce figures, listed in order
## of their appearance in the paper.
.figures_functions = list(
    'plotCiteDistributions',
    'plotByImpactFactorBuckets',
    'plotCitesImpactFactorScatter',
    'plotImpactFactorMedReg')


#' Function to generate figures from the paper.
#'
#' Recreate a figure with a given citations dataset by specifying the table's
#' caption number in the paper.
#'
#' @param n Figure caption number.
#' @param cites_df A data frame with article citations and journal data, as produced by `importData`.
#' @param ... Optional arguments passed to figure functions.
#'
#' @return Nothing. Renders a plot.
#'
makeFigure <- function(n, cites_df, ...) {
    eval(parse(text=.figures_functions[[n]]))(cites_df, ...)
}


plotCiteDistributions <- function(cites_df) {
    ggplot(cites_df, aes(x=citations)) +
    geom_histogram(binwidth=1, fill='steelblue', color='white') +
    xlim(0, 100) +
    facet_wrap(~source, scales='free_y') +
    theme_bw()
}

plotCitesImpactFactorScatter <- function(cites_df) {
    p <- ggplot(cites_df, aes(x=impact_factor, y=citations)) +
        geom_point(position=position_jitter(height=.1, width=.01), alpha=.3, size=.75) +
        ggplot2::geom_smooth(method='lm') +
        theme_bw() +
        labs(x='Impact Factor (log scale)', y='Citations (log scale)')
    plotLogScale(p, c('x', 'y'))
}

plotImpactFactorMedReg <- function(cites_df) {
    p <- ggplot(cites_df, aes(x=impact_factor, y=citations, color=source)) +
        geom_point(position=position_jitter(height=.1, width=.01), alpha=.3, size=.75) +
        facet_wrap(~year, ncol=2) +
        stat_quantile(quantiles=0.5) +
        scale_colour_manual(values=c('orange', 'purple')) +
        labs(x='Impact Factor (log scale)', y='Citations (log scale)') +
        theme_bw()
    plotLogScale(p, c('x', 'y'))
}
