# Get the upper and lower range of a vector of data.
# Rounding the limits to the nearest hundredth
# (or less if the data is less than 100)
getRange <- function(v) {
    maxv <-  max(v)
    maxpow10 <- maxv %>%
        log10 %>%
        floor %>%
        min(., 2) %>%
        (function(x) { 10^x })

    ceil <- ceiling(maxv / maxpow10) * maxpow10
    c(0, ceil)
}

# Make tickmarks from the range (min, max) of the data
getTicks <- function(range) {
    minv <- range[1]; maxv <- range[2]

    ticks <- c(0, 5, 10, 50, 100, 200, 500, 1000)
    ticks <- ticks[ticks >= minv && ticks <= maxv]
    if (maxv > 1000) ticks <- c(ticks, seq(2000, max(maxv, 2000), by=1000))

    ticks
}

# Make a continuous log+1 scale for the x or y axis
# with tickmarks specified from the data.
makeScale <- function(plot, axis=c('x', 'y')) {
    ticks <- plot$mapping[[axis]] %>%
        as.character %>%
        `[[`(plot$data, .) %>%
        getRange %>%
        getTicks

    switch(axis,
           'x' = scale_x_continuous(trans='log1p', breaks=ticks),
           'y' = scale_y_continuous(trans='log1p', breaks=ticks))
}

#' Scale a plot's axis or axes using a x->log(1+x) transformation.
#'
#' @param plot A ggplot2 plot with a defined aesthetic mapping
#' @param xy Axes to scale: 'x', 'y' or c('x', 'y')
#' @return A new ggplot2 plot with scaled axes. The axis labels
#' will still reflect the raw data.
plotLogScale <- function(plot, xy=c('x', 'y', c('x', 'y'))) {
    # Add scales to plot
    if ('x' %in% xy) plot <- plot + makeScale(plot, 'x')
    if ('y' %in% xy) plot <- plot + makeScale(plot, 'y')
    plot
}
