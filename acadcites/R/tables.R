## Functions to create tables in the paper

## List of tables
## --------------
##  1. Sample sizes by cohort and on-/off-Academia
##  2. Top journals by # articles in sample
##  3. Top impact factor journals and their topics
##  4. Divisions, sample %s, and impact factors
##  5. Share of sample articles available elsewhere online by on-/off-Academia
##  6. Citations Summary Statistics
##  7. Impact Factor bin table
##  8. Regression citations against impact factors
##  9. Summary Statistics for Regression Covariates
## 10. Regression results for Linear and Neg.Binomial models
## 11. Share of uncited articles
## 12. Regression results for ZINB model
## 13. Predicted Citation lookup tables
## 14. Predicted Advantage lookup tables
## 15. Predicted Advantage by Division.

## Function names that produce tables, listed in order
## of their appearance in the paper.
.tables_functions = list(
    'sampleSizes',                        # Table 1
    'topJournalsByCount',                 # Table 2
    'topJournalsByImpactFactor',          # Table 3
    'divisionSharesCombined',             # Table 4
    'availableOnlineShares',              # Table 5
    'citationSummaryStats',               # Table 6
    'impactFactorBinsMedians',            # Table 7
    'citesImpactFactorRegression',        # Table 8
    'covariateSummaryStats',              # Table 9
    'regressionResults',                  # Table 10
    'shareUncitedArticles',               # Table 11
    'zinbResults',                        # Table 12
    'predictedCitations',                 # Table 13
    'predictedAdvantages',                # Table 14
    'predictLinearCI',                    # Table 15
    'divisionPredictedCitations'          # Table 16
    )


#' Function to generate tables from the paper.
#'
#' Recreate a table, in markdown format with a given citations
#' dataset by specifying the table's caption number in the paper.
#'
#' @param n Table caption number.
#' @param cites_df A data frame with article citations and journal data, as produced by `importData`.
#' @param ... Optional arguments passed to table functions.

#' @return Nothing. Prints a table, usually in markdown, but possibly plain text or LaTeX.
#'
makeTable <- function(n, cites_df, ...) {
    eval(parse(text=.tables_functions[[n]]))(cites_df, ...)
}


# Table 1. Sample size by cohort and on/off-Academia
sampleSizes <- function(cites_df) {
     counts <- cites_df %>%
        group_by(year, source) %>%
        summarize(count=n()) %>%
        reshape2::dcast(year~source)

    totals <- counts %>% select(-year) %>% colSums %>% c(NA, .)

    rbind(counts, totals) %>%
        mutate(on=format(on, format='d', big.mark=','),
              off=format(off, format='d', big.mark=',')) %>%
        knitr::kable(col.names=c('Year', 'Off-Academia', 'On-Academia'),
                     align=c('l', 'r', 'r'))
}

# Table 2. Most represented journals in the samples
topJournalsByCount <- function(cites_df) {
    cites_df %>%
        group_by(journal_title) %>%
        summarize(count=n()) %>%
        mutate(pct=count/sum(count)) %>%
        arrange(-count) %>%
        slice(1:10) %>%
        mutate(count=format(count, format='d', big.mark=','),
               pct=scales::percent(pct)) %>%
        knitr::kable(col.names=c('Journal', '# Articles', '% Total'),
                     align=c('l', 'r', 'r'))
}


# Table 3. Journals with highest impact factors
topJournalsByImpactFactor <- function(cites_df) {
    cites_df %>%
        group_by(journal_title, year) %>%
        summarize(impact_factor=max(impact_factor), topics=first(topics)) %>%
        group_by(journal_title) %>%
        summarize(impact_factor=mean(impact_factor), topics=first(topics)) %>%
        arrange(-impact_factor) %>%
        mutate(topics=topics %>%
                   str_replace_all(., '\\"|\\{|\\}', '') %>%
                   str_replace_all(., ',', ', ')) %>%
        slice(1:10) %>%
        ## User pander here instead of kable; pander will split
        ## wide column contents into multiple lines to keep table
        ## narrow. Have to manually name and align columns, though.
        pander::pander()
}

# Compute % of sample by division for a given on/off-Academia sample,
# or the full sample.
divisionShares <- function(cites_divs, sample=c('on', 'off', 'all')) {
    if (sample %in% c('on', 'off')) {
        cites_divs <- cites_divs %>% filter(source==sample)
    }

    cites_divs %>%
        selectDivisionCols %>%
        sapply(mean) %>%
        (function(x) { data.frame(division=str_replace(names(x), '^Div\\.\\ ', ''),
                                 pct_articles=x,
                                 stringsAsFactors=FALSE) }) %>%
        set_names(c('division', paste0('pct_articles_', sample)))
}

# Compute median impact factor by division.
divisionImpactFactors <- function(cites_divs) {
    divnames <- cites_divs %>% selectDivisionCols %>% names

    div_meds <- lapply(as.list(divnames),
                       function(d) { median(cites_divs[cites_divs[ , d] == 1,
                                                       'impact_factor'], na.rm=TRUE) } )
    data.frame(division=str_replace(divnames, '^Div\\.\\ ', ''),
               med_impact_factor=unlist(div_meds))
}

# Table 4. Sample shares and impact factors by division.
divisionSharesCombined <- function(cites_df) {
    cites_divs <- cites_df %>% addDivisions

    on_shares <- divisionShares(cites_divs, 'on')
    off_shares <- divisionShares(cites_divs, 'off')
    all_shares <- divisionShares(cites_divs, 'all')
    imp_factors <- divisionImpactFactors(cites_divs)

    all_shares %>%
        left_join(on_shares, by='division') %>%
        left_join(off_shares, by='division') %>%
        left_join(imp_factors, by='division') %>%
        arrange(-pct_articles_all) %>%
        mutate(pct_articles_all=scales::percent(pct_articles_all),
               pct_articles_on=scales::percent(pct_articles_on),
               pct_articles_off=scales::percent(pct_articles_off)) %>%
        knitr::kable(col.names=c('Division',
                                 '% All',
                                 '% On',
                                 '% Off',
                                 'Med. Imp. Factor'),
                     align=c('l', 'r', 'r', 'r', 'r'), digits=2)
}

# Table 5. Share of available-online by sample.
availableOnlineShares <- function(cites_df, cutoff=0) {
    cites_df <- cites_df %>% filter(!is.na(avail_online_score))
    cites_df %>%
        filter(!is.na(avail_online_score)) %>%
        mutate(available_online = avail_online_score > cutoff) %>%
        group_by(source, available_online) %>%
        summarize(available=n()) %>%
        filter(available_online) %>%
        select(-available_online) %>%
        left_join(cites_df %>% group_by(source) %>% summarize(total=n())) %>%
        mutate(pct=scales::percent(available/total),
               available=format(available, format='d', big.mark=','),
               total=format(total, format='d', big.mark=',')) %>%
        t %>%
        `[`(-1,) %>%
        as.data.frame %>%
        `names<-`(c('Off-Academia', 'On-Academia')) %>%
        knitr::kable(align=c('r', 'r'))
}

# Table 6. Summary stats for citations counts.
citationSummaryStats <- function(cites_df) {
    with(cites, tapply(citations, list(source), summary)) %>%
        do.call(rbind, .) %>%
        knitr::kable()
}

# Table 7. Median citations of on- and off-Academia by impact factor bin.
impactFactorBinsMedians <- function(cites_df) {
    pctDiff <- function(on, off) { round(100.0*(on-off)/off, 0)}

    abs_diffs <- compareByImpactFactorBuckets(cites, median, `-`)
    pct_diffs <- compareByImpactFactorBuckets(cites, median, pctDiff)

    tbl <- left_join(abs_diffs %>% rename(abs_diff=comparison),
                    pct_diffs %>%
                        select(-off, -on) %>%
                        rename(pct_diff=comparison),
                    by=c('year', 'if_bucket'))

    knitr::kable(tbl,
                 col.names=c('Year', 'Impact Factor Bin',
                             'Off-Academia', 'On-Academia',
                             'Abs. Diff', '% Diff.'),
                 align='r')
}

# Table 8. Regression of citations on impact factor.
citesImpactFactorRegression <- function(cites_df) {
    mdl <- lm(log1p(citations) ~ log1p(impact_factor), cites_df)
    mdl %>% stargazer::stargazer(type='latex',
                    keep.stat=c('n', 'rsq'),
                    star.cutoffs=NA,
                    dep.var.caption='',
                    dep.var.labels='Citations (log scale)',
                    covariate.labels=c('Impact Factor (log scale)', 'Intercept'),
                    omit.table.layout='n')
}

# Table 9. Summary stats for regression covariates (exluding divisions).
covariateSummaryStats <- function(cites_df) {
    cleanVarName <- function(s) {
        s %>% str_replace_all('_', ' ') %>%
            (function(s) {paste0(toupper(substring(s, 1,1)),
                                substring(s,2)) })
    }
    getStats <- function(var) {
        v <- cites_df %>% select_(var) %>% unlist
        data.frame(var=cleanVarName(var),
                   mean=mean(v, na.rm=TRUE),
                   median=median(v, na.rm=TRUE),
                   stdev=sd(v, na.rm=TRUE))
    }

    lapply(list('age', 'impact_factor', 'online'), getStats) %>%
        do.call(rbind, .) %>%
        knitr::kable(col.names=c('', 'Mean', 'Median', 'Std. Dev.'),
                     align=c('l', 'r', 'r', 'r'), digits=2)
}

covariatesSummaryStatsBySample <- function(cites_df) {
 cleanVarName <- function(s) {
        s %>% str_replace_all('_', ' ') %>%
            (function(s) {paste0(toupper(substring(s, 1,1)),
                                substring(s,2)) })
    }
    getStats <- function(var, sample) {
        v <- cites_df %>% filter(source == sample) %>% select_(var) %>% unlist
        data.frame(source=sample,
                   var=cleanVarName(var),
                   mean=mean(v, na.rm=TRUE),
                   median=median(v, na.rm=TRUE),
                   stdev=sd(v, na.rm=TRUE))
    }

    on <- lapply(list('age', 'impact_factor', 'online'),
                function(x) getStats(x, 'on')) %>%
                    do.call(rbind, .)
    off <- lapply(list('age', 'impact_factor', 'online'),
                 function(x) getStats(x, 'off')) %>%
                     do.call(rbind, .)
    rbind(on, off) %>%

        knitr::kable(col.names=c('', 'Mean', 'Median', 'Std. Dev.'),
                     align=c('l', 'r', 'r', 'r'), digits=2)
}

# Table 10. Linear and Neg. Bin. regression results.
regressionResults <- function(cites_df) {
    cat('Preparing Data . . . ')
    cites_divs <- addDivisions(cites_df)
    cat('DONE.\n')

    cat('Fitting linear model . . . ')
    linfit <- runLinearModel(cites_divs)
    cat('DONE.\n')

    cat('Fitting Neg. Bin. model . . . ')
    nbfit <- runNegBinModel(cites_divs)
    cat('DONE.\n')

    memisc::setCoefTemplate('stat.nostar'=c(est='($est:#)', stat='(($stat:#))'))

    mtable('Linear'=linfit, 'Neg. Binom.'=nbfit,
           coef.style='stat.nostar',
           getSummary = getModelSummary,
           summary.stats=c('N', 'R-squared', 'Deviance',
                           'Log-likelihood', 'AIC')) %>%
    .cleanRegressionTable %>%
    pander::pander(style='grid', emphasize.rownames=FALSE)
}

# Table 11. Share of uncited articles by cohort and off-/on-Academia
shareUncitedArticles <- function(cites_df) {
    cites_df %>%
        mutate(uncited=citations==0) %>%
        group_by(source, year, uncited) %>%
        summarize(n=n()) %>%
        group_by(year, source) %>%
        mutate(pct=n/sum(n)) %>%
        filter(uncited) %>%
        select(source, year, pct) %>%
        reshape2::dcast(year ~ source, value.var='pct') %>%
        mutate(on=scales::percent(on), off=scales::percent(off)) %>%
        knitr::kable(col.names=c('Year', 'Off-Academia', 'On-Academia'),
                     align='r')
}

# Table 12. ZINB Regression results.
zinbResults <- function(cites_df) {
    cat('Preparing Data . . . ')
    cites_divs <- addDivisions(cites_df)
    cat('DONE.\n')

    cat('Fitting zero-inflated Neg. Bin. model . . . ')
    zinbfit <- runZeroInflNegBinGLM(cites_divs)
    cat('DONE.\n')

    zinbfit %>%
        mtable(coef.style='stat.nostar', getSummary=getModelSummary,
               summary.stats=c('N', 'Log-likelihood', 'AIC')) %>%
    .cleanRegressionTable

}


# Make hypothetical input data with which to predict citations.
makeCases <- function(cites_divs, ...) {
    impfs <- quantile(cites_divs$impact_factor, probs=c(.1,.5,.9), na.rm=TRUE)
    divisions <- cites_divs %>%
        selectDivisionCols %>%
        summarise_each(funs(mean))

    case_df = data.frame(
        on_acad=NULL,
        impact_factor=NULL,
        age=NULL,
        online=NULL)

    # Impact factors 10%, 50%, 90%-iles
    for (i in impfs) {
        # Paper age 1-5 years
        for (a in 1:5) {
            # Off and on-Academia
            for (oa in c(0, 1)) {
                # Unavailable and available online elsewhere
                for (on in c(0,1)) {
                    case <- data.frame(on_acad=oa,
                                      impact_factor=i,
                                      age=a,
                                      online=on)
                    case_df <- rbind(case_df, case)
                }
            }
        }
    }
    cbind(case_df, divisions)
}

predictLinearCI <- function(cites_df, age_=5) {
    linfit <- runLinearModel(cites_df %>% addDivisions)
    cases <- makeCases(cites_df %>% addDivisions) %>%
            filter(impact_factor == median(impact_factor, na.rm=TRUE) & age == age_)
    linpreds <- predict(linfit, cases, se.fit=TRUE)
    expected_cites <- (linpreds$fit + 0.5*linpreds$se.fit^2) %>% expm1
    lci_cites <- qlnorm(0.025, linpreds$fit, linpreds$se.fit) -1
    uci_cites <- qlnorm(0.975, linpreds$fit, linpreds$se.fit) -1
    lpi_cites <- qlnorm(0.025, linpreds$fit, linpreds$residual.scale) -1
    upi_cites <- qlnorm(0.975, linpreds$fit, linpreds$residual.scale) -1
    cbind(cases %>% select(age, on_acad, online),
          round(expected_cites, 2),
          paste0('(', round(lci_cites, 2), ', ', round(uci_cites, 2), ')'),
          paste0('(', round(lpi_cites, 2), ', ', round(upi_cites, 2), ')')) %>%
    mutate(on_acad=factor(on_acad, labels=c('N', 'Y')),
               online=factor(online, labels=c('N', 'Y'))) %>%
    knitr::kable(col.names=c('Age', 'On-Academia', 'Online', 'Pred. Cites', '95% Conf. Int.', '95% Pred. Int.'))
}

# Predict citations from the hypothetical data
# using Linear, NB, and ZINB models
predictCases <- function(cites_df) {

    cat('Preparing Data . ')
    cites_divs <- addDivisions(cites_df)
    cat('DONE\n')

    cat('Fitting linear model . . . ')
    linfit <- runLinearModel(cites_divs)
    cat('DONE.\n')
    cat('Fitting Neg. Bin. model . . . ')
    nbfit <- runNegBinModel(cites_divs)
    cat('DONE.\n')
    cat('Fitting ZINB model . . . ')
    zinbfit <- runZeroInflNegBinGLM(cites_divs)
    cat('DONE.\n')

    cases <- makeCases(cites_divs)

    preds_lin <- predict(linfit, cases) %>% expm1
    preds_nb <- predict(nbfit, cases, type='response')
    preds_zinb <- predict(zinbfit, cases, type='response')

    list(preds_lin, preds_nb, preds_zinb) %>%
        do.call(cbind, .) %>%
        as.data.frame %>%
        set_names(c('linear', 'negbin', 'zinb')) %>%
        cbind(cases, .) %>%
        reshape2::melt(id.vars=c('on_acad', 'age', 'impact_factor', 'online'),
                       measure.vars=c('linear', 'negbin', 'zinb'),
                       variable.name='model',
                       value.name='citations') %>%
        mutate(on_acad=factor(on_acad, labels=c('N', 'Y')),
               online=factor(online, labels=c('N', 'Y')),
               impact_factor=factor(impact_factor, labels=c('10th', '50th', '90th')))

}

# Compute predicted citation advantages.
# The advantage is defined as:
# E(Cites_i | X_i, on-Academia, not-online) / E(Cites_i | X_i, off-Academia, not-online) - 1
advantagePreds <- function(preds) {
    preds %>% group_by(model, impact_factor, age) %>%
        mutate(advantage=citations / first(citations)-1)
}

# Format a dataframe of predictions as a table.
makePredictionTable <- function(preds) {
    preds %>%
        reshape2::dcast(model + impact_factor + on_acad + online ~ age) %>%
        knitr::kable(col.names=c('Model', 'IF Pctile',
                                 'On-Academia', 'Online', '1 Year',
                                 '2 Years', '3 Years', '4 Years', '5 Years'),
                     digits=2)
}

# Table 13. Lookup table of predicted citations for different models and
# inputs.
predictedCitations <- function(cites_df) {
    cites_df %>% predictCases %>% makePredictionTable
}

# Table 14. Lookup table of predicted advantages.
predictedAdvantages <- function(cites_df) {
    cites_df %>% predictCases %>% advantagePreds %>% makePredictionTable
}

# Create rows of hypothetical articles each from one research division.
makeDivisionCases <- function(cites_divs) {
    cases <- makeCases(cites_divs)
    divnames <- cites_divs %>% selectDivisionCols %>% names

    makeDivCase <- function(div, cases) {
        cases[ , divnames] <- 0
        cases[ , div] <- 1
        cases$division = str_replace(div, '^Div\\.\\ ', '')
        cases
    }

    lapply(as.list(divnames), function(x) makeDivCase(x, cases)) %>% do.call(rbind, .)
}

# Table 15. Predicted citations and advantage by division, for a 5-year-old
# paper, not online, and at the median impact factor for the given division.
divisionPredictedCitations <- function(cites_df) {
    cites_divs <- addDivisions(cites_df)
    linfit <- runLinearModel(cites_divs)

    div_impfs <- cites_divs %>%
        divisionImpactFactors %>%
        mutate(division = paste0('Div. ', division)) %>%
        rename(impact_factor=med_impact_factor) %>%
        reshape2::dcast(division + impact_factor ~ division, length, value.var='impact_factor')


    cases <- list(0, 1) %>%
        lapply(function(x) {div_impfs %>% mutate(on_acad = x,
                                               online = 0,
                                               age    = 5) }) %>%
        do.call(rbind, .)

    preds <- predict(linfit, cases) %>% expm1

    shares <- divisionShares(cites_divs, 'on') %>%
        left_join(divisionShares(cites_divs, 'off'), by='division') %>%
        set_names(c('division', 'share_on', 'share_off'))

    cases %>%
        cbind(preds) %>%
        mutate(on_acad=ifelse(on_acad==1, 'Y', 'N')) %>%
        select(division, impact_factor, on_acad, preds) %>%
        mutate(division = str_replace(division, '^Div\\.\\ ', '')) %>%
        left_join(shares, by='division') %>%
        reshape2::dcast(division + impact_factor +  share_on + share_off ~ on_acad,
                        value.var='preds') %>%
        mutate(share_on=scales::percent(share_on),
               share_off=scales::percent(share_off),
               difference=Y-N,
               advantage=difference / N) %>%
        arrange(-advantage) %>%
        mutate(advantage=scales::percent(advantage)) %>%
        knitr::kable(digits=2,
                     align=c('l', 'r', 'r', 'r', 'r', 'r', 'r', 'r'),
                     col.names=c('Division',
                         'Med. IF',
                         '% On', '% Off',
                         'Cites Off', 'Cites On',
                         'Diff.', '% Adv.'))
}
