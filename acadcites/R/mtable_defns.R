# Replace definitions of mtable getSummary functions so they return
# a subset of coefficients.

memisc::setCoefTemplate('stat.nostar'=c(est='($est:#)', stat='(($stat:#))'))

getModelSummary <- function(obj, alpha=0.5, ...) {
    UseMethod('getModelSummary')
}

getModelSummary.zeroinfl <- function(obj, alpha=0.5, ...) {
    hurdle_summary <- memisc::getSummary.hurdle(obj, alpha, ...)
    varnames <- hurdle_summary %>% use_series(coef) %>% rownames
    tokeep <- varnames[!str_detect(varnames, '(Div\\.)')]
    hurdle_summary$coef <-
        hurdle_summary %>%
        use_series(coef) %>%
        extract(tokeep, , )

    hurdle_summary
}

getModelSummary.glm <- function(obj, alpha=0.5, ...) {
    coef_tbl <- obj %>% summary %>% coef
    varnames <- rownames(coef_tbl)
    tokeep <- varnames[!str_detect(varnames, 'Div\\.')]
    coefs <- cbind(coef_tbl[tokeep, ], c(NA), c(NA))
    colnames(coefs) <- c('est', 'se', 'stat', 'p', 'lwr', 'upr')
    list(coef=coefs,
         sumstat=c(N=length(obj$fitted.values),
             AIC=AIC(obj),
             logLik=obj$twologlik/2,
             deviance=deviance(obj)))
}

getModelSummary.lm <- function(obj, alpha=0.5, ...) {
    coef_tbl <- obj %>% summary %>% coef
    varnames <- rownames(coef_tbl)
    tokeep <- varnames[!str_detect(varnames, 'Div\\.')]
    coefs <- cbind(coef_tbl[tokeep, ], c(NA), c(NA))
    colnames(coefs) <- c('est', 'se', 'stat', 'p', 'lwr', 'upr')
    list(coefs=coefs,
         sumstat=c(N=length(obj$fitted.values),
         r.squared=summary(obj)$r.squared,
         deviance=deviance(obj),
         AIC=AIC(obj),
         logLik=as.vector(logLik(obj))))
}

.cleanRegressionTable <- function(table) {
    relabel(table,
            `on_acad`='On-Academia',
            `scale(log1p(impact_factor), scale = FALSE)`='Impact factor (log, centered)',
            `scale(log(age), scale = FALSE)` = 'Article age (log centered)',

            `online`='Available online',
            `on_acad x scale(log1p(impact_factor), scale = FALSE)` = 'On-Academia \u00D7 Impact factor',
            `on_acad x scale(log(age), scale = FALSE)`= 'On-Academia \u00D7 Age',
            `on_acad x online`='On-Academia \u00D7 Available online')
}
