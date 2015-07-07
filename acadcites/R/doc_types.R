## Functions for importing and processing results from
## Mechanical Turk classification results files.

# Some helper functions
zipList <- function(keys, values) { values %>% as.list %>% `names<-`(keys) }
reverseList <- function(l) { zipList(unlist(l), names(l)) }
relabelTypes <- function(s, label_map) {
    s %>% factor %>% `levels<-`(label_map) %>% as.character
}


cleanColumnNames <- function(df) {
    df %>%
        names %>%
        str_replace_all('HIT', 'Hit') %>%
        str_replace_all('Title', 'HitTitle') %>%
        str_replace_all("([a-z0-9])([A-Z])", "\\1_\\2") %>%
        str_replace_all("([a-z])([A-Z0-9])", "\\1_\\2") %>%
        str_replace_all('\\.', '_') %>%
        tolower %>%
        str_replace('^input_', '') %>%
        str_replace('^answer_', '')    
}


cleanBatchId <- function(batch_ids) {
    str_replace_all(batch_ids, '^BatchId:|;', '') %>% as.integer
}


convertDateTimes <- function(dtstr) {
    tz <- dtstr %>%
        stringr::str_match('([A-Za-z]{1,}) 2015$') %>%
        `[`(,2)
           
    tz <- if (all(tz == 'PDT')) { 'Etc/GMT-7' } else { 'UTC' }
    
    dtstr %>%
        str_replace_all('[A-Za-z]{1,} 2015', '2015') %>%
        strptime(format='%a %b %d %T %Y', tz=tz) %>%
        as.POSIXct
}


checkWorkTime <- function(df) {
    wts <- (df$submit_time - df$accept_time) %>% as.vector
    all(wts == df$work_time_in_seconds)
}


cleanDocumentType <- function(doctypes) {
    label_map = list(
        'academic_bugfix' = 'Errata/Correction',
        'broken' = 'Broken DOI Link',
        'editorial_or_commentary' = 'Editorial/Commentary',
        'media_review' = 'Book Review, etc.',
        'meeting_summary' = 'Conference Summary',
        'no_idea' = 'Unknown',
        'not_in_english' = 'No English Abstract or Full Text',
        'not_english' = 'No English Abstract or Full Text',
        'original_research' = 'Original Research',
        'other' = 'Other',
        'response' = 'Response or Comment'
        )

    relabelTypes(doctypes, reverseList(label_map))    
}


.COLS_TO_KEEP <- c('hit_id',
                  'batch_id',
                  'creation_time',
                  'worker_id',
                  'assignment_status',
                  'assignment_id',
                  'accept_time',
                  'submit_time',
                  'work_time_in_seconds',
                  'be_cf_id',
                  'title',
                  'doi_url',
                  'document_type')


importMTResults <- function(path) {
    mt = read.csv(path, as.is=TRUE, encoding='UTF-8')
    names(mt) <- cleanColumnNames(mt)
    mt %>%
        mutate(batch_id = cleanBatchId(requester_annotation)) %>%
        select(one_of(.COLS_TO_KEEP)) %>%
        mutate(creation_time = convertDateTimes(creation_time),
               accept_time = convertDateTimes(accept_time),
               submit_time = convertDateTimes(submit_time),
               document_type = cleanDocumentType(document_type)) %>%
        filter(!is.na(document_type)) 
}


classifyDocTypes <- function(mt_results, min_agreement=3) {
    mt_results %>%
        group_by(be_cf_id, document_type) %>%
        summarize(type_mt_cnt = n(), doi_url=first(doi_url)) %>%
        mutate(total_mt_cnt=sum(type_mt_cnt),
               agreement=type_mt_cnt/total_mt_cnt) %>%
        filter(type_mt_cnt >= min_agreement & total_mt_cnt > 2) 
}


importGoldStandard <- function(gspath, ...) {
    read.csv(gspath, as.is=TRUE, ...) %>%
        mutate(document_type = cleanDocumentType(document_type))
    
}

compareMTGoldStandard <- function(gs_df, mt_results, min_agreement=2) {
    gs_df <- gs_df %>% rename(document_type_gs=document_type)
    mt_doctypes <- mt_results %>%
        classifyDocTypes(min_agreement=min_agreement) %>%
        rename(document_type_mt=document_type) %>%
        select(be_cf_id, document_type_mt, agreement, total_mt_cnt)

    gs_df <- gs_df %>%
        left_join(mt_doctypes, by='be_cf_id') %>%
        mutate(mt_orig = !is.na(document_type_mt) &
                   document_type_mt == 'Original Research',
               gs_orig = !is.na(document_type_gs) &
                   document_type_gs == 'Original Research')

    conf_mat <- gs_df %>% group_by(gs_orig, mt_orig) %>% summarize(n=n())
    tp <- conf_mat %>% filter(gs_orig & mt_orig) %>% `$`(n) %>% sum
    tn <- conf_mat %>% filter(!gs_orig & !mt_orig) %>% `$`(n) %>% sum
    fp <- conf_mat %>% filter(!gs_orig & mt_orig) %>% `$`(n) %>% sum
    fn <- conf_mat %>% filter(gs_orig & !mt_orig) %>% `$`(n) %>% sum
    precision <- tp / (tp+fp)
    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    recall <- tp / (tp + fn)
    tn_rate <- tn / (tn + fp)
    fp_rate <- fp / (tn + fp)
    f1 <- 2 * precision * recall / (precision + recall)
    list(conf_mat=conf_mat,
         tp=tp,
         tn=tn,
         fp=fp,
         fn=fn,
         precision=precision,
         recall=recall,
         accuracy=accuracy,
         tn_rate=tn_rate,
         fp_rate=fp_rate,
         f1=f1)
}


filterByTitle <- function(cites) {
    erratum_grep <- stringr::regex('\\berrat(a|um)\\b|(\\bcorrection:)', ignore_case=TRUE)
    editorial_grep <- stringr::regex('editorial|(commentary:)', ignore_case=TRUE)
    book_review_grep <- stringr::regex('\\bbook review\\b', ignore_case=TRUE)

    check_title <- function(title, pattern) { stringr::str_detect(title, pattern)}
    cites %>% filter(!check_title(title, erratum_grep) &
                        !check_title(title, editorial_grep) &
                         !check_title(title, book_review_grep))

}

