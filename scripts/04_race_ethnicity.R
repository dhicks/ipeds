library(tidyverse)
library(tictoc)

library(directlabels)

data_folder = '../data/'

## Prior to AY 1994-95, race+ethnicity are only reported for 2-digit CIP codes
years = 1995:2016
base_filenames = c(
    paste0('c', 94:98, 95:99, '_a'),
    paste0('c', 2000:2016, '_a')
)

cip_codes = tribble(
    ~cip, ~field, ~regex,
    '38', 'philosophy', '38[\\.]?01'#,
    # '23', 'english', '23', 
    # '54', 'history', '54', 
    # '42', 'psychology', '42', 
    # '26', 'biology', '26', 
    # '40', 'physical science', '40'
)

regex = str_c('^', cip_codes$regex, collapse = '|')

## Download files ----
urls = str_c('https://nces.ed.gov/ipeds/datacenter/data/', 
              base_filenames, '.zip')
download_filenames = str_c(data_folder, base_filenames, '.zip') %>% 
    set_names(years)

download = function(target_file, url) {
    if (!file.exists(target_file)) {
        download.file(url, target_file)
    }
    return(TRUE)
}

all(map_lgl(download_filenames, download))


## Load files ----
## ~2 minutes
tic()
dataf = download_filenames %>% 
    # .[c(1, 10, 22)] %>% 
    map(read_csv) %>% 
    map(rename_all, toupper) %>% 
    map(mutate, AWLEVEL = as.character(AWLEVEL)) %>% 
    bind_rows(.id = 'year')
toc()

dataf_wide = dataf %>% 
    filter(AWLEVEL %in% c('5', '05'), str_detect(CIPCODE, regex)) %>% 
    transmute(year = as.integer(year),
              cipcode = CIPCODE, 
              awlevel = AWLEVEL,
              total = ifelse(!is.na(CTOTALT), CTOTALT, 
                             CRACE15 + CRACE16), 
              black = ifelse(!is.na(CBKAAT), CBKAAT, 
                             CRACE03 + CRACE04), 
              indigenous = ifelse(!is.na(CAIANT), CAIANT, 
                                  CRACE05 + CRACE06), 
              asian = ifelse(!is.na(CASIAT), CASIAT, 
                             CRACE07 + CRACE08), 
              pi = CNHPIT, 
              hispanic = ifelse(!is.na(CHISPT), CHISPT, 
                                CRACE09 + CRACE10), 
              white = ifelse(!is.na(CWHITT), CWHITT, 
                             CRACE11 + CRACE12), 
              unknown = ifelse(!is.na(CUNKNT), CUNKNT, 
                               CRACE13 + CRACE14), 
              two_or_more = C2MORT) %>% 
    group_by(year) %>% 
    summarize_if(is.numeric, sum, na.rm = TRUE) %>% 
    mutate_at(vars(black:two_or_more), 
              list(share = ~ . /total)) %>% 
    ## The 0s for PI are actually NAs, from the period before 
    ## PI was reported separately from Asian
    mutate(pi_share = ifelse(pi_share > 0, pi_share, NA))

dataf_long = dataf_wide %>% 
    select(year, ends_with('share')) %>% 
    gather(key = 'group', value = 'share', ends_with('share'))

dataf_long %>% 
    mutate(group = str_remove(group, '_share')) %>% 
    filter(group != 'white') %>% 
    ggplot(aes(year, share, color = group)) +
    geom_line(aes(linetype = group), show.legend = FALSE) +
    geom_dl(aes(label = group), method = 'last.bumpup') +
    # xlim(NA, 2018) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_color_brewer(palette = 'Set1', guide = FALSE) +
    coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(plot.margin = margin(l = 5, b = 5, t = 5, r = 50)) +
    ggtitle("Bachelor's degrees in Philosophy for minority racial-ethnic groups", 
            subtitle = Sys.time())

ggsave('04_re_share.png', 
       height = 4, width = 6, scale = 1.5)

dataf_long %>% 
    mutate(group = str_remove(group, '_share')) %>% 
    filter(group %in% c('hispanic', 'unknown')) %>% 
    ggplot(aes(year, share, fill = group)) +
    geom_area(position = 'stack') +
    scale_fill_viridis_d(option = 'C') +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    ggtitle("Bachelor's degrees in Philosophy, Hispanic vs. Unknown race-ethnicity", 
            subtitle = Sys.time())
ggsave('04_hisp_na.png', 
       height = 4, width = 6, scale = 1.5)
