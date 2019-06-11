## Replicating the plot in <http://schwitzsplinters.blogspot.com/2017/12/women-have-been-earning-30-34-of.html>

library(tidyverse)
library(directlabels)

load('../data/01-ipeds_1984_2016.Rdata')

cip_codes = tribble(
    ~cip, ~field, ~regex,
    '38', 'philosophy', '38[\\.]?01',
    '23', 'english', '23', 
    '54', 'history', '54', 
    '42', 'psychology', '42', 
    '26', 'biology', '26', 
    '40', 'physical science', '40'
)

regex = str_c('^', cip_codes$regex, collapse = '|')

dataf = ipeds_1984_2016 %>%
    filter(awlevel == '5') %>% 
    filter(str_detect(cipcode, regex)) %>% 
    as_tibble() %>% 
    ## Fix some variables
    mutate(year = as.integer(year),
           total_awarded = total_w + total_m, 
           cip = str_trunc(cipcode, 2, ellipsis = '')) %>%
    ## Summarize
    group_by(year, cip) %>% 
    summarize_at(vars(total_w, total_awarded), 
                 funs(sum), na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(share_w = total_w / total_awarded) %>% 
    ## Meaningful CIP labels
    left_join(cip_codes) %>% 
    select(-regex)

# dataf_phd = ipeds_1984_2016 %>%
#     filter(awlevel == '7') %>% 
#     filter(str_detect(cipcode, regex)) %>% 
#     as_tibble() %>% 
#     ## Fix some variables
#     mutate(year = as.integer(year),
#            total_awarded = total_w + total_m, 
#            cip = str_trunc(cipcode, 2, ellipsis = '')) %>%
#     ## Summarize
#     group_by(year, cip) %>% 
#     summarize_at(vars(total_w, total_awarded), 
#                  funs(sum), na.rm = TRUE) %>% 
#     ungroup() %>% 
#     mutate(share_w = total_w / total_awarded) %>% 
#     ## Meaningful CIP labels
#     left_join(cip_codes) %>% 
#     select(-regex)

ggplot(dataf, aes(year, share_w, 
                  color = field, group = field)) +
    geom_line(show.legend = FALSE) +
    geom_dl(aes(label = field), method = 'last.bumpup') +
    scale_y_continuous(name = "bachelor's degrees\nawarded to women (%)", 
                       labels = scales::percent_format(accuracy = 1), 
                       limits = c(0, NA)) +
    # scale_color_brewer(palette = 'Bl') +
    coord_cartesian(clip = 'off') +
    ggtitle('Women have earned 30-34% of philosophy BAs\nin the U.S. since approximately forever', 
            subtitle = '(For values of "forever" ≤ 30 years)') +
    theme_minimal() +
    theme(plot.margin = margin(t = .25, l = .25, b = .25, 
                               r = 1.25, unit = 'in'))

ggsave('03_women_share.png', 
       width = 6, height = 4)    
    
