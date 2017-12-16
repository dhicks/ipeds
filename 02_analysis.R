#' # The Underproduction of Philosophy PhDs #
#' 
#' One common explanation for [the contiuing structural employment problem for philosophy PhDs](http://dailynous.com/2017/10/02/philosophy-phd-program-rankings-apdas-2017-final-report/) is that there are ["too many" philosophy PhDs](http://dailynous.com/2015/12/04/are-there-too-many-philosophy-phds-its-complicated/). That is, according to this explanation, the supply of philosophy PhDs (i.e., the number of new PhDs) is greater than the demand (specifically, the need for people with PhDs in philosophy to teach philosophy classes).  One way to empirically check this explanation is to look at the tenure-track employment rate — what fraction of recent philosophy PhDs are employed as tenure-track faculty?  Insofar as this rate has gone down and remains low, this suggests that the field has been overproducing PhDs. 
#' 
#' But this empirical check ignores the phenomenon of [casualization](https://www.aaup.org/article/president-inequality-corporatization-and-casualization-academic-labor) or [adjunctification](https://adjunctcrisis.com/2017/10/23/how-the-student-became-a-consumer-and-the-professor-became-precarious/).  That is, instead of hiring tenure-track faculty to teach, colleges and universities have hired adjuncts and other contingent faculty.  Given this phenomenon, the number of new tenure-track positions in philosophy doesn't necessarily reflect changes in the demand for philosophy teachers.  So the tenure-track employment rate is not a good measure. 
#' 
#' A more accurate way to measure demand for philosophy teachers would be based on total student enrollments in philosophy courses.  However, as far as I know these data aren't publicly available, either nationally or for individual schools.  I suggest the *number of bachelor's degrees in philosophy* could be a useful proxy for total course enrollments.  [These data are publicly available from IPEDS.](https://nces.ed.gov/ipeds)  This assumes that the ratio of philosophy majors to total enrollments in philosophy courses is roughly constant.  But, given that assumption, changes in the ratio of new BAs to new PhDs can give us a sense of whether philosophy has recently been producing "too many" PhDs.  Specifically, if this ratio is much lower today than it was, say, 25-30 years ago, then this is an indicator that the field has overproduced PhDs.  
#' 
#' In fact, the opposite appears to be the case.  Over the last 15 years, the bachelors:PhD ratio was *elevated*, suggesting that the field has *underproduced* PhDs.  This is not the case in all fields.
#' 
#' For comparison, in this post I examine electrical engineering, English, biology, mathematics and statistics, physical science, and sociology, as well as philosophy.  I use IPEDS data from 1984 through 2016.  As of December 15, 2017, the IPEDS data for 2016 are provisional.  I use simple sums to calculate the number of bachelor's degrees and PhDs in these fields in each year.  This produces slightly higher numbers than [Eric Schwitzgebel reports in this post](https://schwitzsplinters.blogspot.jp/2017/12/sharp-declines-in-philosophy-history.html?m=1) using the same data; usually the difference is 50 bachelor's degrees or fewer.  Since Schwitzgebel does not fully document his methods, I can't tell where this discrepancy comes from.  
#' 
#' 

TODO: github


library(tidyverse)
library(cowplot)

load('01-ipeds_1984_2016.Rdata')

cip_codes = tribble(
    ~cip, ~field, ~regex,
    '14', 'elec. engineering', '14[\\.]?10',
    '23', 'english', '23', 
    '26', 'biology', '26', 
    '27', 'math', '27', 
    '40', 'physical sci.', '40',
    '45', 'sociology', '45[\\.]?11',
    '38', 'philosophy', '38[\\.]?01'
)

regex = str_c('^', cip_codes$regex, collapse = '|')

dataf = ipeds_1984_2016 %>%
    mutate(year = as.integer(year),
           awlevel = as.integer(awlevel), 
           total_awarded = total_w + total_m) %>%
    filter(awlevel %in% c(5, 9, 17), 
           str_detect(cipcode, regex)) %>%
    mutate(cip = str_trunc(cipcode, 2, ellipsis = '')) %>%
    left_join(cip_codes) %>%
    as_tibble()

annual = dataf %>%
    select(year, field, awlevel, total_awarded) %>%
    mutate(level = recode(awlevel, 
                          `5` = 'bachelor',
                          `9` = 'PhD',
                          `17` = 'PhD')) %>%
    select(-awlevel) %>%
    group_by(year, field, level) %>%
    summarize(total_awarded = sum(total_awarded, na.rm = TRUE)) %>%
    spread(level, total_awarded) %>%
    ungroup() %>%
    mutate(PhD.lag5 = lag(PhD, n = 5), 
           PhD.lag10 = lag(PhD, n = 10), 
           ratio = bachelor / PhD,
           ratio.5 = bachelor / PhD.lag5, 
           ratio.10 = bachelor / PhD.lag10)

## Normalize ratio to first ten years
annual = annual %>%
    filter(year < min(year) + 10) %>%
    group_by(field) %>%
    summarize(ratio.base = mean(ratio)) %>%
    ungroup() %>%
    left_join(annual, .) %>%
    mutate(ratio.norm = ratio / ratio.base)

## Plots
ggplot(annual, aes(year, bachelor, color = field)) + 
    geom_line() + 
    scale_color_brewer(palette = 'Set1')
ggplot(annual, aes(year, PhD, color = field)) + 
    geom_line() + 
    scale_color_brewer(palette = 'Set1')

ggplot(annual, aes(year, color = field)) +
    geom_line(aes(y = ratio)) + 
    scale_color_brewer(palette = 'Set1')
ggplot(annual, aes(year, ratio.norm, color = field)) + 
    geom_line() + 
    geom_hline(yintercept = 1, linetype = 'dashed') +
    scale_color_brewer(palette = 'Set1')
