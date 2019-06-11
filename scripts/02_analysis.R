#' ---
#' output:
#'     html_document: 
#'         self_contained: yes
#' ---
#' 
#' # The Underproduction of Philosophy PhDs #
#' 
#' One common explanation for [the contiuing structural employment problem for philosophy PhDs](http://dailynous.com/2017/10/02/philosophy-phd-program-rankings-apdas-2017-final-report/) is that there are ["too many" philosophy PhDs](http://dailynous.com/2015/12/04/are-there-too-many-philosophy-phds-its-complicated/). That is, according to this explanation, the supply of philosophy PhDs (i.e., the number of new PhDs) is greater than the demand (specifically, the need for people with PhDs in philosophy to teach philosophy classes).  One way to empirically check this explanation is to look at the tenure-track employment rate — what fraction of recent philosophy PhDs are employed as tenure-track faculty?  Insofar as this rate has gone down and remains low, this suggests that the field has been overproducing PhDs. 
#' 
#' But this empirical check ignores the phenomenon of [casualization](https://www.aaup.org/article/president-inequality-corporatization-and-casualization-academic-labor) or [adjunctification](https://adjunctcrisis.com/2017/10/23/how-the-student-became-a-consumer-and-the-professor-became-precarious/).  That is, instead of hiring tenure-track faculty to teach, colleges and universities have hired adjuncts and other contingent faculty.  Given this phenomenon, the number of new tenure-track positions in philosophy doesn't necessarily reflect changes in the demand for philosophy teachers.  So the tenure-track employment rate is not a good measure. 
#' 
#' A more accurate way to measure demand for philosophy teachers would be based on total student enrollments in philosophy courses.  However, as far as I know these data aren't publicly available, either nationally or for individual schools.  I suggest the **number of bachelor's degrees in philosophy** could be a useful proxy for total course enrollments.  [These data are publicly available from IPEDS.](https://nces.ed.gov/ipeds)  This assumes that the ratio of philosophy majors to total enrollments in philosophy courses is roughly constant.  But, given that assumption, changes in the ratio of new BAs to new PhDs can give us a sense of whether philosophy has recently been producing "too many" PhDs.  Specifically, if this ratio is much lower today than it was, say, 25-30 years ago, then this is an indicator that the field has overproduced PhDs.  
#' 
#' In fact, the opposite appears to be the case.  Over the last 15 years, the bachelors:PhD ratio was **elevated**, suggesting that the field has **underproduced** PhDs.  This is not the case in all fields.
#' 
#' For comparison, in this post I examine electrical engineering, English, biology, mathematics and statistics, physical science, and sociology, as well as philosophy.  I use IPEDS data from 1984 through 2016.  As of December 15, 2017, the IPEDS data for 2016 are provisional.  I use simple sums to calculate the number of bachelor's degrees and PhDs in these fields in each year.  This produces slightly higher numbers than [Eric Schwitzgebel reports in this post](https://schwitzsplinters.blogspot.jp/2017/12/sharp-declines-in-philosophy-history.html?m=1) using the same data; usually the difference is 50 bachelor's degrees or fewer.  Since Schwitzgebel does not fully document his methods, I can't tell where this discrepancy comes from.  
#' 
#' The complete code to reproduce this post — including data retrieval — is available at <https://github.com/dhicks/overproduction_PhDs>. If you find errors in the code, please submit them as issues on GitHub or email me directly at <hicks.daniel.j@gmail.com>. 

#+ setup, echo = FALSE, warning = FALSE, message = FALSE
## Hide everything except the plots
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

library(tidyverse)
library(cowplot)
library(plotly)

load('../data/01-ipeds_1984_2016.Rdata')

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

#+ filter_data
dataf = ipeds_1984_2016 %>%
    mutate(year = as.integer(year),
           awlevel = as.integer(awlevel), 
           total_awarded = total_w + total_m) %>%
    filter(awlevel %in% c(5, 9, 17), 
           str_detect(cipcode, regex)) %>%
    mutate(cip = str_trunc(cipcode, 2, ellipsis = '')) %>%
    left_join(cip_codes) %>%
    as_tibble()

#+ aggregate_data
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
    group_by(field) %>%
    mutate(PhD.lead5 = lead(PhD, n = 5), 
           PhD.lead10 = lead(PhD, n = 10), 
           ratio = bachelor / PhD,
           ratio.5 = bachelor / PhD.lead5, 
           ratio.10 = bachelor / PhD.lead10) %>%
    ungroup()

## Normalize ratio to first ten years
annual = annual %>%
    filter(year < min(year) + 10) %>%
    group_by(field) %>%
    summarize(ratio.base = mean(ratio)) %>%
    ungroup() %>%
    left_join(annual, .) %>%
    mutate(ratio.norm = ratio / ratio.base)

#' The first plot below shows the total number of bachelors degrees, by field and year.  The plot shows a dramatic rise in biology; a sharp drop-off in physical science in the 1980s, followed by a gradual increase; and more-or-less stable numbers across the other fields.  
## Bachelors
{ggplot(annual, aes(year, bachelor, color = field)) + 
    geom_line() + 
    scale_color_brewer(palette = 'Set1')} %>%
    ggplotly()
#'
#' Next we plot total PhDs. Again, there is a dramatic rise in biology, and smaller increasing trends for most other fields. Physical science is much more prominent in this plot than in the bachelors plot. 
## PhDs
{ggplot(annual, aes(year, PhD, color = field)) + 
    geom_line() + 
    scale_color_brewer(palette = 'Set1')} %>%
    ggplotly()
#'
#' Now we move on to the ratio of new bachelors degrees to new PhDs. Again, we are using bachelors degrees as a proxy for the total demand for philosophy teachers. Comparing this ratio for recent years (the right-hand side) to 25+ years ago (the left-hand side) gives us an indication of whether these fields have been overproducing or underproducing PhDs.  If the ratio is **lower** today than 30 years ago, this suggests that supply (PhDs) has increased relative to demand (bachelors), and so the field has been **overproducing** PhDs.  But if the ratio is **higher** today than 30 years ago, this suggests that demand has increased relative to supply, and so the field has been **underproducing** PhDs.  
{ggplot(annual, aes(year, color = field)) +
    geom_line(aes(y = ratio)) + 
    scale_color_brewer(palette = 'Set1')} %>%
    ggplotly()
#' 
#' Working from top to bottom, the long-term trends for sociology and (to a lesser extent) English is an increasing ratio.  This suggests that these fields have been **underproducing** PhDs.  This story is somewhat more complicated for English, which shows large peaks and troughs.  
#' 
#' In contrast, electrical engineering and math had decreasing ratios in the first 15 years of the data, and recently have been more-or-less flat.  Physical science follows a similar but more mild pattern.  This suggests that these fields **overproduced** PhDs.
#' 
#' Philosophy shows a cyclical pattern, similar to English. During roughly 2002-2010, philosophy's ratio was higher than during 1985-2000, suggesting that the field **underproduced** PhDs. The ratio has decreased slightly in the last few years, and today appears to be roughly where it was in the early 1990s. 
#' 
#' Finally, the ratio for biology has been nearly flat.  While the number of PhDs in biology increased dramatically over the last 20 years, so did the number of bachelors degrees.  
#' 
#' Because the ratios for different disciplines are on different scales, it may be easier to interpret a normalized ratio.  In the following plot, for each field, we first calculate the mean ratio over the first ten years of the data (1984-1994), then divide the ratio in a given year by this mean.  
{ggplot(annual, aes(year, ratio.norm, color = field)) + 
    geom_line() + 
    geom_hline(yintercept = 1, linetype = 'dashed') +
    scale_color_brewer(palette = 'Set1')} %>%
    ggplotly()
#' 
#' As with the previous graph, if this normalized ratio is **high** — above the dashed line at 1.0 — that suggests that the field has **underproduced** PhDs.  This appears to be the case for sociology, philosophy, English, and biology; although philosophy has increased only slightly relative to the 1984-1994 baseline, and English has recently dropped just below this baseline.  There appears to be chronic overproduction in physical science, math, and electrical engineering.  
#' 
#' So, against the common explanation for the structural unemployment problem, this analysis suggests that philosophy (along with English, biology, and sociology) has produced **too few** PhDs, rather than too many.  This suggests, I think, that the right way to address the structural unemployment problem is to focus on de-adjunctification — that is, pushing colleges and universities to turn short-term, part-time teaching contracts into long-term, full-time teaching positions, and to turn long-term, full-time teaching positions into tenure-track faculty positions.  This push is likely to be most effective by organizing strong faculty, adjunct, and graduate student unions.  But that is a topic for another post.  
#' 
#' ## Common Objections and Misinterpretations ##
#' 
#' *Not everyone who enrolls in a philosophy class goes on to major.*
#' 
#' Of course.  My suggestion is that the number of majors gives us an indirect way to measure total course enrollments.  Mathematically, my suggestion is based on the fact that
#' \( \frac{total enrollment}{\# PhDs} = \frac{\# bachelors}{\# PhDs} \times \frac{total enrollment}{\# bachelors}.)
#' The left-hand side is the ratio we really want.  The first term on the right-hand side is the ratio we can calculate with the available data.  If the second term is constant, then the term we can calculate is always proportional to the term we really want.  Then changes in the term that we can calculate (increasing or decreasing) correspond to changes in the term that we really want.  The assumption that the last term is constant is one important limitation of my analysis.  
#' 
#' *Faculty are teaching more courses and larger courses.*
#' 
#' Larger classes and heavier course loads are one way to respond to increased demand for philosophy teaching without increasing the number of philosophy teachers. In terms of the bachelors:PhD ratio, this would mean that the “correct” ratio would be higher. In economics jargon, this would be called increasing “worker productivity,” and it does look more economically efficient. So yes, this is one possibility that my post doesn’t explicitly take into account. I assume that “worker productivity” of philosophy teachers has remained constant. In addition, increasing "productivity" probably also has a number of disadvantages, including decreasing the quality of education, taking time away from research and administration, and harming faculty mental health and general well-being.  Data can be used to examine these negative impacts empirically.  
#' 
#' *This means grad students and grad programs don't have to worry about employment. That's ethically irresponsible.*
#' 
#' The central claim of my analysis is that structural underemployment isn't due to producing too many PhDs.  This doesn't imply that structural underemployment isn't a problem; indeed, it assumes the existence of the problem.  This is like saying "your heart disease isn't caused by high cholesterol."  Ruling out one cause doesn't imply that you don't have heart disease.  Because of the way the demand for philosophy teachers is satisfied — by adjunctification and increasing course loads — competition for long-term positions with reasonable workloads remains extremely tight.  Grad students and grad programs need to be aware of this, and ensure that students are prepared to compete effectively and find other careers.  I would further argue that grad students and grad programs should work for de-adjunctification.  
#' 
#' *STEM fields, including biology, physical science, and electrical engineering, have better non-academic career prospects than philosophy.*
#' 
#' Non-academic career prospects and preparation differ substantially between STEM fields.  While it's common for chemistry PhDs to go into industry, this is less common with physics and biology PhDs.  In [a survey of biology, chemistry, and physics graduate students](http://journals.plos.org/plosone/article/figure?id=10.1371/journal.pone.0036307.g004), about half of chemistry students reported being "encouraged" or "strongly encouraged" to pursue a career in industry; only about 30% of biology and physics grad students were similarly encouraged to pursue industry careers.  At least in biology, [faculty career prospects are comparable to philosophy](http://www.nature.com/news/2011/110420/full/472276a.html), with about one-third of biology PhDs in tenured or tenure-track positions six years after graduation. 
#' 
#' *Your analysis uses too many assumptions.*
#' 
#' A basic lesson from philosophy of science is that substantive assumptions are needed for any data analysis.  This applies as well to the analyses done by your school's Institutional Research office that are used to argue against faculty hiring or for shrinking programs.  In this post, I've tried to make my assumptions explicit, and made my analysis code available for review.  
#' 
#' *De-adjunctification is impossible. (or) You need a solid plan for de-adjunctification.*
#' 
#' The lack of a treatment for the disease, given the diagnosis, doesn't necessarily invalidate the diagnosis.  Part of the goal of my post was to direct our attention away from worrying about whether we should produce fewer PhDs (and who should produce fewer, and the perverse incentives for any given program to produce more PhDs, and so on), and instead direct our attention more towards academic politics.  More fundamentally, if my analysis is right, then we should be working simultaneously to organize and push for reasonable de-adjunctifying reforms.  Even if departments aren't in a position to convert adjunct positions into tenure-track positions, they may be in a position to convert term-by-term part-time contracts into annual or multiyear full-time contracts.  Organization — such as adjunct unions — is important for building power over the long term, in order to push for more radical changes, such as reducing class sizes and teaching loads and hiring more full-time teaching faculty (if not tenure-track faculty).  These two steps aren't a fully-developed plan for de-adjunctification.  But they do move in that direction.  
#' 