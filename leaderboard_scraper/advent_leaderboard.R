library(tidyverse)
library(jsonlite)
library(lubridate)
library(ggtext)
library(showtext)
library(httr)
library(glue)

source('leaderboard_scraper/advent_source.R')

font_add_google(name = "Source Code Pro", family = 'source-code-pro')
showtext_auto()

theme_set(
  cowplot::theme_cowplot() +
    theme(
      text = element_text(color = '#009900', family = 'source-code-pro'),
      plot.title = element_markdown(color = '#cccccc'),
      plot.title.position = 'plot',
      plot.background = element_rect(fill = '#0f0f23'),
      axis.text = element_text(color = "#009900"),
      axis.line = element_line(color = "#009900"),
      axis.title =element_text(color = "#cccccc"),
      axis.ticks = element_line(color = '#009900')
    )
)

dt <- GET(
  url = glue('https://adventofcode.com/2020/leaderboard/private/view/{board_id}.json'),
  cookie
)

dt2 <- content(dt, encoding='UTF-8') %>% 
  .[['members']] %>% 
  tibble(raw = .) %>% 
  unnest_wider(raw) %>%
  unnest_longer(completion_day_level, indices_to = 'day_id') %>% 
  unnest_longer(completion_day_level, indices_to = 'star', values_to = 'completion_ts') %>% 
  unnest_longer(completion_ts, indices_include = F) %>% 
  mutate(completion_ts = as_datetime(as.numeric(completion_ts), tz = "America/New_York"),
         day_id = as.numeric(day_id)) %>% 
  filter(!is.na(star)) %>% 
  pivot_wider(
    names_from = "star",
    values_from = "completion_ts",
    names_prefix = "star_"
  ) %>% 
  select(name, day_id, star_1, star_2) %>% 
  arrange(name, day_id) 


######## Who Had the Quickest Time Between part 1 and part 2?
####(Excludes Day 25)
dt2 %>% 
  filter(day_id != 25) %>% 
  mutate(diff = difftime(star_2, star_1, units = 'secs')) %>%
  arrange(diff) %>% 
  slice(1:10)

######## Who Had the Slowest Time Between part 1 and part 2?
dt2 %>% 
  mutate(diff = difftime(star_2, star_1, units = 'hours')) %>%
  arrange(-diff) %>% 
  slice(1:10)

######## Fastest Part 1 Finish From Release?
dt2 %>% 
  filter(!is.na(star_1)) %>%
  group_by(day_id) %>% 
  mutate(start_time = min(floor_date(star_1, 'day')),
         complete_time = difftime(star_1, start_time, units = 'secs')
         ) %>%
  arrange(complete_time) %>% 
  transmute(name, day_id, complete_time = paste0(as.numeric(complete_time)%/%60,
                                                 ":",
                                                 as.numeric(complete_time)%%60))
  


######## Fastest Part 2 Finish From Release?
dt2 %>% 
  filter(!is.na(star_2)) %>%
  group_by(day_id) %>% 
  mutate(start_time = min(floor_date(star_2, 'day')),
         complete_time = difftime(star_2, start_time, units = "secs")
         )%>%
  arrange(complete_time) %>% 
  transmute(name, day_id, complete_time = paste0(as.numeric(complete_time)%/%60,
                                                 ":",
                                                 as.numeric(complete_time)%%60))

######## Completions Per Hour?
ann <- data.frame(star = rep("Star 1", 3),
                  x = c(4.5, 13.5, 21),
                  y = rep(40, 3),
                  lbl = c('Before Work\n(<10am)', 'Work Hours\n(10am-6pm)', 'After Work\n(>6pm)'))

dt2 %>% 
  select(star_1, star_2) %>% 
  gather(star, time) %>% 
  filter(!is.na(time)) %>%
  mutate(hr = hour(time),
         star = str_replace_all(star, "star_", "Star ")) %>% 
  count(star, hr) %>% 
  ggplot(aes(x = hr, y = n, fill = star)) + 
    geom_col() + 
    geom_vline(aes(xintercept = 9.5), lty = 2, color = "#009900") + 
    geom_vline(aes(xintercept = 17.5), lty = 2, color = "#009900") + 
    geom_text(data = ann,
      aes(x = x, y=y, label = lbl), hjust = 'center',
      color = '#009900', family = 'source-code-pro') + 
    labs(x = "Hour of Day (ET)", y = "# of Completion",
         title = "What Hour of the Day Are Stars Being Earned?") + 
    scale_fill_manual(values = c('Star 2' = '#ffff66',
                               'Star 1' = '#9999cc'), guide = F) + 
    facet_wrap(~star, ncol = 1, scales = "free_y") + 
    theme(
      strip.background = element_rect(fill = '#0f0f23')
    )
  

######## Who Had the Most Top 3 Finishes?
dt2 %>%
  select(name, day_id, star_1, star_2) %>% 
  gather(star, time, -name, -day_id) %>%
  group_by(day_id, star) %>% 
  mutate(rnk = dense_rank(time)) %>% 
  filter(rnk <= 3) %>%
  ungroup() %>% 
  count(name, rnk, sort = T) %>% 
  spread(rnk, n) %>%
  replace_na(list(`1` = 0, `2`=0, `3`=0)) %>% 
  rowwise() %>%
  mutate(`Total Top 3` = sum(c_across(`1`:`3`))) %>% 
  arrange(-`Total Top 3`)
  
  
######## Which Day had the Most Completions?
dt2 %>% 
  select(day_id, star_1, star_2) %>% 
  gather(star, time, -day_id) %>%
  filter(!is.na(time)) %>%
  count(day_id, star) %>% 
  mutate(star = str_replace_all(star, "star_", "Star ")) %>%
  ggplot(aes(x = day_id, y = n, fill = fct_rev(star))) + 
    geom_col() + 
    geom_text(aes(label = n), position = position_stack(vjust = .5)) + 
    labs(x = "Day", y = "# of Stars", 
         title = "How Many Stars Were Earned per Day?",
         fill = "") + 
    scale_fill_manual(values = c('Star 2' = '#ffff66',
                                 'Star 1' = '#9999cc'),
                      guide = guide_legend(reverse = TRUE)) +
    coord_cartesian(xlim = c(1, NA)) + 
    theme(
      strip.background = element_rect(fill = '#0f0f23'),
      legend.position = 'top',
      legend.justification = 'center'
    )
  

######## Which Day Had the Fastest Completions?
dt2 %>% 
  filter(!is.na(star_2)) %>%
  group_by(day_id) %>% 
  mutate(start_time = min(floor_date(star_2, 'day')),
         complete_time = difftime(star_2, start_time, units="hours")
  ) %>%
  summarize(
    completions = n(),
    med_complete = quantile(complete_time, .5),
    avg_complete = mean(complete_time)
  ) %>% 
  arrange(med_complete)


######## Which Day Had The Slowest?
dt2 %>% 
  filter(!is.na(star_2)) %>%
  group_by(day_id) %>% 
  mutate(start_time = min(floor_date(star_2, 'day')),
         complete_time = difftime(star_2, start_time, units="hours")
         ) %>%
  summarize(
    completions = n(),
    med_complete = quantile(complete_time, .5),
    avg_complete = mean(complete_time)
  ) %>% 
  arrange(desc(med_complete))
