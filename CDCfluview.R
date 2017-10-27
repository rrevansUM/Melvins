library(cdcfluview)
library(dplyr)

dat <- get_flu_data(region = "hhs", 
                    sub_region = 1:10, 
                    data_source = "ilinet", 
                    years = 2010:2016)

dat %<>%
  mutate(REGION = factor(REGION,
                        levels =unique(REGION),
                        labels = c("Boston", 
                                   "New York",
                                   "Philadelphia", 
                                   "Atlanta",
                                   "Chicago",
                                   "Dallas",
                                   "Kansas City", 
                                   "Denver",
                                   "San Francisco",
                                  "Seattle"),
                       ordered = TRUE)) %>%
  mutate(
    season_week = ifelse(WEEK >= 40, WEEK - 40, WEEK),
    season = ifelse(WEEK < 40,
                    sprintf("%d-%d", YEAR-1, YEAR),
                    sprintf("%d-%d", YEAR, YEAR+1))
         )

prev_years <- dat %>% filter(season != "2015-2016")
curr_year <- dat %>% filter(season == "2015-2016")

curr_week <- tail(dat, 1)$season_week

ggplot() %>%
  add(geom_point(data = prev_years,
                 aes(x = season_week, y = X..WEIGHTED.ILI, group = season),
                 color = "#969696", size = 1, alpha = 0.5)) %>%
  add(geom_point(data = curr_year,
                 aes(x = season_week, y = X..WEIGHTED.ILI, group = season),
                     color = "red", size = 1.25, alpha = 1)) %>%
  add(geom_line(data = curr_year, 
                aes(x = season_week, y=X..WEIGHTED.ILI, group=season),
                size = 1.25, color="#d7301f")) %>%
  add(geom_vline(xintercept = curr_week, 
                 color = "#d7301f", 
                 size = 0.5,
                 linetype = "dashed", 
                 alpha = 0.5)) %>%
  add(facet_wrap(~REGION, ncol = 3)) %>%
  labs(x = NULL,
       y = "Weighted ILI Index", 
       title = "ILINet - 2010-2016 year weighted flu index history by CDC 
                region-Week Ending Jan 3, 2016 (Red == current season)"
      ) %>%
  add(theme_bw()) %>%
  add(theme(panel.grid=element_blank())) %>%
  add(theme(strip.background=element_blank())) %>%
  add(theme(axis.ticks.x=element_blank())) %>%
  add(theme(axis.text.x=element_blank()))

state_flu <- get_state_data(years = 2000:2016)
glimpse(state_flu)

gg_s <- state_flu %>%
  filter(WEEKEND == "Jan-03-2015") %>%
  select(state = STATENAME, value = ACTIVITY.LEVEL) %>%
  filter(!(state %in% c("Puerto Rico", "New York City"))) %>%
  mutate(value = as.numeric(gsub("Level", "", value))) %>%
  statebins(brewer_pal = "RdPu",
            breaks = 4,
            labels = c("minimal","Low","Moderate","High"),
            legend_position ="bottom",
            legend_title = "ILI Activity Level") %>%
  add(ggtitle("CDC State FluView"))
gg_s