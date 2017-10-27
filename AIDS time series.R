#==============================#
# AIDS time series with plotly #
#==============================#

devtools::install_github("ropensci/plotly@fix/nse")
library(plotly)
library(zoo)
library(tidyverse)
library(data.table)

#Aids Data

df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/Aids%20Data.csv", stringsAsFactors = FALSE)

glimpse(df)

# AIDS Related Deaths

plot.df <- df %>%
  filter(Indicator == "AIDS-related deaths") %>%
  filter(Subgroup %in% c("All ages estimate",
                         "All ages upper estimate",
                         "All ages lower estimate"))
glimpse(plot.df)

plot.df <- plot.df %>%
  select(Subgroup, Time.Period, Data.Value) %>%
  spread(Subgroup, Data.Value) %>%
  data.frame()

glimpse(plot.df)

# Plotting

hovertxt <- paste0("<b>Year: </b>", 
                   plot.df$Time.Period, 
                   "<br>",
                   "<b>Est.: </b>", 
                   round(plot.df$All.ages.estimate/1e6,2),
                   "M<br>",
                   "<b>Lower est.: </b>",
                   round(plot.df$All.ages.lower.estimate/1e6,2),
                   "M<br>",
                   "<b>Upper est.: </b>",
                   round(plot.df$All.ages.upper.estimate/1e6,2), 
                   "M")

p <- plot_ly(plot.df, x = ~Time.Period, showlegend = F) %>%
  add_lines(y = ~All.ages.estimate/1e6,
            line = list(width = 4, color = "#1fabdd"),
            hoverinfo = "text", text = hovertxt) %>%
  add_lines(y = ~All.ages.lower.estimate/1e6,
            line = list(color = "#93d2ef"),
            hoverinfo = "none") %>%
  add_lines(y = ~All.ages.upper.estimate/1e6,
            line = list(color = "#93d2ef"),
            fill = "tonexty",
            hoverinfo = "none")
p

# New HIV Infections

plot.df <- df %>%
  filter(Indicator == "New HIV Infections") %>%
  filter(Subgroup %in% c("All ages estimate",
                         "All ages upper estimate",
                         "All ages lower estimate"))

plot.df <- plot.df %>%
  select(Subgroup, Time.Period, Data.Value) %>%
  spread(Subgroup, Data.Value) %>%
  data.frame()

hovertxt <- paste0("<b>Year: </b>", 
                   plot.df$Time.Period, 
                   "<br>",
                   "<b>Est.: </b>", 
                   round(plot.df$All.ages.estimate/1e6,2),
                   "M<br>",
                   "<b>Lower est.: </b>",
                   round(plot.df$All.ages.lower.estimate/1e6,2),
                   "M<br>",
                   "<b>Upper est.: </b>", 
                   round(plot.df$All.ages.upper.estimate/1e6,2), 
                   "M")

# Add to AIDS related deaths plot

p <- p %>%
  add_lines(data = plot.df, y = ~All.ages.estimate/1e6, 
            line = list(width = 4, color = "#00587b"),
            hoverinfo = "text", text = hovertxt) %>%
  add_lines(data = plot.df, y = ~All.ages.lower.estimate/1e6, 
            line = list(color = "#3d83a3"),
            hoverinfo = "none") %>%
  add_lines(data = plot.df, y = ~All.ages.upper.estimate/1e6, 
            line = list(color = "#3d83a3"),
            fill = "tonexty",
            hoverinfo = "none")
p

# People Receiving ART

x <- c(2010:2015)
y <- c(7501470, 9134270, 10935600, 12936500, 14977200, 17023200)

hovertxt <- paste0("<b>Year:</b>", x, "<br>",
                   "<b>Est.:</b> ", round(y/1e6,2), "M")

p <- p %>%
  add_lines(x = x, y = y/1e6, line = list(width = 5, color = "#e61a20"),
            yaxis = "y2",
            hoverinfo = "text", text = hovertxt)
p

# Layout

p <- p %>%
  layout(xaxis = list(title = "", showgrid = F, ticklen = 4, ticks = "inside",
                      domain = c(0, 0.9)),
         yaxis = list(title = "", gridwidth = 2, domain = c(0, 0.9), range = c(-0.01, 4)),
         yaxis2 = list(overlaying = "y", side = "right", showgrid = F, color = "#e61a20",
                       range = c(5,18)),
         
         annotations = list(
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0, y = 1, showarrow = F, align = "left",
                text = "<b>Keeping the pressure up<br><sup>Worldwide, (in millions)</sup></b>",
                font = list(size = 18, family = "Arial")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0, y = -0.07, showarrow = F, align = "left",
                text = "<b>Source: UNAIDS</b>",
                font = list(size = 10, family = "Arial", color = "#bfbfbf")),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = 1995, y = 3.92, showarrow = F, align = "left",
                text = "<b>New HIV Infections(per year)</b>",
                font = list(size = 12, family = "Arial", color = "#00587b")),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = 1999, y = 1, showarrow = F, align = "left",
                text = "<b>AIDS related deaths (per year)</b>",
                font = list(size = 12, family = "Arial", color = "#1fabdd")),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = 2010, y = 3, showarrow = F, align = "left",
                text = "<b>People receving Anti-<br>Retroviral Therapy (total)</b>",
                font = list(size = 12, family = "Arial", color = "#e61a20")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0.85, y = 0.98, showarrow = F,
                align = "left",
                text = 'Inspired by <a href = "http://www.economist.com/blogs/graphicdetail/2016/05/daily-chart-23">The economist</a>',
                font = list(size = 12, family = "Arial")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.375, y = 0.9, showarrow = F, align = "left",
                text = "<b>Lower bound</b>",
                font = list(size = 10, family = "Arial", color = "#8c8c8c")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.375, y = 0.95, showarrow = F, align = "left",
                text = "<b>Higher bound</b>",
                font = list(size = 10, family = "Arial", color = "#8c8c8c")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.485, y = 0.925, showarrow = F, align = "left",
                text = "<b>Estimate</b>",
                font = list(size = 10, family = "Arial", color = "#8c8c8c"))
         ),
         
         shapes = list(
           list(type = "rectangle",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 0.9, y1 = 0.95,
                fillcolor = "#d9d9d9",
                line = list(width = 0)),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 0.9, y1 = 0.9,
                line = list(width = 2, color = "#8c8c8c")),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 0.95, y1 = 0.95,
                fillcolor = "#bfbfbf",
                line = list(width = 2, color = "#8c8c8c")),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 0.925, y1 = 0.925,
                fillcolor = "#bfbfbf",
                line = list(width = 2, color = "#404040"))),
         
         height = 600,width = 1024)

print(p)
