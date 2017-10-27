#==============================================
# Tabulizer tutorial
# Tabulzer allows for reading data from a pdf
#==============================================

library(tabulizer)
library(tidyverse)

sos_url <- "http://elections.cdn.sos.ca.gov/sov/2016-general/sov/04-historical-voter-reg-participation.pdf"

tab1 <- tabulizer::extract_tables(sos_url)

str(tab1)

head(tab1)

# The (default) result is a list of two matrices, 
# each containing the tables from pages 1 and 2 of the document, respectively.
# A couple of quick cleanups and this becomes a well-formatted data frame:

header <- tab1[[1]][2, ]            # list one, second line

tab1[[1]] <- tab1[[1]][-c(1, 2), ]  # remove headers in table 1

tab1[[2]] <- tab1[[2]][-c(1, 2), ]  # remove headers in table 2

tab1df <- as.data.frame(do.call("rbind", tab1), stringsAsFactors = FALSE)

tab1df <- setNames(tab1df, header)

head(tab1df)

years <- regexpr("[[:digit:]]{4}", tab1df[["Election Date"]])
tab1df$year <- as.numeric(regmatches(tab1df[["Election Date"]], years))
tab1df$RegPerc <- as.numeric(gsub("%", "", tab1df$Registered))

tab1df %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = RegPerc)) +
  ylim(c(0, 100)) +
  labs(y = "% Registered", 
       title = "California Registration, by Year") +
  theme_minimal()
  