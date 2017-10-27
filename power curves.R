#===========================#
# Power Curves with Plotly  #
#===========================#

# When performing Student's t-test to compare the difference in means between 
# two groups, it is a useful exercise to determine the effect of unequal 
# sample sizes in the comparison groups on power. 
# Formally, power can be defined as the probability of rejecting the 
# null hypothesis when the alternative hypothesis is true. 
# Informally, power is the ability of a statistical test to detect an effect,
# if the effect actually exists. 
# Large imbalances generally will not have adequate statistical power to detect 
# even large effect sizes associated with a factor, 
# leading to a high Type II error rate.
# 
# To jusity this reasoning I performed a power analysis for different group sizes.
# I considered the following group sizes, where n1 are the number of subjects 
# in group 1 and n2 are the number of subjects in group 2:
# 
# n1 = 28, n2 = 1406: n1 represents 2% of the entire sample size of 1434
# n1 = 144, n2 = 1290: n1 represents 10% of the entire sample size of 1434
# n1 = 287, n2 = 1147: n1 represents 20% of the entire sample size of 1434
# n1 = 430, n2 = 1004: n1 represents 30% of the entire sample size of 1434
# n1 = 574, n2 = 860: n1 represents 40% of the entire sample size of 1434
# n1 = 717, n2 = 717: equal size groups 
# (this is optimal because it leads to the highest power for a given effect size)

library(pwr) # for power calcs
library(dplyr) # for data manipulation
library(tidyverse)
library(plotly) # for interactive power curves

# Generating Power Calculations

p.table <- cbind(NULL, NULL)

for (i in seq(0,1, length.out = 200)){
  pwrt1 <- pwr.t2n.test(n1 = 28, n2 = 1406,
  sig.level = 0.05, power = NULL, 
  d = i, alternative="two.sided")
  pwrt2 <- pwr.t2n.test(n1 = 144, n2 = 1290,
  sig.level = 0.05, power = NULL, 
  d = i, alternative="two.sided")
  pwrt3 <- pwr.t2n.test(n1 = 287, n2 = 1147,
  sig.level = 0.05, power = NULL, 
  d = i, alternative="two.sided")
  pwrt4 <- pwr.t2n.test(n1 = 430, n2 = 1004,
  sig.level = 0.05, power = NULL, 
  d = i, alternative="two.sided")
  pwrt5 <- pwr.t2n.test(n1 = 574, n2 = 860,
  sig.level = 0.05, power = NULL, 
  d = i, alternative="two.sided")
  pwrt6 <- pwr.t2n.test(n1 = 717, n2 = 717,
  sig.level = 0.05, power = NULL, 
  d = i, alternative="two.sided")
  p.table <- rbind(p.table, cbind(pwrt1$d, pwrt1$power,
                                  pwrt2$d, pwrt2$power,
                                  pwrt3$d, pwrt3$power,
                                  pwrt4$d, pwrt4$power,
                                  pwrt5$d, pwrt5$power,
                                  pwrt6$d, pwrt6$power))
}

p.table <- cbind(seq_len(nrow(p.table)), p.table)

colnames(p.table) <- c("id",
                       "n1=28, n2=1406.effect size",
                       "n1=28, n2=1406.power",
                       "n1=144, n2=1290.effect size",
                       "n1=144, n2=1290.power",
                       "n1=287, n2=1147.effect size",
                       "n1=287, n2=1147.power",
                       "n1=430, n2=1004.effect size",
                       "n1=430, n2=1004.power",
                       "n1=574, n2=860.effect size",
                       "n1=574, n2=860.power",
                       "n1=717, n2=717.effect size",
                       "n1=717, n2=717.power")

# 2. Reshaping data for ggplot2 format

temp <- p.table %>%
  as.data.frame() %>%
  gather(key = name, value = val, 2:13) %>%
  separate(col = name, into = c("group", "var"), sep = ",") %>%
  spread(key = var, value = val)

# 3. Factor Groupings

temp$group <- factor(temp$group, 
                     levels = c("n1=28, n2=1406", 
                                "n1=144, n2=1290", 
                                "n1=287, n2=1147",
                                "n1=430, n2=1004",
                                "n1=574, n2=860", 
                                "n1=717, n2=717"))

# 4. Plotting

p <- ggplot(temp, aes(x = `effect size`, y = power, color = group)) + 
  geom_line(size=2) + 
  theme_bw() + 
  theme(axis.text=element_text(size=14), 
  axis.title=element_text(size=14), 
  legend.text=element_text(size=14)) +
  geom_vline(xintercept = .54, linetype = 2) +
  geom_hline(yintercept = 0.80, linetype = 2)

pow <- matrix(NA, nrow = 200, ncol = 6)
p.table <- cbind(NULL, NULL)
n1 <- c(28, 144, 287, 430, 574, 717)
n2 <- c(1406, 1290, 1147, 1004, 860, 717)
effect.sizes <- seq(0, 1, length.out = 200)

for(j in 1:length(n1)){
  for(i in 1:length(effect.sizes)){
    pow[i,j] <- pwr.t2n.test(n1 = n1[j], 
                             n2 = n2[j], 
                             sig.level = 0.05, 
                             power = NULL, 
                             d = effect.sizes[i], 
                             alternative = "two.sided")$power
  }
}

