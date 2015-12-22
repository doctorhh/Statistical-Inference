library(dplyr)
library(ggplot2)
data(ToothGrowth)

str(ToothGrowth)
head(ToothGrowth)
nrow(ToothGrowth)

summary(ToothGrowth)
sum(is.na(ToothGrowth))
table(ToothGrowth$supp,ToothGrowth$dose)

interaction.plot(ToothGrowth$dose, ToothGrowth$supp, ToothGrowth$len)

q <- ggplot(ToothGrowth,aes(factor(dose),len)) +
      geom_boxplot(aes(fill=dose)) + facet_grid(.~supp)
print(q)

stats_df <-ToothGrowth %>%
      group_by(supp,dose) %>%
      summarise(mean_Len = mean(len), stddev_Len = sd(len), n = n()) 
#%>%
#      mutate(lwr_limit = mean_Len - 2 * stddev_Len/sqrt(n)) %>%
#      mutate(upr_limit = mean_Len + 2 * stddev_Len/sqrt(n))