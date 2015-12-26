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

# Calculating the mean by supplement type and doses for t.test reference
stats_df <-ToothGrowth %>%
      group_by(supp,dose) %>%
      summarise(mean_Len = mean(len), stddev_Len = sd(len), n = n())
# Calculating the mean by dose
stats_df %>% group_by(dose) %>% summarise(mean_d=mean(mean_Len))
# Calculating the mean by supplement type
stats_df %>% group_by(supp) %>% summarise(mean_d=mean(mean_Len))

#T.Test of supplement type
t_VC <- ToothGrowth %>% filter(supp == "VC") %>% select(len)
t_OJ <- ToothGrowth %>% filter(supp == "OJ") %>% select(len)
t.test(t_OJ,t_VC,paired = FALSE, var.equal = FALSE)

#T.Test of dose & supplement type
t_d0 <- ToothGrowth %>% filter(dose == 0.5)
t_d1 <- ToothGrowth %>% filter(dose == 1)
t_d2 <- ToothGrowth %>% filter(dose == 2)

#t.test(t_d0,t_d1,paired = FALSE, var.equal = TRUE)
#t.test(t_d1,t_d2,paired = FALSE, var.equal = TRUE)
#t.test(t_d0,t_d2,paired = FALSE, var.equal = TRUE)

t.test(filter(t_d0,supp=="OJ")$len,filter(t_d0,supp=="VC")$len ,paired = FALSE, var.equal = TRUE)
t.test(filter(t_d1,supp=="OJ")$len,filter(t_d1,supp=="VC")$len ,paired = FALSE, var.equal = TRUE)
t.test(filter(t_d2,supp=="OJ")$len,filter(t_d2,supp=="VC")$len ,paired = FALSE, var.equal = TRUE)
