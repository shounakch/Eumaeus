library(ggplot2)
library(dplyr)

d = readr::read_csv("E:/eumaeusTest_truven_mdcr_Shounak/ConcurrentComparator/e_21183/estimates_t7.csv")

d <- d %>% filter(abs(logRr) <= 5, exposureId == 211831)

plot(density(exp(d$logRr), na.rm=TRUE))
abline(v = 1, lty = 2)

# d <- d %>% mutate("ci95Lb" = exp(logLb95),
#                   "ci95Ub" = exp(logUb95),
#                   "effectSize" = 1)




