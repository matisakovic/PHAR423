library(tidyverse)

df <- read_csv("./data/gusto.csv")

table(df$tx)

sum(df$day30)/nrow(df)

table(df$day30)

df2 <- df %>% filter (tx!="SK+tPA") %>% mutate(tx2=ifelse(tx=="tPA",1,0))

table(df2$tx2, df2$day30)

x <- table(df2$tx2, df2$day30)

a <- x[1,1]
b <- x[1,2]
c <- x[2,1]
d <- x[2,2]

RR <- (d/(c+d))/(b/(a+b))
OR <- a*d/(b*c)
SE_log_OR <- sqrt(1/a+1/b+1/c+1/d)
log_OR <- log(OR)
lb <- log_OR - 1.96*SE_log_OR
ub <- log_OR + 1.96*SE_log_OR

lb2 <- exp(lb)
ub2 <- (exp(ub))

