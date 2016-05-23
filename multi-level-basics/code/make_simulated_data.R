
#### name:make_simulated_data ####
setwd("multi-level-basics")
x  <- seq(0, 6, by = 0.05)
set.seed(42)
y <- -0.5 + sample(rnorm(length(x),0,0.05))
set.seed(123)
y2 <- (-0.33 + 0.1*x) + sample(rnorm(length(x),0,0.05))
set.seed(1)
y3 <- (-0.5 + 0.1*x) + sample(rnorm(length(x),0,0.05))

dat <- data.frame(y = y, x, z = 'group1')
dat <- rbind(dat, data.frame(y = y2, x, z = 'group2'))
dat <- rbind(dat, data.frame(y = y3, x, z = 'group3'))
str(dat)
write.csv(dat, "data/simulated_data.csv", row.names = F)
