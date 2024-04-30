library(mirt)
library(tidyverse)
library(usefun)

library(extrafont)

loadfonts(device="win")  
par(family = "Times New Roman")
theme_set(theme_classic())

set.seed(12344)

theta <- rnorm(1000)

I <- 20

pars <- cbind(rgamma(I, 16, 12),
              rnorm(I),
              rep(0, I),
              rep(1, I))

dat <- genPattern_4PL(th = theta, pars = pars, seed = 4567)


fit_2pl <- mirt(dat,
                itemtype = "2PL")

fit_3pl <- mirt(dat,
                itemtype = "3PL",
                technical = list(NCYCLES = 5000))


dat <- data.frame("theta_2pl" = fscores(fit_2pl), 
                  "theta_3pl" = fscores(fit_3pl))


theta_plot <- ggplot(dat, aes(x = theta_2pl, y = theta_3pl)) +
              geom_point() +
  theme(text = element_text(family = "Times New Roman",
                            size = 14)) +
  labs(x = "\u03b8 2PL",
       y = "\u03b8 3PL") +
  theme(panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'),
        legend.position=c(.8,.25))


print(theta_plot)


ggsave("theta_plot.png", 
       theta_plot,
       width = 7, height = 4.5, dpi = 300, units = "in")














