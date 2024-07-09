library(tidyverse)


source("C:/Users/fabio/Dropbox/Work/Useful R scripts/GGplot_defaults.R")

####### Figure 1 #######

irf_4PL <- function(theta, a = 1, b = 0, c = 0, d = 1)
{
   (c + (d-c)*(exp(a * (theta - b))/ (1 + exp(a * (theta - b)))))
}

irf_CLL <- function(theta, a = 1, b = 0)
{
   1 - exp( -exp( a*(theta - b)))
}

irf_NLL <- function(theta, a = 1, b = 0)
{
   exp( -exp( -a*(theta - b)))
}


theta <- seq(-3.2, 3.2, .01)



# ggsave("Results_figures/CLL_NLL.png", 
#        CLL_NLL,
#        width = 7, height = 4.5, dpi = 300, units = "in")


## make using base R instead

col_pallet <- c("#999999", 
                "#E69F00", 
                "#56B4E9", 
                "#009E73", 
                "#F0E442", 
                "#0072B2", 
                "#D55E00", 
                "#CC79A7", 
                "#271DEB")

png("CLL_NLL.png", width = 7, height = 7, units = "in", res = 300)

par(lwd = 2,
    bg = NA,
    family = "Times New Roman")

curve(irf_CLL(x), xlim = c(-3, 3), ylim = c(-.5, 1),
      bty = "l", xlab = expression(theta),
      ylab = "",
      col = col_pallet[4], yaxt = "n")
curve(irf_NLL(x), add = TRUE, col = col_pallet[9])
curve(irf_4PL(x), add = TRUE, col = col_pallet[7])

theta2 <- seq(-6, 6, by = .001) # 2PL
P <- irf_4PL(theta2)
theta1 <- log(-log(1 - P)) # CLL
theta3 <- -log(-log(P)) # NLL
points(theta2, -.1 -dnorm(theta2), col = col_pallet[7], lty = 2, type = 'l')
points(density(theta1)$x, -.1 -density(theta1)$y, type = 'l', lty = 2, col = col_pallet[4])
points(density(theta3)$x, -.1 -density(theta3)$y, type = 'l', lty = 2, col = col_pallet[9])

abline(h = 0, lty = 3)
abline(h = .5, lty = 3)
lines(c(0, 0), c(0, 1), lty = 3)
axis(side = 2, at = c(-.51, -.31, -.1, .25, .5, .75, 1),
     labels = c(.4, .2, 0, .25, .5, .75, 1))
mtext(text = expression(paste(P, "(", italic(y), " = 1 | ", theta, ")")),
      side = 2, line = 2, at = .5)
mtext(text = expression(paste(theta, "  Density")), side = 2, line = 2, at = -.3)

legend("topleft", legend = c("1PL", "CLL", "NLL"), bty = "n", lty = 1, 
       col = col_pallet[c(7, 4, 9)])

dev.off()


# check that this makes sense
summary(irf_4PL(theta2))
summary(irf_CLL(theta1))
summary(irf_NLL(theta3))
