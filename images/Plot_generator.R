
library(usefun)
library(tidyverse)
theme_set(theme_classic())


library(extrafont)

# load font
loadfonts(device="win")  
par(family = "Times New Roman")


source("C:/Users/fabio/Dropbox/Work/Useful R scripts/GGplot_defaults.R")


###### Define other functions and theta ######


irf_CLL <- function(theta, a = 1, b = 0)
{
  1 - exp( -exp( a*(theta - b)))
}


irf_NLL <- function(theta, a = 1, b = 0)
{
  exp( -exp( -a*(theta - b)))
}

irf_stukel_vec <- Vectorize(irf_stukel.2PL)


theta <- seq(-3.2, 3.2, .01)




####### CLL vs 3pl ######

CLL_vs_3pl <- ggplot(data.frame(x = theta),aes(x = x)) +
   geom_function(fun = irf_CLL, args = list(a = .6,b = .3), aes(colour = "CLL"), 
                 linewidth = .8) +
   geom_function(fun = irf_4PL, args = list(c = .10), aes(colour = "3PL, c = .10"), 
                 linewidth = .8) +
   scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
   theme(text = element_text(family = "Times New Roman",
                             size = 14)) +
   labs(x = "\u03b8",
        y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ")"))) +
   scale_colour_manual(values = c("red","blue"),
                       name = "") +
   ylim(c(0, 1)) +
   theme(panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA), 
         legend.background = element_rect(fill='transparent'),
         legend.text=element_text(size=14),
         legend.position=c(.8,.25))



print(CLL_vs_3pl)


ggsave("CLL_vs_3pl.png", 
       CLL_vs_3pl,
       width = 7, height = 4.5, dpi = 300, units = "in")




###### NLL vs 3plu ######

NLL_vs_3plU <- ggplot(data.frame(x = theta),aes(x = x)) +
   geom_function(fun = irf_NLL, args = list(a = .6,b = -.3), aes(colour = "NLL"), 
                 linewidth = .8) +
   geom_function(fun = irf_4PL, args = list(d = .9), aes(colour = "3PLu, d = .90"), 
                 linewidth = .8) +
   scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
   theme(text = element_text(family = "Times New Roman",
                             size = 14)) +
   labs(x = "\u03b8",
        y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ")"))) +
   scale_colour_manual(values = c("red","blue"),
                       name = "") +
   ylim(c(0, 1)) +
   theme(panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA), 
         legend.background = element_rect(fill='transparent'),
         legend.text=element_text(size=14),
         legend.position=c(.8,.25))


print(NLL_vs_3plU)


ggsave("NLL_vs_3plU.png", 
       NLL_vs_3plU,
       width = 7, height = 4.5, dpi = 300, units = "in")


####### Empirical quantiles Viz #######

norm_decile <-  ggplot()+
   stat_function(fun = plogis,
                 color = "red") +
   scale_x_continuous(limits = c(-3, 3),
                      expand = c(0, 0)) +
   scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
   geom_segment(aes(x = qnorm(c(seq(.1, .9, by = .1))),
                    y = 0,
                    xend = qnorm(c(seq(.1, .9, by = .1))),
                    yend = plogis(qnorm(c(seq(.1, .9, by = .1))))),
                color = "red",
                linetype = 2) +
   geom_segment(aes(x = qnorm(c(seq(.1, .9, by = .1))),
                    y = plogis(qnorm(c(seq(.1, .9, by = .1)))),
                    xend = -3 ,
                    yend = plogis(qnorm(c(seq(.1, .9, by = .1))))),
                color = "red",
                linetype = 2) +   
   xlab("Deciles of Normal Distribution") +
   theme(text = element_text(colour = "black"),
         legend.position="bottom",
         legend.title.position = "top",
         legend.title = element_text(hjust = 0.5),
         strip.text = element_text(face = "bold"),
         strip.background = element_blank(),
         panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         legend.background = element_rect(fill='transparent'),
         axis.text = element_text(family = "Times New Roman", 
                                  colour = "black"))+
   guides(color = guide_legend(title = NULL),
          shape = guide_legend(title = NULL))

print(norm_decile)


ggsave("norm_decile.png", 
       norm_decile,
       width = 7, height = 4.5, dpi = 300, units = "in")



library(ExtDist)


gumbel_decile <- ggplot()+
   stat_function(fun = pGumbel,
                 color = "blue") +
   scale_x_continuous(limits = c(-3, 3),
                      expand = c(0, 0)) +
   scale_y_continuous(limits = c(0,1),
                      expand = c(0, 0)) +
   geom_segment(aes(x = qnorm(c(seq(.1, .9, by = .1))),
                    y = 0,
                    xend = qnorm(c(seq(.1, .9, by = .1))),
                    yend = pGumbel(qnorm(c(seq(.1, .9, by = .1))))),
                linetype = 2,
                color = "blue") +
   xlab("Deciles of Gumbel Distribution") +
   theme(text = element_text(colour = "black"),
         legend.position="bottom",
         legend.title.position = "top",
         axis.title.y = element_blank(),
         legend.title = element_text(hjust = 0.5),
         strip.text = element_text(face = "bold"),
         strip.background = element_blank(),
         panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         legend.background = element_rect(fill='transparent'),
         axis.text = element_text(family = "Times New Roman", 
                                  colour = "black"))+
   guides(color = guide_legend(title = NULL),
          shape = guide_legend(title = NULL))

print(gumbel_decile)


ggsave("gumbel_decile.png", 
       gumbel_decile,
       width = 7, height = 4.5, dpi = 300, units = "in")



####### Stukel fig #######


Stukel_fig <- ggplot(data.frame(x = theta),aes(x = x)) +
   geom_function(fun = irf_stukel_vec, args = list(alpha1 = 1), aes(colour = "la = 0, ua = 1"), 
                 linewidth = .8) +
   geom_function(fun = irf_stukel_vec, args = list(alpha2 = -1), aes(colour = "la = -1, ua = 0"), 
                 linewidth = .8) +
   geom_function(fun = irf_stukel_vec, args = list(alpha1 = .5, alpha2 = -1.5), aes(colour = "la = -1.5, ua = .5"), 
                 linewidth = .8) +
   scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
   theme(text = element_text(family = "Times New Roman",
                             size = 14)) +
   labs(x = "\u03b8",
        y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ")"))) +
   scale_colour_manual(values = c("red","blue", "magenta"),
                       name = "") +
   ylim(c(0, 1)) +
   theme(panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA), 
         legend.background = element_rect(fill='transparent'),
         legend.text=element_text(size=14),
         legend.position=c(.8,.25))


print(Stukel_fig)


ggsave("Stukel_fig.png", 
       Stukel_fig,
       width = 7, height = 4.5, dpi = 300, units = "in")

####### Prior viz for Appendix ########

model_priors <- ggplot()+
   stat_function(fun = dnorm, args = c(mean = 0, sd =.5),
                 aes(color = "log(a)")) +
   stat_function(fun = dexp, args = c(rate = 3), 
                 aes(color = "sigma(a)")) +
   stat_function(fun = dnorm, args = c(mean = 0, sd = 1), 
                 aes(color = "b")) +
   stat_function(fun = dlnorm, args = c(meanlog = .25, sdlog = .5), 
                 aes(color = "sigma(b)")) +
   scale_x_continuous(limits = c(-2, 4.2),
                      expand = c(0, 0)) +
   scale_y_continuous(limits = c(0,2.7), expand = c(0, 0)) +
   ylab("Density") +
   xlab("Prior Value") +
   labs(colour = "Parameter") +
   theme(text = element_text(colour = "black"),
         strip.text = element_text(face = "bold"),
         strip.background = element_blank(),
         panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         legend.background = element_rect(fill='transparent'),
         axis.text = element_text(family = "Times New Roman", 
                                  colour = "black"))

print(model_priors)


ggsave("model_priors.png", 
       model_priors,
       width = 7, height = 4.5, dpi = 300, units = "in")

