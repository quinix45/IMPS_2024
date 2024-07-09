
library(usefun)
library(tidyverse)
theme_set(theme_classic())


library(extrafont)

# load font
loadfonts(device="win")  
par(family = "Times New Roman")


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


####### Stukel fig#####


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



