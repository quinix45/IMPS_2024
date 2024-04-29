
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


irf_LPE <- function(th, ksi = 1){
   irf_4PL(theta = th)^ksi
}

irf_LPE(1, 3)


theta <- seq(-3.2, 3.2, .01)


###### Figure 1: 2PL examples #####


two_pl <- ggplot(data.frame(x = theta),aes(x = x)) +
  geom_function(fun = irf_4PL, args = list(b = 1), aes(colour = "b = 1, a = 1")) +
  geom_function(fun = irf_4PL, args = list(a = 1.5), aes(colour = "b = 0, a = 1.5")) +
  geom_function(fun = irf_4PL, aes(colour = "b = 0, a = 1"))+
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  theme(text = element_text(family = "Times New Roman",
                            size = 14)) +
  labs(x = " \u03b8",
       y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ",", italic(a), ",", italic(b), ")"))) +
  scale_colour_manual(values = c("red","blue", "magenta"),
                      name = "") +
  theme(panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    legend.background = element_rect(fill='transparent'),
    legend.position=c(.8,.25))



print(two_pl)


ggsave("two_pl.png", 
       two_pl,
       width = 7, height = 4.5, dpi = 300, units = "in")



###### 1PL ######

one_pl <- ggplot(data.frame(x = theta),aes(x = x)) +
          geom_function(fun = irf_4PL, args = list(b = 1), aes(colour = "b = 1")) +
          geom_function(fun = irf_4PL, args = list(b = - 1), aes(colour = "b = -1")) +
          geom_function(fun = irf_4PL, aes(colour = "b = 0"))+
          scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
          theme(text = element_text(family = "Times New Roman",
                            size = 14)) +
  labs(x = " \u03b8",
       y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ",", italic(b), ")"))) +
  scale_colour_manual(values = c("red","blue", "magenta"),
                      name = "") +
  theme(panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'),
        legend.position=c(.8,.25))



print(one_pl)


ggsave("one_pl.png", 
       one_pl,
       width = 7, height = 4.5, dpi = 300, units = "in")



###### 3PL ######

three_pl <- ggplot(data.frame(x = theta),aes(x = x)) +
            geom_function(fun = irf_4PL, args = list(a = 1, b = 1, c = .15), aes(colour = "a = 1, b = 1, c = .15")) +
            geom_function(fun = irf_4PL, args = list(a = 1.5, b = 0, c = .10), aes(colour = "a = 1.5, b = 0, c = .10")) +
            geom_function(fun = irf_4PL, args = list(a = 1, b = - 1, c = .10), aes(colour = "a = 1, b = - 1, c = .10"))+
            scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
            theme(text = element_text(family = "Times New Roman",
                            size = 14)) +
  labs(x = " \u03b8",
       y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ",", 
                            italic(b), ",", 
                            italic(a), ",", 
                            italic(c), ")"))) +
  scale_colour_manual(values = c("red","blue", "magenta"),
                      name = "") +
   ylim(c(0, 1)) +
  theme(panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'),
        legend.position=c(.8,.25))



print(three_pl)


ggsave("three_pl.png", 
       three_pl,
       width = 7, height = 4.5, dpi = 300, units = "in")



###### 4PL ######

four_pl <- ggplot(data.frame(x = theta),aes(x = x)) +
  geom_function(fun = irf_4PL, args = list(a = 1, b = 1, c = .15, d = .90 ), aes(colour = "a = 1, b = 1, c = .15 , d = .90")) +
  geom_function(fun = irf_4PL, args = list(a = 1.5, b = 0, c = .10, d = .75), aes(colour = "a = 1.5, b = 0, c = .10, d = .75")) +
  geom_function(fun = irf_4PL, args = list(a = 1, b = - 1, c = .10, d = .85), aes(colour = "a = 1, b = - 1, c = .10, d = .85"))+
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  theme(text = element_text(family = "Times New Roman",
                            size = 14)) +
  labs(x = " \u03b8",
       y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ",", 
                            italic(b), ",", 
                            italic(a), ",", 
                            italic(c), ",",
                            italic(d), ")"))) +
  scale_colour_manual(values = c("red","blue", "magenta"),
                      name = "") +
   ylim(c(0, 1)) +
  theme(panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'),
        legend.position=c(.8,.25))



print(four_pl)


ggsave("four_pl.png", 
       four_pl,
       width = 7, height = 4.5, dpi = 300, units = "in")


###### Symmetric Example ######

symmetric_irf <- ggplot(data.frame(x = theta),aes(x = x)) +
  geom_function(fun = irf_4PL, color = "red") +
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  theme(text = element_text(family = "Times New Roman",
                            size = 14)) +
   geom_vline(xintercept = 0, linetype = 3) +
   geom_hline(yintercept = .5, linetype = 3) +
  labs(x = " \u03b8",
       y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ")"))) +
  theme(panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'),
        legend.position=c(.8,.25))



print(symmetric_irf)


ggsave("symmetric_irf.png", 
       symmetric_irf,
       width = 7, height = 4.5, dpi = 300, units = "in")


###### Symmetric Example ######

asymmetric_irf <- ggplot(data.frame(x = theta),aes(x = x)) +
   geom_function(fun = irf_LPE, args = list(ksi = .5 ), aes(colour = "\U03BE = .5")) +
   geom_function(fun = irf_LPE, args = list(ksi = 2), aes(colour = "\U03BE = 2")) +
   scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
   geom_vline(xintercept = 0, linetype = 3) +
   geom_hline(yintercept = .5, linetype = 3) +
   theme(text = element_text(family = "Times New Roman",
                             size = 14)) +
   labs(x = "\u03b8",
        y = expression(paste(P, "(", italic(Y), " = 1 | ", theta, ")"))) +
   scale_colour_manual(values = c("red","blue"),
                       name = "") +
   theme(panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA), 
         legend.background = element_rect(fill='transparent'),
         legend.position=c(.8,.25))


print(asymmetric_irf)


ggsave("asymmetric_irf.png", 
       asymmetric_irf,
       width = 7, height = 4.5, dpi = 300, units = "in")



####### CLL vs 3pl ######

CLL_vs_3pl <- ggplot(data.frame(x = theta),aes(x = x)) +
   geom_function(fun = irf_CLL, args = list(a = .6,b = .3), aes(colour = "CLL")) +
   geom_function(fun = irf_4PL, args = list(c = .10), aes(colour = "3PL, c = .10")) +
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
         legend.position=c(.8,.25))



print(CLL_vs_3pl)


ggsave("CLL_vs_3pl.png", 
       CLL_vs_3pl,
       width = 7, height = 4.5, dpi = 300, units = "in")







###### NLL vs 3plu ######

CLL_vs_3plU <- ggplot(data.frame(x = theta),aes(x = x)) +
   geom_function(fun = irf_NLL, args = list(a = .6,b = -.3), aes(colour = "CLL")) +
   geom_function(fun = irf_4PL, args = list(d = .9), aes(colour = "3PLu, d = .9")) +
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
         legend.position=c(.8,.25))


print(CLL_vs_3plU)


ggsave("CLL_vs_3plU.png", 
       CLL_vs_3plU,
       width = 7, height = 4.5, dpi = 300, units = "in")















