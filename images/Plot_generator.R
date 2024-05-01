
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


irf_stukel_vec <- Vectorize(irf_stukel.2PL)


theta <- seq(-3.2, 3.2, .01)


###### Figure 1: 2PL examples #####


two_pl <- ggplot(data.frame(x = theta),aes(x = x)) +
  geom_function(fun = irf_4PL, args = list(b = 1), aes(colour = "b = 1, a = 1"), 
                linewidth = .8) +
  geom_function(fun = irf_4PL, args = list(a = 1.5), aes(colour = "b = 0, a = 1.5"), 
                linewidth = .8) +
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
    legend.text=element_text(size=14), 
    legend.position=c(.8,.25))



print(two_pl)


ggsave("two_pl.png", 
       two_pl,
       width = 7, height = 4.5, dpi = 300, units = "in")



###### 1PL ######

one_pl <- ggplot(data.frame(x = theta),aes(x = x)) +
          geom_function(fun = irf_4PL, args = list(b = 1), aes(colour = "b = 1"), 
                        linewidth = .8) +
          geom_function(fun = irf_4PL, args = list(b = - 1), aes(colour = "b = -1"), 
                        linewidth = .8) +
          geom_function(fun = irf_4PL, aes(colour = "b = 0"), 
                        linewidth = .8)+
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
        legend.text=element_text(size=14), 
        legend.position=c(.8,.25))



print(one_pl)


ggsave("one_pl.png", 
       one_pl,
       width = 7, height = 4.5, dpi = 300, units = "in")



###### 3PL ######

three_pl <- ggplot(data.frame(x = theta),aes(x = x)) +
            geom_function(fun = irf_4PL, args = list(a = 1, b = 1, c = .15), aes(colour = "a = 1, b = 1, c = .15"), 
                          linewidth = .8) +
            geom_function(fun = irf_4PL, args = list(a = 1.5, b = 0, c = .10), aes(colour = "a = 1.5, b = 0, c = .10"), 
                          linewidth = .8) +
            geom_function(fun = irf_4PL, args = list(a = 1, b = - 1, c = .10), aes(colour = "a = 1, b = - 1, c = .10"), 
                          linewidth = .8)+
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
        legend.text=element_text(size=14), 
        legend.position=c(.8,.25))



print(three_pl)


ggsave("three_pl.png", 
       three_pl,
       width = 7, height = 4.5, dpi = 300, units = "in")



###### 4PL ######

four_pl <- ggplot(data.frame(x = theta),aes(x = x)) +
  geom_function(fun = irf_4PL, args = list(a = 1, b = 1, c = .15, d = .90 ), aes(colour = "a = 1, b = 1, c = .15 , d = .90"), 
                linewidth = .8) +
  geom_function(fun = irf_4PL, args = list(a = 1.5, b = 0, c = .10, d = .75), aes(colour = "a = 1.5, b = 0, c = .10, d = .75"), 
                linewidth = .8) +
  geom_function(fun = irf_4PL, args = list(a = 1, b = - 1, c = .10, d = .85), aes(colour = "a = 1, b = - 1, c = .10, d = .85"), 
                linewidth = .8)+
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
        legend.text=element_text(size=14), 
        legend.position=c(.8,.25))



print(four_pl)


ggsave("four_pl.png", 
       four_pl,
       width = 7, height = 4.5, dpi = 300, units = "in")


###### Symmetric Example ######

symmetric_irf <- ggplot(data.frame(x = theta),aes(x = x)) +
  geom_function(fun = irf_4PL, color = "red", 
                linewidth = .8) +
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
        legend.text=element_text(size=14),
        legend.position=c(.8,.25))



print(symmetric_irf)


ggsave("symmetric_irf.png", 
       symmetric_irf,
       width = 7, height = 4.5, dpi = 300, units = "in")


###### Symmetric Example ######

asymmetric_irf <- ggplot(data.frame(x = theta),aes(x = x)) +
   geom_function(fun = irf_LPE, args = list(ksi = .5 ), aes(colour = "\U03BE = .5"), 
                 linewidth = .8) +
   geom_function(fun = irf_LPE, args = list(ksi = 2), aes(colour = "\U03BE = 2"), 
                 linewidth = .8) +
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
         legend.text=element_text(size=14),
         legend.position=c(.8,.25))


print(asymmetric_irf)


ggsave("asymmetric_irf.png", 
       asymmetric_irf,
       width = 7, height = 4.5, dpi = 300, units = "in")



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

CLL_vs_3plU <- ggplot(data.frame(x = theta),aes(x = x)) +
   geom_function(fun = irf_NLL, args = list(a = .6,b = -.3), aes(colour = "CLL"), 
                 linewidth = .8) +
   geom_function(fun = irf_4PL, args = list(d = .9), aes(colour = "3PLu, d = .9"), 
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


print(CLL_vs_3plU)


ggsave("CLL_vs_3plU.png", 
       CLL_vs_3plU,
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



###### Item Averaging ######


col_pallet <- c("#999999", 
                "#E69F00", 
                "#56B4E9", 
                "#009E73", 
                "#F0E442", 
                "#0072B2", 
                "#D55E00", 
                "#CC79A7", 
                "#271DEB")



options(ggplot2.discrete.colour = col_pallet)

item_summaries <- readRDS("item_summaries.RDS")


item_summaries$model <- recode(item_summaries$model,
                               `2pl` = "2PL",
                               `1pl` = "1PL")

colnames(item_summaries)[3] <- "Model"



letters <- item_summaries %>% 
   filter(Info_crit == "looic") %>% 
   filter(item %in% c("letter.7", "letter.33", "letter.34", "letter.58")) %>% 
   ggplot(aes(x = item, y = sum_values, color = Model)) +
   geom_point(aes(col = Model, shape = Model), size = 3) +
   coord_flip() +
   labs(y = "LOOcv",
        x = "Items") +
   theme(text = element_text(family = "Times New Roman",
                             size = 14),
         panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA),
         legend.text=element_text(size=14), 
         legend.background = element_rect(fill='transparent'))



print(letters)


ggsave("Letters_plot.png", 
       letters,
       width = 7, height = 4.5, dpi = 300, units = "in")




rotate <- item_summaries %>% 
   filter(Info_crit == "looic") %>% 
   filter(item %in% c("rotate.3", "rotate.4", "rotate.6", "rotate.8")) %>% 
   ggplot(aes(x = item, y = sum_values, color = Model)) +
   geom_point(aes(col = Model, shape = Model), size = 3) +
   coord_flip() + 
   labs(y = "LOOcv",
        x = "Items") +
   theme(text = element_text(family = "Times New Roman",
                             size = 14),
         panel.background = element_rect(fill='transparent'), 
         plot.background = element_rect(fill='transparent', color=NA),
         legend.text=element_text(size=14), 
         legend.background = element_rect(fill='transparent'))



print(rotate)


ggsave("Rotate_plot.png", 
       rotate,
       width = 7, height = 4.5, dpi = 300, units = "in")

