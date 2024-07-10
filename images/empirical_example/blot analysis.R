library(brms)
library(tidyverse)
data("blot", package = "psychTools")
dat_long <- blot %>% mutate(id = 1:n()) %>%
  pivot_longer(-id, names_to = "item", values_to = "response") %>%
  mutate(resp = as.numeric(response))



# brms arguments

n_cores <- 4
n_threads <- 2
chains <- 4
iter <- 3000
warmup <- 1500

######### 1PL Full ######### 

formula_1pl <- bf(response ~ inv_logit(exp(alog)*(theta - b)), 
                  theta ~ 0 + (1 | id),
                  b ~ 1 + (1 | item),
                  alog ~ 1,
                  nl = TRUE)

prior_1pl <- prior("normal(0, 1)", class = "b", nlpar = "b") +
  prior("lognormal(.25, .5)", class = "sd", nlpar = "b") +
  prior("normal(0, .5)", class = "b", nlpar = "alog") +
  prior("constant(1)", class = "sd", group = "id", nlpar = "theta") 


fit_1pl <- brm(formula = formula_1pl,
                       data = dat_long,
                       family = brmsfamily("bernoulli", 
                                           "identity"),
                       seed = 54321,
                       prior = prior_1pl,
                       cores = n_cores,
                       chains = chains,
                       threads = n_threads,
                       save_pars = save_pars(all = TRUE))


######### 2PL Full ######### 


formula_2pl <- bf(response ~ inv_logit(exp(alog)*(theta - b)),
                  theta ~ 0 + (1 | id),
                  b ~ 1 + (1 | item),
                  alog ~ 1 + (1 | item),
                  nl = TRUE)

prior_2pl <- prior("normal(0, 1)", class = "b", nlpar = "b") +
  prior("lognormal(.25, .5)", class = "sd", nlpar = "b") +
  prior("normal(0, .5)", class = "b", nlpar = "alog") +
  prior("exponential(3)", class = "sd", group = "item", nlpar = "alog") +
  prior("constant(1)", class = "sd", group = "id", nlpar = "theta")



fit_2pl <- brm(formula = formula_2pl,
                       data = dat_long,
                       family = brmsfamily("bernoulli", 
                                           "identity"),
                       seed = 65432,
                       prior = prior_2pl,
                       chains = chains,
                       cores = n_cores,
                       threads = n_threads,
                       save_pars = save_pars(all = TRUE))

######### CLL 1PL Full #########

formula_CLL_1 <- bf(response ~ 1 - exp( -exp( exp(alog) *(theta - b))),
                    theta ~ 0 + (1 | id),
                    # Difficulty*discrimination
                    b ~ 1 + (1 | item),
                    # Discrimination
                    alog ~ 1, 
                    nl = TRUE)


prior_CLL_1 <- prior("normal(0, 1)", class = "b", nlpar = "b") +
  prior("lognormal(.25, .5)", class = "sd", nlpar = "b") +
  prior("normal(0, .5)", class = "b", nlpar = "alog") +
  prior("constant(1)", class = "sd", group = "id", nlpar = "theta")


fit_CLL_1 <- brm(formula = formula_CLL_1,
                         data = dat_long,
                         family = brmsfamily("bernoulli", 
                                             "identity"),
                         seed = 76543,
                         prior = prior_CLL_1,
                         chains = chains,
                         init = 0,
                         cores = n_cores, 
                         threads = n_threads,
                         save_pars = save_pars(all = TRUE))


######### CLL 2PL Full #########

formula_CLL_2 <- bf(response ~ 1 - exp( -exp( exp(alog) *(theta - b))),
                    theta ~ 0 + (1 | id),
                    # Difficulty*discrimination
                    b ~ 1 + (1 | item),
                    # Discrimination
                    alog ~ 1 + (1 | item), 
                    nl = TRUE)


prior_CLL_2 <- prior("normal(0, 1)", class = "b", nlpar = "b") +
  prior("lognormal(.25, .5)", class = "sd", nlpar = "b") +
  prior("normal(0, .5)", class = "b", nlpar = "alog") +
  prior("exponential(3)", class = "sd", group = "item", nlpar = "alog") +
  prior("constant(1)", class = "sd", group = "id", nlpar = "theta") 


fit_CLL_2 <- brm(formula = formula_CLL_2,
                         data = dat_long,
                         family = brmsfamily("bernoulli", 
                                             "identity"),
                         seed = 87654,
                         prior = prior_CLL_2,
                         chains = chains,
                         cores = n_cores, 
                         threads = n_threads,
                         save_pars = save_pars(all = TRUE))



######### NLL 1PL #########

formula_NLL_1 <- bf(response ~ exp( -exp(-exp(alog)*(theta - b))),
                    theta ~ 0 + (1 | id),
                    # Difficulty*discrimination
                    b ~ 1 + (1 | item),
                    # Discrimination
                    alog ~ 1, 
                    nl = TRUE)



prior_NLL_1 <- prior("normal(0, 1)", class = "b", nlpar = "b") +
  prior("lognormal(.25, .5)", class = "sd", nlpar = "b") +
  prior("normal(0, .5)", class = "b", nlpar = "alog") +
  prior("constant(1)", class = "sd", group = "id", nlpar = "theta")


fit_NLL_1 <- brm(formula = formula_NLL_1,
                         data = dat_long,
                         family = brmsfamily("bernoulli", 
                                             "identity"),
                         seed = 98765,
                         prior = prior_NLL_1,
                         chains = chains,
                         cores = n_cores, 
                         threads = n_threads,
                         save_pars = save_pars(all = TRUE))


######### NLL 2PL #########


formula_NLL_2 <- bf(response ~ exp( -exp(-exp(alog)*(theta - b))),
                    theta ~ 0 + (1 | id),
                    # Difficulty*discrimination
                    b ~ 1 + (1 | item),
                    # Discrimination
                    alog ~ 1 + (1 | item), 
                    nl = TRUE)


prior_NLL_2 <- prior("normal(0, 1)", class = "b", nlpar = "b") +
  prior("lognormal(.25, .5)", class = "sd", nlpar = "b") +
  prior("normal(0, .5)", class = "b", nlpar = "alog") +
  prior("exponential(3)", class = "sd", group = "item", nlpar = "alog") +
  prior("constant(1)", class = "sd", group = "id", nlpar = "theta")


fit_NLL_2 <- brm(formula = formula_NLL_2,
                         data = dat_long,
                         family = brmsfamily("bernoulli", 
                                             "identity"),
                         seed = 43210,
                         prior = prior_NLL_2,
                         chains = chains,
                         cores = n_cores, 
                         threads = n_threads,
                         save_pars = save_pars(all = TRUE))



# save results
# 
# mod_list <- list(fit_1pl,
#                  fit_2pl,
#                  fit_CLL_1,
#                  fit_CLL_2,
#                  fit_NLL_1,
#                  fit_NLL_2)
# 
# saveRDS(mod_list, "mod_list.RDS")

mod_list <- readRDS("mod_list.RDS")


inv_logit <- plogis # error if I don't include this


loo_1pl <- brms::loo(mod_list[[1]], 
                     reloo = TRUE,
                     reloo_args = list(cores = 4))

loo_2pl <- brms::loo(mod_list[[2]],
                     reloo = TRUE,
                     reloo_args = list(cores = 4))

loo_1CLL <- brms::loo(mod_list[[3]], 
                      reloo = TRUE,
                      reloo_args = list(cores = 4))

loo_2CLL <- brms::loo(mod_list[[4]], 
                      reloo = TRUE,
                      reloo_args = list(cores = 4))

loo_1NLL <- brms::loo(mod_list[[5]], 
                      reloo = TRUE,
                      reloo_args = list(cores = 4))

loo_2NLL <- brms::loo(mod_list[[6]], 
                      reloo = TRUE,
                      reloo_args = list(cores = 4))

 
 # loo_list <- list(loo_1pl,
 #                  loo_2pl,
 #                  loo_1CLL,
 #                  loo_2CLL,
 #                  loo_1NLL,
 #                  loo_2NLL)
 # 
 # saveRDS(loo_list, "loo_list.RDS")


loo_list <- readRDS("loo_list.RDS")


elpd_mat <- cbind(loo_list[[1]][["pointwise"]][,1],
                  loo_list[[2]][["pointwise"]][,1],
                  loo_list[[3]][["pointwise"]][,1],
                  loo_list[[4]][["pointwise"]][,1],
                  loo_list[[5]][["pointwise"]][,1],
                  loo_list[[6]][["pointwise"]][,1])


colnames(elpd_mat) <- c("1PL",
                        "2PL",
                        "1CLL",
                        "2CLL",
                        "1NLL",
                        "2NLL")




source("Functions.R")



item_elpd_list <- list()

for(j in 1:length(unique(dat_long$item))){
  
  item_elpd_list[[j]] <- elpd_mat[which(dat_long$item == unique(dat_long$item)[j]),]
}

names(item_elpd_list) <- unique(dat_long$item)


# calculate weights

bma_weights <- lapply(item_elpd_list, bma_weight, boot_n = 2000)

bma_matrix <-cbind(round(bind_rows(bma_weights), 3))

stack_weights <- lapply(item_elpd_list, stacking_weights) 

stack_matrix <- round(bind_rows(stack_weights), 3)
names(stack_matrix) <- names(bma_matrix)



weights_full_bma <-  c(004, .117, .136, .474, .000, .269)
weights_full_stack <- c(.000, .000,  .509, .007, .000, .485)

## do kernel smoothing

evalpoints <- qnorm(seq(0,1, by = .01))[2:100]

ksmod <- KernSmoothIRT::ksIRT(responses = blot, key = 1, format = 1, 
                              evalpoints = evalpoints)
idx <- 1:9 * 10

ks_probs <- ksmod$OCC[1:35 * 2 - 1, idx + 3]

# get number of items

n_items <- length(unique(dat_long$item))

# get number of people

n_person <- length(unique(dat_long$id))

# Get total number of MCMC draws

n_draws <- (iter - warmup)*chains

# specify number of quantiles (1/quantiles)

quantiles <- .1

# id to track replication number

# id <- rep(i, n_items)


# Calculate empirical quantile probabilities for all models/conditions

quant_1pl_emp <- Quantile_prob(data.frame(mod_list[[1]]),
                               n_items = n_items,
                               n_person = n_person,
                               quantiles = quantiles,
                               method = "empirical",
                               irf_function = "4PL")

idx <- rep(1:35, 4000) ## I think that this is correct
quant_1pl_emp_mean <- apply(quant_1pl_emp, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]


quant_2pl_emp <- Quantile_prob(data.frame(mod_list[[2]]),
                               n_items = n_items,
                               n_person = n_person,
                               quantiles = quantiles,
                               method = "empirical",
                               irf_function = "4PL")

quant_2pl_emp_mean <- apply(quant_2pl_emp, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]

quant_CLL1_emp <- Quantile_prob(data.frame(mod_list[[3]]),
                                n_items = n_items,
                                n_person = n_person,
                                quantiles = quantiles,
                                method = "empirical",
                                irf_function = "2CLL")

quant_CLL1_emp_mean <- apply(quant_CLL1_emp, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]

quant_CLL2_emp <- Quantile_prob(data.frame(mod_list[[4]]),
                                n_items = n_items,
                                n_person = n_person,
                                quantiles = quantiles,
                                method = "empirical",
                                irf_function = "2CLL")

quant_CLL2_emp_mean <- apply(quant_CLL2_emp, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]


quant_NLL1_emp <- Quantile_prob(data.frame(mod_list[[5]]),
                                n_items = n_items,
                                n_person = n_person,
                                quantiles = quantiles,
                                method = "empirical",
                                irf_function = "2NLL")

quant_NLL1_emp_mean <- apply(quant_NLL1_emp, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]


quant_NLL2_emp <- Quantile_prob(data.frame(mod_list[[6]]),
                                n_items = n_items,
                                n_person = n_person,
                                quantiles = quantiles,
                                method = "empirical",
                                irf_function = "2NLL")

quant_NLL2_emp_mean <- apply(quant_NLL2_emp, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]


# Calculate theoretical quantile probabilities for all models/conditions

quant_1pl_theo <- Quantile_prob(data.frame(mod_list[[1]]),
                                n_items = n_items,
                                n_person = n_person,
                                quantiles = quantiles,
                                method = "theoretical",
                                irf_function = "4PL")

quant_1pl_theo_mean <- apply(quant_1pl_theo, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]


quant_2pl_theo <- Quantile_prob(data.frame(mod_list[[2]]),
                                n_items = n_items,
                                n_person = n_person,
                                quantiles = quantiles,
                                method = "theoretical",
                                irf_function = "4PL")
quant_2pl_theo_mean <- apply(quant_2pl_theo, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]


quant_CLL1_theo <- Quantile_prob(data.frame(mod_list[[3]]),
                                 n_items = n_items,
                                 n_person = n_person,
                                 quantiles = quantiles,
                                 method = "theoretical",
                                 irf_function = "2CLL")

quant_CLL1_theo_mean <- apply(quant_CLL1_theo, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]

quant_CLL2_theo <- Quantile_prob(data.frame(mod_list[[4]]),
                                 n_items = n_items,
                                 n_person = n_person,
                                 quantiles = quantiles,
                                 method = "theoretical",
                                 irf_function = "2CLL")

quant_CLL2_theo_mean <- apply(quant_CLL2_theo, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]


quant_NLL1_theo <- Quantile_prob(data.frame(mod_list[[5]]),
                                 n_items = n_items,
                                 n_person = n_person,
                                 quantiles = quantiles,
                                 method = "theoretical",
                                 irf_function = "2NLL")

quant_NLL1_theo_mean <- apply(quant_NLL1_theo, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]



quant_NLL2_theo <- Quantile_prob(data.frame(mod_list[[6]]),
                                 n_items = n_items,
                                 n_person = n_person,
                                 quantiles = quantiles,
                                 method = "theoretical",
                                 irf_function = "2NLL")

quant_NLL2_theo_mean <- apply(quant_NLL2_theo, 2, function(x) tapply(x, idx, mean))[, -c(1, 11)]



# create lists for sampling for empirical method

mod_list_full_emp <- list(quant_1pl_emp,
                          quant_2pl_emp,
                          quant_CLL1_emp,
                          quant_CLL2_emp,
                          quant_NLL1_emp,
                          quant_NLL2_emp)

# create lists for sampling for theoretical method


mod_list_full_theo <- list(quant_1pl_theo,
                           quant_2pl_theo,
                           quant_CLL1_theo,
                           quant_CLL2_theo,
                           quant_NLL1_theo,
                           quant_NLL2_theo)

set.seed(329053239)

n_draws <- 6000

prob_averaged_mod_stack_emp <- Quantile_sample(mod_list_full_emp,
                                               n_items = n_items,
                                               n_draws = n_draws,
                                               weights = weights_full_stack)
prob_averaged_mod_stack_emp_mean <- t(sapply(prob_averaged_mod_stack_emp, colMeans, na.rm = TRUE))[, -c(1, 11)]

prob_averaged_mod_bma_emp <- Quantile_sample(mod_list_full_emp,
                                             n_items = n_items,
                                             n_draws = n_draws,
                                             weights = weights_full_bma)

prob_averaged_mod_bma_emp_mean <- t(sapply(prob_averaged_mod_bma_emp, colMeans, na.rm = TRUE))[, -c(1, 11)]


### Theoretical


prob_averaged_mod_stack_theo <- Quantile_sample(mod_list_full_theo,
                                                n_items = n_items,
                                                n_draws = n_draws,
                                                weights = weights_full_stack)

prob_averaged_mod_stack_theo_mean <- t(sapply(prob_averaged_mod_stack_theo, colMeans, na.rm = TRUE))[, -c(1, 11)]



prob_averaged_mod_bma_theo <- Quantile_sample(mod_list_full_theo,
                                              n_items = n_items,
                                              n_draws = n_draws,
                                              weights = weights_full_bma)

prob_averaged_mod_bma_theo_mean <- t(sapply(prob_averaged_mod_bma_theo, colMeans, na.rm = TRUE))[, -c(1, 11)]

## Item averaging


### empirical


prob_averaged_item_stack_emp <- Quantile_item_sample(mod_list_full_emp,
                                                     n_items = n_items,
                                                     n_draws = n_draws,
                                                     weights = stack_matrix)


prob_averaged_item_stack_emp_mean <- t(sapply(prob_averaged_item_stack_emp, colMeans, na.rm = TRUE))[, -c(1, 11)]


prob_averaged_item_bma_emp <- Quantile_item_sample(mod_list_full_emp,
                                                   n_items = n_items,
                                                   n_draws = n_draws,
                                                   weights = bma_matrix)

prob_averaged_item_bma_emp_mean <- t(sapply(prob_averaged_item_bma_emp, colMeans, na.rm = TRUE))[, -c(1, 11)]


### Theoretical


prob_averaged_item_stack_theo <- Quantile_item_sample(mod_list_full_theo,
                                                      n_items = n_items,
                                                      n_draws = n_draws,
                                                      weights = stack_matrix)

prob_averaged_item_stack_theo_mean <- t(sapply(prob_averaged_item_stack_theo, colMeans, na.rm = TRUE))[, -c(1, 11)]



prob_averaged_item_bma_theo <- Quantile_item_sample(mod_list_full_theo,
                                                    n_items = n_items,
                                                    n_draws = n_draws,
                                                    weights = bma_matrix)

prob_averaged_item_bma_theo_mean <- t(sapply(prob_averaged_item_bma_theo, colMeans, na.rm = TRUE))[, -c(1, 11)]



## Model selection


### empirical


prob_selection_stack_emp <- Quantile_sample(mod_list_full_emp,
                                            n_items = n_items,
                                            n_draws = n_draws,
                                            weights = ifelse(weights_full_stack < weights_full_stack[which(weights_full_stack == max(weights_full_stack))], 0, 1))

prob_selection_stack_emp_mean <- t(sapply(prob_selection_stack_emp, colMeans, na.rm = TRUE))[, -c(1, 11)]



prob_selection_bma_emp <- Quantile_sample(mod_list_full_emp,
                                          n_items = n_items,
                                          n_draws = n_draws,
                                          weights = ifelse(weights_full_bma < weights_full_bma[which(weights_full_bma == max(weights_full_bma))], 0, 1))

prob_selection_bma_emp_mean <- t(sapply(prob_selection_bma_emp, colMeans, na.rm = TRUE))[, -c(1, 11)]


### Theoretical


prob_selection_stack_theo <- Quantile_sample(mod_list_full_theo,
                                             n_items = n_items,
                                             n_draws = n_draws,
                                             weights = ifelse(weights_full_stack < weights_full_stack[which(weights_full_stack == max(weights_full_stack))], 0, 1))

prob_selection_stack_theo_mean <- t(sapply(prob_selection_stack_theo, colMeans, na.rm = TRUE))[, -c(1, 11)]


prob_selection_bma_theo <- Quantile_sample(mod_list_full_theo,
                                           n_items = n_items,
                                           n_draws = n_draws,
                                           weights = ifelse(weights_full_bma < weights_full_bma[which(weights_full_bma == max(weights_full_bma))], 0, 1))

prob_selection_bma_theo_mean <- t(sapply(prob_selection_bma_theo, colMeans, na.rm = TRUE))[, -c(1, 11)]



save.image("RealDataRes.RData")

#### Analyze Results Here ####
library(brms)
library(tidyverse)

load("RealDataRes.RData")

max(rhat(fit_1pl), na.rm = TRUE)
max(rhat(fit_2pl), na.rm = TRUE)
max(rhat(fit_CLL_1), na.rm = TRUE)
max(rhat(fit_CLL_2), na.rm = TRUE)
max(rhat(fit_NLL_1), na.rm = TRUE)
max(rhat(fit_NLL_2), na.rm = TRUE)

min(neff_ratio(fit_1pl) * 4000, na.rm = TRUE)
min(neff_ratio(fit_2pl) * 4000, na.rm = TRUE)
min(neff_ratio(fit_CLL_1) * 4000, na.rm = TRUE)
min(neff_ratio(fit_CLL_2) * 4000, na.rm = TRUE)
min(neff_ratio(fit_NLL_1) * 4000, na.rm = TRUE)
min(neff_ratio(fit_NLL_2) * 4000, na.rm = TRUE)


dat_long[which(loo_1pl$diagnostics$pareto_k > .7), ]
dat_long[which(loo_2pl$diagnostics$pareto_k > .7), ]
dat_long[which(loo_1CLL$diagnostics$pareto_k > .7), ]
dat_long[which(loo_2CLL$diagnostics$pareto_k > .7), ]
dat_long[which(loo_1NLL$diagnostics$pareto_k > .7), ]
dat_long[which(loo_2NLL$diagnostics$pareto_k > .7), ]

loo_compare(loo_1pl, loo_2pl, loo_1CLL, loo_2CLL, loo_1NLL, loo_2NLL)


apply(bma_matrix, 1, which.max) %>% table
4 / 35 # 1PL or 2PL
16 / 35 # CLL or 2CLL
15 / 35 # NLL or 2NLL

apply(stack_matrix, 1, which.max) %>% table
7 / 35 # 1PL or 2PL
16 / 35 # CLL or 2CLL
12 / 35 # NLL or 2NLL

table(apply(bma_matrix, 1, which.max), apply(stack_matrix, 1, which.max))


# library(RColorBrewer)
# cols <- brewer.pal(n = 6, name = "Paired")

cols <- c("#999999", #1
          "#E69F00", #2
          "#56B4E9", #3
          "#009E73", #4
          "#F0E442", #5
          "#0072B2", #6
          "#D55E00", #7
          "#CC79A7", #8
          "#271DEB", #9
          "black")   #10



source("C:/Users/fabio/Dropbox/Work/Useful R scripts/GGplot_defaults.R")


plot_emp_weights <- bma_matrix %>% mutate(item = 1:n(), type = "BMA+ Weights", across(`1PL`:`2NLL`, as.numeric)) %>%
  bind_rows(stack_matrix %>% as.data.frame %>% mutate(item = 1:n(), type = "Stacking Weights",
                                                      across(`1PL`:`2NLL`, as.numeric),
                                                      score = -1 * `1CLL` - .999 * `2CLL` + .01 * `1NLL` + .0099 * `2NLL`)) %>%
  pivot_longer(-c(item, type, score), names_to = "model", values_to = "weight") %>%
  group_by(item, model) %>% mutate(score = max(score, na.rm = TRUE)) %>% ungroup %>%
  mutate(model = factor(model, levels = c("1CLL", "2CLL", "1PL", "2PL", "1NLL", "2NLL")), 
         weight = as.numeric(weight),
         item = fct_reorder(factor(item), score),
         model) %>%
  ggplot(aes(item, weight, fill = model)) + geom_col() + coord_flip() + 
  scale_fill_manual(values = cols[c(2, 7, 1, 10, 3, 9)]) + facet_wrap(~type) + 
  labs(x = "item number", fill = "") +
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
         axis.line=element_blank(),
         legend.background = element_rect(fill='transparent'),
         axis.text = element_text(family = "Times New Roman", 
                                  colour = "black")) +
   guides(fill = guide_legend(nrow = 1, 
                              byrow = TRUE,
                              reverse = TRUE))

print(plot_emp_weights)



ggsave(filename = "plot_emp_weights.png", 
       plot =  plot_emp_weights, 
       bg = "transparent",
       width = 8, 
       height = 7, 
       dpi = 300)



# items 4, 5 have some pretty big differences
i <- 12 # pick an item

plot(1:9, prob_averaged_mod_stack_emp_mean[i, ] - ks_probs[i, ], type = 'l', ylim = c(-.5, .5))
lines(1:9, prob_averaged_mod_stack_theo_mean[i, ] - ks_probs[i, ], type = 'l')
lines(1:9, prob_averaged_item_stack_emp_mean[i, ] - ks_probs[i, ], type = 'l')
lines(1:9, prob_averaged_item_stack_theo_mean[i, ] - ks_probs[i, ], type = 'l')

lines(1:9, prob_selection_stack_emp_mean[i, ] - ks_probs[i, ], type = 'l')
lines(1:9, prob_selection_stack_theo_mean[i, ] - ks_probs[i, ], type = 'l')
abline(h = 0, lty = 3)

plot(1:9, prob_averaged_mod_bma_emp_mean[i, ] - ks_probs[i, ], type = 'l', ylim = c(-.2, .2))
lines(1:9, prob_averaged_mod_bma_theo_mean[i, ] - ks_probs[i, ], type = 'l')
lines(1:9, prob_averaged_item_bma_emp_mean[i, ] - ks_probs[i, ], type = 'l')
lines(1:9, prob_averaged_item_bma_theo_mean[i, ] - ks_probs[i, ], type = 'l')
lines(1:9, prob_selection_bma_emp_mean[i, ] - ks_probs[i, ], type = 'l')
lines(1:9, prob_selection_bma_theo_mean[i, ] - ks_probs[i, ], type = 'l')
abline(h = 0, lty = 3)


plot(1:9, prob_averaged_mod_stack_emp_mean[i, ], ylim = c(0, 1), type = 'l')
lines(1:9, prob_averaged_item_stack_emp_mean[i, ], col = 2)
lines(1:9, prob_selection_stack_emp_mean[i, ], col = 3)

plot(1:9, prob_averaged_mod_bma_emp_mean[i, ], ylim = c(0, 1), type = 'l')
lines(1:9, prob_averaged_item_bma_emp_mean[i, ], col = 2)
lines(1:9, prob_selection_bma_emp_mean[i, ], col = 3)

plot(1:9, prob_averaged_mod_stack_theo_mean[i, ], ylim = c(0, 1), type = 'l')
lines(1:9, prob_averaged_item_stack_theo_mean[i, ], col = 2)
lines(1:9, prob_selection_stack_theo_mean[i, ], col = 3)

plot(1:9, prob_averaged_mod_bma_theo_mean[i, ], ylim = c(0, 1), type = 'l')
lines(1:9, prob_averaged_item_bma_theo_mean[i, ], col = 2)
lines(1:9, prob_selection_bma_theo_mean[i, ], col = 3)


# another option is to compare the expected response functions

plot(1:9, colSums(prob_averaged_mod_stack_emp_mean), ylim = c(0, 35), type = "l", pch = 16)
lines(1:9, colSums(prob_averaged_mod_stack_theo_mean), pch = 16)
lines(1:9, colSums(prob_averaged_item_stack_emp_mean), pch = 16)
lines(1:9, colSums(prob_averaged_item_stack_theo_mean), pch = 16)
lines(1:9, colSums(prob_selection_stack_emp_mean), pch = 16)
lines(1:9, colSums(prob_selection_stack_theo_mean), pch = 16)


# what's the story? maybe look at 3 prototype items with 3 different weight patterns
# look at the different IRFs and what averaging, selection, and item-level section does

# items 5, 14, maybe 21, 24


i <- 35

plot(1:9, quant_1pl_emp_mean[i, ], type = 'l', ylim = c(0, 1))
lines(1:9, quant_2pl_emp_mean[i, ], type = 'l', lty = 2)

lines(1:9, quant_CLL1_emp_mean[i, ], type = 'l', col = 2)
lines(1:9, quant_CLL2_emp_mean[i, ], type = 'l', col = 2, lty = 2)

lines(1:9, quant_NLL1_emp_mean[i, ], type = 'l', col = 4)
lines(1:9, quant_NLL2_emp_mean[i, ], type = 'l', col = 4, lty = 2)

i <- 5


plot(qnorm(seq(.1, .9, by = .1)), quant_1pl_theo_mean[i, ] - prob_averaged_mod_bma_theo_mean[i, ], type = 'l', 
     ylim = c(-.1, .1))
lines(qnorm(seq(.1, .9, by = .1)), quant_2pl_theo_mean[i, ] - prob_averaged_mod_bma_theo_mean[i, ], type = 'l', lty = 2)

lines(qnorm(seq(.1, .9, by = .1)), quant_CLL1_theo_mean[i, ] - prob_averaged_mod_bma_theo_mean[i, ], type = 'l', col = 2)
lines(qnorm(seq(.1, .9, by = .1)), quant_CLL2_theo_mean[i, ] - prob_averaged_mod_bma_theo_mean[i, ], type = 'l', col = 2, lty = 2)

lines(qnorm(seq(.1, .9, by = .1)), quant_NLL1_theo_mean[i, ] - prob_averaged_mod_bma_theo_mean[i, ], type = 'l', col = 4)
lines(qnorm(seq(.1, .9, by = .1)), quant_NLL2_theo_mean[i, ] - prob_averaged_mod_bma_theo_mean[i, ], type = 'l', col = 4, lty = 2)

abline(h = 0, lty = 3)


# for item 14, stacking weights are equivalent to MS on 1NLL
# model selection with BMA+ weights chooses the CLL
# cols <- RColorBrewer::brewer.pal(5, "Dark2")

cols <- c("#999999", #1
          "#E69F00", #2
          "#56B4E9", #3
          "#009E73", #4
          "#F0E442", #5
          "#0072B2", #6
          "#D55E00", #7
          "#CC79A7", #8
          "#271DEB", #9
          "black")   #10


png(filename = "Real Data Predictions.png", width = 9, height = 6, 
    units = "in", res = 300)

i <- 14

par(mfrow = c(1, 2),
    bg = NA,
    family = "Times New Roman")

plot(qnorm(seq(.1, .9, by = .1)), quant_1pl_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], type = 'l',
     ylim = c(-.2, .2), xlab = expression(theta), ylab = "Probability Difference from 2CLL",
     main = "Individual Model Predictions", bty = 'l', lwd = 2, col = cols[1])
lines(qnorm(seq(.1, .9, by = .1)), quant_2pl_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[2])
lines(qnorm(seq(.1, .9, by = .1)), quant_CLL1_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[3])
lines(qnorm(seq(.1, .9, by = .1)), quant_NLL1_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[4])
lines(qnorm(seq(.1, .9, by = .1)), quant_NLL2_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[6])
abline(h = 0, lty = 3, lwd = 1.5)
legend("bottomright", legend = c("1PL", "2PL", "1CLL", "1NLL", "2NLL"),
       col = cols[c(1, 2, 3, 4, 6)], lty = 1, bty = "n", lwd = 2)

plot(qnorm(seq(.1, .9, by = .1)), prob_averaged_item_bma_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], type = 'l',
     ylim = c(-.2, .2), xlab = expression(theta), ylab = "Probability Difference from 2CLL",
     main = "Averaged Predictions", bty = 'l', lwd = 2, col = cols[7])
lines(qnorm(seq(.1, .9, by = .1)), prob_averaged_item_stack_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[8])
lines(qnorm(seq(.1, .9, by = .1)), prob_averaged_mod_bma_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[9])
lines(qnorm(seq(.1, .9, by = .1)), prob_averaged_mod_stack_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[10])
abline(h = 0, lty = 3, lwd = 1.5)
legend("bottomright", legend = c("Item-Level Average, BMA+", "Item-Level Average, Stacking",
                                 "Test-Level Average, BMA+", "Test-Level Average, Stacking"),
       bty = "n", lty = 1, col = cols[c(7, 8, 9, 10)], lwd = 2)

par(mfrow = c(1, 1),
    bg = NA)

dev.off()

cols <- c("#999999", #1
          "#E69F00", #2
          "#56B4E9", #3
          "#009E73", #4
          "#F0E442", #5
          "#0072B2", #6
          "#D55E00", #7
          "#CC79A7", #8
          "#271DEB", #9
          "black")   #10


png(filename = "Real Data Predictions All Items.png", width = 24, height = 24, 
    units = "in", res = 300)

par(mfrow = c(7, 5),
    bg = NA,
    family = "Times New Roman")

for(i in 1:35){
  plot(qnorm(seq(.1, .9, by = .1)), quant_1pl_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], type = 'l',
       ylim = c(-.2, .2), xlab = expression(theta), ylab = "Probability Difference from CLL2",
       main = paste("Item", i), bty = 'l', lwd = 2, col = cols[1])
  lines(qnorm(seq(.1, .9, by = .1)), quant_2pl_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[2])
  lines(qnorm(seq(.1, .9, by = .1)), quant_CLL1_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[3])
  lines(qnorm(seq(.1, .9, by = .1)), quant_NLL1_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[4])
  lines(qnorm(seq(.1, .9, by = .1)), quant_NLL2_theo_mean[i, ] - quant_CLL2_theo_mean[i, ], lwd = 2, col = cols[6])
  abline(h = 0, lty = 3, lwd = 1.5)
  if(i == 1) legend("bottomright", legend = c("1PL", "2PL", "1CLL", "1NLL", "2NLL"),
                    col = cols[c(1, 2, 3, 4, 6)], lty = 1, bty = "n", lwd = 2)
}

dev.off()



# first thing, we need a good sense of how different these curves are in the first place
# for each item and quantile, what is the SD of p across the 6 models?

range_emp <- t(sapply(1:35, function(i){
  apply(rbind(quant_1pl_emp_mean[i, ],
              quant_2pl_emp_mean[i, ],
              quant_CLL1_emp_mean[i, ],
              quant_CLL2_emp_mean[i, ],
              quant_NLL1_emp_mean[i, ],
              quant_NLL2_emp_mean[i, ]), 2, function(x) diff(range(x)))
}))


range_theo <- t(sapply(1:35, function(i){
  apply(rbind(quant_1pl_theo_mean[i, ],
              quant_2pl_theo_mean[i, ],
              quant_CLL1_theo_mean[i, ],
              quant_CLL2_theo_mean[i, ],
              quant_NLL1_theo_mean[i, ],
              quant_NLL2_theo_mean[i, ]), 2, function(x) diff(range(x)))
}))

mean(range_emp)
mean(range_theo)

mean(range_emp < range_theo)
colMeans(range_emp < range_theo)
rowMeans(range_emp < range_theo)


SDs_emp <- t(sapply(1:35, function(i){
  apply(rbind(quant_1pl_emp_mean[i, ],
              quant_2pl_emp_mean[i, ],
              quant_CLL1_emp_mean[i, ],
              quant_CLL2_emp_mean[i, ],
              quant_NLL1_emp_mean[i, ],
              quant_NLL2_emp_mean[i, ]), 2, sd)
}))


SDs_theo <- t(sapply(1:35, function(i){
  apply(rbind(quant_1pl_theo_mean[i, ],
              quant_2pl_theo_mean[i, ],
              quant_CLL1_theo_mean[i, ],
              quant_CLL2_theo_mean[i, ],
              quant_NLL1_theo_mean[i, ],
              quant_NLL2_theo_mean[i, ]), 2, sd)
}))

round(SDs_emp, 3)
round(SDs_theo, 3)

mean(SDs_emp < SDs_theo)
colMeans(SDs_emp < SDs_theo)
rowMeans(SDs_emp < SDs_theo) # for every item except 32 and 4, empirical quantile preds are closer together than theoretical preds


# posterior theta means for the 6 models
theta_1pl <- posterior_summary(fit_1pl, variable = "r_id__theta")[, 1]
theta_2pl <- posterior_summary(fit_2pl, variable = "r_id__theta")[, 1]
theta_1cll <- posterior_summary(fit_CLL_1, variable = "r_id__theta")[, 1]
theta_2cll <- posterior_summary(fit_CLL_2, variable = "r_id__theta")[, 1]
theta_1nll <- posterior_summary(fit_NLL_1, variable = "r_id__theta")[, 1]
theta_2nll <- posterior_summary(fit_NLL_2, variable = "r_id__theta")[, 1]

plot(density(theta_1pl))
points(density(theta_2pl), type = 'l')
points(density(theta_1cll), type = 'l', col = 2)
points(density(theta_2cll), type = 'l', col = 2)
points(density(theta_1nll), type = 'l', col = 4)
points(density(theta_2nll), type = 'l', col = 4)
curve(dnorm(x), add = TRUE, lty = 2)

