# libraries --------------------------------------------------------------------
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE
library(tidyverse) # for data manipulations
library(rstan)
library(fastDummies)
library(psych) 
library(cowplot)
library(ggdist)

set.seed(16)
# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("gamma.stan")

 # prepare the data
data <- read.csv("videogame_sales.csv")

plt_data <- as.data.frame(data)

give.n <- function(x){
  return(c(y = -0.5, label = length(x))) 
}

p <- ggplot(plt_data, aes(x = Platform, y=Sales, color = Genre)) +
  geom_boxplot(outlier.size = 6) + 
  stat_summary(fun.data = give.n, geom = "text", size = 10, fun.y = median, position = position_dodge(width=0.75)) + 
  theme(legend.position = c(.9, .9), text = element_text(size = 25)) +
  ylim(-0.5, 8)
p <- p + scale_color_manual(values=c("#E94F37", "#3F88C5")) 



data$Platform <- factor(data$Platform)
data$Genre <- factor(data$Genre)
contrasts(data$Platform) <- contr.treatment(n_distinct(data$Platform))
contrasts(data$Genre) <- contr.treatment(n_distinct(data$Genre))
X <- model.matrix(~ Platform, data)
X_genre <- model.matrix(~ Genre, data)
Y <- data$Sales


stan_data_platform <- list(n = nrow(X), m = ncol(X), x = X, y = Y)
stan_data_genre <- list(n = nrow(X_genre), m = ncol(X_genre), x = X_genre, y = Y)



pairs.panels(data,
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(X_genre,
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# fit
fit <- model$sample(
  data = stan_data_platform,
  iter_warmup =2000,
  iter_sampling = 3000
)

# fit
fit2 <- model$sample(
  data = stan_data_genre,
  iter_warmup =2000,
  iter_sampling = 3000
)



# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws("b"))

mcmc_trace(fit2$draws("b"))

# summary
fit$summary("b")

fit2$summary("b")



# platform
df <- as_draws_df(fit$draws("b"))
df %>% select(-.chain, -.iteration, -.draw)

PC <- exp(df$`b[1]`)
PS <- exp(df$`b[1]` + df$`b[2]`)
XBOX <- exp(df$`b[1]` + df$`b[3]`)

platform_data <- bind_cols(PC, PS, XBOX)

colnames(platform_data) <- c('PC', 'PS', 'XBOX')

platform_data <- platform_data %>% gather(Platform, Value)

ggplot(data = platform_data, aes(x = Value, y = Platform)) +
  stat_eye(fill = "skyblue", alpha = 0.75) +   theme(text = element_text(size = 25))

ggplot(data = platform_data, aes(x = Value, color = Platform)) +
  geom_density() +   theme(text = element_text(size = 25))


library(dplyr)
cdat <- platform_data %>%  group_by(Platform) %>%  summarise(rating.mean = mean(Value),
                                                             sem = sd(Value),
                                                             ci.low = mean(Value) - 1.96*sem,
                                                             ci.upp = mean(Value) + 1.96*sem)

cdat.dens <- ggplot_build(ggplot(platform_data, aes(x=Value, colour=Platform)) + geom_density())$data[[1]] %>%
  mutate(Platform = ifelse(group == 1, "PC", ifelse(group==2, 'PS', 'XBOX'))) %>%
  left_join(cdat) %>%
  select(y, x, Platform, rating.mean, sem, ci.low, ci.upp) %>%
  group_by(Platform) %>%
  mutate(dens.mean = approx(x, y, xout = rating.mean)[[2]],
         dens.cilow = approx(x, y, xout = ci.low)[[2]],
         dens.ciupp = approx(x, y, xout = ci.upp)[[2]]) %>%
  select(-y, -x) %>%
  slice(1)



ribbon <- ggplot_build(ggplot(platform_data, aes(x=Value, colour=Platform)) + geom_density())$data[[1]] %>%
  mutate(Platform = ifelse(group == 1, "PC", ifelse(group==2, 'PS', 'XBOX'))) %>%
  left_join(cdat.dens) %>%
  group_by(Platform) %>%
  filter(x >= ci.low & x <= ci.upp) %>%
  select(Platform, x, y)

ribbon <- rbind(data.frame(Platform = c("PC", "PS", "XBOX"), x = c(0.552, 0.581, 0.622), y = c(0, 0, 0)), 
                as.data.frame(ribbon), 
                data.frame(Platform = c("PC", "PS", "XBOX"), x = c(0.816, 0.924, 1.04), y = c(0, 0, 0)))



ggplot() +
  geom_polygon(data = ribbon, aes(x = x, y = y, fill = Platform), alpha = .2) +
  geom_density(data = platform_data, aes(x = Value, colour = Platform)) +
  geom_segment(data = cdat.dens, aes(x = rating.mean, xend = rating.mean, y = 0, yend = dens.mean, colour = Platform),
               linetype = "dashed", size = 1) +
  geom_segment(data = cdat.dens, aes(x = ci.low, xend = ci.low, y = 0, yend = dens.cilow, colour = Platform),
               linetype = "dotted", size = 1) +
  geom_segment(data = cdat.dens, aes(x = ci.upp, xend = ci.upp, y = 0, yend = dens.ciupp, colour = Platform),
               linetype = "dotted", size = 1) + 
  theme(legend.position = c(.9, .9)) + xlab("value") + ylab("density") + 
  theme(text = element_text(size = 25)) + 
  scale_fill_manual(values=c("#0A9396", "#EE9B00", "#001219"))



# genre
df2 <- as_draws_df(fit2$draws("b"))
df2 %>% select(-.chain, -.iteration, -.draw)

RolePlaying <- exp(df2$`b[1]`)
Shooter <- exp(df$`b[1]` + df$`b[2]`)


genre_data <- bind_cols(RolePlaying, Shooter)

colnames(genre_data) <- c('RolePlaying', 'Shooter')

genre_data <- genre_data %>% gather(Genre, Value)

ggplot(data = genre_data, aes(x = Value, y = Genre)) +
  stat_eye(fill = "skyblue", alpha = 0.75) +   theme(text = element_text(size = 25))

ggplot(data = genre_data, aes(x = Value, color = Genre)) +
  geom_density() +   theme(text = element_text(size = 25))






cdat <- genre_data %>%  group_by(Genre) %>%  summarise(rating.mean = mean(Value),
                                                       sem = sd(Value),
                                                       ci.low = mean(Value) - 1.96*sem,
                                                       ci.upp = mean(Value) + 1.96*sem)

cdat.dens <- ggplot_build(ggplot(genre_data, aes(x=Value, colour=Genre)) + geom_density())$data[[1]] %>%
  mutate(Genre = ifelse(group == 1, "RolePlaying", "Shooter")) %>%
  left_join(cdat) %>%
  select(y, x, Genre, rating.mean, sem, ci.low, ci.upp) %>%
  group_by(Genre) %>%
  mutate(dens.mean = approx(x, y, xout = rating.mean)[[2]],
         dens.cilow = approx(x, y, xout = ci.low)[[2]],
         dens.ciupp = approx(x, y, xout = ci.upp)[[2]]) %>%
  select(-y, -x) %>%
  slice(1)



ribbon <- ggplot_build(ggplot(genre_data, aes(x=Value, colour=Genre)) + geom_density())$data[[1]] %>%
  mutate(Genre = ifelse(group == 1, "RolePlaying", "Shooter")) %>%
  left_join(cdat.dens) %>%
  group_by(Genre) %>%
  filter(x >= ci.low & x <= ci.upp) %>%
  select(Genre, x, y)

ribbon <- rbind(data.frame(Genre = c("RolePlaying", "Shooter"), x = c(0.549, 0.581), y = c(0, 0)), 
                as.data.frame(ribbon), 
                data.frame(Genre = c("RolePlaying", "Shooter"), x = c(0.826 , 0.924), y = c(0, 0)))



ggplot() +
  geom_polygon(data = ribbon, aes(x = x, y = y, fill = Genre), alpha = .2) +
  geom_density(data = genre_data, aes(x = Value, colour = Genre)) +
  geom_segment(data = cdat.dens, aes(x = rating.mean, xend = rating.mean, y = 0, yend = dens.mean, colour = Genre),
               linetype = "dashed", size = 1) +
  geom_segment(data = cdat.dens, aes(x = ci.low, xend = ci.low, y = 0, yend = dens.cilow, colour = Genre),
               linetype = "dotted", size = 1) +
  geom_segment(data = cdat.dens, aes(x = ci.upp, xend = ci.upp, y = 0, yend = dens.ciupp, colour = Genre),
               linetype = "dotted", size = 1) + 
  theme(legend.position = c(.9, .9)) + xlab("value") + ylab("density") + 
  theme(text = element_text(size = 25)) + 
  scale_fill_manual(values=c("#E94F37", "#3F88C5"))





# probability

df_platform <- data.frame(PC = exp(df$`b[1]`),
                      PS = exp(df$`b[1]` + df$`b[2]`),
                      XBOX = exp(df$`b[1]` + df$`b[3]`))
df_genre <- data.frame(RolePlaying = exp(df2$`b[1]`),
                      Shooter = exp(df$`b[1]` + df$`b[2]`))

# P(XBOX_mu > PS_mu)
mcse(df_platform$XBOX > df_platform$PS)
mcse(df_platform$XBOX > df_platform$PC)
mcse(df_platform$PS > df_platform$PC)

mcse(df_genre$Shooter  > df_genre$RolePlaying & df_platform$XBOX > df_platform$PS)

mcse(df_genre$Shooter <= df_genre$RolePlaying & df_platform$XBOX > df_platform$PC & df_platform$XBOX > df_platform$PS)


mcse(df_genre$Shooter > df_genre$RolePlaying)


mcse(df_platform$XBOX)
mcse(df_platform$PS)
mcse(df_platform$PC)


mcse(df_genre$Shooter)
mcse(df_genre$RolePlaying)
