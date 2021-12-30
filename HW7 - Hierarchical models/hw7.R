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
library(loo) # for WAIC and LOOIC calculations
library(HDInterval)
library(dplyr)
require(plyr)


basketball_shots <- read.csv("basketball_shots.csv", sep = ';')
# loading the model ------------------------------------------------------------
model <- cmdstan_model("basket.stan")

basketball_shots <- basketball_shots[basketball_shots$SpecialRim == 0,]

basketball_shots[basketball_shots$PlayerID == 3,]

basketball_shots[basketball_shots$PlayerID == 20,]



# discarding when special_rim == 1

basketball_shots <- basketball_shots[order(basketball_shots$PlayerID),]

Position <- unique(basketball_shots$Position)

Position_new <- seq(length(Position))
data_pos <- data.frame(Position, Position_new)
basketball_shots <- merge(x = basketball_shots, 
                           y = data_pos, by = "Position", all = TRUE)

c <- basketball_shots[c('PlayerID', 'Position_new')]
c <- c[order(c$PlayerID),]



n_t <- length(basketball_shots$PlayerID)
n_p <- length(unique(basketball_shots$PlayerID))
n_g <- length(unique(basketball_shots$Position))
y <- basketball_shots$Made
throw_player <- basketball_shots$PlayerID
player_group <- unique(c)$Position_new


stan_data <- list(n_t=n_t,
                  n_p=n_p,
                  n_g=n_g,
                  y=y,
                  throw_player=throw_player,
                  player_group=player_group)

fit <- model$sample(data = stan_data,
                    #parallel_chains = 4,
                    seed = 1)

mcmc_trace(fit$draws())
fit$summary()

# prvo vprasanje:


mcse(fit$draws("theta_p[20]") > fit$draws("theta_p[3]"))

# drugo vprasanje:



center_players <- unique(basketball_shots[basketball_shots$Position == 'C',]$PlayerID)
forward_players <- unique(basketball_shots[basketball_shots$Position == 'F',]$PlayerID)
guard_players <- unique(basketball_shots[basketball_shots$Position == 'G',]$PlayerID)


# center and forward
get_probability <- function(players_pos1, players_pos2, n_iter) {
  
  win_ <- c()
  other_win_ <- c()
  tie_ <- c()
  
  for (i in 1:n_iter)
  {
    centr <- sample(players_pos1, 1)
    forw <- sample(players_pos2, 1)
    
    centr_param <- paste(paste("theta_p[", centr, sep=""), "]", sep="")
    forw_param <- paste(paste("theta_p[", forw, sep=""), "]", sep="")
    
    centr_prob <- sample(fit$draws(centr_param), 1)
    forw_prob <- sample(fit$draws(forw_param), 1)
    
    
    first_win <- centr_prob * (1-forw_prob)
    second_win <- (1-centr_prob) * forw_prob
    tie <- 1 - first_win - second_win
    
    win_ <- c(win_, first_win)
    other_win_ <- c(other_win_, second_win)
    tie_ <- c(tie_, tie)
  }
  return(list(win_, other_win_, tie_))
}

q1 <- get_probability(center_players, forward_players, 1000)

mcse(q1[[1]]) # first win
mcse(q1[[2]]) # second win
mcse(q1[[3]]) # tie


q2 <- get_probability(center_players, guard_players, 1000)

mcse(q2[[1]]) # first win
mcse(q2[[2]]) # second win
mcse(q2[[3]]) # tie

q3 <- get_probability(forward_players, guard_players, 1000)

mcse(q3[[1]]) # first win
mcse(q3[[2]]) # second win
mcse(q3[[3]]) # tie


##### 

basketball_shots_analysis <- basketball_shots[,c("PlayerID", "Made", "Position")]


player_avg <- aggregate(basketball_shots_analysis, list(basketball_shots_analysis$PlayerID), mean)
player_avg <- player_avg[, c("PlayerID", "Made")]
player_groups <- unique(basketball_shots_analysis[, c("PlayerID", "Position")])

new_tbl <- merge(x = player_groups, y = player_avg, by = "PlayerID")
names(new_tbl)[names(new_tbl) == "Position"] <- "Position2"


new_tbl$Position <- mapvalues(new_tbl$Position2, 
                               from=c("C", "F", "G"), 
                               to=c("Center", "Forward", "Guard"))


g <- ggplot(new_tbl, aes(x=Made, fill=Position)) + geom_density(alpha=0.3) + 
  theme(legend.position = c(.1, .9), text = element_text(size = 25),
        legend.key = element_rect(fill = "transparent")) +
xlab("shot success rate") + ylab("density")




g





ggplot(plt_data, aes(x = Platform, y=Sales, color = Genre)) +
  geom_boxplot(outlier.size = 6) + 
  stat_summary(fun.data = give.n, geom = "text", size = 10, fun.y = median, position = position_dodge(width=0.75)) + 
  theme(legend.position = c(.9, .9), text = element_text(size = 25)) +
  ylim(-0.5, 8)
