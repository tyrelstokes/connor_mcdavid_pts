#################################
###############################

library(cmdstanr)
library(posterior)
library(dplyr)
library(ggplot2)
library(bayestestR)
library(ggridges)

######################

Mcdavid2 <- read.csv("McDavid_df.csv")
oilers <- read.csv("oilers_schedule_2122.csv")

names(oilers)[4] <- "Team"
oilers$Home <- as.numeric(grepl("vs",oilers$Vs))

tms <- unique(oilers$Team)

tms2 <- c("VAN", "CGY", "ANA","ARI","VGK","PHI","SEA","NSH","NYR","DET","BOS","BUF","STL","WPG","CHI","DAL","PIT","LAK","MIN","CAR","TOR","CBJ","SJS","NJD",
          "NYI","OTT","FLA","MTL","WSH","TBL","COL")

oilers$Teams_2 <- plyr::mapvalues(oilers$Team, from = tms, to = tms2)


oilers$opp_int <- plyr::mapvalues(oilers$Teams_2, from = c(unique(Mcdavid2$opponent[1:412]),"SEA"), to = c(1:31))

df_s7 <- Mcdavid2 %>% filter(season2 ==7)

N_games <- nrow(df_s7)## How many games have already past this season

curr_assists <- sum(df_s7$Assists)
curr_goals <- sum(df_s7$Goals)

oilers2 <- oilers[-c(1:N_games),]

####################################
####################################

mcdata3 <- list(N = nrow(Mcdavid2),Assists = Mcdavid2$Assists, Goals = Mcdavid2$Goals,
                opponent = as.numeric(Mcdavid2$opp_int), season = as.numeric(Mcdavid2$season2), home = Mcdavid2$Home,
                ns = 7, nt = 31,N2 = nrow(oilers2),opponent_remain = as.numeric(oilers2$opp_int), season_remain = rep(7,nrow(oilers2)),
                home_remain = oilers2$Home,  cur_goals = curr_goals,cur_assists = curr_assists)

### Note there have been a few changes with how stan handles declaring integers
### If you are using rstan for either of these models you may have to change the 
#### stan files to reflect that

mod_original <- cmdstan_model("mcdavid_pts_assists.stan")



fitt_s <-  mod_original$sample(data = mcdata3, parallel_chains = 4, iter_warmup = 2500, iter_sampling = 4000, adapt_delta = 0.9, refresh = 100)

s1 <- fitt_s$summary()
#################################
################################
### Run AR2 model 
### This model is experimental at this stage, may be bugs
mod_ar2 <- cmdstan_model("mcdavid_ar2.stan")

fitt_2 <-  mod_ar2$sample(data = mcdata3, parallel_chains = 4, iter_warmup = 2500, iter_sampling = 4000, adapt_delta = 0.9, refresh = 100)


s2 <- fitt_2$summary()


################################################
################################################

### Compare the two models


point_draws <- as_draws_df(fitt_s$draws(c("pred_total_points","pred_total_goals","pred_total_assists")))
names(point_draws)[1:3] <- c("Point_Total", "Goal_Total","Assist_Total")
point_draws$type = "RW1 + AR2"

point_draws_ar2 <- as_draws_df(fitt_2$draws(c("pred_total_points","pred_total_goals","pred_total_assists")))
names(point_draws_ar2)[1:3] <- c("Point_Total", "Goal_Total","Assist_Total")

point_draws_ar2$type = "AR2 + AR2"


point_draws_c <- rbind(point_draws,point_draws_ar2)

p <- ggplot(point_draws_c, aes(x = Point_Total)) +geom_histogram(aes(y = stat(count / sum(count)), fill = type), color = "black") +
  geom_vline(xintercept = mean(point_draws$Point_Total[point_draws$type =="RW1 + AR2"]), color = "blue")+ ggtitle("Connor McDavid Posterior Predicted Points")+
  facet_wrap(~type)

p


ggplot(point_draws_c, aes(x=Point_Total,y = type, fill = type),alpha = .5) +stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = F,alpha = 0.5)+
  scale_fill_manual(values =c("firebrick","dodgerblue"))+
  geom_vline(xintercept = mean(point_draws_c$Point_Total[point_draws_c$type =="RW1 + AR2"]), color = "blue")+
  geom_vline(xintercept = mean(point_draws_c$Point_Total[point_draws_c$type =="AR2 + AR2"]), color = "red")


ggplot(point_draws_c, aes(x=Goal_Total,y = type, fill = type),alpha = .5) +stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = F,alpha = 0.5)+
  scale_fill_manual(values =c("firebrick","dodgerblue"))+
  geom_vline(xintercept = mean(point_draws_c$Goal_Total[point_draws_c$type =="RW1 + AR2"]), color = "blue")+
  geom_vline(xintercept = mean(point_draws_c$Goal_Total[point_draws_c$type =="AR2 + AR2"]), color = "red")


ggplot(point_draws_c, aes(x=Assist_Total,y = type, fill = type),alpha = .5) +stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = F,alpha = 0.5)+
  scale_fill_manual(values =c("firebrick","dodgerblue"))+
  geom_vline(xintercept = mean(point_draws_c$Assist_Total[point_draws_c$type =="RW1 + AR2"]), color = "blue")+
  geom_vline(xintercept = mean(point_draws_c$Assist_Total[point_draws_c$type =="AR2 + AR2"]), color = "red")


ci1 <- ci(point_draws$Goal_Total,method = "HDI", ci = 0.93)
ci2 <- ci(point_draws$Assist_Total,method = "HDI", ci = 0.93)

ci3 <- ci(point_draws$Point_Total, method = "HDI", ci = 0.93)

ci1
ci2
ci3

ci1_a <- ci(point_draws_ar2$Goal_Total,method = "HDI", ci = 0.93)
ci2_a <- ci(point_draws_ar2$Assist_Total,method = "HDI", ci = 0.93)

ci3_a <- ci(point_draws_ar2$Point_Total, method = "HDI", ci = 0.93)

ci1_a
ci2_a
ci3_a
