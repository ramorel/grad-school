##################################################
## Project: Trial Research Qualifying Paper -- SAMPLE DATA
## Script purpose: To test incidence of brokerage
## Date Created: 
## Last Updated:
## Author: rpm
##################################################
library(statnet)
library(ggplot2)
library(tidyverse)

# My project uses data that I cannot publicly share. So I will use a publicly available
# set to demonstrate this code. The `ergm` pack has a number of sample social networks.

# I will use the "faux.desert.high" network, which approximates the structure the network I 
# used for my study. It is directed, has 107 nodes (compared to 84) and a density of 0.04 (compared to 0.07)

data("faux.desert.high")
summary(faux.desert.high, print.adj = FALSE)

# Visulaize the network
ggraph(faux.desert.high) +
  geom_node_point(aes(colour = factor(grade))) +
  geom_edge_fan(colour = "grey40", alpha = 0.5) +
  theme_void()

# Brokerage using grade as grouping variable
brkrg <- brokerage(faux.desert.high, faux.desert.high %v% "grade")$raw.nli

head(brkrg)

# Specify the baseline ERG model
model <- ergm(faux.desert.high ~ edges + mutual + 
                    intransitive +
                    gwesp(0.75, T) + 
                    gwdsp(1, T) +
                    nodematch("grade", diff = T))

summary(mad.model)
pdf("mcmc_diagnostics.pdf")
mcmcdiag <- mcmc.diagnostics(mad.model)
dev.off()
madm.gof <- gof(mad.model)
pdf("model1_diagnostics.pdf")
par(mfrow=c(2,2))
plot(madm.gof)
dev.off()

sim.m1 <- simulate(model, 
                   nsim=1000,
                   seed=475670,
                   basis=faux.desert.high),
control=control.simulate.ergm(MCMC.burnin=10000, 
                              MCMC.interval=10000))

model.intrans <- sapply(1:1000, function(x) summary(sim.m1[[x]] ~ intransitive))
model.densdist <- sapply(1:1000, function(x) gden(sim.m1[[x]]))
model.transdist <- sapply(1:1000, function(x) gtrans(sim.m1[[x]]))
model.tridist <- sapply(1:1000, function(x) summary(sim.m1[[x]] ~ triangle))

brokerage_test <- function(simulations, observed_network, group_attibute, p.value = 0.05) {
  require(purrr)
  require(dplyr)
  require(sna)
  observed_brokerage <- brokerage(observed_network, group_attibute)$raw.nli
  observed_brokerage <- observed_brokerage %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "Name") %>% 
    mutate(group = group_attibute)
  brkg <- map(simulations, ~brokerage(., group_attibute)) %>% 
    map(., `[[`, "raw.nli") %>% 
    map(., ~mutate(as.data.frame(.), 
                   group = group_attibute)) %>% 
    bind_rows(.)
  
  dist <- brkg %>% 
    group_by(group) %>% 
    sample_n(1000) %>%
    split(., .$group)
  obs <- observed_brokerage %>% 
    split(., .$group)
  
  w_I <- list()
  for (i in 1:length(dist)){
    w_I[[i]] <- sapply(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$w_I >= obs[[i]]$w_I[x]))
  }
  w_O <- list()
  for (i in 1:length(dist)){
    w_O[[i]] <- sapply(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$w_O >= obs[[i]]$w_O[x]))
  }
  b_IO <- list()
  for (i in 1:length(dist)){
    b_IO[[i]] <- sapply(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$b_IO >= obs[[i]]$b_IO[x]))
  }
  b_OI <- list()
  for (i in 1:length(dist)){
    b_OI[[i]] <- sapply(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$b_OI >= obs[[i]]$b_OI[x]))
  }
  b_O <- list()
  for (i in 1:length(dist)){
    b_O[[i]] <- sapply(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$b_O >= obs[[i]]$b_O[x]))
  }
  t <- list()
  for (i in 1:length(dist)){
    t[[i]] <- sapply(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$t >= obs[[i]]$t[x]))
  }
  
  b_scores <- data.frame(Name = bind_rows(obs)[,1],
                         Coordinator = unlist(w_I),
                         Consultant = unlist(w_O),
                         Representative = unlist(b_IO),
                         Gatekeeper = unlist(b_OI),
                         Liaison = unlist(b_O),
                         Total = unlist(t),
                         stringsAsFactors = F) %>% 
    arrange(Name)
  
  b_dummy <- b_scores %>% 
    select(Name, Coordinator:Total) %>% 
    mutate_at(vars(Coordinator:Total), funs(if_else(.<=p.value,1,0))) %>% 
    mutate(any = if_else(rowSums(.[,2:7]) > 0, 1, 0))
  
  table(b_dummy[,2])
  table(b_dummy[,8])
  
  b_names <- b_dummy %>% 
    filter(any == 1) %>% 
    select(Name)
  return(b_names)
}

brokerage_test(model_sim)

####Derive G&F brokerage scores from simulated networks####
brkrg_sim <- map(model_sim, ~brokerage(., . %v% "grade")) %>% 
  map(`[[`, "raw.nli") %>%
  map(as_data_frame) %>% 
  map(., ~ mutate(., grade = faux.desert.high %v% "grade")) %>% 
  bind_rows(.)

####deriving probability distributions from simulations, draws for 1000####
dist <- brkg %>% 
  group_by(sector) %>% 
  sample_n(1000) %>%
  split(., .$sector)
obs <- mad.attr %>% 
  select(Name,
         sector,
         starts_with("w_"),
         starts_with("b_"),
         t) %>% 
  split(., .$sector)

for (i in dist) {
  for (j in 1:6) {
    map(obs, ~ ecdf(i[, j])(.x[, j]))
  }
}
map(dist, ~ {
  map(obs, ~ ecdf(dist[[1]]$w_I)(.x$w_I))
})
w_I <- list()
for (i in 1:length(dist)){
  w_I[[i]] <- map_int(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$w_I >= obs[[i]]$w_I[x]))
}
w_O <- list()
for (i in 1:length(dist)){
  w_O[[i]] <- map_int(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$w_O >= obs[[i]]$w_O[x]))
}
b_IO <- list()
for (i in 1:length(dist)){
  b_IO[[i]] <- map_int(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$b_IO >= obs[[i]]$b_IO[x]))
}
b_OI <- list()
for (i in 1:length(dist)){
  b_OI[[i]] <- sapply(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$b_OI >= obs[[i]]$b_OI[x]))
}
b_O <- list()
for (i in 1:length(dist)){
  b_O[[i]] <- map_int(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$b_O >= obs[[i]]$b_O[x]))
}
t <- list()
for (i in 1:length(dist)){
  t[[i]] <- map_int(1:nrow(obs[[i]]), function(x) mean(dist[[i]]$t >= obs[[i]]$t[x]))
}

b_scores <- data.frame(Name = bind_rows(obs)[,1],
                       Coordinator = unlist(w_I),
                       Consultant = unlist(w_O),
                       Representative = unlist(b_IO),
                       Gatekeeper = unlist(b_OI),
                       Liaison = unlist(b_O),
                       Total = unlist(t),
                       stringsAsFactors = F) %>% 
  arrange(Name) %>% 
  full_join(mad.attr, ., by = "Name")

b_dummy <- b_scores %>% 
  select(Name, Coordinator:Total) %>% 
  mutate_at(vars(Coordinator:Total), funs(if_else(.<=0.01,1,0))) %>% 
  mutate(any = if_else(rowSums(.[,2:7]) > 0, 1, 0))

table(b_dummy[,2])
table(b_dummy[,8])

b_dummy %>% 
  filter(any == 1) %>% 
  select(Name)

brokers <- mad.attr %>% 
  select(c(1,2,7,12:14,17:ncol(mad.attr))) %>% 
  bind_cols(b_dummy[,c(2:ncol(b_dummy))])

brokers <- brokers %>% 
  filter(any==1)
brokers$Name

currentDate <- Sys.Date() 
brokerdate <- paste("brokers",currentDate,".csv",sep="")
attrdate <- paste("mad.attr",currentDate,".csv",sep="")
write.csv(brokers, file = brokerdate,row.names = F)
write.csv(mad.attr, file = attrdate, row.names = F)
