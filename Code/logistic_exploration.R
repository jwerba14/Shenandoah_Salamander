## y is occurence
## a is intercept
## b1 is slope

# set up
library(ggplot2)
library(tidyverse)
set.seed(102)

## parameters to follow
params <- c("a", "b1")

#fit
ni <- 5000
nb <- 20
nt <- 1
nc <- 3
na <- 10000
mi <- 1000


## dataframe of effect sizes
efs <- expand.grid(
  b1 = c(0.5,2,4,10),
  a = c(-2,2)
)

# temperature
TEMP1 <- seq(from = 65, to = 78, length.out = 100)
TEMP <- (TEMP1 - mean(TEMP1) ) / sd(TEMP1)  ## normalize temperature

## run loop to fit over 3 different priors 
#fit_sp has prior dnorm(0,0.368)
# fit_sp_wider prior dnorm (0,0.01)
# fit_sp_narrow prior dnorm (0, 0.8)


for (i in 1:nrow(efs)){
  
## bernoulli is binomial with size = 1 ?
y_sp <- rbinom(100, 1, plogis(efs$a[i]+efs$b1[i]*TEMP))

#bundle data
dat_sp <-  list(
  ## Parameter estimation
  N = 100,
  TEMP = TEMP,
  y = y_sp
  
)


# fit

fit_sp <- jags.model(data = dat_sp, inits = NULL,  
         #parameters.to.save = params, 
         file = "simple_logistic.txt", 
         n.chains = 3, 
         #n.iter = 10000, 
         #n.thin = 10,
         n.adapt = 1000)

samples <- coda.samples(fit_sp, variable.names = params, n.iter = 5000, thin = 1)
autocorr(samples)


# with wider priors 
fit_sp_w <- autojags(data = dat_sp, inits = NULL,  
                   parameters.to.save = params, 
                   model.file = "simple_logistic_wider.txt", 
                   n.chains = nc, 
                   n.thin = nt, 
                   n.burnin = nb, 
                   n.adapt = na,
                   max.iter = mi,
                   iter.increment = ni,
                   parallel = TRUE)

# with narrower priors
fit_sp_n <- autojags(data = dat_sp, inits = NULL,  
                     parameters.to.save = params, 
                     model.file = "simple_logistic_narrow.txt", 
                     n.chains = nc, 
                     n.thin = nt, 
                     n.burnin = nb, 
                     n.adapt = na,
                     max.iter = mi,
                     iter.increment = ni,
                     parallel = TRUE)


out <- data.frame(
  a = rep(efs$a[i], length(fit_sp$sims.list$a)),
  b1 = rep(efs$b1[i], length(fit_sp$sims.list$a)),
  a_post_sp = fit_sp$sims.list$a,
  a_post_sp_w = fit_sp_w$sims.list$a,
  a_post_sp_n = fit_sp_n$sims.list$a,  
  b1_post_sp = fit_sp$sims.list$b1,
  b1_post_sp_w = fit_sp_w$sims.list$b1,
  b1_post_sp_n = fit_sp_n$sims.list$b1
)

## save outputs
if (i == 1 ) {
 out.f <- out
} else {
  
  out.f <- rbind(out.f,out )
}  


}

# rearrange output for graphing
out.g <- out.f %>% pivot_longer(-c(a,b1), names_to = "prior_width" )



# graph set up
theme_set(theme_bw()) 
theme_update(axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),
             axis.title.x = element_text(size = 14),
             axis.title.y = element_text(size = 14),
             legend.title = element_text(size = 12),
             legend.text = element_text(size = 10),
             legend.spacing = unit(0.25, "cm"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.spacing = unit(0, "lines"),
             legend.key = element_rect(fill = "white"),
             panel.spacing.y = unit(-0.25, "lines"),
             panel.border = element_rect(colour = "black", 
                                         fill = NA, size = 1),
             strip.text.x = element_text(size = 18, colour = "black", 
                                         face = "bold"))

# graph
adat <- out.g %>% dplyr::select(-b1) %>% filter(prior_width == "a_post_sp" | prior_width == "a_post_sp_n" | prior_width == "a_post_sp_w" )
graph_a <-  ggplot(data = adat, aes(x = value)) + 
  geom_density(aes(color = prior_width)) + 
  geom_vline(aes(xintercept = a ), color = "purple", size = 1.0, linetype = "dashed" ) +
  facet_wrap(~a) + ggtitle("intercept estimate")
print(graph_a)


bdat <- out.g %>% dplyr::select(-a) %>% filter(prior_width == "b1_post_sp" | prior_width == "b1_post_sp_n" | prior_width == "b1_post_sp_w" )
graph_b <-  ggplot(data = bdat, aes(x = value)) + 
  geom_density(aes(color = prior_width)) + 
  geom_vline(aes(xintercept = b1 ), color = "purple", size = 1.0, linetype = "dashed" )+
  facet_wrap(~b1) + ggtitle("slope estimate") 
print(graph_b)
