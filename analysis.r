library(ggplot2)
library(rethinking)
"Conduct a series of Bayesian regression analyses to test whether an association is genuine.
Author: Jinghua Xu
Last modified date: 21 Oct, 2019
Honor Code:  I pledge that this program represents my own work. I took a reference to the example codes in the textbook Statistical Rethinking."

"to-be-addressed issues list:
-questionable slope in hierarchical models with multiple random effects
-sloppy priors in all models to be improved
-more prior predictive checks to be added and ran
-models with null pointers"

"given codes in project description"


d <- read.csv('soundInventoryPopulation.csv')

options(repr.plot.width=5, repr.plot.height=5)
head(d)
ggplot(d, aes(x=population, y=nPhonemes)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_x_log10() +
  scale_y_log10()


"a simple linear regression with log(population size) as independent variable and log(sound inventory size) as dependent variable"

ggplot(d, aes(x=log(population), y=log(nPhonemes))) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_x_continuous() +
  scale_y_continuous()


"a hierarchical model with language families as random effect"

#data pre-process
d$log_pop <- log(d$population)
dd <- d[complete.cases(d$population) , ]
dd$log_pop_std <- dd$log_pop /mean(dd$log_pop)
dd$np_std <- dd$nPhonemes /max(dd$nPhonemes)

print(mean(dd$np_std))

dat1.1 <- list(
  pop = dd$log_pop_std,
  np = dd$np_std,
  lf = as.integer(dd$glottologFamily)
)


m1.1 <- ulam(
  alist(
    pop ~ dnorm(mu, sigma),
    mu <- a[lf] + b[lf]*(np - 0.2506349),
    #adaptive priors
    #temporary priors, to be amended
    a[lf] ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b[lf] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat1.1, chains = 4, cores = 4,iter=1000, log_lik = TRUE)

precis(m1.1, depth = 2)
WAIC(m1.1)
#prior predictive check
set.seed(1111)
N <- 111
a <- rnorm(N,a_bar, a_sigma)
a_bar <- rnorm(N,0, 1.5)
b <- rnorm(N,b_bar, b_sigma)
b_bar <- rnorm(N,0, 1.5)
a_sigma <- rexp(N,1)
b_sigma <- rexp(N,1)
sigma <- rexp(N,1)

plot( NULL , xlim=range(dd$log_pop_std) , ylim=c(-30,30) ,
      xlab="standard log population" , ylab="nPhonemes" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( " b_bar ~ dnorm(0, 1.5), a_bar ~ dnorm(0, 1.5)" )
xbar <- mean(dd$np_std)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(dd$log_pop_std) ,
                        to=max(dd$log_pop_std) , 
                        add=TRUE ,
                        col=col.alpha("black",0.2) )


"a hierarchical model with continents as random effect"

#data pre-process
d$log_pop <- log(d$population)
dd <- d[complete.cases(d$population) , ]
dd$log_pop_std <- dd$log_pop /mean(dd$log_pop)
dd$np_std <- dd$nPhonemes /max(dd$nPhonemes)


dat1.2 <- list(
  pop = dd$log_pop_std,
  np = dd$np_std,
  cid = as.integer(dd$continent)
)


m1.2 <- ulam(
  alist(
    pop ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(np - 0.2506349),
    #adaptive priors
    #temporary priors, to be amended
    a[cid] ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b[cid] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat1.2, chains = 4, cores = 4,iter=1000, log_lik = TRUE)

precis(m1.2, depth = 2)
WAIC(m1.2)
compare(m1.1, m1.2)
#prior predictiva check is carried out the same way as in the previous model

"a hierarchical model with both language families and continents as random effect."

#data pre-process
d$log_pop <- log(d$population)
dd <- d[complete.cases(d$population) , ]
dd$log_pop_std <- dd$log_pop /mean(dd$log_pop)
dd$np_std <- dd$nPhonemes /max(dd$nPhonemes)

dat1.3 <- list(
  pop = dd$log_pop_std,
  np = dd$np_std,
  cid = as.integer(dd$continent),
  lf = as.integer(dd$glottologFamily)
)


m1.3 <- ulam(
  alist(
    pop ~ dnorm(mu, sigma),
    #a questionable parameter b
    mu <- c[lf] + a[cid] + b*(np - 0.2506349),
    #adaptive priors
    #temporary priors, to be amended
    a[cid] ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    c[lf] ~ dnorm(c_bar, c_sigma),
    c_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    c_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat1.3, chains = 4, cores = 4,iter=1000, log_lik = TRUE)

precis(m1.3, depth = 2)
WAIC(m1.3)
compare(m1.1, m1.2, m1.3)


#prior predictive check(to be amended)
set.seed(1111)
N <- 111
a <- rnorm(N,a_bar, a_sigma)
a_bar <- rnorm(N,0, 1.5)
b <- rnorm(N,b_bar, b_sigma)
b_bar <- rnorm(N,0, 1.5)
c <- dnorm(c_bar, c_sigma)
c_bar <- dnorm(0, 1.5)
a_sigma <- rexp(N,1)
b_sigma <- rexp(N,1)
sigma <- rexp(N,1)
c_sigma <- rexp(N,1)

plot( NULL , xlim=range(dd$log_pop_std) , ylim=c(-30,30) ,
      xlab="standard log population" , ylab="nPhonemes" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( " b_bar ~ dnorm(0, 1.5), a_bar ~ dnorm(0, 1.5), c_bar <- dnorm(0, 1.5)" )
xbar <- mean(dd$np_std)
for ( i in 1:N ) curve( a[i] + c[i] + b[i]*(x - xbar) ,
                        from=min(dd$log_pop_std) ,
                        to=max(dd$log_pop_std) , 
                        add=TRUE ,
                        col=col.alpha("black",0.2) )

"a hierarchical model with language families as random effect(a Poisson regression instead of a linear regression)"

dat2.1 <- list(
  pop = dd$log_pop_std,
  np = d$nPhonemes,
  lf = as.integer(dd$glottologFamily)
)

m2.1 <- ulam(
  alist(
np ~ dpois(lambda),
log(lambda) <- a[lf] + b[lf]*np,
#adaptive priors
#temporary priors, to be amended
a[lf] ~ dnorm(a_bar, a_sigma),
a_bar ~ dnorm(0, 1.5),
b[lf] ~ dnorm(b_bar, b_sigma),
b_bar ~ dnorm(0, 1.5),
a_sigma ~ dexp(1),
b_sigma ~ dexp(1),
sigma ~ dexp(1)
  ), data = dat2.1, chains = 4, log_lik = TRUE)

precis(m2.1, depth = 2)
WAIC(m2.1)
compare(m1.1, m1.2, m1.3, m2.1)

"a hierarchical model with continents as random effect(a Poisson regression instead of a linear regression)"

dat2.2 <- list(
  pop = dd$log_pop_std,
  np = d$nPhonemes,
  cid = as.integer(dd$continent)
)

m2.2 <- ulam(
  alist(
    np ~ dpois(lambda),
    log(lambda) <- a[cid] + b[cid]*np,
    #adaptive priors
    #temporary priors, to be amended
    a[cid] ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b[cid] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat2.2, chains = 4, log_lik = TRUE)

precis(m2.2, depth = 2)
WAIC(m2.2)
compare(m1.1, m1.2, m1.3, m2.1, m2.2)


"a hierarchical model with both language family and continents as random effects(a Poisson regression instead of a linear regression)"

#data pre-process
d$log_pop <- log(d$population)
dd <- d[complete.cases(d$population) , ]
dd$log_pop_std <- dd$log_pop /mean(dd$log_pop)

dat2.3 <- list(
  pop = dd$log_pop_std,
  np = d$nPhonemes,
  cid = as.integer(dd$continent),
  lf = as.integer(dd$glottologFamily)
)


m2.3 <- ulam(
  alist(
    np ~ dpois(lambda),
    log(lambda) <- c[lf] + a[cid] + b*(np - 0.2506349),
    #adaptive priors
    #temporary priors, to be improved
    a[cid] ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    c[lf] ~ dnorm(c_bar, c_sigma),
    c_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    c_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat2.3, chains = 4, log_lik = TRUE)

precis(m2.3, depth = 2)
WAIC(m2.3)
compare(m1.1, m1.2, m1.3, m2.1, m2.2, m2.3)
"The first column contains the WAIC values. Smaller values are better, and the models are ordered by WAIC, from best to worst. "

"sample to see HPDI"
"how to sample from mcmc"


samples <- extract.samples(m1.3)
HPDI(samples, prob = 0.85)


'In the best model, does the 89% Highest Posterior Density interval for the slope include 0?'

