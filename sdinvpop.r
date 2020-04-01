library(ggplot2)
library(rethinking)

"Conduct a series of Bayesian regression analyses to test whether the association positive correlation between population size and sound inventory size is genuine.
Author: Jinghua Xu
Honor Code:  I pledge that this program represents my own work."


d <- read.csv('soundInventoryPopulation.csv')

"First impression"
options(repr.plot.width=5, repr.plot.height=5)
head(d)
ggplot(d, aes(x=population, y=nPhonemes)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_x_log10() +
  scale_y_log10()

# Linear models


"A simple linear regression with log(population size) as independent variable and log(sound inventory size) as dependent variable(non-bayesian)"

ggplot(d, aes(x=log(population), y=log(nPhonemes))) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_x_continuous() +
  scale_y_continuous()

# data pre-process
dd = d[complete.cases(d$population) , ]
dd$log_pop = log(d$population)
dd$log_pop_std = dd$log_pop /mean(dd$log_pop)
dd$np_std = dd$nPhonemes /max(dd$nPhonemes)
dd$log_np = log(d$nPhonemes)

head(dd)

print(mean(dd$np_std))
print(mean(dd$log_np))


# prior predictive check
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


"A simple linear regression with log(population size) as independent variable and log(sound inventory size) as dependent variable(bayesian)"
dat1.0 = list(
  lpop = dd$log_pop,
  lnp = dd$log_np)

m1.0 <- ulam(
  alist(
    lpop ~ dnorm(mu, sigma),
    mu <- a + b*(lnp - 3.501372),
    # conventional priors
    a ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat1.0, chains = 4, cores = 4,iter=1000, log_lik = TRUE)

# Tables of marginal distributions.
precis(m1.0, depth = 2)
"A fine model with reasonable n_eff(about the sample size) and Rhat(approach 1) values"
WAIC(m1.0)

# posterior
post1.0 <- extract.samples( m1.0 )
# display raw data and sample size
plot( dd$log_pop , dd$log_np ,
      xlim=range(dd$log_pop) , ylim=range(dd$log_np) ,
      col=rangi2 , xlab="log population" , ylab="log sound inventary size" )
mtext(concat("N = ",1000))
# plot the lines, with transparency
for ( i in 1:100 )
  curve( post1.0$a[i] + post1.0$b[i]*(x-3.501372) ,
         col=col.alpha("black",0.3) , add=TRUE )

"A corelation between the 2 variables can be seen from the posterior plot. So far still unclear if the corelation is fake or not."


"A hierarchical model with language families as random effect"


dat1.1 <- list(
  pop = dd$log_pop_std,
  np = dd$np_std,
  lf = as.integer(dd$glottologFamily)
)

# prior predictive simulation for the conventional priors
curve( dnorm( x , 0 , 1.5 ) , from= -10 , to=10 )
curve((dexp(x, 1)))


m1.1 <- ulam(
  alist(
    pop ~ dnorm(mu, sigma),
    mu <- a[lf] + b[lf]*(np - 0.2506349),
    # conventional priors
    a[lf] ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b[lf] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat1.1, chains = 4, cores = 4,iter=1000, log_lik = TRUE)

# Tables of marginal distributions.
precis(m1.1, depth = 2)
"A fine model with reasonable n_eff(about the sample size) and Rhat(approach 1) values"
WAIC(m1.1)


# posterior
post1.1 <- extract.samples( m1.1 )
post1.1

# display raw data and sample size
plot( dd$log_pop_std , dd$np_std ,
      xlim=range(dd$log_pop_std) , ylim=range(dd$np_std) ,
      col=rangi2 , xlab="standardized log population" , ylab="standardized sound inventary size" )
mtext(concat("N = ",100))

# change j(1:5) to alter glotto family(random effect control)
j = 1

# plot the lines, with transparency
for ( i in 1:100 )
    curve( post1.1$a[j,i] + post1.1$b[j,i]*(x-0.2506349) ,
           col=col.alpha("black",0.3) , add=TRUE )

"The correaltion does not show within each language family from the posterior plot, we have a first evidence that the corelation can be fake."


"a hierarchical model with continents as random effect"

#data pre-process
d$log_pop <- log(d$population)
dd <- d[complete.cases(d$population) , ]
dd$log_pop_std <- dd$log_pop /mean(dd$log_pop)
dd$np_std <- dd$nPhonemes /max(dd$nPhonemes)

head(dd)


dat1.2 <- list(
  pop = dd$log_pop_std,
  np = dd$np_std,
  cid = as.integer(dd$continent)
)


m1.2 <- ulam(
  alist(
    pop ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(np - 0.2506349),
    # conventianl priors
    a[cid] ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b[cid] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat1.2, chains = 4, cores = 4,iter=1000, log_lik = TRUE)

precis(m1.2, depth = 2)
# fine n_eff values about the sample size and Rhat values approaching 1

WAIC(m1.2)
compare(m1.1, m1.2)

# posterior
post1.2 <- extract.samples( m1.2 )
post1.2

# display raw data and sample size
plot( dd$log_pop_std , dd$np_std ,
      xlim=range(dd$log_pop_std) , ylim=range(dd$np_std) ,
      col=rangi2 , xlab="standardized log population" , ylab="standardized sound inventary size" )
mtext(concat("N = ",100))

# change j(1:6) to alter continent(random effect control)
j = 2

# plot the lines, with transparency
for ( i in 1:100 )
  curve( post1.2$a[i,j] + post1.2$b[i,j]*(x - 0.2506349),
         col=col.alpha("black",0.3) , add=TRUE )

"The correaltion does not show within each language family from the posterior plot, we have a further evidence that the corelation can be fake."

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
    # a questionable parameter b
    mu <- c[lf] + a[cid] + b*(np - 0.2506349),
    # conventional priors
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
# a bad model

WAIC(m1.3)
compare(m1.1, m1.2, m1.3)


# prior predictive check
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
for ( i in 1:N ) curve( a[i] + c[i] + b[i]*(x - 0.2506349) ,
                        from=min(dd$log_pop_std) ,
                        to=max(dd$log_pop_std) , 
                        add=TRUE ,
                        col=col.alpha("black",0.2) )

# posterior
post1.3 <- extract.samples( m1.3 )
post1.3

# display raw data and sample size
plot( dd$log_pop_std , dd$np_std ,
      xlim=range(dd$log_pop_std) , ylim=range(dd$np_std) ,
      col=rangi2 , xlab="standardized log population" , ylab="standardized sound inventary size" )
mtext(concat("N = ",100))

# change j(1:6) to alter continent(random effect control)
j = 1

# change k(1:5) to alter language family(random effect control)
k = 1

# plot the lines, with transparency
for ( i in 1:100 )
  curve( post1.3$a[i,k] +post1.3$c[j,i] + post1.3$b[i]*(x - 0.2506349),
         col=col.alpha("black",0.3) , add=TRUE )

"Absolutely no sign of a corelation between the 2 variables with 2 random effects being considered at the same time. Given the observations from previous 3 models, we can draw the conclusion that the corelation between the 2 variable of intrests is fake, at least not linear."


# Poisson models

################
"a simple Poisson regression with log(population size) as independent variable and log(sound inventory size) as dependent variable"

dat2.0 = list(
  lpop = dd$log_pop,
  lnp = dd$log_np)

m2.0 <- ulam(
  alist(
    lnp ~ dpois(lambda),
    log(lambda) <- a + b*lpop,
    # conventional priors
    a ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1.5),
    b ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1.5),
    a_sigma ~ dexp(1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dat2.0, chains = 4, log_lik = TRUE)

precis(m2.0, depth = 2)
WAIC(m2.0)
#####################

"a hierarchical model with language families as random effect(a Poisson regression instead of a linear regression)"

dat2.1 <- list(
  pop = dd$log_pop_std,
  np = d$nPhonemes,
  lf = as.integer(dd$glottologFamily)
)

m2.1 <- ulam(
  alist(
    np ~ dpois(lambda),
    log(lambda) <- a[lf] + b[lf]*pop,
    # conventional priors
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

# posterior

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
    # conventional prior
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
    # conventional priors
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

precis(m2.3, depth =2) 
WAIC(m2.3)
compare(m1.1, m1.2, m1.3, m2.1, m2.2, m2.3)

"The first column contains the WAIC values. Smaller values are better, and the models are ordered by WAIC, from best to worst. "

"The best model is m1.3 according to WAIC values"


samples <- extract.samples(m1.3)
slope = samples$b
HPDI(samples$b, prob = 0.89)

'In the best model, the 89% Highest Posterior Density interval for the slope include 0'
