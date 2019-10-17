# Term-Project-Bayesian-Regression-Analysis

*A bayesian regression analysis with R and Stan（an R interface to Stan in rethinking package).<br/>*

Conduct a series of Bayesian regression analyses to test whether an association between two variables(Sound inventory size vs. population size) is genuine, with rethinking package.<br/>
 Specifically, fit the following models:<br/>
* a simple linear regression with log(population size) as independent variable and log(sound inventory size) as dependent variable<br/>
* a hierarchical model with language families as random effect,<br/>
* a hierarchical model with language families as random effect,<br/>
* a hierarchical model with both language families and continents as random effect.<br/>

Use Stan to estimate the posterior distributions.<br/>
Repeat theses analysis with a Poisson regression instead of a linear regression, with untrans- formed number of phonemes as dependent variable.<br/>

Using prior predictive checks to determine a wisely-chosen prior.<br/>
Model comparison via WAIC.<br/>

A discussion about findings is included.
