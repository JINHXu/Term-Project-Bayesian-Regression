# Sound-Inventory-Population with bayesian regression method

**A bayesian regression analysis with R and Stan（an R interface to Stan in rethinking package).<br/>**

This effect has been critically discussed in
Steven Moran, Daniel McCloy, and Richard Wright. Revisiting the population vs phoneme- inventory correlation. Language, 88(4):877–893, 2012

Conduct a series of Bayesian regression analyses to test whether an association between two variables(Sound inventory size vs. population size) is genuine, with rethinking package.<br/>
 Specifically, fit the following models:<br/>
* a simple linear regression with log(population size) as independent variable and log(sound inventory size) as dependent variable<br/>
* a hierarchical model with language families as random effect,<br/>
* a hierarchical model with continents as random effect,<br/>
* a hierarchical model with both language families and continents as random effect.<br/>

Use Stan to estimate the posterior distributions.<br/>
Repeat theses analysis with a Poisson regression instead of a linear regression, with untransformed number of phonemes as dependent variable.<br/>

Use prior predictive checks to determine a wisely-chosen prior.<br/>
Model comparison via WAIC.<br/>

A discussion about findings is included.
