# Bayesian Regression Analysis: population vs phoneme inventory size

The assocaite between the two variables: population size vs phoneme inventory size has been critically discussed in

> Steven Moran, Daniel McCloy, and Richard Wright. Revisiting the population vs phoneme inventory correlation. Language, 88(4):877–893, 2012

This regression analysis employs a Bayesian approached implemented with R and Stan（an R interface to Stan in `rethinking` package).

> _Bayesian data analysis: count all the ways data can happen, accoding to assumptions. Assumptions with more ways that are consistent with data are more plausible._

## Analysis

The analysis fits data to the following models:

* a simple linear regression with log(population size) as independent variable and log(sound inventory size) as dependent variable

* a hierarchical model with language families as random effect

* a hierarchical model with continents as random effect

* a hierarchical model with both language families and continents as random effect

Then the analysis are then repeated with a Poisson regression instead of a linear regression, with untransformed number of phonemes as dependent variable.

- Posterior distributions are estimated using Stan.

- A proper prior is determined through predictive checks.

- Models are compared via WAIC.

## Code 

* View the [code](https://github.com/JINHXu/soundInventoryPopulation/blob/master/sdinvpop.r)

* View on [Kaggle](https://www.kaggle.com/xujinghua/soundinventarypopulation)

* View [Jupyter notebook](https://github.com/JINHXu/soundInventoryPopulation/blob/master/soundinventarypopulation.ipynb)
