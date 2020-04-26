# Sound inventory size vs. population size
**A bayesian regression analysis with R and Stan（an R interface to Stan in rethinking package).<br/>**

> _Bayesian data analysis: count all the ways data can happen, accoding to assumptions. Assumptions with more ways that are consistent with data are more plausible._


Conduct a series of Bayesian regression analyses to test whether an association between two variables(Sound inventory size vs. population size) is genuine, with the rethinking package.<br/>
This effect has been critically discussed in
Steven Moran, Daniel McCloy, and Richard Wright. Revisiting the population vs phoneme- inventory correlation. Language, 88(4):877–893, 2012<br/>

 Specifically, fit the following models:<br/>
* a simple linear regression with log(population size) as independent variable and log(sound inventory size) as dependent variable<br/>
* a hierarchical model with language families as random effect,<br/>
* a hierarchical model with continents as random effect,<br/>
* a hierarchical model with both language families and continents as random effect.<br/>

Use Stan to estimate the posterior distributions.<br/>
Repeat theses analysis with a Poisson regression instead of a linear regression, with untransformed number of phonemes as dependent variable.<br/>

Use prior predictive checks to determine a wisely-chosen prior.<br/>
Model comparison via WAIC.<br/>

_View the [code](https://github.com/JINHXu/soundInventoryPopulation/blob/master/sdinvpop.r)_<br/>
_View on [Kaggle](https://www.kaggle.com/xujinghua/soundinventarypopulation)_<br/>
_View [Jupyter notebook](https://github.com/JINHXu/soundInventoryPopulation/blob/master/soundinventarypopulation.ipynb)_
