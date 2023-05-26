
# Data Description

## Book description
4623 individual demand time series, each containing 48 half-hourly
observations per day and covering the period from July 2009 to December 2010. 

In section 1.2.3.1 of the book it mentions that only the households data were kept from the original dataset. The dataset has demand data for 2672 households.

In section 1.2.3.2 they mention that only data for 2010 is kept. Hence why we have 16799 rows, 16799/48 = 349.9.

I think "special" days have already been removed. e.g christmas is not here.


## electBook package
The dataset is on: https://github.com/mfasiolo/electBook/blob/master/data/Irish.RData

In package, a description of the dataset is given:

The data is stored as a list of free components:

- `indCons`
Each coloumn is the electricity demand for a single household. Observations every 48 hours.
16799 rows, 2672 columns

- `extra`
16799 rows  
Each row gives information on the time point that the data was retrieved. For example the day of the week.

- `survey` 
2672 rows  
Gives details about the households.


# Gaussian process regression

$$Y_i^0 = f(x_i^0) + \epsilon_i, \quad \epsilon_i \sim \mathcal{N}(0, \sigma^2) \quad f \sim GP(0,K) \quad \sigma^2 \sim \delta_{\lambda}$$

$$
f|y_{1:n}^0 \sim GP(f_n, K_n) \\
f_n(x) = k_n(x)^T (K_n + \lambda I_n)^{-1} y_{1:n}^0 \\
K_n(x,x') = k(x,x') - k_n(x)^T(K_n + \lambda I_n)^{-1}k_n(x')
$$

What is meant by a function being distributed as a GP is that: 
given a set of points in the domain $(x_1, \dots, x_n)$ a function $f$ being distributed as a GP, means that $(f(x_1), \dots, f(x_n))$ has a mvn distribution with mean given by the mean function evaluated at the data points and the covariance matrix given by the gram matrix evaluated at the points.