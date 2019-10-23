## Summary of Chapter 6

- There are very many Python packages expressely designed for scientists
- Find out the ones you will need the most for your research
- `numpy` and `scipy` provide a new data type (`array`) that is essential for manipulating data. The huge package has much to offer for statistics, random number generation, linear algebra, differential equations, numerical integration, optimization, and much more
- `pandas` brings `R`'s data frames into Python. Excellent to manipulate data organized in spreadsheets.
- `Biopython` allows you to perform a lot of calculations for molecular biology in an automated fashion

Instead of going on reviewing each tool, we are going to consider two long examples: the first will allow us to practice our `numpy`, the second our `pandas`.

### Warmup exercise: bifurcation diagram for the logistic map

To work a bit with `numpy` and `scipy`, we're going to explore a simple model for population dynamics that however has an interesting behavior, the [logistic map](https://en.wikipedia.org/wiki/Logistic_map). See the nice paper by May ([Nature 1976](https://www.nature.com/articles/261459a0)).

The model is very simple, and is based on discrete dynamics:

<a href="https://www.codecogs.com/eqnedit.php?latex=x_{t&plus;1}&space;=&space;r&space;x_t&space;(1&space;-&space;x_t)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?x_{t&plus;1}&space;=&space;r&space;x_t&space;(1&space;-&space;x_t)" title="x_{t+1} = r x_t (1 - x_t)" /></a>

With initial population size between 0 and 1. Importantly, depending on the parameter  *r*, the population can:

- go extinct
- reach a stable equilibrium
- cycle through a set of values
- display chaotic dynamics

Behavior:

- With *r* between 0 and 1, the population will eventually die, independent of the initial population.
- With *r* between 1 and 2, the population will quickly approach the value  *r−1/r*, independent of the initial population.
- With *r* between 2 and 3, the population will also eventually approach the same value *r−1/r*, but first will fluctuate around that value for some time.
- With *r* between 3 and  *1+sqrt(6)≃3.44949* from almost all initial conditions the population will approach permanent oscillations between two values. These two values are dependent on *r*.
- With *r* between 3.44949 and 3.54409 (approximately), from almost all initial conditions the population will approach permanent oscillations among four values.
- With *r* increasing beyond 3.54409, from almost all initial conditions the population will approach oscillations among 8 values, then 16, 32, etc. The lengths of the parameter intervals that yield oscillations of a given length decrease rapidly.
- At  *r≃3.56995* we have the onset of chaos, at the end of the period-doubling cascade. From almost all initial conditions, we no longer see oscillations of finite period. Slight variations in the initial population yield dramatically different results over time, a prime characteristic of chaos.
- Most values of *r* beyond 3.56995 exhibit chaotic behaviour, but there are still certain isolated ranges of *r* that show non-chaotic behavior; these are sometimes called islands of stability. For instance, beginning at  *1 + sqrt(8)*  (approximately 3.82843) there is a range of parameters *r* that show oscillation among three values, and for slightly higher values of *r* oscillation among 6 values, then 12 etc.


1. Write code to project the population forward, tracking the population size at time t in the vector `x_t`. Include a parameter that allows to discard the first part of the time series, because we want to remove transient dynamics.

2. Write code to plot the time series (x-axis: time, y-axis: population size).

3. Use the following code to extract the inflection points (i.e., local maxima/minima) in the time series. Plot these values against the value of *r* used to generate the time series, obtaining a bifurcation diagram like


<img src="https://upload.wikimedia.org/wikipedia/commons/b/bd/Bifurcation_diagram_logistic_map_lambda_0_to_4.png" alt="image from the paper" width="500">

Code for extracting max/min:

```python
from scipy.signal import argrelextrema
def get_minmax(x_t):
    local_max = x_t[argrelextrema(x_t, np.greater)[0]]
    local_min = x_t[argrelextrema(x_t, np.less)[0]]
    return np.concatenate((local_max, local_min))
```

Here's a possible [solution](solutions/ch6_lm)

### Warmup exercise: nepotism and gender imbalance in Italian academia

Curiously, I have written two papers on this topic. Here we are going to use the data by Grilli and Allesina ([PNAS 2017](https://www.pnas.org/content/114/29/7600)) to find about gender inequality and nepotism in Italian professors. 

You find the data [here](https://github.com/StefanoAllesina/namepairs/tree/master/data)

1. Using `pandas`, read the `csv` file `ita_2000.csv` directly from the internet.

2. Take a look at the data: for each of the 52,000 Italian professors who were active in 2000, it reports a code for the first name, a code for the last name, the gender, rank, university and discipline. 

3. Take for example `Eng-Ind` (industrial engineering): there are 396 women and 3734 men (for a total of 4130 professors). Are there too few women? To test this, compute the probability of sampling less than 396 women out of 4130 professors when drawing them at random without repetition. Contrast this with what expected using the binomial distribution. 

4. Repeat for each discipline: which ones have an "excess" of women? Which ones a "scarcity"?

5. Try with the data for 2015: are things getting better?

6. Now turn to last names: a scarcity of last names in a discipline could signal nepotistic practices (as children would bear the same last name as the father). Take `Law`: there are 3721 professors, and we find 3037 names. Are these many names, or less than we would expect? Compute an approximate p-value for the probability of finding fewer names when sampling from the set of professors. 

7. Repeat for each discipline: which ones are most likely to be impacted by nepotism?

8. Try with the data for 2015: are things getting better?

Here's a possible [solution](solutions/ch6_nepo)

**Note:** This would be so much simpler if we could use `dplyr` in `R`; wait for Chapter 9!

