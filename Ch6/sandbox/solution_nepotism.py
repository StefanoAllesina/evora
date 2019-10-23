import pandas as pd
import numpy as np
import scipy.stats # for binomial


# 1. Using `pandas`, read the `csv` file `ita_2000.csv` directly from the internet.

url = 'https://raw.githubusercontent.com/StefanoAllesina/namepairs/master/data/ita_2000.csv'
data = pd.read_csv(url)


# 2. Take a look at the data: for each of the 52,000 Italian professors who were active in 2000, it reports a code for the first name, a code for the last name, the gender, rank, university and discipline. 

print(data)

# 3. Take for example `Eng-Ind` (industrial engineering): there are 396 women and 3734 men (for a total of 4130 professors). Are there too few women? To test this, compute the probability of sampling less than 396 women out of 4130 professors when drawing them at random without repetition. Contrast this with what expected using the binomial distribution. 

# We're going to write a function, because we need to repeat this for all disciplines below

def prob_women(data, discipline = "Eng-Ind", nrep = 1000):
    # count number of women in discipline
    data_disc = data[data['sector'] == discipline]
    observed = sum(data_disc['gender'] == 'F')
    total = data_disc.shape[0]
    prob = 0.0
    for i in range(nrep):
        sampled = sum(data.sample(n = total)['gender'] == 'F')
        if sampled <= observed:
            prob = prob + 1.0
    prob = prob / nrep
    # now compute probability according to binomial
    # q is the proportion of women
    q = sum(data['gender'] == 'F') / data.shape[0]
    # this is the exact probability
    binom_prob = scipy.stats.binom.cdf(observed, total, q)
    return {"discipline": discipline, 
            "F": observed,
            "tot": total,
            "prob": round(prob, 3),
            "binom_prob": round(binom_prob, 3)
            }

prob_women(data, discipline = "Eng-Ind", nrep = 1500)


# 4. Repeat for each discipline: which ones have an "excess" of women? Which ones a "scarcity"?

# extract all disciplines
disciplines = set(data['sector'])

for d in disciplines:
    tmp = prob_women(data, d, 2500)
    print("===========")
    for key, val in tmp.items():
        print(key, val)
    print("===========")


# 6. Now turn to last names: a scarcity of last names in a discipline could signal nepotistic practices (as children would bear the same last name as the father). Take `Law`: there are 3721 professors, and we find 3037 names. Are these many names, or less than we would expect? Compute an approximate p-value for the probability of finding fewer names when sampling from the set of professors. 

# Again, we are going to write a function, so that it is easy to run this for each discipline.

def prob_last_names(data, discipline = "Law", nrep = 1500):
    data_disc = data[data['sector'] == discipline]
    observed = len(set(data_disc['last_id']))
    total = data_disc.shape[0]
    prob = 0.0
    for i in range(nrep):
        sampled = len(set(data.sample(n = total)['last_id']))
        if sampled <= observed:
            prob = prob + 1.0
    prob = prob / nrep
    return {"discipline": discipline, 
            "observed": observed,
            "tot": total,
            "prob": round(prob, 3)
            }


prob_last_names(data)


# 7. Repeat for each discipline: which ones are most likely to be impacted by nepotism?

for d in disciplines:
    tmp = prob_last_names(data, d, 2500)
    print("===========")
    for key, val in tmp.items():
        print(key, val)
    print("===========")

