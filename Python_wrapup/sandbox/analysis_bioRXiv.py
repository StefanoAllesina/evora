import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
get_ipython().run_line_magic('matplotlib', 'notebook')

data = pd.read_csv('bioRXiv.csv')

# 1. Document the change in number of submissions: plot the number of submission per week for each year, and check whether it's growing.

# transform to datetime
data.date = pd.to_datetime(data.date)

# now you can access week number
# data.date.dt.month
# and year
# data.date.dt.year
# create a new variable (numeric):
# 2018 + 3/12 is the 3rd month of 2018
month_year = pd.to_numeric(data.date.dt.year)  
month_year = month_year + pd.to_numeric(data.date.dt.month) / 12

# now count submissions per month
submission_month = month_year.groupby(month_year).size()

# and plot!
submission_month.plot()

# 2. Draw the distribution of the number of authors in the *Subject Area*. If people choose different areas, we can see whether different disciplines have different cultures.

bins = np.linspace(1, max(data['num_authors']) + 1, max(data['num_authors']) + 1)
data['num_authors'].hist(bins = bins)

data['num_authors'].mean()

data['num_authors'].median()

data['num_authors'].mode()

