import csv # for handling csv files

# 1. Using python, read the file and extract the names of 
#    the countries involved in the experiment.

# Strategy: 
# - open the csv using DictReader for easy access
# - go through each row
# - store the countries in a set to exclude duplicates

# initialize data structure
countries = set([])
# open the file
with open("../data/Blake_et_al_Nature_2015.csv") as f:
    # create csv reader: each row is a dictionary
    dr = csv.DictReader(f)
    # for each row
    for row in dr:
        # add to countries without duplicates
        countries.add(row['country'])
# print result
print("Exercise 1")
for cc in countries:
    print(cc)
print("")


# 2. How many M, F for each country?

# Strategy:
# - data structure: dictionary of dictionary
#   example: {'Canada': {'M': 27, 'F': 34}}
# - start by initializing the dictionary
# - for each row:
#   - if the country is already present, update value
#   - if not, create the country and initialize at zero

gender_c = {}
# open the file
with open("../data/Blake_et_al_Nature_2015.csv") as f:
    # create csv reader: each row is a dictionary
    dr = csv.DictReader(f)
    # for each row
    for row in dr:
        # extract info
        my_country = row['country']
        my_gender = row['actor.gender']
        # the country is not present
        if my_country not in gender_c.keys():
           # add to dictionary
           gender_c[my_country] = {'M': 0, 'F': 0}
        # now update value based on gender
        gender_c[my_country][my_gender] = gender_c[my_country][my_gender] + 1
# print result
print("Exercise 2")
for key, item in gender_c.items():
    print("======", key, "======")
    for gender, value in item.items():
        print(gender, value)
print("")

# 3. For each country, select actors of age 10-12. 
#    Are females more likely than males to reject 
#    when the distribution is unequal and to their advantage?

# Strategy:
# - We need to consider only ages 10-12 and AI
# - For each country, we need to keep track of the
#   probability of accept/reject for M and F

# Data structure
# Let's use a dictionary of dictionaries of dictionaries
# e.g., 
# {'Canada': {'M': {'accept': 33, 'reject': 22}}

data = {}
with open("../data/Blake_et_al_Nature_2015.csv") as f:
    dr = csv.DictReader(f)
    for row in dr:
        # extract info
        my_country = row['country']
        my_gender = row['actor.gender']
        # cast as float 
        my_age = float(row['actor.age.years'])
        my_condition = row['condition']
        my_decision = row['decision']
        # filter the records
        # note that sometimes we have 'NA'
        if my_condition == 'AI' and my_age <= 12 and my_age >= 10 and my_decision != 'NA':
            # the country is not present
            if my_country not in data.keys():
                # add to dictionary
                data[my_country] = {'M': {'accept': 0, 'reject': 0},
                                    'F': {'accept': 0, 'reject': 0}}
            # update value
            data[my_country][my_gender][my_decision] = data[my_country][my_gender][my_decision] + 1

# results
print("Exercise 3")
for cc, item in data.items():
    print("======", cc, "======")
    print("prob accept M:", 
          round(item['M']['accept'] / 
          (item['M']['accept'] + item['M']['reject']), 3))
    print("prob accept F:", 
          round(item['F']['accept'] / 
          (item['F']['accept'] + item['F']['reject']), 3))
print("")

