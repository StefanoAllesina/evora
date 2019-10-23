## Summary of Chapter 3

- Programming in Python

- Many programming languages are available. Pick at least 3:
     - One for prototyping, simple tasks (my lab: `Python`)
     - One for stats and plotting (my lab: `R`)
     - One for large-scale simulations and computing-intensive tasks (my lab: `C`)

- We use jupyter, because it looks the same on any architecture

- Open a terminal, go to the `week2/sandbox`, and type `jupyter notebook`

### Assignment

Take whatever is on the right and put it in a "box"; label the box with the name on the left.

```python
x = 6 # create a box containing the int 6 and call it x
```

How do we test for equality?

```python
2 * 3 == 6
```

### Basic data types

- To determine the type of a variable, call `type(x)`
- `int`: integers (positive or negative) `x = 6`
- `float`: floating-point representation of real numbers `x = 6.0`
- `complex`: complex numbers `x = 3 + 2j`
- `str`: characters or strings `x = 'ciao'`
- `bool`: `True/False` (Boolean) `y = 7 > 5`


### Operators

```python
x = 5
y = 3

x + y # addition
x - y # subtraction
x * y # multiplication
x / y # division
x ** y # exponentiation x^y
x // y # integer division
x % y # modulo (remainder integer division)

x == y # equals
x != y # differs
x > y # greater than
x <= y # lower or equal

a = True
b = False

a & b # and
a and b
a | b # or
a or b
!b # not b
not b

a in b # test for membership

# precedence
2 * 3 ** 3 == 54
# better use parentheses
(2 * 3) ** 3 == 216
```

### Types in Python

- Dynamic typing: the type of a variable is determined at runtime. 
- Casting: you can change the variable type (if sensible).
- Object oriented: each type comes with a set of functions (*methods*).
- To see the variables (and functions) you've created, type `who`

```python
x = 8 # now x is an integer
x = x / 2
print(x)

y = "77" # this is a string
x + int(y) # cast to integer

x = "a string"
print("now x is", x)

# see all methods available for stings
dir(x)
x.upper()
```

### More on strings

```python
s = "my Test string"
len(s) # num characters
s + " is not longer" # concatenation
s.replace("my", "your") # replacement
s.split(" ") # split into list of strings
" space around string ".strip() # remove space on both ends
s.upper() # to uppercase
s.lower() # to lowercase
";".join(["a", "b", "c"]) # concatenate list of strings
```

### Data structures

List: ordered collection of values

```python
li = ["a", 13, True, ["a", "b"]]
li2 = list("abcdefg")
# indexing starts at 0
li[0]
li[4] # error!
# slice
li[:2]
# negative indices
li[-3:]
# mutable: you can overwrite values
li[3] = "bb"
# extend list
li.append(25)
# sort will sort in place
sort(li)
# combine lists
li + li2
# find element
li.index("a")
# insert/remove
li.insert(3, "to add")
li.remove("a")
# remove and return last element
x = li.pop()
# count occurrences
li.count(13)
# reverse in place
li.reverse()
```

Dictionaries: most pythonic data structure; key-value pairs.

```python
d = {'first': 'first value', 'second':[1,2]}
d.keys() # access keys
d.values() # access values
# assign a new key-value pair
d['third'] = 'we can add a new value'
# overwrite previous values
d['third'] = 'another value'
# remove a key and assign value
st = d.pop('third')
# join two dictionaries
d2 = {'1': 1, '2': 2}
d.update(d2)
# the order of keys is arbitrary!
# anything "immutable" can serve as key
d[1] = '1'
```

Tuples: immutable lists

```python
tu = (1, 1, 'two', 3, 'five')
# access elements as in list
tu[0]
tu[-1]
# count occurrences
tu.count(1)
# find index
tu.index('two')
```

Sets: no duplicates

```python
# create a set 
s1 = set([1, 1, 2, 4, 8, 8, 16])
s2 = {2, 3, 5, 7, 11, 13}
# intersection
s1 & s2
s1.intersection(s2)
# union 
s1 | s2
s1.union(s2)
# difference
s1 ^ s2
s1.symmetric_difference(s2)
# subset and superset
s3 = {1, 3, 8}
s3.issubset(s1 | s2)
(s1|s2).issuperset(s3)
```

Summary of data structures:
```python
(1, 2) # tuple
{1, 2} # set
{1: 1, 2: 2} # dictionary
[1, 2] # list
```

### Some built-in function

```python
## Mathematical
abs(x) # absolute value
max(x) # largest value (input string, list, set, tuple, ...)
min(x) # smallest value
round(x, 3) # rounding
sum(x) # sum all values

## Casting
bool(x) # to Boolean
complex(x) # to complex number
float(x) # to floating point
int(x) # to integer
str(x) # to string

## Useful functions
all(x) # returns True if all elements are True
any(x) # returns True if any element is True
range(1, 5, 2) # generates a range of values
```

### Branching

One of the pillars of programming: depending on the state of a variable, take alternate paths in the code. "Choose your own adventure!"

*If you decide to fight the squid with your spear gun, hoping to scare it off, turn to page 17.*

*If you decide to signal Maray to pull you up at top speed, knowing you will get the bends, turn to page 19.*

```python
if squid:
   page = 17
elif signal:
   page = 19
else:
   page = 3245
```

### Looping

Another pillar of programming: repeat the same code, possibly with small variations.

First, we need to see the function `range`:

```python
list(range(5))
# [0, 1, 2, 3, 4]
list(range(1, 7 ,2)) # from, to, step
# [1, 3, 5]
```

Now we can write a for loop
```python
for i in range(10):
   print(i)
```

Use `for` loops to iterate over a list, a string, or a tuple. If you want to go on until a condition is met, use `while`:

```python
x = 12345
while x % 266 != 0:
  x = x + 1
print(x)
```

### Reading and writing files

Input/output:

- text files
- structured text files (e.g., `csv`)
- binary files (e.g., `mp3`)

Flow:
- open a file (for writing, reading, or appending)
- read/write 
- close the file

Example:
```python
f = open('test.txt', 'w') # r for reading, a for appending
# now f is a file object
f.write('ciao\n') # \n is the line terminator in Linux
f.close()
```

Automatically close file when you're done:

```python
with open('test.txt', 'w') as f:
    f.write('ciao\n')
```

Useful library to manipulate `csv` files:

```python
import csv

with open('test.csv', 'r') as f:
    # read as dictionary:
    # the header defines the keys
    # very handy: you don't have to 
    # remember the position of the cols
    rd = csv.DictReader(f)
    for row in rd:
       for key, value in row.items()
           print(key, value)
```

### Warmup: Fairness in seven societies

Blake *et al.* ([Nature 2015](https://www.nature.com/articles/nature15703)) studied how the sense of fairness develops in different cultures. They conducted experiments to test whether and when children became adverse to disadvantageous inequity (peer receives more than self) and advantageous inequity (self receives more than a peer). Their data support the claim that disadvantageous inequity aversion emerged across all populations by middle childhood, while advantageous inequity aversion is more variable.

Experimental design:

<img src="https://media.nature.com/lw926/nature-assets/nature/journal/v528/n7581/images/nature15703-f1.jpg" alt="image from the paper" width="500">

Description:
- the "actor" on the left is presented with an allocation (of candies); they can accept/reject it acting on the levers
- the top is a situation in which the actor faces *disadvantageous inequity*; the bottom *advantageous inequity*

The data (taken from [Data Dryad](https://datadryad.org/resource/doi:10.5061/dryad.g3925)) are stored in `Ch3/data/Blake_et_al_Nature_2015.csv`. The csv file contains several columns:

- `condition`: the treatment (`AI` or `DI`).
- `eq.uneq`: was the distribution equal (`E`) or unequal (`U`)?
- `decision`: did the actor `accept` or `reject`?
- `country`: the country of the actor.
- `actor.id`: identifier for the actor (each actor did several trials).
- `actor.age.years`, `actor.gender`: age and gender of the actor.
- `dist`, `trial`: each of 16 trial was associated with a distribution. For `AI`, actor-recipient were either 1-1 or 4-1; for `DI` either 1-1 or 1-4; each was repeated four times.
- `value`: children were tested with `high` or `low` rewards (e.g., for Canada, Skittles vs. Goldfish crackers)


1. Using python, read the file and extract the names of the countries involved in the experiment.
2. How many `M`, `F` for each country?
3. For each country, select actors of age 10-12. Are females more likely than males to reject when the distribution is unequal and to their advantage?

Here's the [solution](solutions/ch3)

