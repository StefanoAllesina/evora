## Summary of Chapter 4

Writing good code in Python (or any other language!)

1. Divide the problem into small parts and write functions for each part
2. Check whether there are packages or module that can be handy
3. Split complex programs into modules
4. Write to be read
5. USE A DEBUGGER!
6. Unit testing to make sure everything is alright when you change something

### Functions

```python
def function_1(x):
    # x: function argument
    # code here
    y = x ** 3
    return(y) # optional
    
def default_args(x = 5, y = 'test'):
    # if user does not specify, use default
    return([x, y]) # you can return only one object!
```

### Importing packages and modules

```python
import scipy
import numpy as np # shorthand
from numpy import log # only one function
```

Discuss namespace pollution

### Warmup: Simple functions

1. Write a function that computes the area of a circle given its radius (*A = π r^2*).
2. Modify the function such that it can compute the area of a circle or a square (use an optional function argument to determine what to do).
3. The area of a regular polygon is half of the product between the *apothem* (line segment from the center to the midpoint of one of its sides), and the *perimeter*. Write a function to compute the area of regular polygons accepting as input the number of sides, and the length of the side. 
   * compute the apothem using trigonometry (*s* is the length of the side, *n* the number of sides):
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/7f3ca9bac6717a58814db4c9c4977e0d9f25133d" alt="apothem" width="100">
   * compute the perimeter
   * return the area
   * examples: area of pentagon with unit sides is about 1.72; area of hexagon with sides of length 2 is about 10.39

### Splitting program into modules

1. Save functions into a `py` file (e.g., `test_mod.py`)
2. Import functions into main program 

```python
import test_mod
# now you can access the functions
test_mod.my_function()
```

### Writing style

Extensive guide: [PEP8](https://pep8.org/)

Use comments judiciously:

- short comment to describe what the code is doing
- docstrings to document what the program is supposed to be doing, what is the input, what is the output

### Debugging

All you need to do is

```python
%pdb
```

It's that easy. No point in printing everything (as you PI probably does...)

- Set a random seed to reproduce the error
- Use breakpoints (if you're lazy, just type `1 / 0` where you want to break)
- Know your [error codes](https://docs.python.org/3/library/exceptions.html#bltin-exceptions) (they can really help!)

### Unit testing

- Unit testing is one of the most powerful techniques to ensure that your code is correct. 
- Scientific code is supposed to be correct (otherwise is not science!).
- It helps documenting the code extensively, and will facilitate extending the code.


### Warmup: Get admitted to UofC

U Chicago is famous for the witty essay questions for undergrad admissions. A recent, funny one:

> Due to a series of clerical errors, there is exactly one typo (an extra letter, a removed letter, or an altered letter) in the name of every department at the University of Chicago. Oops! Describe your new intended major. Why are you interested in it and what courses or areas of focus within it might you want to explore? Potential options include Commuter Science, Bromance Languages and Literatures, Pundamentals: Issues and Texts, Ant History... a full list of unmodified majors ready for your editor's eye is available [here](https://collegeadmissions.uchicago.edu/academics/areas-study).
—Inspired by Josh Kaufman, AB'18

- What is described is a form of *Levenshtein distance* (aka edit distance): you want to produce strings that at distance 1 from the original.
- Go [here](https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Python) and 'steal' a functions for computing the Levenshtein distance in Python (I have used version 6)
- Now we need a (long) list of English words. Use this code to get a list of 370,000 words:

```python
import urllib.request
# original url
url = 'https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt'
with urllib.request.urlopen(url) as f:
    words = f.readlines()
words = [word.decode('UTF8').strip() for word in words]
lengths = [len(word) for word in words]
```

- Now write a function that takes the name of a major (without punctuation) and:
  1. sets it to lowercase
  2. for each word with more than three characters, tries to substitute another word with distance exactly 1
  3. prints the "new" major
  
Good examples:

- Creative Waiting
- Ecology and Revolution
- International Delations
- Visual Tarts
- Cancel Biology

Here's the [solution](solutions/ch4)

