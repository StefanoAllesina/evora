## Summary of Chapter 5

- Regular expressions allow to extract text that matches a pattern
- You write the pattern using a special notation (metacharacters)
- You use the pattern to search in the text, producing a "match" or extracting all occurrences

Uses:

- Parse semi-structured files
- Extract information from free text
- More sophisticated find-replace
- ...

Strengths:

- One line of code can do a lot!
- Very fast
- Well-implemented in Python (cfr. R)

Weaknesses:

- Very difficult to debug
- Can be hard to write the "perfect" regex

What you want to be doing: 

<img src="https://imgs.xkcd.com/comics/regular_expressions.png" alt="regex hero" width="700">

What you don't want to be doing: 

<img src="https://pbs.twimg.com/media/Cr7mS_OWcAA7Hzt.jpg:large" alt="regex by trial and error" width="700">

### Basics

```python
import re # need to load module
# "literal" match --- match exactly what you wrote
re.findall(r'a', 'ciao') # returns ['a']
re.findall(r'99', '1099099') # returns ['99', '99']
```

### Metacharacters

- `\n` match a new line
- `\t` match a tab
- `\s` match a space
- `\d` match a digit (0123456789)
- `\w` match a "word character" (a-z, A-Z, _, 0-9)
- `.` match any character (note: no backslash --- to match a dot use `\.`)
- `[abc87]` match any of the characters between the brackets
- `[a-z]` match any character between `a` and `z`
- `[^ATGC]` do not match characters `ATGC`
- `\b` match word boundary (used to detect full words)

```python
re.findall(r'\w\n\w', 'This is a line\nThis is another line')
# returns ['e\nT']

re.findall(r'\d\.\d.', '12.3456')
# returns ['2.34']
```

### Quantifiers

- `?` match 0 or 1 time
- `*` match 0 or more times
- `+` match 1 or more times
- `{5}` match exactly 5 times
- `{5,}` match at least 5 times
- `{5, 7}` match between 5 and 7 times (included)

```python
re.findall(r'\d*\.\d{,2}', '123.45678')
# returns ['123.45']
re.findall(r'\d*\.\d*', '12345678')
# returns []
```

### Anchors

- `$` match at the end of the line
- `^` match at the beginning of the line


### Alternations

- `a|b` match either `a` or `b`

### Raw strings

The `r` in front of the string means "do not interpret special characters". Note the difference between:

```python
print('my long \n line')
```

which returns
```
my long
 line
```
and 
```python
print(r'my long \n line')
```
which returns
```
my long \n line
```

### Groups

Groups are defined by round brackets. The can be used to:

- Organize your match (i.e., match separate pieces)

```python
re.findall(r'(\d+\.?\d*)\s[\+\-]\s(\d+\.?\d*)i', '50.3 + 25i')
```
```
[('50.3', '25')]
```

- To capture text flanked by known regions

```python
re.findall(r'Error: ([\w\s\_]+)', 'Error: page not found --- terminate')
```
```
['page not found ']
```

- To build complicated parts that would be difficult to write otherwise (e.g., complex alternations)

### Functions in re

- `re.findall` returns a list of matches (groups are organized in a tuple)
- `re.search` finds a match; returns a match object
- `re.compile` stores the pattern, improving the speed
- `re.split` split lines according to regex
- `re.sub` substitution guided by regex

### Greedy vs non-greedy
- `? * +` are all greedy: they match as much as possible
- to make them "reluctant", add a `?`

```python
re.match(r'.*\s', 'a sentence with multiple words').group()
'a sentence with multiple '

re.match(r'.*?\s', 'a sentence with multiple words').group()
'a '
```

### Warmup exercise

We are going to exploit a small ~~bug~~ feature of Scopus.

Here's how to get the data for a given university (the file is provided in the sandbox for University of Chicago):

- Go to [Scopus.com](https://www.scopus.com)
- Click on "Affiliations"
- Search for "University of Chicago"
- Click on "University of Chicago"
- Click on the number of Authors in the top-right panel
- The page should show the twenty most prolific authors with an affiliation to U of C
- Go to the URL bar in your browser, and modify the address: instead of `resultsPerPage=20&` use `resultsPerPage=2000&`
- Now you have information on 2000 authors!
- Save the page in your sandbox as `scopus.html`
- You are now the proud owner of a list of the 2000 most prolific authors at the University

1. For each author, use regular expressions to extact:
   - Name
   - Number of papers
   - The first in the list of disciplines
   
2. Which discipline is most represented? Take the list of authors you've just produced, and tally the number of authors by discipline.

3. What is the average number of papers for these authors?

Here's the [solution](solutions/ch5)

**Note:** We have parsed HTML using regex, which is completely unnecessary; there are specialized tools for this task.


