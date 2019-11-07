## Review of Chapter 8

**Assignment and data types:**

Use the assignment operator `<-` (`Alt + -`); the equal sign `=` works as well, but is deprecated.

```r
a <- 5 # create a variable called a and assign 5 to it
print(a)
```

To determine the type of a variable, use `class(a)`:

```r
class(a)
a <- "ciao" # note dynamic typing
class(a)
```

Basic data types in `R`:

- `character` (strings)
- `numeric` (real numbers)
- `integer` (integer numbers)
- `complex` (complex numbers)
- `logical` (`TRUE`, `FALSE`)
- `factor` (categorical values)

**Operators**

- `+ - * / ^` work as expected
- `x %% y` modulus
- `x %/% y`  integer division
- `x %in% y` test for membership

### Data structures

**Vectors**

Contain a one-dimensional array of values of the same type:

```r
v <- c(1, 2, 3, 4) # combine
# R starts counting at 1 (different from Python)
v[1]
v[1:2]
v[-1] # without the first element (different from Python)
v[c(1, 3)] # non-adjacent
v[c(TRUE, FALSE, TRUE, TRUE)] # using logical
v[v %in% c(1, 3)] # using membership
v[v < 3] # using logical condition
```

You can operate on whole vectors:

```r
sum(v)
prod(v)
v ^ 2
sqrt(v)
```

Length of a vector:

```{r}
length(v)
```

**Matrices and arrays**

Two- or multi-dimensional arrays containing data of the same type:

```r
m <- matrix(data = 1:9, nrow = 3, ncol = 3, byrow = TRUE)
m
```

Accessing rows/cols:

```r
m[1,] # first row
m[,1] # first col
# note that R drops dimensions automatically
is.matrix(m[1,])
# to prevent it
is.matrix(m[1, , drop = FALSE])
```

Multidimensional:

```r
aa <- array(data = 1:27, dim = c(3,3,3))
aa
```

**Lists**

Collection of objects indexed by position or name:

```r
ll <- list(v = c(1,2,3), n = c("a", "b", "c"))
ll
ll$v # $ like . in Python
ll$n
ll[[1]] # note the double brackets
ll[[2]][c(1,3)]
```

**Data frames**

Possibly, the most used data structure in `R`. Store spreadsheet-like data:

```r
df <- read.csv("Goldberg2010_data.csv",
               stringsAsFactors = FALSE, # by default, strings are treated as categorical values
               quote = "") 
# first few row --- tail(df) for the last few
head(df)
# structure
str(df)
# extract column
df$Species[1:2]
df[,"Species"][1:2]
df[1:2, 1]
# extract row by index
df[3:4,]
# extract row using logical operators
df[df$Species == "Acnistus_arborescens",]
df[df$Status == 2,]
```

### Reading and writing data

For `csv` files, use `read.table` (space/tab separated), `read.csv` (comma-separated), or `read.csv2` (semi-colon separated). `write.table` etc. write csv files.

Important options:

- reading: `stringsAsFactors = FALSE` read strings as `character` instead of factors
- writing: `row.names = FALSE` (do not write row numbers)

## Conditional branching

```{r
if (condition == TRUE){
  # this is executed when the condition is true
} else {
  # this when the condition is false
}
```

## Looping

`for` loop:

```r
for (i in a_vector_or_list){
  do_something(i)
}
```

Example:

```r
for (i in 2:10){
  print(c(i, i * (i - 1) / 2 ))
}
```

`while` loop:

```r
while (a_contidion_is_true){
  do_something()
  # update condition!
}
```


### User-defined functions

Anatomy:

```r
my_func <- function(arg1 = "default_value", arg2){
  # ...
  # body of the function
  # ...
  # return statement
  return(my_result)
}
```

### Special topics

[How to make sure your code is readable and easy to share.](readability)

[How to make sure your code is correct.](unit_testing)


### Warmup exercise: TED Talks

For our warmup, we are going to use a spreadsheet with information on 992 TED talks. The data were adapted from

> Kinnaird, Katherine M. and John Laudun. 2018. [TED Talks Data Set](https://github.com/johnlaudun/tedtalks/data).

- Plot an histogram for the number of views. Is the distribution approximately log-normal?

- Transform the `duration` to seconds

**Hint:** Look [here](https://stackoverflow.com/questions/29067375/r-convert-hoursminutesseconds)

- Plot duration in seconds vs. log number of views: does duration correlate with views?

- Count the number of days since publication, and plot against log views

**Hint:** Look [here](https://stackoverflow.com/questions/5261347/finding-days-since-date-in-r-from-csv-file)

- Find the top 10 tags

- For each top tags, add a column to the data frame specifying if the tag is present

**Hint:** you could use the function `grepl`

- Build a linear model with 
  - Response variable = log(views)
  - Explanatory variables = published_days, seconds, technology, science, culture, etc.
  - Which tags significantly increase views?

**Hint:** Look [here](https://stackoverflow.com/questions/7742301/using-column-numbers-not-names-in-lm)

- Add to the model the effect of the top 10 speakers by number of talks. Does this improve the fit?

Here's a possible [solution](solutions/ch8)
