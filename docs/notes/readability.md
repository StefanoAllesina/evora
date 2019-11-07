Writing code that other scientists can read (and understand)
================
Stefano Allesina
Nov 2019

-   [Great resources:](#great-resources)
-   [Some advice from *Wilson et al. 2017*](#some-advice-from-wilson-et-al.-2017)
-   [Organization of data analysis](#organization-of-data-analysis)
-   [Style Guide](#style-guide)

Great resources:
----------------

-   Wilson G, Aruliah DA, Brown CT, Chue Hong NP, Davis M, Guy RT, et al. (2014) [*Best Practices for Scientific Computing*](https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1001745). **PLoS Biol** 12(1): e1001745.

-   Wilson G, Bryan J, Cranston K, Kitzes J, Nederbragt L, Teal TK (2017) [*Good enough practices in scientific computing.*](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005510) **PLoS Comput Biol** 13(6): e1005510.

-   British Ecological Society (2017) [*A Guide to Reproducible Code in Ecology and Evolution*](https://www.britishecologicalsociety.org/wp-content/uploads/2017/12/guide-to-reproducible-code.pdf)

Some advice from *Wilson et al. 2017*
-------------------------------------

**Data management**

-   Save the raw data. *\[and do not modify it!\]*
-   Ensure that raw data are backed up in more than one location.
-   Create the data you wish to see in the world.
-   Create analysis-friendly data. *\[good for computing ![\\neq](https://latex.codecogs.com/png.latex?%5Cneq "\neq") good for humans\]*
-   Record all the steps used to process data.
-   Anticipate the need to use multiple tables, and use a unique identifier for every record.
-   Submit data to a reputable DOI-issuing repository so that others can access and cite it.

**Software**

-   Place a brief explanatory comment at the start of every program.
-   Decompose programs into functions.
-   Be ruthless about eliminating duplication.
-   Always search for well-maintained software libraries that do what you need.
-   Test libraries before relying on them.
-   Give functions and variables meaningful names.
-   Make dependencies and requirements explicit.
-   Do not comment and uncomment sections of code to control a program's behavior.
-   Provide a simple example or test data set.
-   Submit code to a reputable DOI-issuing repository. *\[e.g., OSF, GitHub, BitBucket\]*

**Collaboration**

-   Create an overview of your project.
-   Create a shared "to-do" list for the project. *\[GitHub offers wiki and issue tracking\]*
-   Decide on communication strategies.
-   Make the license explicit.
-   Make the project citable.

**Project organization**

-   Put each project in its own directory, which is named after the project. *\[exception: data too large\]*
-   Put text documents associated with the project in the `doc` directory.
-   Put raw data and metadata in a `data` directory and files generated during cleanup and analysis in a results directory.
-   Put project source code in the `src` directory.
-   Put external scripts or compiled programs in the `bin` directory.
-   Name all files to reflect their content or function.

**Manuscripts**

-   Write manuscripts using online tools with rich formatting, change tracking, and reference management.
-   Write the manuscript in a plain text format that permits version control.

Organization of data analysis
-----------------------------

To ensure reproducibility, **automate the whole analysis**. You should be able to replicate the whole analysis with few commands.

Divide the project into chunks. Write a program for each chunk. String the chunks together in a master program.

Avoid absolute paths; use relative paths instead.

``` r
# This will work on Bill's computer
temperature <- read_csv("C:/Users/Bill_Gates/Desktop/tmp/tmp2/mydata.csv")

# This works if one copies the project folder
temperature <- read_csv("../../data/temperature_data/temp_01_2018.csv")
```

Split the program into coherent parts. Each part has its own file.

    01_download_data.R
    02_clean_data.R
    03_statistical_analysis.R
    04_plotting.R
    05_build_tables.R
    master.R

Start each file with a short description. Import packages first, then your own code.

``` r
# -------------------------------------------------------------------
# Maximum likelihood of niche model
# Stefano Allesina [sallesina@uchicago.edu], Oct 2017
# ...
# ...
# -------------------------------------------------------------------
library(igraph)
library(tidyverse)
# will install if not installed already
if(!require(ggthemes)){
    install.packages("ggthemes")
    library(ggthemes)
}
# -------------------------------------------------------------------
source("general_functions.R")
source("network_plotting.R")
```

If you are copying and pasting code, there's something wrong.

This is your best friend:

``` r
rm(list = ls())
```

Style Guide
-----------

See Hadley Wickham's suggestions at <http://adv-r.had.co.nz/Style.html>

Google's `R` style guide <https://google.github.io/styleguide/Rguide.xml>

### Naming files

You want to name files so that it is obvious what they're supposed to contain.

-   All files containing `R` code should end in `.R`. Use `.RData` for data structures saved within `R`.
-   Use `_` to separate words
-   If they are meant to be run in sequence, number them
-   Avoid names that are too long

<!-- -->

    # Good examples

    organize_data.R
    fit_model.R
    plot_results.R

    0_download_data.R
    1_organize_data.R
    2_fit_models.R

    # Bad examples

    stuff.txt
    final_2_revised.R
    misc

### Naming objects

Again, try to make it apparent what type of data/function this might be. Use verbs for functions, and nouns for variables. Be expressive. Autocompletion means you can use slightly longer names.

-   Use lowercase for variables and functions
-   Separate words using underscore
-   You can use all UPPERCASE for constants
-   Do not call your variables/functions with the name of built-in functions
-   Use `i,j,k` for counters, `x,y,z` for coordinates
-   Do not call your variables `l,I,o,O`

``` r
# Good:
body_mass <- 0.003 # grams
temperature <- 278 # Kelvins

compute_GC_content <- function(DNA){
  #...
}

plot_survival_analysis <- function(surv_data){
  #...
}
```

``` r
# Bad
mean <- mean(runif(10))
o <- O * I

for (ping in 1:10){
  for (pong in 2:21){
    #...
  }
}
```

### Spacing

-   Put space around operators
-   Only exceptions `1:5`, `dplyr::mutate()`, `dplyr:::bad_cols()`
-   Place a space to the left of each `(`: `a <- (x + y) ^ z`
-   No space when calling a function `a <- sd(x)`
-   No space before square bracket `a <- x[3]`
-   Place a space after each comma
-   Indent code between curly braces:

``` r
for (i in 1:100) {
  # check whether it's even
  if (i %% 2 == 0) {
    print(paste(i, "is even!"))
  }
}
```

-   Indentation: never mix Tabs and spaces. RStudio automatically inserts (2) spaces when you press tab.

### Assignment

Use `<-` for assignment `Alt + -` in RStudio. Use `=` for default values and function arguments.

### Invoking functions

Include the names of the arguments (you can press `Tab` for autocompletion)

``` r
heigths <- rnorm(100, 160, 10)
heights <- rnorm(n = 100, mean = 160, sd = 10)
```

### Comments

``` r
# General comments have their own line(s)
# They are used to explain *WHAT* the code is doing
# E.g. Fit the model of Newton et al. 2001

# Inline comments are used to explain *HOW* the code is doing that
# A comment is not a translation of the code into English
x <- y + z  # sum two numbers
# Rather, it is used to explain passages that might be mysterious otherwise
x <- y[i + 1]  # i is indexed starting at 0, hence the i+1

# Use a special type of lines to separate parts of the program
# ------------------------------------------------------------------
# Main functions
# ------------------------------------------------------------------
```

### Functions with multiple arguments

Sometimes, it is better to split the arguments over multiple lines:

``` r
get_phenotype_dynamics <- function(pars, # parameter object, see get_params
                                   random_seed = 100, 
                                   tot_time = 100, # time of integration
                                   step_time = 0.1 # step for reporting
                                   ){
  # ...
}
```

Similarly, when you are returning a list:

``` r
my_function <- function(){
  # ...
  return(list(
    H = H, # the matrix of probabilities
    membership = membership, # the vector of memberships
    random_seed = random_seed, # seed used to generate the data
    p = p # the parameter controlling the skewness
  ))
}
```
