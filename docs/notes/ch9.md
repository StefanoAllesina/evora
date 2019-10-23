## Summary of Chapter 9

- The `tidyverse` provides a set of packages for the organization, manipulation and visualization of large data sets.
- `tidyverse` "an opinionated collection of R packages designed for data science. All packages share an underlying design philosophy, grammar, and data structures."
- Based on the pipeline operator `%>%` (type `Ctrl + Shift + M`): write code that is easy to read and debug.

You can load all the essential packages in the bundle running

```r
library(tidyverse)
```

### Data: top 500 albums according to "Rolling Stone"

```r
# This is a better routine to load large data sets
# part of package readr
dt <- read_csv("https://goo.gl/W4SSdj", 
               locale = locale(encoding = "latin1"))
```

### How to take a look around

```r
head(dt) # first few rows
tail(dt) # last few rows
glimpse(dt) # structure
View(dt) # spreadsheet
```

### Pipelines

Example: number of top 500 rock albums by *The Beatles*:

```r
# lots of dollar signs
nrow(dt[dt$Artist == "The Beatles" & dt$Genre == "Rock",])
```

Decade with the highest number of rock top 500 albums

```r
# the nested code quickly becomes difficult to read
as.integer(names(sort(table(dt$Year[dt$Genre == "Rock"] %/% 10), 
                      decreasing = TRUE)[1])) * 10
```

Solution: pipelines `%>%` (`Ctrl + Shift + M`) allow you to "unroll" your code:

```r
# Easy to read code. No dollar signs. Easy to add/remove pieces
dt %>% 
  filter(Genre == "Rock", Artist == "The Beatles") %>% 
  tally()
```

```r
dt %>% 
  filter(Genre == "Rock") %>% 
  mutate(decade = (Year %/% 10) * 10) %>% 
  group_by(decade) %>% summarise(tot_albums = n()) %>% 
  arrange(desc(tot_albums))
```

### Subsetting rows

```r
# using logical criteria
dt %>% 
  filter(Artist == "The Beatles", Year > 1968)
# at random
dt %>% 
  sample_frac(0.01)
dt %>% 
  sample_n(3)
# by position
dt %>% 
  slice(1:3)
# ordering by a col
dt %>% 
  top_n(2, Year)
dt %>% 
  top_n(1, desc(Artist))
```

### Subsetting cols

```r
dt %>% 
  select(Artist) %>% 
  head(3)
# by name (regex etc)
dt %>% 
  select(starts_with("A")) %>% 
  head(2)
# distinct rows
dt %>% select(Artist) %>% distinct()
```

### Ordering 

```r
dt %>% arrange(Artist) %>% head(3)
dt %>% arrange(desc(Artist), Year) %>% head(3)
```

### Adding new columns

```r
# as a function of current cols
dt %>% mutate(decade = (Year %/% 10) * 10) %>% head(3)
# ex novo
dt %>% add_column(I_have_it = NA)
# renaming col
dt %>% rename(yr = Year)
```

### Summaries

```r
# distribution of length band name
dt %>% 
  select(Artist) %>% 
  mutate(namelen = nchar(Artist)) %>% 
  summarise(min = min(namelen),
            mean = mean(namelen), 
            median = median(namelen),
            stdev = sd(namelen),
            max = max(namelen))
```

### Groups
The true strength of this approach is the possibility of grouping observations, and perform operations based on these groups:

```r
# Number of top albums by genre
dt %>% group_by(Genre) %>% tally() %>% arrange(desc(n))

# Order of top albums by band
dt %>% 
  select(Artist, Year) %>% 
  arrange(Year) %>% 
  add_column(tmp = 1) %>% 
  group_by(Artist) %>% 
  mutate(num = cumsum(tmp)) %>% 
  filter(Artist %in% c("The Beatles", "Bob Dylan"))

# Number of albums by Genre/Year
dt %>% group_by(Year, Genre) %>% tally()
```

### From messy tables to tidy and back

Goal: produce a heatmap with rows -> Artists, cols -> Years and color each configuration with a top album

```r
for_heatmap <- dt %>% 
  select(Artist, Year) %>% 
  distinct() %>% 
  add_column(tmp = 1) %>% 
  spread(Year, tmp, fill = 0) %>% 
  gather(Year, Top, -Artist)
```

### Plotting

```r
for_heatmap %>% 
  ggplot()+ aes(x = Year, y = Artist, fill = as.factor(Top)) + geom_tile() + scale_fill_manual(values = c("white", "black"))
```

### Warmup: Baby names

We are going to analyze data from the Social Security Administration on baby names. To load data on baby names, run

```r
library(tidyverse)
# read the data
dt <- read_csv("https://tinyurl.com/ycc4ndkd") %>% select(-state)
```

The data are composed of about 1.4M rows. Each row reports a the number of times (`count`) a name was given to boys (`sex == M`) or girls (`F`) in a certain `year`. Names occurring less than 5 times per gender are not reported. The data spans 1980 to 2015. 

1. Find the number of distinct names for boys and girls.
2. Find the name/year combination with highest `count` for each `sex`.
3. Plot the number of distinct names for `M` or `F` in time.
4. As you can see, the diversity of names has been growing. Plot the number of "effective" names in time. First, compute Shannon's entropy for each year/sex. Then exponentiate to obtain the number of names that would produce the same entropy while having equal frequency.
5. Plot the difference in number of distinct names between `F` and `M` in time.
6. Find the frequency with wich each name ends with a certain letter. Show a barplot for the probability that a name (for boys or girls) ends with a given letter. Use years 1950, 1975, 2000 and 2015.
7. Plot the popularity of each letter for ending names in time, for boys and girls. Which letters are growing, which declining?
8. Find the most popular names today that were not present in the 1950 data, and viceversa.
9. Check out some interesting trends: what happens to Neo after "The Matrix" (1999), Madonna after "Like a Virgin" (1983), and to Elvis after the debut (1956) and death (1977), Hermione after the first Harry Potter movie (2001), Diana after the royal wedding (1981) and death (1997), etc.?

Here's a possible [solution](solutions/week8)

