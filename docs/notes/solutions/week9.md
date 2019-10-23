## Possible solution to warmup problem Week 9

```r
library(tidyverse)

# 1. Write a function that accepts a filename for the edges, and one for the vertices, 
# and performs the cleaning operations outlined above. The function should return a list 
# with a `tibble` called `vertices` and another called `edges` Note however that the 
# names of the columns vary among data sets! Can you find a way to rename them anyway?
  
clean_data <- function(filevertices = "../data/Dataset 2. Business_vertexlist.txt", 
                       fileedges = "../data/Dataset 1. Business_edgelist.txt"){
  vertices <- read_tsv(filevertices)
  edges <- read_tsv(fileedges)
  vertices[vertices == "."] <- NA
  vertices <- vertices %>% rename(id = `# u`)
  # solution from
  # stackoverflow.com/questions/43578723/conditional-replacement-of-column-name-in-tibble-using-dplyr
  vertices <- vertices %>% rename_all(~sub('NRC.*', 'nrc', .x))
  vertices <- vertices %>% rename_all(~sub('USN.*', 'usn', .x))
  vertices <- vertices %>% rename(region = Region)
  vertices$usn <- as.integer(vertices$usn)
  edges <- edges %>% rename(from = `# u`, to = "v")
  # reorder academic ranks
  edges <- edges %>% mutate(rank = factor(rank, levels = c("Asst", "Assoc", "Full")))
  return(list(vertices = vertices,
              edges = edges))
}

# 2. Run the data cleaning on the three data sets, and store the lists in 
# `cs`, `business` and `histry` (no `o` otherwise you rename the command `history`)

cs <- clean_data("../data/Dataset 4. ComputerScience_vertexlist.txt",
                 "../data/Dataset 3. ComputerScience_edgelist.txt")

business <- clean_data("../data/Dataset 2. Business_vertexlist.txt",
                       "../data/Dataset 1. Business_edgelist.txt")

histry <- clean_data("../data/Dataset 6. History_vertexlist.txt",
                     "../data/Dataset 5. History_edgelist.txt")

# 3. Produce a barplot of the count of M/F hires for each rank. 
# In which field to we have the less gender imbalance? 
# Is the proportion of women increasing in time?

for (dt in list(cs, business, histry)){
  show(ggplot(data = dt$edges) + aes(x = rank, fill = gender) + geom_bar(position = "dodge"))
}

# 4. For each discipline, tally the number of times each institution is the PhD granting 
# institution (`feeder`). Join with the vertices table to get the name of the institution.

count_feeder <- function(dt){
  counts <- dt$edges %>% group_by(from) %>% tally()
  counts <- counts %>% left_join(dt$vertices %>% rename(from = id))
  counts <- counts %>% filter(institution != "All others")
  counts <- counts %>% arrange(desc(n))
  # use factors so that they remain in order when plotted
  counts <- counts %>% mutate(institution = factor(institution, unique(institution)))
  return(counts)
}

cs_feeder <- count_feeder(cs)
business_feeder <- count_feeder(business)
histry_feeder <- count_feeder(histry)

# 5. Write a function that plots the cumulative distribution
# of number of hires --- do few institutions contribute 
# disproportionaltely?

get_cumulative <- function(feeders){
  pl <- ggplot(feeders) + 
  aes(x = institution, y = cumsum(n / sum(n))) + 
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust = 1))
  return(pl)
}

show(get_cumulative(cs_feeder) + ggtitle("Computer science"))
show(get_cumulative(business_feeder) + ggtitle("Business"))
show(get_cumulative(histry_feeder) + ggtitle("History"))

# 6. Compute the Gini coefficient for the number of hires from each institution. 
# How does it compare with economic inequality?

# a simple solution would be to install the package "ineq" and use the function 
# provided by the package

# another solution uses some linear algebra
compute_Gini <- function(x){
  n <- length(x)
  M <- matrix(x, n, n)
  G <- sum(abs(M - t(M))) / (2 * sum(x) * n)
  return(G)
}

print(paste("Gini index CS:", compute_Gini(cs_feeder$n)))
print(paste("Gini index Business:", compute_Gini(business_feeder$n)))  
print(paste("Gini index History:", compute_Gini(histry_feeder$n)))  

# 7. For each institution, compute the proportion of hires with a PhD from a given institution

get_proportions <- function(dt){
  proportion <- dt$edges %>% group_by(from, to) %>% tally() %>% ungroup() %>% 
    group_by(to) %>% mutate(prop = n / sum(n))
  # now add labels
  proportion <- proportion %>% left_join(dt$vertices %>% select(id, institution) %>% 
                                         rename(from = id, from_inst = institution))  
  proportion <- proportion %>% left_join(dt$vertices %>% select(id, institution) %>% 
                                           rename(to = id, to_inst = institution))  
  proportion <- proportion %>% arrange(to, desc(prop)) %>% ungroup()
  return(proportion)
}

cs_prop <- get_proportions(cs) 
business_prop <- get_proportions(business)
histry_prop <- get_proportions(histry)

# 8. For each discipline, find the 10 most "inbred" institutions
# (i.e., those hiring their own graduates in higher proportion)

for (dd in list(cs_prop, business_prop, histry_prop)){
  print(dd %>% filter(from_inst == to_inst) %>% top_n(10, prop) %>% arrange(desc(prop)))
}

```
