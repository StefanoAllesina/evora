library(tidyverse)
# read the data
dt <- read_csv("https://tinyurl.com/ycc4ndkd") %>% select(-state)

# 1. Find the number of distinct names for boys and girls.

ex1 <- dt %>% select(sex, name) %>% 
  distinct() %>% group_by(sex) %>% tally()
print(ex1)

# 2. Find the name/year combination with highest `count` for each `sex`.

ex2 <- dt %>% group_by(sex) %>% top_n(1, count)
print(ex2)

# 3. Plot the number of distinct names for `M` or `F` in time.

ex3 <- dt %>% ggplot() + 
  aes(x = year, fill = sex) + 
  geom_bar(position = "dodge")
show(ex3)

# 4. As you can see, the diversity of names has been growing. 
# Plot the number of "effective" names in time. 
# First, compute Shannon's entropy for each year/sex. 
# Then exponentiate to obtain the number of names that 
# would produce the same entropy while having equal frequency. 

ent <- dt %>% group_by(sex, year) %>% 
  mutate(p = count / sum(count)) %>% 
  summarise(entropy = -sum(p * log(p)))

ex4 <- ent %>% ggplot() + 
  aes(x = year, colour = sex, y = exp(entropy)) + 
  geom_point() + geom_line() + 
  ylab("Number of effective names")
show(ex4)

# 5. Plot the difference in number of distinct names between `F` and `M` in time.

diff_unique_names <- dt %>% 
  group_by(sex, year) %>% 
  tally() %>% 
  spread(sex, n) %>% 
  mutate(difference = `F` - `M`)

ex5 <- ggplot(diff_unique_names) + 
  aes(x = year, y = difference) + 
  geom_point() + geom_line()
show(ex5)

# 6. Find the frequency with wich each name ends with a certain letter. 
# Show a barplot for the probability that a name (for boys or girls) 
# ends with a given letter. Use years 1950, 1975, 2000 and 2015.

last_letter <- dt %>% mutate(last_letter = stringr::str_sub(name, -1)) %>% 
  group_by(year, sex, last_letter) %>% 
  summarise(tot = sum(count)) 
last_letter <- last_letter %>% 
  group_by(year, sex) %>% 
  mutate(prob = tot / sum(tot))

ex6 <- ggplot(data = last_letter %>% 
                filter(year %in% c(1950, 1975, 2000, 2015))) + 
  aes(x = last_letter, y = prob) + 
  geom_col() + facet_grid(sex ~ year)
show(ex6)

# 7. Plot the popularity of each letter for ending names in time, for boys and girls. 
# Which letters are growing, which declining?
  
ex7 <- ggplot(data = last_letter) + 
  aes(x = year, y = prob, label = last_letter, colour = last_letter) + 
  geom_text() + geom_line() + facet_wrap(~sex) + scale_y_log10()
show(ex7)

# 8. Find the most popular names today that were not present in the 1950 data, and viceversa.
dt2015 <- dt %>% filter(year == 2015) %>%
  group_by(sex, name) %>%
  summarise(tot = sum(count)) %>%
  arrange(desc(tot))
dt1950 <- dt %>% filter(year == 1950) %>%
  group_by(sex, name) %>%
  summarise(tot = sum(count)) %>%
  arrange(desc(tot))

in_2015_not_1950 <- dt2015 %>% anti_join(dt1950 %>% select(name, sex))
print(in_2015_not_1950 %>% group_by(sex) %>% top_n(10, tot) %>% arrange(sex, desc(tot)))

in_1950_not_2015 <- dt1950 %>% anti_join(dt2015 %>% select(name, sex))
print(in_1950_not_2015 %>% group_by(sex) %>% top_n(10, tot) %>% arrange(sex, desc(tot)))

# 9. Track particular names through time
library(ggrepel)
plot_frequency_name <- function(dt, myname = "Elvis", mysex = "M", highlight = c(1956, 1977), mylabel){
  tmp <- dt %>% filter(name == myname, sex == mysex) 
  pl <- tmp %>% ggplot() + aes(x = year, y = per_100k_within_sex) + geom_line()
  tmp2 <- tmp %>% filter(year %in% highlight)
  tmp2$mylabel <- mylabel
  pl <- pl + geom_point(data = tmp2 , colour = "black") + geom_text_repel(data = tmp2 , colour = "black", aes(label = mylabel),
                                                                          nudge_y       = 36,
                                                                          segment.size  = 0.2,
                                                                          segment.color = "darkblue",
                                                                          direction     = "x"
                                                                          ) + ggtitle(myname) + 
    xlim(c(1950, 2015)) + theme_bw() + ylab("Frequency per 100k within sex") + xlab("Year")
  show(pl)
  ggsave(pl, filename = paste0("~/Desktop/ImagesFlorida/", myname, ".png"))
}

plot_frequency_name(dt, "Elvis", "M", c(1956, 1977), c("Debut", "Death")) # debut, death
plot_frequency_name(dt, "Neo", "M", c(1999), "The Matrix") # The Matrix
plot_frequency_name(dt, "Madonna", c(1983)) # debut
plot_frequency_name(dt, "Hermione", "F", c(2001), "First HP movie") # first HP movie
plot_frequency_name(dt, "Diana", "F", c(1981, 1997), c("Wedding", "Death")) # wedding, death
plot_frequency_name(dt, "Rocky", c(1976)) # first movie of the series
plot_frequency_name(dt, "Amelie", c(2001)) # movie
plot_frequency_name(dt, "Osama", "M", c(2001), "9/11") # 9/11
plot_frequency_name(dt, "Daenerys", "F", c(2012), "Game of Thrones (series)") # Game of thrones
plot_frequency_name(dt, "Tiger", c(1997)) # first masters tournament
plot_frequency_name(dt, "Scarlett", "F", c(2003), "Lost in translation") # lost in translation
plot_frequency_name(dt, "Gwyneth", c(1996)) # Emma
plot_frequency_name(dt, "Kanye", c(2004)) # College dropout
plot_frequency_name(dt, "Alanis", c(1995)) # Jagged little pill
plot_frequency_name(dt, "Macaulay", c(1990)) # Home alone
plot_frequency_name(dt, "Miley", c(2006)) # Hannah Montana

