ted <- read.csv("../data/ted.csv", stringsAsFactors = FALSE)

# 1. Plot an histogram for the number of views. Is the distribution approximately log-normal?
hist(ted$views)
hist(log(ted$views))

# 2. Transform the `duration` to seconds
ted$seconds <- strtoi(as.difftime(ted$duration, format = "%H:%M:%S", units = "secs"))

# 3. Plot duration in seconds vs. log number of views: does duration correlate with views?
plot(ted$seconds, log(ted$views))

# 4. Count the number of days since publication, and plot against log views
ted$published_days <- as.numeric(Sys.Date() - as.Date(ted$published, format = "%m/%d/%y"))
plot(ted$published_days, log(ted$views))

# 5. Find the top 10 tags
top_tags <- names(sort(table(unlist(str_split(ted$tags, ","))), decreasing = TRUE)[1:10])

# 6. For each top tags, add a column to the data frame specifying if the tag is present
# Hint: you need to use `grepl`
for (tag in top_tags){
  ted[,tag] <- grepl(tag, ted$tags)
}

# 7. Build a linear model with 
# - Response variable = log(views)
# - Explanatory variables = published_days, seconds, technology, science, culture, etc.
# - Which tags significantly increase views?
my_model <- lm(
  as.formula(paste("log(views)", "~",
                   paste('`', colnames(ted)[8:19], '`', sep = "", collapse = "+"))
  ),
  data=ted
)
summary(my_model)

# 8. Add to the model the effect of the top 10 speakers by number of talks.
# Does this improve the fit?
top_speakers <- names(sort(table(ted$speaker_name), decreasing = TRUE)[1:10])
for (speaker in top_speakers){
  ted[,speaker] <- ted$speaker_name == speaker
}

my_model <- lm(
  as.formula(paste("log(views)", "~",
                   paste('`', colnames(ted)[8:29], '`', sep = "", collapse = "+"))
  ),
  data=ted
)
summary(my_model)
