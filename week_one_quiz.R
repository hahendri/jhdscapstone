# file size
file.info("./data/final/en_US/en_US.blogs.txt")[[1]]/1000000

# lines of text
dim(df.twitter)[[1]]

# length of longest line
df.quiz <- df.combined
df.quiz$nchar <- nchar(df.quiz$text)
library(dplyr)
df.quiz <- arrange(df.quiz, desc(nchar))
df.quiz[1, c(1,3)]

# love / hate
n <- sum(grepl("love", df.twitter$text))
d <- sum(grepl("hate", df.twitter$text))
n/d
rm(n)
rm(d)

# Tweet with "biostats"
df.twitter[grep("biostats", df.twitter$text), 2]


# Tweets with phrase "A computer once beat me at chess, but it was no match for 
# me at kickboxing"
sum(grepl("^A computer once beat me at chess, but it was no match for me at kickboxing$", df.twitter$text))