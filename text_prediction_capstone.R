# Set working directory
setwd("C:/Users/harla/Desktop/Coursera/jhdscapstone")

# Add libraries needed for project
library(readr)
library(caret)
library(quanteda)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
library(doSNOW)

# Download and unzip the data, delete files that will not be needed
if (!file.exists("data")){
      dir.create("data")  
}      
datasetfileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(datasetfileURL, destfile = "./data/textdataset.zip", method="curl")
unzip("./data/textdataset.zip", exdir = "./data")
file.remove("./data/final/de_DE/de_DE.blogs.txt")
file.remove("./data/final/de_DE/de_DE.news.txt")
file.remove("./data/final/de_DE/de_DE.twitter.txt")
file.remove("./data/final/fi_FI/fi_FI.blogs.txt")
file.remove("./data/final/fi_FI/fi_FI.news.txt")
file.remove("./data/final/fi_FI/fi_FI.twitter.txt")
file.remove("./data/final/ru_RU/ru_RU.blogs.txt")
file.remove("./data/final/ru_RU/ru_RU.news.txt")
file.remove("./data/final/ru_RU/ru_RU.twitter.txt")
file.remove("./data/textdataset.zip")

# Use readr to read txt files into R
raw.data.blogs <- read_lines("./data/final/en_US/en_US.blogs.txt", skip = 0, n_max = -1L, na = character(),
           locale = default_locale(), progress = interactive())
raw.data.news <- read_lines("./data/final/en_US/en_US.news.txt", skip = 0, n_max = -1L, na = character(),
                             locale = default_locale(), progress = interactive())
raw.data.twitter <- read_lines("./data/final/en_US/en_US.twitter.txt", skip = 0, n_max = -1L, na = character(),
                             locale = default_locale(), progress = interactive())

# Convert raw.data.xxx into data frames
df.blogs <- as.data.frame(raw.data.blogs)
df.news <- as.data.frame(raw.data.news)
df.twitter <- as.data.frame(raw.data.twitter)

remove(raw.data.blogs)
remove(raw.data.twitter)
remove(raw.data.news)

# Add label column to df.xxx to indicate where text came from for futher analysis, 
# if warranted: convert label column to a factor, rename text column, 
# change text column to character, and reorder columns
df.blogs$label <- "blog"
      df.blogs$label <- as.factor(df.blogs$label)
            colnames(df.blogs)[[1]] <- "text"
                  df.blogs$text <- as.character(df.blogs$text)
                        df.blogs <- df.blogs[ , c("label", "text")]
df.news$label <- "news"
      df.news$label <- as.factor(df.news$label)
            colnames(df.news)[[1]] <- "text"
                  df.news$text <- as.character(df.news$text)
                        df.news <- df.news[ , c("label", "text")]
df.twitter$label <- "twitter"
      df.twitter$label <- as.factor(df.twitter$label)
            colnames(df.twitter)[[1]] <- "text"
                  df.twitter$text <- as.character(df.twitter$text)
                        df.twitter <- df.twitter[ , c("label", "text")]

# Combine df.xxx data frames in order to sample.  We would like to maintain the 
# proportion of the label column in the sample.  When working with Natural Language
# Processing and N-Grams, the data increases enormously.  We are sampling to manage
# the size of the training set.  Add column for number of characters in $text.  Add
# column for number of words in $text
df.combined <- rbind(df.blogs, df.news, df.twitter)
df.combined$nchar <- nchar(df.combined$text)
df.combined$words <- sapply(strsplit(df.combined$text, " "), length)

remove(df.blogs)
remove(df.news)
remove(df.twitter)

# Sample to reduce size of data and create training data set.  I am using 20% to get
# a small sample set which reduces the number of lines from 4 million to 854 thousand.
set.seed(1234)
data.partition <- createDataPartition(df.combined$label, times = 1, p = 0.2, list = FALSE)
train.data <- df.combined[data.partition,]
<<<<<<< HEAD

remove(df.combined)
remove(data.partition)
=======
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
                        
# Create Corpus from train.data
train.corpus <- corpus(train.data)

# Develop token.function to Tokenize the Corpus in uni-grams to assigned object corpus.token.  
# This includes tokenizing as words and removing punctuation, numbers, and symbols.  
# Removing these shouldn't affect our model to predict next word.  I will also remove 
# profanity since I don't want to predict profanity with my model.
token.function <- function(nameofcorpus, remove.profanity = TRUE) {
      corpus.token <- tokens(nameofcorpus, what = "word", remove_numbers = TRUE,
                             remove_symbols = TRUE, remove_punct = TRUE)

# If.else for removing profanity based on remove.profanity argument.  Downloads 
# Google's list of banned words as a txt document.
      if(remove.profanity == TRUE) {
            badwordsURL <- "https://www.freewebheaders.com/download/files/full-list-of-bad-words_text-file_2018_07_30.zip"
            download.file(badwordsURL, destfile = "./data/badwords.zip", method="curl")
            unzip("./data/badwords.zip", exdir = "./data")
            file.remove("./data/badwords.zip")
            bad.words <- read_lines("./data/full-list-of-bad-words_text-file_2018_07_30.txt", 
                                    skip = 13, n_max = -1L, na = character(), 
                                    locale = default_locale(), progress = interactive())
            corpus.token <- tokens_remove(corpus.token, bad.words, valuetype = "fixed", 
                                    verbose = TRUE)
            str(corpus.token)
      } else {
            str(corpus.token)
      }
corpus.token <<- corpus.token 
}

# Run token.function for train.corpus with remove profanity
token.function(train.corpus, remove.profanity = TRUE)

# Create unigram DFM then data.table
train.dfm.unigram <- dfm(corpus.token)
train.dfm.unigram <- as.data.table(textstat_frequency(train.dfm.unigram))

# Create bigram DFM then data.table
train.dfm.bigram <- tokens_ngrams(corpus.token, n = 2)
train.dfm.bigram <- dfm(train.dfm.bigram)
train.dfm.bigram <- as.data.table(textstat_frequency(train.dfm.bigram))

# Create trigram DFM then data.table
train.dfm.trigram <- tokens_ngrams(corpus.token, n = 3)
train.dfm.trigram <- dfm(train.dfm.trigram)
train.dfm.trigram <- as.data.table(textstat_frequency(train.dfm.trigram))

#Create four-gram DFM then data.table
train.dfm.quadgram <- tokens_ngrams(corpus.token, n = 4)
train.dfm.quadgram <- dfm(train.dfm.quadgram)
train.dfm.quadgram <- as.data.table(textstat_frequency(train.dfm.quadgram))

#Create five-gram DFM then data.table
train.dfm.pentgram <- tokens_ngrams(corpus.token, n = 5)
train.dfm.pentgram <- dfm(train.dfm.pentgram)
train.dfm.pentgram <- as.data.table(textstat_frequency(train.dfm.pentgram))

# Remove english stopwords from train.dfm.1 for analysis
train.dfm.stopwords <- dfm(corpus.token, remove = stopwords("english"))

# Exploratory Analysis
# Ratio of train.data$label
g.label.bar <- ggplot(data = train.data, aes(label))
g.label.bar + geom_bar(stat = "count") + labs(x = "Text Source", 
            y = "Count of Observations", 
            title = "Proportion of Text Source in Training Dataset")

# Table and prop.table of train.data$label
table(train.data$label)
prop.table(table(train.data$label))

# Histogram of train.data$nchar
g.nchar.hist <- ggplot(data = train.data, aes(nchar))
g.nchar.hist + geom_histogram(color = "darkblue", fill = "lightblue") + 
      xlim(0, 500) + geom_vline(aes(xintercept = mean(nchar)), color = "black", 
            linetype = "dashed", size = 1) + 
      labs(x = "Number of Characters per Observation with >500 Removed and Mean Displayed", 
            y = "Count", 
            title = "Histogram of Characters per Observation in Training Dataset") +
      geom_text(aes(label = round(mean(nchar), 0), y = 0, x = mean(nchar)),
                vjust = -1, col = 'black', size = 3)

# Histogram of train.data$words
g.word.hist <- ggplot(data = train.data, aes(words))
g.word.hist + geom_histogram(color = "darkblue", fill = "lightblue") + xlim(0, 300) +
      geom_vline(aes(xintercept = mean(words)), color = "black", linetype = "dashed", 
                 size = 1) + 
      labs(x = "Number of Words per Observation with >300 Removed and Mean Displayed", 
           y = "Count", 
           title = "Histogram of Words per Observation in Training Dataset") +
      geom_text(aes(label = round(mean(words), 0), y = 0, x = mean(words)),
                vjust = -1, col = 'black', size = 3)

# Plot frequency of top 50 words from dfm with stopwords removed

features.dfm.top <- textstat_frequency(train.dfm.stopwords, n = 50)
features.dfm.top$feature <- with(features.dfm.top, reorder(feature, -frequency))

ggplot(features.dfm.top, aes(x = feature, y = frequency)) +
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

# List of top 50 words per $label from dfm with stopwords removed 

features.dfm.group <- topfeatures(train.dfm.stopwords, n = 50, scheme = "docfreq",
      groups = "label")

names(features.dfm.group$blog)
names(features.dfm.group$news)
names(features.dfm.group$twitter)

#Build Model Data Tables
unigram <- train.dfm.unigram[, 1:2]
<<<<<<< HEAD
remove(train.dfm.unigram)
unigram$probability <- unigram$frequency/sum(unigram$frequency)

bigram <- train.dfm.bigram[, 1:2]
remove(train.dfm.bigram)
bigram <- separate(bigram, "feature", into = c("base1", "predicted"), sep = "_",
                   remove = FALSE, extra = "merge")

trigram <- train.dfm.trigram[, 1:2]
remove(train.dfm.trigram)
trigram <- separate(trigram, "feature", into = c("base1", "base2", "predicted"), sep = "_",
                    remove = FALSE, extra = "merge")

quadgram <- train.dfm.quadgram[, 1:2]
remove(train.dfm.quadgram)
quadgram <- separate(quadgram, "feature", into = c("base1", "base2", "base3", "predicted"), 
                     sep = "_", remove = FALSE, extra = "merge")

pentagram <- train.dfm.pentgram[, 1:2]
remove(train.dfm.pentgram)
pentagram <- separate(pentagram, "feature", into = c("base1", "base2", "base3", "base4", "predicted"), 
                     sep = "_", remove = FALSE, extra = "merge")
=======
unigram$probability <- unigram$frequency/sum(unigram$frequency)

bigram <- train.dfm.bigram[, 1:2]
bigram <- separate(bigram, "feature", into = c("base1", "predicted"), sep = "_",
                   remove = FALSE, extra = "merge")
remove(train.dfm.bigram)

trigram <- train.dfm.trigram[, 1:2]
trigram <- separate(trigram, "feature", into = c("base1", "base2", "predicted"), sep = "_",
                    remove = FALSE, extra = "merge")
remove(train.dfm.trigram)

quadgram <- train.dfm.quadgram[, 1:2]
quadgram <- separate(quadgram, "feature", into = c("base1", "base2", "base3", "predicted"), 
                     sep = "_", remove = FALSE, extra = "merge")
remove(train.dfm.quadgram)

pentagram <- train.dfm.pentgram[, 1:2]
pentagram <- separate(pentagram, "feature", into = c("base1", "base2", "base3", "base4", "predicted"), 
                     sep = "_", remove = FALSE, extra = "merge")
remove(train.dfm.pentgram)
>>>>>>> 057f9064ead60757160b226089d9a868800b102c

#Build bigram model
bigram_predict <- function(base.21 = NULL) {
      
<<<<<<< HEAD
      bigram.base <- bigram.s[base1 == base.21]
      
      if(nrow(bigram.base) > 0) {
            b.p1 <- bigram.base$frequency/sum(bigram.s$frequency)
            b.p2 <- as.numeric(rep((unigram[feature == base.21][, 3]), times = length(b.p1)))
            bigram.base$biprobability <- b.p1/b.p2
            bigram.base <- setorder(bigram.base, -biprobability)
            predict <<- print(bigram.base[1, 2])
            predict.other <<- if(nrow(bigram.base) == 1) {
                  
                  print(na.omit(unigram[1:4, 1]))
                  
            } else {
                  
                  print(na.omit(bigram.base[2:5, 2]))
                  
            }
            
            plot <<- ggplot(na.omit(bigram.base[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                  geom_point() + 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Bigrams")
            plot
=======
      bigram.base <- bigram[base1 == base.21]
      
      if(nrow(bigram.base) > 0) {
            b.p1 <- bigram.base$frequency/sum(bigram$frequency)
            b.p2 <- as.numeric(rep((unigram[feature == base.21][, 3]), times = length(b.p1)))
            bigram.base$biprobability <- b.p1/b.p2
            bigram.base <- setorder(bigram.base, -biprobability)
            print(na.omit(bigram.base[1:5, 3]))
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
      
      } else {
            
            unigram <- setorder(unigram, -probability)
            if(unigram[1, 1] == base.21) {
<<<<<<< HEAD
                  predict <<- print(unigram[2, 1])
                  predict.other <<- print(na.omit(unigram[3:6, 1]))
                  
                  plot <<- ggplot(na.omit(unigram[1:50,]), aes(x = reorder(feature, -frequency), y = frequency)) +
                        geom_point() + 
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                  plot
            
            } else {
                  
                  predict <<- print(unigram[1, 1])
                  predict.other <<- print(na.omit(unigram[2:5, 1]))
                  
                  plot <<- ggplot(na.omit(unigram[1:50,]), aes(x = reorder(feature, -frequency), y = frequency)) +
                        geom_point() + 
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                  plot
=======
                  paste(unigram[2, 1])
            
            } else {
                  
                  paste(unigram[1, 1])
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
                  
                  } 
            }
}

start <- Sys.time()
<<<<<<< HEAD
bigram_predict(base.21 = "the")
=======
bigram_predict(base.21 = "lot")
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
Sys.time() - start

#Build a trigram model
trigram_predict <- function(base.31 = NULL, base.32 = NULL) {
      
<<<<<<< HEAD
      trigram.base <- trigram.s[base1 == base.31 & base2 == base.32]
      
      if(nrow(trigram.base) > 0) {
            t.p1 <- trigram.base$frequency/sum(trigram.s$frequency)
            t.p2 <- as.numeric(rep((bigram.s[base1 == base.31 & predicted == base.32][, 3]), times = length(t.p1)))
            t.p3 <- as.numeric(rep(sum(bigram.s$frequency), times = length(t.p1)))
            t.p4 <- t.p2/t.p3
            trigram.base$triprobability <- t.p1/t.p4
            trigram.base <- setorder(trigram.base, -triprobability)
            predict <<- print(trigram.base[1, 3])
            predict.other <<- if(nrow(trigram.base) == 1) {
                  
                  print("No Suggestions")
                  
            } else {
                  
                  print(na.omit(trigram.base[2:5, 3]))
                  
            }
            
            plot <<- ggplot(na.omit(trigram.base[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                  geom_point() + 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Trigram Predicted Words")
            plot
=======
      trigram.base <- trigram[base1 == base.31 & base2 == base.32]
      
      if(nrow(trigram.base) > 0) {
            t.p1 <- trigram.base$frequency/sum(trigram$frequency)
            t.p2 <- as.numeric(rep((bigram[base1 == base.31 & predicted == base.32][, 4]), times = length(t.p1)))
            t.p3 <- as.numeric(rep(sum(bigram$frequency), times = length(t.p1)))
            t.p4 <- t.p2/t.p3
            trigram.base$triprobability <- t.p1/t.p4
            trigram.base <- setorder(trigram.base, -triprobability)
            print(na.omit(trigram.base[1:5, 4]))
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
      
      } else {
            
           bigram_predict(base.21 = base.32)
            
      }
}
  
start <- Sys.time()
<<<<<<< HEAD
trigram_predict(base.31 = "the", base.32 = "neighborhood")
=======
trigram_predict(base.31 = "if", base.32 = "we")
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
Sys.time() - start 

#Build a quadgram model
quadgram_predict <- function(base.41 = NULL, base.42 = NULL, base.43 = NULL) {
      
<<<<<<< HEAD
      quadgram.base <- quadgram.s[base1 == base.41 & base2 == base.42 & base3 == base.43]
      
      if(nrow(quadgram.base) > 0) {
            q.p1 <- quadgram.base$frequency/sum(quadgram.s$frequency)
            q.p2 <- as.numeric(rep((trigram.s[base1 == base.41 & base2 == base.42 & predicted == base.43][, 4]), times = length(q.p1)))
            q.p3 <- as.numeric(rep(sum(trigram.s$frequency), times = length(q.p1)))
            q.p4 <- q.p2/q.p3
            quadgram.base$quadprobability <- q.p1/q.p4
            quadgram.base <- setorder(quadgram.base, -quadprobability)
            predict <<- print(quadgram.base[1, 4])
            predict.other <<- if(nrow(quadgram.base) == 1) {
                  
                  print("No Suggestions")
                  
            } else {
                  
                  print(na.omit(quadgram.base[2:5, 4]))
                  
            }
            
            plot <<- ggplot(na.omit(quadgram.base[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                  geom_point() + 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Quadgram Predicted Words")
            plot
=======
      quadgram.base <- quadgram[base1 == base.41 & base2 == base.42 & base3 == base.43]
      
      if(nrow(quadgram.base) > 0) {
            q.p1 <- quadgram.base$frequency/sum(quadgram$frequency)
            q.p2 <- as.numeric(rep((trigram[base1 == base.41 & base2 == base.42 & predicted == base.43][, 5]), times = length(q.p1)))
            q.p3 <- as.numeric(rep(sum(trigram$frequency), times = length(q.p1)))
            q.p4 <- q.p2/q.p3
            quadgram.base$quadprobability <- q.p1/q.p4
            quadgram.base <- setorder(quadgram.base, -quadprobability)
            print(na.omit(quadgram.base[1:5, 5]))
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
            
      } else {
            
            trigram_predict(base.31 = base.42, base.32 = base.43)
            
      }
      
}

start <- Sys.time()
quadgram_predict(base.41 = "just", base.42 = "a", base.43 = "little")
Sys.time() - start 

#Build a pentagram model
pentagram_predict <- function(base.51 = NULL, base.52 = NULL, base.53 = NULL, base.54 = NULL) {
      
<<<<<<< HEAD
      pentagram.base <- filter(pentagram.s, base1 == base.51, base2 == base.52, base3 == base.53,
                                    base4 == base.54)
      
      if(nrow(pentagram.base) > 0) {
            penta.p1 <- pentagram.base$frequency/sum(pentagram.s$frequency)
            penta.p2 <- as.numeric(rep((filter(quadgram.s, base1 == base.51, base2 == base.52,  
                        base3 == base.53, predicted == base.54)[, 5]), times = length(penta.p1)))
            penta.p3 <- as.numeric(rep(sum(quadgram.s$frequency), times = length(penta.p1)))
            penta.p4 <- penta.p2/penta.p3
            pentaprobability <- penta.p1/penta.p4
            pentagram.base.2 <- cbind(pentagram.base, pentaprobability)
            pentagram.base.3 <- setorder(pentagram.base.2, -pentaprobability)
            predict <<- print(pentagram.base[1, 5])
            predict.other <<- if(nrow(pentagram.base) == 1) {
                  
                  print("No Suggestions")
                  
            } else {
                  
                  print(na.omit(pentagram.base[2:5, 5]))
                  
            }
            
            pent.plot <<- ggplot(na.omit(pentagram.base[1:50,]), aes(x = reorder(predicted, -frequency), y = frequency)) +
                  geom_point() + 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Pentagram Predicted Words")
            pent.plot
=======
      pentagram.base <- pentagram[base1 == base.51 & base2 == base.52 & base3 == base.53 &
                                    base4 == base.54]
      
      if(nrow(pentagram.base) > 0) {
            penta.p1 <- pentagram.base$frequency/sum(pentagram$frequency)
            penta.p2 <- as.numeric(rep((quadgram[base1 == base.51 & base2 == base.52 & 
                        base3 == base.53 & predicted == base.54][, 6]), times = length(penta.p1)))
            penta.p3 <- as.numeric(rep(sum(quadgram$frequency), times = length(penta.p1)))
            penta.p4 <- penta.p2/penta.p3
            pentagram.base$pentaprobability <- penta.p1/penta.p4
            pentagram.base <- setorder(pentagram.base, -pentaprobability)
            print(na.omit(pentagram.base[1:5, 6]))
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
            
      } else {
            
            quadgram_predict(base.41 = base.52, base.42 = base.53, base.43 = base.54)
            
      }
      
}

start <- Sys.time()
<<<<<<< HEAD
pentagram_predict(base.51 = "to", base.52 = "go", base.53 = "to", base.54 = "the")
Sys.time() - start 

# Plot frequency of top 50 words from prediction function
=======
pentagram_predict(base.51 = "the", base.52 = "end", base.53 = "of", base.54 = "the")
Sys.time() - start 

# Plot frequency of top 50 words from dfm with stopwords removed
>>>>>>> 057f9064ead60757160b226089d9a868800b102c

mydat <- na.omit(pentagram.base[1:50, c(1, 7)])
mydat <- setorder(mydat, -frequency)

<<<<<<< HEAD
ggplot(mydat, aes(x = reorder(feature, -frequency), y = frequency)) +
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Reduce Size of Data.Tables

unigram.size <- format(object.size(unigram), units = "Mb")

bigram.size.start <- format(object.size(bigram), units = "Mb")
bigram.s <- bigram[, -1]
remove(bigram)
bigram.s <- bigram.s[, head(.SD, 50), by = base1]
bigram.size.final <- format(object.size(bigram.s), units = "Mb")

trigram.size.start <- format(object.size(trigram), units = "Mb")
trigram.s <- trigram[, -1]
trigram.s <- trigram.s[frequency > 1]
remove(trigram)
trigram.s <- trigram.s[, head(.SD, 50), by = list(base1, base2)]
trigram.size.final <- format(object.size(trigram.s), units = "Mb")

quadigram.size.start <- format(object.size(quadgram), units = "Mb")
quadgram.s <- quadgram[, -1]
quadgram.s <- quadgram.s[frequency > 1]
quadgram.s <- quadgram.s[, head(.SD, 50), by = list(base1, base2, base3)]
quadgram.size.final <- format(object.size(quadgram.s), units = "Mb")
remove(quadgram)

pentagram.size.start <- format(object.size(pentagram), units = "Mb")
pentagram.s <- pentagram[, -1]
pentagram.s <- pentagram.s[frequency > 1]
pentagram.s <- pentagram.s[, head(.SD, 50), by = list(base1, base2, base3, base4)]
pentagram.size.final <- format(object.size(pentagram.s), units = "Mb")
remove(pentagram)

write.csv(unigram, "./data/unigram.csv")
write.csv(bigram.s, "./data/bigram.csv")
write.csv(trigram.s, "./data/trigram.csv")
write.csv(quadgram.s, "./data/quadgram.csv")
write.csv(pentagram.s, "./data/pentagram.csv")
=======
ggplot(mydat, aes(x = feature, y = frequency)) +
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
