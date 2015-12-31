
library(quanteda)
library(dplyr)

# example(corpus)
# mydfm <- dfm(acq1)
# mydfm <- dfm(ukimmigCorpus)
# 
# 
# topfeatures(mydfm)
# plot(mydfm)
# 
# txt <- c("This is software testing: looking for (word) pairs!  
#          This [is] a software testing again. For.",
#          "Here: this is more Software Testing, looking again for word pairs.")
# collocations(txt, size = 2:3)
# collocations(txt)
# collocations(txt, removePunct = TRUE)

# ##Word frequencies
# Quanteda package was used to calculte document-feature matrix
# 
# ```{r echo=TRUE, cache=TRUE}
# library(quanteda)
# dssCorp <- corpus(textfile("final/en_US/en_US.blogs.txt"))
# ```
# 
# 
# ```{r echo=TRUE, cache=TRUE}
# dssDfm <- dfm(dssCorp)
# hist(topfeatures(dssDfm, 100), breaks = 50)
# ```

lines <- readLines("final/en_US/en_US.blogs.txt")
stri_stats_general(lines)

usb <- dfm(lines, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 1)
dim(usb)
str(usb)

dssCorp <- corpus(textfile("final/en_US/en_US.blogs.txt"))
usb2 <- dfm(dssCorp, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 1)


dssCorp2 <- corpus(textfile("final/en_US/*"))

dfm2 <- dfm(dssCorp$documents)

summary(dssCorp)
dssDfm <- dfm(dssCorp)
top <- topfeatures(dssDfm, 50)
top

dssDfm[1, 1:10]
topfeatures(dssDfm, 10)

plot(dssDfm[1, 1:10])

plot(top)

str(top)

tail(top)

hist(topfeatures(dssDfm, 100), breaks = 50)
hist(dssDfm)

hist(top)
hist(dssDfm)
barplot(top)

summary(top)

sum(top)
sum(dssDfm)
quantile(dssDfm, probs = 0.5)

quantile(top, probs = 0.5)

plot(dssDfm)
summary(dssDfm)

sddTokets <- tokenize(dssCorp, )
kwic(dssCorp)


