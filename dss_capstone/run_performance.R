
source("cap_functions.R")

# ngrams <- readRDS("ngrams_2016_01_03.rds")

total <- c(read_file("final/en_US/en_US.blogs.txt"),
           read_file("final/en_US/en_US.news.txt"),
           read_file("final/en_US/en_US.twitter.txt"))

make_ngrams_batch(nbatches = 10, total, ngram = 3)

ng <- read_batched_wfm("wfm_2016-01-05_00_52")


make_table_batch(nbatches = 5, ng, nlength = 3)
dt <- read_batched_table("dt_2016-01-03_20_38")

pred1 <- predict_next(dt, "Go on a romantic date at the")[1:100]
pred1$rel_freq <- pred1$freq/sum(ngrams)

