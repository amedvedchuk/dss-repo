
source("cap_functions.R")

# ngrams <- readRDS("ngrams_2016_01_03.rds")

total <- c(read_file("final/en_US/en_US.blogs.txt"),
           read_file("final/en_US/en_US.news.txt"),
           read_file("final/en_US/en_US.twitter.txt"))

make_ngrams_batch(nbatches = 5, total, ngram = c(1:4))


ngb <- read_batched_wfm("wfm_ng._2016-01-05_14_09")
# is_hash = FALSE 
make_table_batch(nbatches = 5, ngb$ng1, nlength = 1)
make_table_batch(nbatches = 5, ngb$ng2, nlength = 2)
make_table_batch(nbatches = 5, ngb$ng3, nlength = 3)
make_table_batch(nbatches = 5, ngb$ng4, nlength = 4)

dtl <- list(
    dt1 = read_batched_table("dt_nl1_2016-01-05_16"),
    dt2 = read_batched_table("dt_nl2_2016-01-05_16"),
    dt3 = read_batched_table("dt_nl3_2016-01-05_16"),
    dt4 = read_batched_table("dt_nl4_2016-01-05_16")
)



ng1 <- read_batched_wfm("wfm_2016-01-05_12_33")
make_table_batch(nbatches = 5, ng1, nlength = 3)
dt3 <- read_batched_table("dt_nl3_2016-01-05_15_06")
dt3h <- dt3
dt3h$prefix <- sapply(dt3h$prefix, hash)

pred1 <- predict_next(dt, "Go on a romantic date at the")[1:100]
pred1$rel_freq <- pred1$freq/sum(ngrams)

