
source("cap_functions.R")

ngrams <- readRDS("ngrams_2016_01_03.rds")
make_table_batch(nbatches = 5, ngrams, nlength = 3)
dt <- read_batched_table("dt_2016-01-03_20_38")

