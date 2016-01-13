
source("cap_functions.R")

# ngrams <- readRDS("ngrams_2016_01_03.rds")

total <- c(read_file("final/en_US/en_US.blogs.txt"),
           read_file("final/en_US/en_US.news.txt"),
           read_file("final/en_US/en_US.twitter.txt"))

total <- sample(total, length(total)*0.3)

make_ngrams_batch(nbatches = 5, total, ngram = c(1:4))


# ngb <- read_batched_wfm("wfm_ng._2016-01-10")
# # is_hash = FALSE 
# make_table_batch(nbatches = 5, ngb$ng1, nlength = 1)
# make_table_batch(nbatches = 5, ngb$ng2, nlength = 2)
# make_table_batch(nbatches = 5, ngb$ng3, nlength = 3)
# make_table_batch(nbatches = 5, ngb$ng4, nlength = 4)

# ngb <- 
# is_hash = FALSE 
make_table_batch(nbatches = 5, read_batched_wfm("wfm_ng1_2016-01-11"), nlength = 1)
make_table_batch(nbatches = 5, read_batched_wfm("wfm_ng2_2016-01-11"), nlength = 2)
make_table_batch(nbatches = 5, read_batched_wfm("wfm_ng3_2016-01-11"), nlength = 3)
make_table_batch(nbatches = 5, read_batched_wfm("wfm_ng4_2016-01-11"), nlength = 4)


dtl <- list(
    dt1 = read_batched_table("dt_nl1_2016-01-11"),
    dt2 = read_batched_table("dt_nl2_2016-01-11"),
    dt3 = read_batched_table("dt_nl3_2016-01-11"),
    dt4 = read_batched_table("dt_nl4_2016-01-11")
)

saveRDS(dtl, "dtl_from_2016-01-11.rds")
dtl <- readRDS("dtl_from_2016-01-11.rds")

dtl2 <- list(
    dt1 = dtl$dt1,
    dt2 = dtl$dt2[freq>1],
    dt3 = dtl$dt3[freq>1],
    dt4 = dtl$dt4[freq>1]
)

dtl3 <- list(
    dt1 = dtl$dt1,
    dt2 = dtl$dt2[freq>2],
    dt3 = dtl$dt3[freq>3],
    dt4 = dtl$dt4[freq>4]
)

dtl4 <- list(
    dt1 = dtl$dt1[freq>1],
    dt2 = dtl$dt2[freq>2],
    dt3 = dtl$dt3[freq>3],
    dt4 = dtl$dt4[freq>4]
)

dtl4$dt1_cnt = sum(dtl4$dt1$freq)
dtl4$dt2_cnt = sum(dtl4$dt2$freq)
dtl4$dt3_cnt = sum(dtl4$dt3$freq)
dtl4$dt4_cnt = sum(dtl4$dt4$freq)


saveRDS(dtl4, "dtl4_from_2016-01-11.rds")
dtl4 <- readRDS("dtl4_from_2016-01-11.rds")


dtl5 <- list(
    dt1 = dtl$dt1[freq>2],
    dt2 = dtl$dt2[freq>3],
    dt3 = dtl$dt3[freq>4],
    dt4 = dtl$dt4[freq>5]
)

# -------------------------------------------------
saveRDS(dtl, "dtl_prepd_030.rds")

dtlh4 <- dtl4
# dtlh4$dt1$prefix <- sapply(dtl4$dt1$prefix, hash),
dtlh4$dt2$prefix <- sapply(dtl4$dt2$prefix, hash)
dtlh4$dt3$prefix <- sapply(dtl4$dt3$prefix, hash)
dtlh4$dt4$prefix <- sapply(dtl4$dt4$prefix, hash)

saveRDS(dtl, "dtlh4_from_2016-01-11.rds")


predict_backoff(dtl, "Go on a romantic date at the")
# Quiz 3:
# Q1
predict_backoff(dtl, "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
                ,non_stop = T
                ,candidates = c("eat","give","sleep","die"))
# give - wrong, try die

# Q2
predict_backoff(dtl, "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
                ,non_stop = T
                ,candidates = c("marital","horticultural","spiritual","financial")
                )
# financial  - wrong, try spiritual - NOK (

# Q3
predict_backoff(dtl, "I'd give anything to see arctic monkeys this"
                ,non_stop = T
                ,candidates = c("morning","decade","month","weekend")
)
# morning - wrong, try morning

# Q4
predict_backoff(dtl, "Talking to your mom has the same effect as a hug and helps reduce your"
                # ,non_stop = T
                ,candidates = c("happiness","stress","hunger","sleepiness")
)
# stress

# Q5
predict_backoff(dtl, "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
                ,non_stop = T
                ,candidates = c("picture","walk","minute","look")
)
# look - wrong try picture

# Q6
predict_backoff(dtl, "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
                ,non_stop = T
                ,candidates = c("case","account","incident","matter")
)
# case - wrong try matter

# Q7
predict_backoff(dtl, "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
                # ,non_stop = T
                ,candidates = c("arm","finger","toe","hand")
)
# hand

# Q8
predict_backoff(dtl, "Every inch of you is perfect from the bottom to the"
                # ,non_stop = T
                # ,candidates = c("happiness","stress","hunger","sleepiness")
)
# top

# Q9
predict_backoff(dtl, "I'm thankful my childhood was filled with imagination and bruises from playing"
                # ,non_stop = T
                ,candidates = c("outside","weekly","inside","daily")
)
# outside

# Q10
predict_backoff(dtl, "I like how the same people are in almost all of Adam Sandler's"
                ,non_stop = T
                # ,candidates = c("pictures","novels","stories","movies")
)
# lno answer ( try movie

#--------------------------------------------------


ng1 <- read_batched_wfm("wfm_2016-01-05_12_33")
make_table_batch(nbatches = 5, ng1, nlength = 3)
dt3 <- read_batched_table("dt_nl3_2016-01-05_15_06")
dt3h <- dt3
dt3h$prefix <- sapply(dt3h$prefix, hash)

pred1 <- predict_next(dt, "Go on a romantic date at the")[1:100]
pred1$rel_freq <- pred1$freq/sum(ngrams)

