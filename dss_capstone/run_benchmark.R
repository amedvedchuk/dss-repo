
setwd("../dsci-benchmark/")
setwd("../../dsci-benchmark/")


source("../dsci-benchmark/benchmark.R")
source("benchmark.R")

m_predict <- function(phrase){
    res <- predict_backoff(dtl4, phrase
                    ,non_stop = F
                    ,show_last = 3
                    ,verbose = F
                    ,is_hash = F
                    ,simple_out = T
                    ,max_ng = 5
                    # ,order_res = c("prob_w", "l_freq")
                    # ,order_res = c("freq", "l_freq")
                    # ,prob_weights = c(0.005, 0.005, 1)
                    )                  
    res
}

# for l_freq test case:
# "this is your chance to"

m_predict("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
predict_backoff(dtl4, "When you breathe, I want to be the air for you.5", non_stop = T)

predict_backoff(dtl4, "I'd live and I'd", non_stop = F, order_res = c("prob_w", "l_freq"))
predict_backoff(dtl4, "Go on a romantic date at the", non_stop = T, order_res = c("prob_w", "l_freq"), prob_weights = c(0.01, 0.01, 1))
predict_backoff(dtl4, "might exists in", non_stop = T, order_res = c("prob_w", "l_freq"), simple_out = F, show_last = 10)
predict_backoff(dtl4, "", non_stop = T, order_res = c("prob_w", "l_freq"), simple_out = F, show_last = 10)$lastw[2]

predict_backoff(dtl4, "Go on a romantic date at the")
predict_backoff(dtl4, "this is my first ")


m_predict("Go on a romantic date at the")

predict_backoff(dtlh4, "Go on a romantic date at the", is_hash = T)
predict_backoff(dtl4, "Pretty sure that was a broken", non_stop = T)
predict_backoff(dtl, "work through the eyes of an excited", non_stop = T, order_res = c("prob_w", "l_freq"))




benchmark(m_predict, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)
