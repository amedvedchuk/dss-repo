

library(quanteda)
library(stringi)
library(data.table)
library(hashr)
library(dplyr)

predict_backoff <- function(dl, phrase, 
                            show_last = 3, 
                            non_stop = F, 
                            is_hash = FALSE,
                            verbose = T,
                            simple_out = F,
                            candidates = c(),
                            prob_weights = c(0.4, 0.4, 1),
                            order_res  = c("nlength", "freq", "l_freq"),
                            max_ng = 5
){
    # tokens <- unlist(tokenize(tolower("After years of work Semmi was vey tired"), removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 3))
    
    m_cat <- create_cat(verbose)
    
    # cat("phrase = ", phrase, "\n", sep="")
    
    getLastNgram <- function(nlength){
        tokens <- unlist(tokenize(tolower(phrase), 
                                  removeNumbers = TRUE, 
                                  removeTwitter = TRUE, 
                                  removePunct = T,  
                                  ngrams = nlength))
        last_ngram <- tokens[length(tokens)]
        # print(last_ngram)
        m_cat("last ngram:", last_ngram, "\n")
        
        if(is_hash){
            last_ngram <- hash(last_ngram)
            m_cat("last bigram:", last_ngram, "\n")
        }
        last_ngram
    }
    
    getAllVariants <- function(dt, nlength, dt_cnt = 1){
        
        last_ngram <- getLastNgram(nlength)
        
        variants <- dt[prefix == last_ngram]
        
        variants$l_freq <- dl$dt1[variants$lastw]$freq
        #         variants$prob <- variants$freq*100/dt_cnt
        #         variants$prob_w <- prod(prob_weights[length(prob_weights):nlength]) * variants$prob
        #         variants$weight <- variants$prob_w/variants$prob
        variants$nlength <- nlength
        
        # variants <- variants[order(variants$freq, variants$l_freq, decreasing = T),]
        # setorderv(variants, order_res, order = -1)
        # setorderv(res, order_res, rep(-1, length(order_res)))
        
        
        m_cat("found variants:", nrow(variants), "\n")
        if(length(candidates)>0) {
            variants <- variants[lastw %in% candidates]
        }
        #         if(nrow(variants) > show_last){
        #             variants <- variants[1:show_last,]
        #         }
        # print(variants)
        variants
    }
    
    make_out <- function(res){
        
        
        if(ncol(res)>2){
            # setorderv(res, order_res, rep(-1, length(order_res)))
            setorderv(res, order_res, order = -1)
        }
        
        if(nrow(res) > show_last){
            res <- res[1:show_last,]
        }
        
        if(simple_out){
            unlist(res$lastw)
        } else {
            res
        }
    }
    
    # to_print <- system.time({
    m_cat("start to predict, showlast = ", show_last, "\n", sep= "")
    
    variants <- data.table()
    
    for(idx in max_ng:2){
        v <- getAllVariants(
            dl[[paste("dt", idx, sep="")]], 
            idx-1, 
            dt_cnt = dl[[paste("dt", idx, "_cnt", sep="")]]
        )
        variants <- rbind(variants, v)
        
        if(nrow(variants)>0 && !non_stop){
            return(make_out(variants))
        } 
    }
        
    
#     variants <- rbind(variants, getAllVariants(dl$dt5, 4, dt_cnt = dl$dt5_cnt))
#     
#     if(nrow(variants)>0 && !non_stop){
#         return(make_out(variants))
#     }
#     
#     variants <- rbind(variants, getAllVariants(dl$dt4, 3, dt_cnt = dl$dt4_cnt))
#     
#     if(nrow(variants)>0 && !non_stop){
#         return(make_out(variants))
#     }
#     
#     variants <- rbind(variants, getAllVariants(dl$dt3, 2, dt_cnt = dl$dt3_cnt))
#     
#     if(nrow(variants)>0 && !non_stop){
#         return(make_out(variants))
#     }
#     
#     variants <- rbind(variants, getAllVariants(dl$dt2, 1, dt_cnt = dl$dt2_cnt))
    
    if(nrow(variants)>0){
        return(make_out(variants))
    } else {
        # return("the")
        return(make_out(dl$dt1[order(dl$dt1$freq, decreasing = T)][1:show_last]))
    }
    
}

create_cat <- function(verbose=T){
    function(...){
        if(verbose){
            cat(...)
        }
    }
}

predict_next <- function(dt, phrase, is_hash = FALSE){
    # tokens <- unlist(tokenize(tolower("After years of work Semmi was vey tired"), removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 3))
    
    tokens <- unlist(tokenize(tolower(phrase), removeNumbers = TRUE, removeTwitter = TRUE, removePunct = T,  ngrams = 2))
    last_bigram <- tokens[length(tokens)]
    print(last_bigram)
    print(cat("last bigram:", last_bigram))
    
    if(is_hash){
        last_bigram <- hash(last_bigram)
    }
    print(cat("last bigram:", last_bigram))
    variants <- dt[prefix == last_bigram]
    variants <- variants[order(variants$freq, decreasing = T),]
    print(cat("found variants:", length(variants)))
    variants
    
    #     getNgram <- function(tokens, ngram_len){
    #         tlen <- length(tokens)
    #         if(tlen > ngram_len){
    #             tokens[tlen-ngram_len:ngram_len]
    #         } else {
    #             tokens
    #         }
    #     }
}

make_ngrams_batch <- function(nbatches = 1, subject, ...){
    
    do_work_batched(nbatches, subject, 
                    function(row, start_time,  ...){
                        print(row)
                        
                        ng <- list(...)$ngram
                        
                        replaced <- preproces_data(subject[row[1]:row[2]])
                        
                        sapply(ng, function(ngram){
                            replaced <- make_dfm(replaced, ngram)
                            file_name <- paste("wfm_ng",ngram,"_",format(start_time, "%Y-%m-%d_%H_%M_"), row[3], ".rds", sep = "")
                            print(paste("save batch to file: ", file_name))
                            saveRDS(replaced, file_name)
                            NULL
                        })
                        
                    }, ...)
}

make_ngrams <- function(lines, ngram){
    
    replaced <- preproces_data(lines)
    replaced <- make_dfm(replaced, ngram)
    print("start col sums...")
    replaced <- colSums(replaced)
    print("done!")
}

make_dfm <- function(lines, ngram){
    
    to_print <- system.time({
        
        print("tokenize to ngrams...")
        replaced <- tokenize(toLower(lines), removeNumbers = TRUE, removeTwitter = TRUE, removePunct = T, ngrams = ngram)
        print("head for tokens")
        print(head(replaced, 5))
        
        # dfm_ng <- dfm(sentences, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE, removeTwitter = TRUE, ngrams = ngram)
        print(Sys.time())
        print("start DFM creation....")
        replaced <- dfm(replaced, toLower = F)
        print("top features for DFM:")
        print(topfeatures(replaced, 5))
        
    })
    print(to_print)
    replaced
}

preproces_data <- function(lines){
    to_print <- system.time({
        # remove non ASCII
        print(Sys.time())
        print("remove non ASCII: [›—’‘“]+")
        # replaced <- stri_replace_all(lines, "", regex = "[›—’‘“]+|(&?amp;)+|(&?lt;)+|(&?gt;)+|class=\"[^\"]*\"|style=\"[^\"]*\"")
        replaced <- stri_replace_all(lines, "", regex = "(&?amp;)+|(&?lt;?)+|(&?gt;)+|class=\"[^\"]*\"|style=\"[^\"]*\"|background:[^\"]*\"|[/;]*span|goog.{1,64}-spellcheck-word")
        
        #         print(Sys.time())
        #         print("remove non ASCII: [[:space:]]+[a-zA-Z]*([^A-Za-z \\d[:punct:]]+[a-zA-Z]*)+")
        #         replaced <- stri_replace_all(replaced, "", regex = "[[:space:]]+[a-zA-Z]*([^A-Za-z \\d[:punct:]]+[a-zA-Z]*)+")
        # replaced <- stri_replace_all(replaced, "", regex = "[[:space:]]+[a-zA-Z]*([^A-Za-z \\d\"\\.!,\\(\\)\\?\\-']+[a-zA-Z]*)+")
        
        print(Sys.time())
        print("start tokenisation to sentences...")
        # tokenise to sentences
        replaced <- unlist(tokenize(replaced, what = "sentence"))
        print(Sys.time())
        print("replace non word\\space\\punct with space")
        replaced <- stri_replace_all(replaced, " ", regex = "[_[^a-zA-Z\\s']]+")
        print(Sys.time())
        #         print("replace non ASCII again")
        #         replaced <- stri_replace_all(replaced, "", regex = "[[:space:]]+[a-zA-Z]*([^A-Za-z \\d[:punct:]]+[a-zA-Z]*)+")
        # print(Sys.time())
        print(cat("sentences length: ", length(replaced)))
        print("head for sentences:")
        print(head(replaced, 10))
        
    })
    print(to_print)
    replaced
}

merge_word_fm <- function(dfm1, dfm2){
    if(is.dfm(dfm1)){
        dfm1 <- colSums(dfm1)
    }
    if(is.dfm(dfm2)){
        dfm2 <- colSums(dfm2)
    }
    
    ind <- intersect(names(dfm1), names(dfm2))
    diff1 <- setdiff(names(dfm1), names(dfm2))
    diff2 <- setdiff(names(dfm2), names(dfm1))
    dmf <- c(dfm1[ind]+dfm2[ind], dfm1[diff1], dfm2[diff2])
    dmf
}

do_work_batched <- function(nbatches = 1, subject, FUN, ...){
    batches <- c(0, round(1:nbatches*(length(subject)/nbatches)))
    # batches <- c(0, round(1:3*(1500/3)))
    start_time <- Sys.time()
    
    batches <- data.frame( start = batches[1:nbatches]+1, end=batches[2:(nbatches+1)], idx = 1:nbatches)
    print(batches)
    
    apply(batches, 1, FUN, start_time, ...)
}

make_table_batch <- function(nbatches = 1, subject, ...){
    
    do_work_batched(nbatches, subject, 
                    function(row, start_time,  ...){
                        print(row)
                        nl <- list(...)$nlength
                        dt <- make_table(subject[row[1]:row[2]], ...)
                        file_name <- paste("dt_nl", nl, "_" ,format(start_time, "%Y-%m-%d_%H_%M_"), row[3], ".rds", sep = "")
                        print(paste("save batch to file: ", file_name))
                        saveRDS(dt, file_name)
                        # i <<- i+1
                        NULL
                    }, ...)
    
}

read_batched_wfm <- function(prefix){
    print(paste("read batched WFM by prefix: ", prefix))
    files <- grep(prefix, list.files("."), value = T)
    
    dt <- NULL
    
    if(length(files) == 1){
        
        print(paste("readfile: ", files[1]))
        dt <- readRDS(files[1])
        
    } else {
        
        ng_bloks <- stri_extract_last_regex(files, pattern = "ng.", simplify = T) %>% unique()
        print(ng_bloks)
        
        if(length(ng_bloks) == 1){
            print(paste("readfile: ", files[1]))
            dt <- readRDS(files[1])
            sapply(files[2:length(files)], function(file){
                print(paste("readfile: ", file))
                dt <<- merge_word_fm(dt, readRDS(file))
            })
        } else {
            
            print("detected multiple blocks...")
            dt <- list()
            sapply(ng_bloks, function(block){
                files_in_block <- grep(block, files, value = T)
                print(paste("readfile:", files_in_block[1], "to block:", block))
                dt_in_blok <- readRDS(files_in_block[1])
                sapply(files_in_block[2:length(files_in_block)], function(file){
                    print(paste("readfile:", file, "to block:", block))
                    dt_in_blok <<- merge_word_fm(dt_in_blok, readRDS(file))
                })
                dt[[block]] <<- dt_in_blok
            })  
        }
    }
    dt
}

read_batched_table <- function(prefix){
    files <- grep(prefix, list.files("."), value = T)
    dt <- data.table()
    sapply(files, function(file){
        cat("read dt file:", file, "\n")
        dt <<- rbind(dt, readRDS(file))
    })
    cat("readed table row size: ", nrow(dt), "\n", sep="")
    cat("set key for data.table = prefix\n")
    setkey(dt, prefix)
    dt
}

make_table <- function(ngrams, nlength = 2, is_hash = FALSE){
    
    to_print <- system.time({
        print("start table preparation...")
        
        if(nlength > 1){
            splitted <- stri_split_fixed(names(ngrams), pattern = "_", simplify = T)
            ulen <- dim(splitted)[2] 
            print(paste(c("rows", "cols"), dim(splitted), collapse = " "))
            
            if(nlength == 2){
                final_dt <- data.table(prefix = splitted[,1],
                                       lastw = splitted[,2],
                                       freq = ngrams
                )
            } else {
                final_dt <- data.table(prefix = apply(splitted[,1:nlength-1], 1, stri_c, collapse="_"),
                                       lastw = splitted[,nlength],
                                       freq = ngrams
                )
            }
            
        } else {
            final_dt <- data.table(lastw = names(ngrams),
                                   freq = ngrams
            )
        }
        
        if(is_hash){
            print("start hashing...")
            
            final_dt$prefix <- sapply(final_dt$prefix, hash)
            print("hashing done!")
            
        }
        setkey(final_dt, prefix)
        print("done!")
        
        # final_dt
    })
    print(to_print)
    final_dt
}

read_file <- function(file){
    con <- file(file, open="rb")
    res <- readLines(con, encoding="UTF-8", skipNul = T)
    close(con)
    res
}
