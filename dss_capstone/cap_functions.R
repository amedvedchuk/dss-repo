

library(quanteda)
library(stringi)
library(data.table)
library(hashr)

predict_next <- function(dt, phrase, is_hash = FALSE){
    # tokens <- unlist(tokenize(tolower("After years of work Semmi was vey tired"), removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 3))
    
    tokens <- unlist(tokenize(tolower(phrase), removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 2))
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


make_ngrams <- function(lines, ngram = 2){
    
    to_print <- system.time({
        # remove non ASCII
        print(Sys.time())
        print("remove non ASCII: [›—’‘“]+")
        replaced <- stri_replace_all(lines, "", regex = "[›—’‘“]+")
        print(Sys.time())
        print("remove non ASCII: [[:space:]]+[a-zA-Z]*([^A-Za-z \\d[:punct:]]+[a-zA-Z]*)+")
        replaced <- stri_replace_all(replaced, "", regex = "[[:space:]]+[a-zA-Z]*([^A-Za-z \\d[:punct:]]+[a-zA-Z]*)+")
        # replaced <- stri_replace_all(replaced, "", regex = "[[:space:]]+[a-zA-Z]*([^A-Za-z \\d\"\\.!,\\(\\)\\?\\-']+[a-zA-Z]*)+")
        
        print(Sys.time())
        print("start tokenisation to sentences...")
        # tokenise to sentences
        replaced <- unlist(tokenize(replaced, what = "sentence"))
        print(Sys.time())
        print("replace punctuation")
        replaced <- stri_replace_all(replaced, "", regex = "[[:punct:]]+")
        print(Sys.time())
        print("replace non ASCII again")
        replaced <- stri_replace_all(replaced, "", regex = "[[:space:]]+[a-zA-Z]*([^A-Za-z \\d[:punct:]]+[a-zA-Z]*)+")
        print(Sys.time())
        print(cat("sentences length: ", length(replaced)))
        
        print("tokenize to ngrams...")
        replaced <<- tokenize(toLower(replaced), removeNumbers = TRUE, removeTwitter = TRUE, removePunct = T,  ngrams = ngram)
        
        # dfm_ng <- dfm(sentences, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE, removeTwitter = TRUE, ngrams = ngram)
        print(Sys.time())
        print("start DFM creation....")
        replaced <<- dfm(replaced)
        
        print("start col sums...")    
        replaced <<- colSums(replaced)
        print("done!")
    })
    print(to_print)
    replaced
}

make_table <- function(ngrams, nlength = 2, is_hash = FALSE){
    to_print <- system.time({
        print("start table preparation...")
        splitted <- stri_split_fixed(names(ngrams), pattern = "_", simplify = T)
        ulen <- dim(splitted)[2] 
        print(paste(c("rows", "cols"), dim(splitted), collapse = " "))
        final_dt <- data.table(prefix = apply(splitted[,1:nlength-1], 1, stri_c, collapse="_"),
                               lastw = splitted[,nlength],
                               freq = ngrams
        )
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

