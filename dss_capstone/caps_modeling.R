
total <- c(readLines("final/en_US/en_US.blogs.txt"),
           readLines("final/en_US/en_US.news.txt"),
           readLines("final/en_US/en_US.twitter.txt"))
total2 <- total[round(runif(n = length(total)*0.3, min=1, max=length(total)))]
rm(total2)

lines <- readLines("tags.txt")
stri_replace_all(lines[1:10], "", regex = "[?????]+|(&?amp;)+|(&?lt;?)+|(&?gt;)+|class=\"[^\"]*\"|style=\"[^\"]*\"|background:[^\"]*\"|[/;]*span|goog.{1,64}-spellcheck-word")
lines[9]
replaced

total <- total2

length(total2)/length(total)

ngrams <- make_ngrams(total, 3)
dt2 <- make_table(ngrams, 3)
dt2

saveRDS(ngrams, "ngrams_2016_01_03.rds")

lines <- readLines("final/en_US/en_US.blogs.txt", n = 1000)

lines[1:5]

replaced <- stri_replace_all(lines, "", regex = "[[:space:]]+[a-zA-Z]*([^A-Za-z \\d[:punct:]]+[a-zA-Z]*)+")
replaced[1:5]

replaced <- stri_replace_all(lines, "<rep>\n", regex = "[0-9]*.[0-9]+")
replaced <- stri_replace_all(lines, "<rep>\n", regex = "[.!?]")
replaced[1:3]
write(replaced[1:3], file = "replaced.txt")

tokenize(c("this is one iline \n this is another line"), what = "sentence")
unlist(tokenize(lines[1:1000], what = "sentence", removeNumbers = T, removePunct = T))


res <- tokenize(toLower(lines), removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 3)
res
res[1]
lines[1]

stri_match_all(lines[1:3], regex="[a-zA-Z]*([^A-Za-z \\d\\.!,\\(\\)\\?\\-']+[a-zA-Z]*)+")
unlist(stri_match_all(lines[1:1000], regex="[[:space:]]+[a-zA-Z]*([^A-Za-z \\d\"\\.!,\\(\\)\\?\\-']+[a-zA-Z]*)+"))

unlist(stri_match_all(total[1:1000], regex="[[:space:]]+[a-zA-Z]*([^A-Za-z \\d[:punct:]]+[a-zA-Z]*)+"))
x <- unlist(stri_match_first(total[1:1000], regex="[_[^\\w\\s[:punct:]]]+")); x[!is.na(x)]
x <- unlist(stri_match_first(dtl$dt2[5638500:5638517]$prefix, regex="[^a-zA-Z0-9\\s']+")); x[!is.na(x)]



unlist(stri_match_all(lines[1:1000], regex="[[:punct:]]+"))
unlist(stri_match_all(total[300:500], regex="(&?amp;)+|(&?lt;)+|(&?gt;)+|class=\"[^\"]*\"|style=\"[^\"]*\""))




unlist(stri_match_all_regex(lines[1:1000], pattern ="...[0-9]+([\\.\\-:/,][0-9]+)*..."))
unlist(stri_match_all_regex(lines[1:1000], pattern ="....._+....."))
unlist(stri_match_all_regex(lines[1:1000], pattern ="...(https?:\\/\\/)?([\\w\\.]+)\\.([a-z]{2,6}\\.?)(\\/[\\w\\.]*)*\\/?..."))


unlist(stri_match_all_regex(tokenize(lines[1:1000], removeNumbers = T, removeSeparators = T, removePunct = T), pattern ="...[0-9]+([\\.\\-:/,][0-9]+)*..."))


 stri_stats_general(lines)

lines <- c("one two three four five six seven eigth nine ten one three four")

dt_old <- td

ngrams <- make_ngrams(lines, 3)
dt2 <- make_table(ngrams, 3)
dt2
dt <- make_table(ngrams, 3, is_hash = T)

predict_next(dt2, "two three four", is_hash = T)


dt
predict_next(dt, "After years of work Semmi was very", is_hash = T)

in_the <- dt["in_the"]
in_the





usb <- dfm(lines, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 3)

head(usb)

dt_usb <- data.table(usb)

unigrams <- colSums(usb)
h_uni <- head(unigrams)
h_uni

names(h_uni[1])
names(as.list(h_uni[1]))

apply(h_uni, 1, print)

dim(h_uni)

splitted <- stri_split_fixed(names(h_uni), pattern = "_", simplify = T)
ulen <- dim(splitted)[2]
final-dt <- data.table(unigram = apply(splitted[,1:ulen-1], 1, stri_c, collapse="_"),
           wlast = splitted[,ulen],
           freq = h_uni)




lapply(as.list(h_uni), function(row){
    row
#     words <- stri_split_fixed(names(row), pattern = "_", simplify = T)
#     words
#     len <- 3
#     c(words[1:len], words[len]) 
    # str(row)
    # print(dimnames(row))
    # names(row)
})

names(h_uni)



stri_c(, collapse = "_")

stri_extract_all("this_is_sparta", regex = "_")
stri_extract_all_boundaries("this_is_sparta", "_")

head(sort(unigrams, decreasing = T))
names(unigrams)
dt_uni <- data.table(word = names(unigrams), count = unigrams)


head(unigrams)

# plot(unigrams)

usb@Dim

str(dt_usb)

object.size(dt_usb)
object.size(usb)
object.size(unigrams)
object.size(dt_uni)
