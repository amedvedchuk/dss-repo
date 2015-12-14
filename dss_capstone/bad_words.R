download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", destfile = "bad_words.txt")

badWords <- readLines("bad_words.txt",  )
badWords

