library(stringr)

obama.text <- readLines('./datasets/Obama.txt', encoding = "utf-8")

obama.text[10]   # we shall see how this changes

# let us first preprocess

# STEP 1 (Make everything lowercase)
obama.text <- obama.text %>% str_to_lower()
obama.text[10]

# STEP 2 (Process the apostrophe)

obama.text <- obama.text %>% 
    str_replace_all(pattern = c("n't" = " not", 
                                "'ve" = " have", 
                                "'ll" = " will",
                                "'m" = " am",
                                "'re" = " are"))

obama.text[10]    # notice it has that's = that is, we could turns 's = is, but that is not true. 'Ram's pet' does not mean 'Ram is pet'

obama.text <- obama.text %>%
    str_replace_all(pattern = c(
        "this's" = "this is",
        "that's" = "that is",
        "it's" = "it is"
    ))

obama.text[10]

# now we process "Ram's pet" or "Simpsons' pet" to "Ram pet" or "Simpson pet"

obama.text <- obama.text %>% str_replace_all( c("'s" = "") )
obama.text[10]


# STEP 3 (Process the Mr., Mrs. salutation)

obama.text[1]

obama.text <- obama.text %>% str_replace_all(c("mr." = "", 
                                               "mrs." = "",
                                               "ms." = "",
                                               "sr." = ""))
obama.text[1]


# STEP 4 (Remove everything except letters and spaces)

obama.text <- obama.text %>% str_replace_all(pattern = "[^a-zA-Z\\s]", replacement = " ")  # the pattern is regex, it maches everything within the 3rd bracket
obama.text[1]

# STEP 5 (Remove extra trailing whitespaces)
# as you can see, lots and lots of extra spaces in obama.text[1]

obama.text <- obama.text %>% str_replace_all(pattern = "[\\s]+", replacement = " ")   # regex \\s means space, using + means occurence of once or more
obama.text[1]
obama.text[10]
obama.text[50]

###########################################
# Now we split up the words

obama.words <- obama.text %>% str_split(" ", simplify = T)

dim(obama.words)    # there are 76 paragraphs, containing 150 words each. Not all the words, for examples
obama.words[1, 40]   # after some time, it is the empty string, but this gives us a good way to collect all the words for each sentence

x = table(as.vector(obama.words))  # these are all words
x <- x[names(x) != ""]    # remove the empty string from the word list
# there are 1456 words


# let us see some top appearing words
temp <- sort(x, decreasing = T)[1:25]
print(temp)

barplot(temp, names.arg = names(temp), col = rainbow(25), las = 2)   # as you can see, these are mostly stopwords


# Let us now try to create term document matrix, for that, we shall use tidyr package
library(tidyr)

temp <- data.frame(obama.words)
temp$Document <- 1:76     # this is the document number


temp1 <- temp %>% gather("appearence","word",1:150)
temp1 <- temp1[temp1$word!= "", ]   # there are now 6138 words appearining in total combining all paragraphs

term.docmat <- table(temp1$word, temp1$Document)
dim(term.docmat)

term.docmat[1:10, 1:10]

# notice that, column sums gives me the number of words in a document
nwords <- colSums(term.docmat)



# now, we compute tf-idf for each word

tf <- numeric(length(x))
names(tf) <- names(x)

idf <- numeric(length(x))
names(idf) <- names(x)


# compute tf from each word
for (w in names(tf)) {
    tfs <- term.docmat[w, ] / nwords   # this gives me term frequency for each word
    tf[w] <- mean(tfs)    # average of term frequencies
    idf[w] <- ncol(term.docmat) / sum(term.docmat[w, ] > 0)    # this is inverse document frequency without log scale
}


summary(tf)
summary(idf)    # now you get the idea why log is required

# compute tf idf

tfidf <- tf * log(idf)

# let us see some top appearing words
temp <- sort(tfidf, decreasing = T)[1:50]
print(temp)

barplot(temp, names.arg = names(temp), col = rainbow(50), las = 2)   # as you can see, america, united, economy, bless, god pops out















