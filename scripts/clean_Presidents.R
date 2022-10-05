library(stringr)
library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)

###########################################
# loading library

speeches <- list.files('./datasets/presidential-speeches/')
speech_dates <- as.Date(substr(speeches, 1, 10))

change_dates <- as.Date(c("1789-04-30", "1797-03-04", "1801-03-04", "1809-03-04", "1817-03-04", "1825-03-04","1829-03-04",
                          "1837-03-04","1841-03-04","1841-04-04","1845-03-04","1849-03-04","1850-07-09", "1853-03-04",
                          "1857-03-04","1861-03-04","1865-04-15","1869-03-04","1877-03-04","1881-03-04", "1881-09-19",
                          "1885-03-04","1889-03-04","1893-03-04","1897-03-04","1901-09-14","1909-03-04","1913-03-04",
                          "1921-03-04", "1923-08-02","1929-03-04","1933-03-04","1945-04-12","1953-01-20","1961-01-20",
                          "1963-11-22","1969-01-20","1974-08-09", "1977-01-20","1981-01-20","1989-01-20","1993-01-20",
                          "2001-01-20","2009-01-20","2017-01-20", "2021-01-01"))


pres_names <- c("George Washington", "John Adams", "Thomas Jefferson", "James Madison", "James Monroe", "John Quincy Adams",
                "Andrew Jackson", "Martin Van Buren", "William Henry Harrison", "John Tyler", "James K. Polk", "Zachary Taylor",
                "Millard Fillmore","Franklin Pierce", "James Buchanan", "Abraham Lincoln", "Andrew Johnson", "Ulysses S. Grant",
                "Rutherford B. Hayes","James A. Garfield","Chester A. Arthur", "Grover Cleveland", "Benjamin Harrison",
                 "Grover Cleveland", "William McKinley", "Theodore Roosevelt", "William Howard Taft", "Woodrow Wilson",
                 "Warren G. Harding", "Calvin Coolidge", "Herbert Hoover", "Franklin D. Roosevelt", "Harry S. Truman",
                "Dwight D. Eisenhower", "John F. Kennedy", "Lyndon B. Johnson", "Richard Nixon", "Gerald Ford", "Jimmy Carter",
                "Ronald Reagan", "George H. W. Bush", "Bill Clinton", "George W. Bush", "Barack Obama", "Donald Trump")


party_names <- c( "Unaffiliated", "Federalist", rep("Democratic-Republican", 4), rep("Democratic", 2), rep("Whig", 2),
                  "Democratic", rep("Whig", 2), rep("Democratic", 2), "Republican", "National Union", rep("Republican", 4),
                  "Democratic", "Republican", "Democratic", rep("Republican", 3), "Democratic", rep("Republican", 3), 
                  rep("Democratic", 2), "Republican", rep("Democratic", 2), rep("Republican",2), "Democratic", rep("Republican",2), 
                  "Democratic", "Republican", "Democratic", "Republican")

names(party_names) <- names(pres_names) <- change_dates[1:45]

speech_data <- as.list(numeric(length(speeches)))
a = cut(speech_dates, change_dates)

pb <- txtProgressBar(min = 0, max = length(speeches), style = 3)
for (i in 1:length(speeches)) {
    tmp <- read_lines(paste0('./datasets/presidential-speeches/', speeches[i]))
    speech_data[[i]] <- tibble(line = 1:length(tmp), text = tmp)
    speech_data[[i]]$president <- pres_names[a[i]]
    speech_data[[i]]$party <- party_names[a[i]]
    speech_data[[i]]$date <- speech_dates[i]

    setTxtProgressBar(pb, value = i)
}

speech_df <- bind_rows(speech_data)
pres_df <- tibble(date = change_dates, party = party_names, president = pres_names)

saveRDS(speech_df, file = "./datasets/US-president-speech.Rds")
saveRDS(pres_df, file = "./datasets/US-president.Rds")


#################################

speech_df <- readRDS('./datasets/US-president-speech.Rds')

# first we filter out all the blank spaces
speech_df <- speech_df %>% dplyr::filter(text != "")

# each speech starts with a line "President: <Name of the President>", which needs to be removed.
speech_df <- speech_df %>% dplyr::filter(line > 1)

# lowercase all text
speech_df <- speech_df %>% mutate(text = str_to_lower(text) )


# the next thing is processing apostrophe
patterns <- c(
    "n't" = " not", 
    "'ve" = " have", 
    "'ll" = " will",
    "'m" = " am",
    "'re" = " are",
    "this's" = "this is",
    "that's" = "that is",
    "it's" = "it is",
    "'s" = "",
    "mr." = "", 
    "mrs." = "",
    "ms." = "",
    "sr." = ""
)
speech_df <- speech_df %>% mutate(text = str_replace_all(string = text, pattern = patterns ) )

# now remove everything expect alphabets
speech_df <- speech_df %>% mutate(text = str_replace_all(text, pattern = "[^a-zA-Z\\s]", replacement = " ") )
    
# now we have clean text, so we can unnest tokens
words_df <- speech_df %>% unnest_tokens(word, text)
    
# removing stopwords
data("stop_words")
words_df <- words_df %>% anti_join(stop_words)

# couting tf-idf and term document matrix
term_doc <- words_df %>% group_by(date, word) %>% summarise(count = n())
term_doc <- term_doc %>% bind_tf_idf(word, date, count)

group_df <- speech_df %>% group_by(date) %>% summarise(president = president[1], party = party[1])
term_doc <- term_doc %>% left_join(group_df, by = c("date" = "date"))

saveRDS(term_doc, './datasets/US-president-speech-termdoc.Rds')

#########################################

term_doc <- readRDS('./datasets/US-president-speech-termdoc.Rds')
term_doc <- term_doc %>% filter(party %in% c("Republican", "Democratic") )
pres_dat <- readRDS('./datasets/US-president.Rds')
party_vec <- pres_dat$party
names(party_vec) <- pres_dat$president

speech_dates <- unique(term_doc$date)
set.seed(1234)
trainIndex <- sample(speech_dates, size = 720)

# create training set
traindata <- term_doc %>% filter(date %in% trainIndex)
traindata %>% group_by(party) %>% summarise(n = length(unique(date)))

# create testing set
testdata <- term_doc %>% filter(! (date %in% trainIndex))
testdata %>% group_by(party) %>% summarise(n = length(unique(date)))

# let's create a visualization graph respresenting correlation between presidents
library(widyr)
president_cors <- traindata %>% pairwise_cor(president, word, count, sort = TRUE)

# plot
library(ggraph)
set.seed(1011)

a <- president_cors %>%
    filter(correlation > .3) %>%
    igraph::graph_from_data_frame()

igraph::V(a)$party <- party_vec[names(igraph::V(a))]

a %>% ggraph(layout = "linear", circular = T)+
    geom_edge_link(aes(alpha = correlation, width = correlation), color = "lightblue") +
    geom_node_point(size = 6, aes(color = party)) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()

######################################













