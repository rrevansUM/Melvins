########################################################
# Term Frequency and tf-idf Using Tidy Data Principles #
########################################################

# A central question in text mining and natural language processing is how to quantify what a document is about. 
# Can we do this by looking at the words that make up the document? 
# One way to approach how important a word can be is its term frequency (tf), 
# how frequently a word occurs in a document. 
# There are words in a document, though, that occur many times but may not be important; 
# in English, these are probably words like "the", "is", "of", and so forth. 
# You might take the approach of adding words like these to a list of stop words and removing them before analysis, 
# but it is possible that some of these words might be more important in some documents than others. 
# A list of stop words is not a sophisticated approach to adjusting term frequency for commonly used words.
# 
# Another approach is to look at a term's inverse document frequency (idf), 
# which decreases the weight for commonly used words and increases the weight 
# for words that are not used very much in a collection of documents. This can 
# be combined with term frequency to calculate a term's tf-idf, the frequency of a term adjusted for how rarely it is used. 
# It is intended to measure how important a word is to a document in a collection (or corpus) of documents. 
# It is a rule-of-thumb or heuristic quantity; 
# while it has proved useful in text mining, search engines, etc., 
# its theoretical foundations are considered less than firm by information theory experts. 
# The inverse document frequency for any given term is defined as
# 
# We can use tidy data principles to approach tf-idf analysis and use consistent, 
# effective tools to quantify how important various terms are in a document that is part of a collection

# Let's start by looking at the published novels of Jane Austen and examine first term frequency, then tf-idf. 
# We can start just by using dplyr verbs such as group_by and join. What are the most commonly used words in Jane Austen's novels?
# (Let's also calculate the total words in each novel here, for later use.)

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()
  
total_words <- book_words %>% 
  group_by(book) %>%
  summarize(total=sum(n))

book_words <- left_join(book_words, total_words)
book_words

# The usual suspects are here, "the", "and", "to", and so forth. 
# Let's look at the distribution of n/total for each novel, 
# the number of times a word appears in a novel divided by the total number of terms (words) in that novel. 
# This is exactly what term frequency is.

library(viridis)

p <- ggplot(book_words, aes(n/total, fill=book))
p <- p + geom_histogram(alpha=0.8, show.legend=FALSE)
p <- p + xlim(NA, 0.0009)
p <- p + labs(title="Term Frequency Distribution in Jane Austen's Novels", y="count")
p <- p + facet_wrap(~book, ncol=2, scales="free_y")
p <- p + theme_minimal(base_size=13)
p <- p + scale_fill_viridis(end=0.85, discrete = TRUE)
p <- p + theme(strip.text=element_text(hjust=0)) + theme(strip.text=element_text(face="italic"))
p

# There are very long tails to the right for these novels (those extremely common words!) that I have not shown in these plots. 
# These plots exhibit similar distributions for all the novels, 
# with many words that occur rarely and fewer words that occur frequently. 
# The idea of tf-idf is to find the important words for the content of each document by decreasing the weight for commonly 
# used words and increasing the weight for words that are not used very much in a collection or corpus of documents, 
# in this case, the group of Jane Austen's novels as a whole. Calculating tf-idf attempts to find the words that are important 
# (i.e., common) in a text, but not too common. Let's do that now.

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

# Notice that idf and thus tf-idf are zero for these extremely common words. 
# These are all words that appear in all six of Jane Austen's novels, 
# so the idf term (which will then be the natural log of 1) is zero. 
# The inverse document frequency (and thus tf-idf) is very low (near zero) 
# for words that occur in many of the documents in a collection; 
# this is how this approach decreases the weight for common words. 
# The inverse document frequency will be a higher number for words that occur in fewer of the documents in the collection. 
# Let's look at terms with high tf-idf in Jane Austen's works

book_words %>% select(-total) %>% arrange(desc(tf_idf))

# Here we see all proper nouns, names that are in fact important in these novels. 
# None of them occur in all of novels, and they are important, characteristic words for each text. 
# Some of the values for idf are the same for different terms because there are 6 documents in this 
# corpus and we are seeing the numerical value for ln(6/1), ln(6/2), etc. 
# Let's look at a visualization for these high tf-idf words.

# devtools::install_github("lionel-/ggstance")
library(ggstance)
library(ggthemes)

plot_austen <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word))))
plot_austen

q <- ggplot(plot_austen[1:20,], aes(tf_idf, word, fill=book, alpha=tf_idf))
q <- q + geom_barh(stat="identity")
q <- q + labs(title="Highest tf-idf words in Jane Austen's Novels", y = NULL, x="tf-idf")
q <- q + theme_tufte(base_family="Arial", base_size=13, ticks=FALSE)
q <- q + scale_alpha_continuous(range=c(0.6, 1), guide=FALSE)
q <- q + scale_x_continuous(expand=c(0,0))
q <- q + scale_fill_viridis(end=0.85, discrete=TRUE)
q <- q + theme(legend.title=element_blank())
q <- q + theme(legend.justification=c(1,0), legend.position=c(1,0))
q

#  Let's look at the novels individually

plot_austen <- plot_austen %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup
plot_austen

r <- ggplot(plot_austen, aes(tf_idf, word, fill = book, alpha = tf_idf)) 
r <- r + geom_barh(stat = "identity", show.legend = FALSE)
r <- r + labs(title = "Highest tf-idf words in Jane Austen's Novels", y = NULL, x = "tf-idf")
r <- r + facet_wrap(~book, ncol = 2, scales = "free")
r <- r + theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE)
r <- r + scale_alpha_continuous(range = c(0.6, 1))
r <- r + scale_x_continuous(expand=c(0,0))
r <- r + scale_fill_viridis(end = 0.85, discrete=TRUE)
r <- r + theme(strip.text=element_text(hjust=0))
r <- r + theme(strip.text = element_text(face = "italic"))
r

# Still all proper nouns! 
# These words are, as measured by tf-idf, 
# the most important to each novel and most readers would likely agree.

