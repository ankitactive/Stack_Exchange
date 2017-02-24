#Clearing environment and objects
rm(list = ls())

#setting working directory
setwd("C:/Users/ankit/Desktop/Stack_exchange")
getwd()

# libs
library(data.table)
library(ggplot2)
library(tm)

#Reading data
data_path <- 'C:/Users/ankit/Desktop/Stack_exchange'
list.files(data_path)

dat_bio <- read.csv("biology.csv", stringsAsFactors = F)
dat_cook <- read.csv("cooking.csv", stringsAsFactors = F)
dat_crypt <- read.csv("crypto.csv", stringsAsFactors = F)
dat_diy <- read.csv("diy.csv", stringsAsFactors = F)
dat_robot <- read.csv("robotics.csv", stringsAsFactors = F)
dat_travel <- read.csv("travel.csv", stringsAsFactors = F)

# attach a category label
dat_bio$category <- 'biology'
dat_cook$category <- 'cooking'
dat_crypt$category <- 'crypto'
dat_diy$category <- 'diy'
dat_robot$category <- 'robotics'
dat_travel$category <- 'travel'

# combine and remove from environment
dat_all <- rbind(dat_bio, dat_cook, dat_crypt, dat_diy, dat_robot, dat_travel)
rm(dat_bio, dat_cook, dat_crypt, dat_diy, dat_robot, dat_travel)


# duplicate id values across categories, concat id to category and we're good
print(sum(duplicated(dat_all$id)))  # 27,441

dat_all$id_cat <- paste0(dat_all$id, '|', dat_all$category)
print(sum(duplicated(dat_all$id_cat)))  # 0 perrrfect


#DECLARING CUSTOM FUNCTIONS
# helper functions (some of these should move to their own sourceable file) =====

# removes all html tags
remove_html_tags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


# custom tokenizer
custom_tokenizer <- function(param_big_string) {
  #' lower-cases, removes punctuation, new line and return characters, 
  #' and removes unnecessary whitespace, then strsplits 
  split_content <- sapply(param_big_string, removePunctuation, preserve_intra_word_dashes=T)
  split_content <- sapply(split_content, function(y) gsub("[\r\n]", " ", y))
  split_content <- sapply(split_content, tolower)
  split_content <- sapply(split_content, function(y) gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", y, perl=TRUE))
  return(split_content <- (sapply(split_content, strsplit, " ")))
}

# quick clean on content column
dat_all$content <- remove_html_tags(dat_all$content)

# tokenize
token_content <- custom_tokenizer(dat_all$content)
token_tags <- custom_tokenizer(dat_all$tags)
token_titles <- custom_tokenizer(dat_all$title)

# quick glance to make sure we're on track
print(dat_all[1000,2])

#Looping through empty character matrix
# initialize a big empty character matrix
results <- matrix("", ncol=10, nrow=nrow(dat_all))

for(i in 1:nrow(dat_all)) {
  if(i %% 10000 == 0) { print(paste0('printing index every 10k indeces: ', i)) } 
  
  # subset
  this_tags <- token_tags[[i]]
  this_title <- token_titles[[i]]
  this_content <- token_content[[i]]
  
  
  # this was an after thought, slapping the id in the results matrix
  results[i, 10] <- dat_all$id_cat[i]  # id cat is the real unique id  
  
  
  # super simple error checking.. no lengths should be zero
  if(length(this_tags) == 0 | length(this_title) == 0 | length(this_content) == 0) {
    print('length == 0, skipping this line')
    next
  }
  
  # length
  this_tag_length <- length(this_tags) 
  results[i, 1] <- this_tag_length
  this_title_length <- length(this_title) 
  results[i, 2] <- this_title_length
  this_content_length <- length(this_content) 
  results[i, 3] <- this_content_length
  
  # matches
  sum_title_matches <- sum(this_tags %in% this_title) #  - col 4
  results[i, 4] <- sum_title_matches
  sum_content_matches <- sum(this_tags %in% this_content) #  - col 5
  results[i, 5] <- sum_content_matches
  
  # percents - a bit verbose here, but it is self-documenting
  results[i, 6] <- round(sum_title_matches / this_tag_length * 100, digits=2) # perc_tag_title_matches - col 6
  results[i, 7] <- round(sum_content_matches / this_tag_length * 100, digits=2) # perc_tag_content_matches - col 7
  results[i, 8] <- round(sum_title_matches / this_title_length * 100, digits=2) # perc_title_richness - col 8
  results[i, 9] <- round(sum_content_matches / this_content_length * 100, digits=2) # perc_content_richness - col 9
  
}

#JOINING DATA BACK TOGETHER
# join back in to original data
result_names <- c('tag_length', 'title_length', 'content_length', 'title_matches', 
                  'content_matches', 'percent_title_matches', 'percent_content_matches', 
                  'title_tag_richness', 'content_tag_richness', 'id_cat')
results_df <- data.frame(results, stringsAsFactors = F)
names(results_df) <- result_names


# quicker merges with dt
setDT(dat_all); setkey(dat_all, id_cat)
setDT(results_df); setkey(results_df, id_cat)

initial_tag_matches <- merge(x=dat_all, results_df, all=T)


setDF(initial_tag_matches)


for(i in 7:15) {
  # this won't work with a data.table
  initial_tag_matches[, i] <- as.numeric(initial_tag_matches[, i])
}

#Plotting results
#percentage of tags that exactly match a word in the title
theme_set(theme_gray(base_size = 12))

# all overlapping with transparency 
ggplot(initial_tag_matches, aes(x = as.numeric(percent_title_matches), fill=category )) +
  geom_density(alpha=0.3) +
  ggtitle("Density: Percent of Tags that EXACTLY Match a Word in the Title") +
  labs(x='Percent of Tag Words Found in Title') +
  geom_rug(color='darkblue')


#percentage if tags that exactly match a word in the content
ggplot(initial_tag_matches, aes(x = as.numeric(percent_content_matches), fill=category )) +
  geom_density(alpha=0.3) +
  ggtitle("Density: Percent of Tags that EXACTLY Match a Word in the Content") +
  labs(x='Percent of Tag Words Found in Content') +
  geom_rug(color='darkblue')


#HISTOGRAM AND DENSITY OF MATCHING TAGS
# faceted density and histogram for % tags matching title
#DENSITY:percent of tags that exactly match a word in the title
ggplot(initial_tag_matches, aes(x = as.numeric(percent_title_matches) )) +
  geom_density(alpha=0.8, fill='darkred') +
  facet_wrap(~category) +
  ggtitle("Density: Percent of Tags that EXACTLY Match a Word in the Title") +
  labs(x='Percent of Tag Words Found in Title') +
  geom_rug(color='darkgreen')

#HISTOGRAM:percentage of tags that exactly match a title in the title
ggplot(initial_tag_matches, aes(x = as.numeric(percent_title_matches) )) +
  geom_histogram(alpha=0.8, fill='darkred', binwidth=10) +
  facet_wrap(~category) +
  ggtitle("Histogram: Percent of Tags that EXACTLY Match a title in the Title") +
  labs(x='Percent of Tag Words Found in Title') +
  geom_rug(color='darkgreen')



#Histogram and Density of Tags Matching Content
# faceted density and histogram for % tags matching content
#DENSITY:percent of tags that exactly match a word in the content
ggplot(initial_tag_matches, aes(x = as.numeric(percent_content_matches) )) +
  geom_density(alpha=0.6, fill='darkgreen') +
  facet_wrap(~category) +
  ggtitle("Density: Percent of Tags that EXACTLY Match a Word in the Content") +
  labs(x='Percent of Tag Words Found in Content') +
  geom_rug(color='darkred')

#HISTOGRAM:percent of tags that exactly match a word in the content
ggplot(initial_tag_matches, aes(x = as.numeric(percent_content_matches) )) +
  geom_histogram(alpha=0.6, fill='darkgreen', binwidth=10) +
  facet_wrap(~category) +
  ggtitle("Histogram: Percent of Tags that EXACTLY Match a Word in the Content") +
  labs(x='Percent of Tag Words Found in Content') +
  geom_rug(color='darkred')
