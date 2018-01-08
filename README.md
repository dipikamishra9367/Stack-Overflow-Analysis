# Stack-Overflow-Analysis
Stack overflow data analysis

#Installing and loading required packages
#install.packages("tidytext")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("XML")
#install.packages("RCurl")
#install.packages("wordcloud")
#install.packages("rvest")
#install.packages("RODBC")
library("RCurl")
library("XML") 
library("lubridate")
library("ggplot2")
library("dplyr")
library("stringr")
library("tidytext")
library("tidyverse")
library("wordcloud")
library("rvest")
library("RODBC")

# Creating Function to scrape data from Stack overflow using RCurl
stack_data = function(url, num_pages)
{
  scrape_data = NULL
  #empty data frame for overall data set
  for(i in 1:num_pages)
  {
    page = getURLContent(url)
    doc = htmlParse(page, asText = TRUE)
    # +++++ Get the posts on current page +++++
    postspath = "//div[@class = 'question-summary']"
    posts = getNodeSet(doc, postspath)
    i = length(posts)
    d = NULL
    #empty data frame for values of a single page
    # +++++ Process posts on current page +++++
    i = posts
    for(i in 1:15) {
      p = posts[[i]]
      # ===== ID Number =====
      id = xpathApply(doc, postspath, xmlGetAttr, "id")[[i]]
      id.ans = gsub("question-summary-", "", id)
      # ===== Author =====
      author = getNodeSet(p, ".//div[@class = 'user-details']/a")
      author.ans = tryCatch(xmlValue(author[[1]]), error = function(e) return(NA))
      # ===== Time Posted =====
      path1 = ".//div[@class = 'user-action-time']/span"
      time.ans = xpathApply(p, path1, xmlGetAttr, "title")[[1]]
      # ===== Title of Post =====
      title = getNodeSet(p, ".//div/h3/a[@class = 'question-hyperlink']")
      title.ans = xmlValue(title[[1]])

        # ===== Reputation Level =====
      replevel = getNodeSet(p, ".//span[@class = 'reputation-score']")
      rep.ans = tryCatch(xmlValue(replevel[[1]]), error = function(e) return(NA))
      # ===== Current Views =====
      path2 = ".//div[@class = 'views ']"
      views = xpathApply(p, path2, xmlGetAttr, "title")[[1]]
      views.ans = gsub(" views", "", views)
      # ===== Current Num. of Answers =====
      path3 = ".//div[@class = 'status unanswered']/strong | .//div[@class = 'status answered']/strong"
      answers = getNodeSet(p, path3)
      num.ans = xmlValue(answers[[1]], trim = TRUE)
      # ===== Votes =====
      votes = getNodeSet(p, ".//div/span[@class = 'vote-count-post ']")
      votes.ans = xmlValue(votes[[1]], trim = TRUE)
      # ===== URL of Post =====
      path4 = ".//div/h3/a[@class = 'question-hyperlink']"
      url.ans = xpathApply(p, path4, xmlGetAttr, "href")[[1]]
      url.ans = paste("http://stackoverflow.com", url, sep = "") # ===== Tags =====
      path6 = ".//div[@class = 'summary']/div[2]"
      tags = xpathApply(p, path6, xmlGetAttr, "class")[[1]]
      tags = gsub("tags ", "", tags)
      tags = gsub("t-", "", tags)
      tags = gsub(" ", "; ", tags)
      # ===== Combine all answers into a row =====
      # id, date, title, tags, url, views, votes, answers, user, reputation & rbind it to data frame
      d = rbind(d, data.frame(id.ans, time.ans, title.ans, tags, url.ans, views.ans, votes.ans, num.ans, author.ans, rep.ans))
    }
    # end for loop for looping over individual posts
    # ===== rbind individual pages to master data frame =====
    scrape_data = rbind(scrape_data, d)


  }
  # end for loop for looping over individual pages
  colnames(scrape_data) = c("id", "date", "title", "tags", "url", "views", "votes", "answers", "user", "reputation")
  return(scrape_data)
}

# Running loop to scrape more number of pages from given URL for both R and Python

 python_data = NULL
 python_data1 = NULL
 for(j in 1:25) {

 u = paste("https://stackoverflow.com/questions/tagged/python?page=",j,  "" ,sep = '')
 python_data = stack_data(u,j)
   python_data1 = rbind(python_data1,python_data)
   }

r_data = NULL
 r_data1 = NULL
 for(j in 1:22) {

 u = paste("https://stackoverflow.com/questions/tagged/r?page=",j,  "" ,sep = '')
   r_data = stack_data(u,j)
 r_data1 = rbind(r_data1,r_data)
}

#write.csv(r_data1,"Rdata.csv.csv")
#write.csv(python_data1, "Python_Data.csv")

# Loading R and python data which is already scraped in above function
r_data1 <- read.csv("Rdata.csv.csv")
python_data1 <- read.csv("Python_data.csv")
# Analysing R data to answer what time of day Questions are posted
# Extracting hours from date time and creating data frame to plot a graph
hours_R <- hour(r_data1$date)
Rhours_df <- as.data.frame(table(hours_R))
colnames(Rhours_df) <- c("Hour_of_the_day", "Number_of_Questions")

# Plotting graph to show at what time frequency of questions getting posted is higher.
# From graph we can see that there is higher frequency of questions getting posted in 
# the evening from 5 to 9 PM whereas frequency very low after midnight till early
# morning
ggplot(Rhours_df,aes(x= Hour_of_the_day,y= Number_of_Questions, 
                     group= Number_of_Questions,fill= Number_of_Questions))+
geom_bar(stat="identity")+ 
scale_fill_gradient("##FF6666")

r_data1$date <- as.POSIXct(r_data1$date)

# This is trending line for questions posted over last 3 days. From graph we can see 
# that more questions were posted on weekdays and frequency reduced over the weekend
r_data1 %>%
  count(Week = round_date(date, "hour")) %>%
  ggplot(aes(Week, n, colour = "blue")) +
  geom_line()

# Determining number of questions posted on weekdays and weekend
is_weekend <- function(d) {
  ifelse(wday(d, label = TRUE) %in% c("Sun", "Sat"), "Weekend", "Weekday")
}

questions_wday <- r_data1 %>%
  mutate(Weekend = is_weekend(date))

type_total <- questions_wday %>%
  count(Weekend) %>%
  rename(TypeTotal = n)
# Number of questions posted on weekdays are way higher than nmber of questions posted
# on weekends
type_total

# What are trending topics in R

r_data_weekend <- questions_wday %>%  filter(Weekend == 'Weekend')
r_data_weekday <- questions_wday %>%  filter(Weekend == 'Weekday')

# Creating trending function to determine what are trending topics in R and Python
# Cleaning scraped data using gsub and removing special characters manually 
trending <- function (y) {
y <- gsub(";", ",", y)
find.listr <- list("r,", "?fÂ»","python,", "-3?fÂ»x", "-2?fÂ»7", "python")
find.stringr <- paste(unlist(find.listr), collapse = "|")
y <- gsub(find.stringr, replacement = "", y)
word_list_r <- strsplit(y, ",")
sep_words_r <- unlist(word_list_r)
trending_topics_r <- as.data.frame(tail(sort(table(sep_words_r)), 11))
trending_topics_r$sep_words_r <- gsub(" shiny", "shiny", trending_topics_r$sep_words_r)
trending_topics_r <- aggregate(Freq ~ sep_words_r, data = trending_topics_r, FUN = sum)
trending_topics_r <- trending_topics_r[c(-10),]
colnames(trending_topics_r) <- c("Trending_Topics", "Number_of_occurance")
return(trending_topics_r)
}

# Scraping data for R using trensing function
trending_topics_r <- trending(r_data1$tags)
# Trending topics in R
# Out of trending tags in R, ggplot and dataframe tags have higher number of questions
# posted (407 and 347 respectively) compared to other tags in R.
ggplot(trending_topics_r,aes(x= Trending_Topics,y= Number_of_occurance, 
                     group= Number_of_occurance,fill= Number_of_occurance))+
geom_bar(stat="identity")+ 
ggtitle("Trending topics in R") +
geom_text(aes(label= Number_of_occurance), vjust = 0.9, color ='red') +
scale_fill_gradient("Count of Topics") +
coord_flip()

# Analysing trending topic in R on weekday and weekend
trending_topics_r_weekend <- trending(r_data_weekend$tags)
trending_topics_r_weekday <- trending(r_data_weekday$tags)

require(gridExtra)
wday <- ggplot(trending_topics_r_weekday,aes(x= Trending_Topics,y= Number_of_occurance,   group= Number_of_occurance,fill= Number_of_occurance))+
geom_bar(stat="identity")+ 
 ggtitle("Treding topic on Weekday in R") +
geom_text(aes(label= Number_of_occurance), vjust = 0.9, color ='red') +
scale_fill_gradient("Count of Topics") +
coord_flip()

wend <- ggplot(trending_topics_r_weekend,aes(x= reorder(Trending_Topics,Number_of_occurance),y= Number_of_occurance, 
                     group= Number_of_occurance,fill= Number_of_occurance))+
geom_bar(stat="identity")+ 
geom_text(aes(label= Number_of_occurance), vjust = 0.9, color ='red') +
  ggtitle("Treding topic on Weekendin R") +
scale_fill_gradient("Count of Topics") +
coord_flip()

# Here we are analysing number of questions posted in trending topics on weekdays and
# weekends. From graph it is seen that total number of questions posted on weekdays 
# on each topic is higher than number of questions posted on weekedns
grid.arrange(wday, wend, ncol=2,widths=c(1.5,1.5))

#Counting views for each trending topic - R
trending_topics_rchar <- as.character(trending_topics_r$Trending_Topics)
trending_views_r <- NULL
trending_views_all_r <- NULL

for(k in 1:10) {
  matching_r <- r_data1[grep(trending_topics_rchar[k], r_data1$tags),]
  matching_r$views <- as.numeric(matching_r$views)
  total_views_r <- as.character(sum(matching_r$views))
  trending_views_r <- data.frame(trending_topics_rchar[k], total_views_r)
  trending_views_all_r <- rbind(trending_views_all_r, trending_views_r)
}
colnames(trending_views_all_r) <- c("Trending_Topics_R", "TotalViews")

trending_views_all_r[,2] <- as.numeric(as.character(trending_views_all_r[,'TotalViews']))

# Total views for dataframe and ggplot are much higher than views of other topics. 
# Whereas views for javascript and matrix have comparatively lower views. We can 
# conclude that R programmers mostly face issues with ggplot and dataframe topics 
# and hence they post more number of questions and views answers related to the same.
ggplot(trending_views_all_r,aes(x= reorder(Trending_Topics_R,TotalViews),y= TotalViews  , group= TotalViews,fill= TotalViews))+
  geom_bar(stat = 'identity')+ 
  geom_text(aes(label= TotalViews), vjust = 0.9, color ='red') +
  labs(x = "Trending Tags", y= "Total Views") +
    ggtitle("Total Views of the trending topic  in R") +
    scale_fill_gradient("Number of Views in Trending Tags") +
  coord_flip()
  
  
  # What time of the day questions are posted - python

hours_py <- hour(python_data1$date)
pyhours_df <- as.data.frame(table(hours_py))
colnames(pyhours_df) <- c("Hour_of_the_day", "Number_of_Questions")

# Plotting graph to show at what time frequency of questions getting posted is higher.
# From graph we can see that there is higher frequency of questions getting posted at
# 5 am EST and then 1 pm EST. it might be possible because stack overflow is used 
# worldwide and python programmers from other country are more active during this time
# to post or answer questions related to Python.
ggplot(pyhours_df,aes(x= Hour_of_the_day,y= Number_of_Questions, 
                     group= Number_of_Questions,fill= Number_of_Questions))+
  geom_bar(stat="identity")+ 
  geom_text(aes(label= Number_of_Questions), vjust = 0.9, color ='black') +
  ggtitle("What time of the day questions are posted - Python") +
  scale_fill_gradient(low = "light green", high = "dark green")

questions_wday_python <- python_data1 %>%
  mutate(Weekend = is_weekend(date))

type_total_py <- questions_wday_python %>%
  count(Weekend) %>%
  rename(TypeTotal = n)
type_total_py
# We scraped 25 pages for both R and Python data. When we analysed the data we were
# able to distinguish R data for weekdays and weekend but for python, all 25 pages 
# data was generated in the weekend. Which clearly indicated that Python has more 
# number of active users and more number of questions gets posted related to Python.

##Filtering weekend and weekday day
python_data_weekend <- questions_wday_python %>%  filter(Weekend == 'Weekend')
python_data_weekday <- questions_wday_python %>%  filter(Weekend == 'Weekday')

#What are trending topics in Python
trending_topics_py <- trending(python_data1$tags)
trending_topics_py <- trending_topics_py[-c(1:2),]

# From graph it is seen that, django, pandas and numpy have more number of questions
# posted than any other topic in python.
ggplot(trending_topics_py,aes(x= reorder(Trending_Topics, Number_of_occurance),y= Number_of_occurance, group= Number_of_occurance,fill= Number_of_occurance))+
geom_bar(stat="identity")+ 
geom_text(aes(label= Number_of_occurance), vjust = 0.9, color ='black') +
labs(x = "Trending Topics", y= "Number of Occurance") +
ggtitle("What are trending topics in Python") +  
scale_fill_gradient(low = "light green", high = "dark green") +
coord_flip()

#Counting views for each trending topic - Python 
trending_topics_char <- as.character(trending_topics_py$Trending_Topics)
trending_views <- NULL
trending_views_all <- NULL
i = 1
for(i in 1:10) {
matching <- python_data1[grep(trending_topics_char[i], python_data1$tags),]
matching$views <- as.numeric(matching$views)
total_views <- as.character(sum(matching$views))
trending_views <- data.frame(trending_topics_char[i], total_views)
trending_views_all <- rbind(trending_views_all, trending_views)
}
colnames(trending_views_all) <- c("Trending_Topics_python", "Total_Views")

trending_views_all[,2] <- as.numeric(as.character(trending_views_all[,'Total_Views']))

trending_views_all <- na.omit(trending_views_all)
trending_views_all

# Total views for pandas, numpy and django are much higher than views of other topics # Whereas views for javascript and file have comparatively lower views. We can 
# conclude that python programmers mostly face issues with pandas, numpy and django
# topics and hence they post more number of questions and views answers related to the
# same. 
ggplot(trending_views_all,aes(x= reorder(Trending_Topics_python,Total_Views),y= Total_Views  , group= Total_Views,fill= Total_Views))+
  geom_bar(stat = 'identity')+ 
  geom_text(aes(label= Total_Views), vjust = 0.9, color ='red') +
  labs(x = "Trending Tags", y= "Total Views") +
  ggtitle("Total Views of the trending topic  in python") +
  scale_fill_gradient(low = "light green", high = "dark green") +
  coord_flip()
  
  # What are questions asked in that trending topic in R ?
# We have used unnest_token function to seperate each word in R dataframe and count 
# it's number of occurance in the dataframe.
title_words <- r_data1 %>% select(title,views)  %>% 
  unnest_tokens(word, title, token = stringr::str_split, pattern = " ") %>% count(word, sort = TRUE)


title_word_counts <- title_words %>% 
  anti_join(stop_words, by = c("word" = "word"))

# We have made word cloud of words that are present in question titles of R
suppressWarnings(wordcloud(title_word_counts$word,title_word_counts$n,col=terrain.colors(length(title_word_counts$word)),min.freq =40))

# questions asked in tredning topics in R
trending_topics_rchar <- as.character(trending_topics_r$Trending_Topics)

trending_questions <- NULL
trending_questions_all <- NULL
# i = 4
for(i in 1:10) {
matching <-r_data1[grep(trending_topics_rchar[i], r_data1$title),]
matching$title <- (matching$title)
if(length(matching$title)==0)  {
            next
        } else {
trending_questions <- data.frame(trending_topics_rchar[i], matching$title) 
trending_questions_all <- rbind(trending_questions_all, trending_questions)
}
}
colnames(trending_questions_all) <- c("Trending_Topics", "Trending_questions")

trending_questions <- data.frame(unique(trending_questions_all$Trending_questions))
colnames(trending_questions) <- c( "Trending_questions")
trending_questions$Trending_questions <- as.character(trending_questions$Trending_questions)

title_words <- trending_questions %>% select(Trending_questions)  %>% 
  unnest_tokens(word, Trending_questions, token = stringr::str_split, pattern = " ") %>% count(word, sort = TRUE)


title_word_counts <- title_words %>% 
  anti_join(stop_words, by = c("word" = "word"))

# This is word cloud for trending topics in R
suppressWarnings(wordcloud(title_word_counts$word,title_word_counts$n,col=terrain.colors(length(title_word_counts$word)),min.freq =40))

title_words <- python_data1 %>% select(title,views)  %>% 
  unnest_tokens(word, title, token = stringr::str_split, pattern = " ") %>% count(word, sort = TRUE)

title_word_counts <- title_words %>% 
  anti_join(stop_words, by = c("word" = "word"))

# This is word cloud for all questions asked related to Python
suppressWarnings(wordcloud(title_word_counts$word,title_word_counts$n,col=terrain.colors(length(title_word_counts$word)),min.freq =40))

trending_questions <- NULL
trending_questions_all <- NULL
#i = 1
for(i in 1:10) {
matching <- python_data1[grep(trending_topics_char[i], python_data1$title),]
matching$title <- (matching$title)
#total_views <- as.character(sum(matching$views))
trending_questions <- data.frame(trending_topics_char[i], matching$title)
trending_questions_all <- rbind(trending_questions_all, trending_questions)
}

colnames(trending_questions_all) <- c("Trending_Topics", "Trending_questions")

trending_questions <- data.frame(unique(trending_questions_all$Trending_questions))
colnames(trending_questions) <- c( "Trending_questions")
trending_questions$Trending_questions <- as.character(trending_questions$Trending_questions)

title_words <- trending_questions %>% select(Trending_questions)  %>% 
  unnest_tokens(word, Trending_questions, token = stringr::str_split, pattern = " ") %>% count(word, sort = TRUE)

title_word_counts <- title_words %>% 
  anti_join(stop_words, by = c("word" = "word"))

# We have made word cloud of words that are present in trending topic in python
suppressWarnings(wordcloud(title_word_counts$word,title_word_counts$n,col=terrain.colors(length(title_word_counts$word)),min.freq =40))

trending_topics_rchar <- as.character(trending_topics_r$Trending_Topics)
trending_topics_rchar

related_tags_frame <- NULL
related_tags_data_frame<- NULL
#i 
for(i in 1:10) {
  p = paste("https://stackoverflow.com/questions/tagged/r+",trending_topics_rchar[i],  "" ,sep = '') 
#Reading the HTML code from the website
webpage <- read_html(p)

#Using CSS selectors to scrap the rankings section
related_tags_html <- html_nodes(webpage,'.js-gps-related-tags')
#Converting the related data to text
related_tags <- html_text(related_tags_html)
#Let's have a look at the rankings

if(length(related_tags) ==0)  {
            next
        }
else 
          {

related_tags <- gsub("[\r\n]", "", related_tags)

trim <- function( x ) {

   gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}



related_tags <- trim(related_tags)

related_tags <- str_replace_all(related_tags, "[^[:alnum:]]", " ")
related_tags <- gsub('[0-9]+', '', related_tags)
related_tags_data <- data.frame(strsplit(gsub("[^[:alnum:] ]", "", related_tags), " +"))
related_tags_data<- related_tags_data[c(-1:-2),]

related_tags_data <- data.frame(related_tags_data)

related_tags_data <- related_tags_data[1:29,]

related_tags_data_frame <- data.frame(trending_topics_rchar[i],related_tags_data)
related_tags_frame <- rbind(related_tags_frame,related_tags_data_frame)
}
}
head(related_tags_data_frame,10)
# We have also scraped related tags for given question. Here is the example of shiny
# tag. These are various tags which are related to shiny tags and appear on the same 
# page along with questions asked related to shiny.
```

```{r}
# Analysis on users and views
users <- as.data.frame(tail(sort(table(r_data1$user)), 10))
users$Var1 <- as.character(users$Var1)
max_users1 <- NULL
#m = 1
for (m in 1:10) {
  max_users <- subset(r_data1, r_data1$user == users$Var1[m])
  max_users1 <- rbind(max_users1, max_users)
  }
max_users1 <- max_users1[,10:11]
users_repu <- unique(max_users1)
# Following is the list of users who answered most number of questions. Pan is the 
# user who answered maximum number of times (67 times) 
users[order(users$Freq, decreasing = TRUE),]

# Following is the list of users who answered most nmber of questions with their 
# reputation
users_repu[order(users_repu$reputation, decreasing = TRUE),]

##Pushing the data scraped in SQL server for saving the historical data and further analysis required in future
##used RODBC package 
channel <- odbcDriverConnect("driver=SQL Server;server=LAPTOP-1MVK9TRM")
 r_data1$date <- as.factor(as.POSIXct(r_data1$date))
#pushing R data
sqlSave(channel,r_data1, rownames = FALSE)
#Pushing Sql Data
sqlSave(channel,python_data1, rownames = FALSE)

