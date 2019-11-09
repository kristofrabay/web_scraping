library(rvest)
library(data.table)

# 10 news shown on one page, starting with 0, then 10, 20, etc..
my_url <- "https://boeing.mediaroom.com/news-releases-statements?l=10&o="
start <- 0
url <- paste0(my_url, start)
boeing <- read_html(url)

#checking content of downloaded HTML
write_html(boeing, "C:/Users/Krisz/Desktop/ceu/materials/fall_2/web_scraping/course_work/assignment/web_scraping/boeing/boeing.html")
View(boeing)

# selecting a couple of pages to be downloaded
# loading HTML items (teaser texts not available for all items, so need to deal with NA values)
# get one item --> lapply for all items --> bind lists together

pages <- seq(from = 0, to = 100, by = 10)

urls <- paste0(my_url,pages)

boeing_final <- rbindlist(lapply(urls, function(url){
  
  boeing <- read_html(url)
  
  boeing_news_item <- boeing %>% html_nodes('.wd_item')
  
  boeing_DF <- rbindlist(lapply(boeing_news_item, function(item){
    
    title <- item %>% html_nodes('.wd_tooltip-trigger a') %>% html_text
    date <- item %>% html_nodes('.wd_date') %>% html_text
    summ <- item %>% html_nodes('.wd_subtitle') %>% html_text
    summ <- ifelse(length(summ)==0, NA, summ)
    links <- item %>% html_nodes('.wd_tooltip-trigger a') %>% html_attr('href')
    boeing_df <- data.frame("title"=title,
                            "date" = date,
                            "summary" = summ,
                            "link" = links)
    return(boeing_df)
    
  }))
  
  return(boeing_DF)
  
}))

View(boeing_final)

saveRDS(boeing_final, file = "C:/Users/Krisz/Desktop/ceu/materials/fall_2/web_scraping/course_work/assignment/web_scraping/boeing/boeing_news.rds")




### some text analysis

#install.packages("stopwords")
library(stopwords)
#install.packages("tm")
library(tm)
#install.packages("wordcloud")
library(wordcloud)
library(wordcloud2)

# including the word Boeing
title_text <- gsub("'","",gsub("[^0-9A-Za-z///' ]","",tolower(toString(boeing_final$title))))
cleaned <- gsub("  "," ",removeWords(title_text, stopwords()))
wc <- Corpus(VectorSource(cleaned))
dtm <- TermDocumentMatrix(wc)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud2(d)



# excluding the word Boeing

title_text2 <- gsub("'","",gsub("[^0-9A-Za-z///' ]","",tolower(toString(boeing_final$title))))
title_text2 <- gsub("boeing","",title_text2)
cleaned2 <- gsub("  "," ",removeWords(title_text2, stopwords()))
wc2 <- Corpus(VectorSource(cleaned2))
dtm2 <- TermDocumentMatrix(wc2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 10)

wordcloud2(d2)




