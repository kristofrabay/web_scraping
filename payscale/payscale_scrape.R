### GET ALL JOB DATA FROM PAYSCALE

library(rvest)
library(jsonlite)
library(data.table)
library(tidyverse)
library(pbapply)

main_url <- "https://www.payscale.com/research/US/Job"
main_url_html <- read_html(main_url)

# first load all industries
industries <- main_url_html %>% html_nodes(".related-content-card__title") %>% html_text() #get all industries covered on website


# secondly load every possible scenario: industry - starting letter of job
urls <- unlist(lapply(seq(1,length(industries)), function (x){
  paste0("https://www.payscale.com/research/US/Job/", industries[x], "/", LETTERS)
})) # got a list containing all possible links on payscale


# check if link referring to industry/letter is "broken" or not, discard broken ones.
# also: map industry to newly created link
working_urls <- list()
i = 0
working_urls <- unlist(pblapply(urls, function(check_url_error) {
  i = i + 1
  if (httr::http_error(check_url_error) == "FALSE") {
    append(check_url_error, working_urls[i])
  }
}))


# get actual links that point to sites containing information
# only keeping those with salaries, not hourly rates
create_salary_urls <- function(url) {

tryurl <- read_html(url) %>% 
  html_nodes(".subcats__links__item") %>% 
  html_attr("href")

tryurl_jobs <- gsub("/research/","",tryurl)

tryurl_url <- paste0("https://www.payscale.com/research/", tryurl_jobs) %>% .[!grepl("hourly_rate",tolower(.))]
  
return(tryurl_url)

}


# create every possible job link that will be used to extract data from
all_jobs_urls <- unlist(pblapply(working_urls, create_salary_urls))
# 4926 jobs found (by links)


#all_jobs_urls <- all_jobs_urls[!grepl(paste0("research/NA", collapse = "|"), all_jobs_urls)] <-- no need any more, as errorous links were found earlier
write_csv(as.data.frame(all_jobs_urls), "payscale/joburls.csv")


# function for one job data extraction
get_one_job <- function(url) {

job <- read_html(url)
job_json <- job %>% html_nodes(xpath = "//script[@type='application/json']") %>% html_text()
job_read <- fromJSON(job_json)

name <- ifelse(is.null(job_read$props$pageProps$pageData$dimensions$job),"NA",job_read$props$pageProps$pageData$dimensions$job)
description <- ifelse(is.null(job_read$props$pageProps$pageData$narratives$description),"NA",job_read$props$pageProps$pageData$narratives$description)
hourly_wage_q50 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$hourlyRate$`50`),"NA",job_read$props$pageProps$pageData$compensation$hourlyRate$`50`)
salary_q25 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$salary$`25`),"NA",job_read$props$pageProps$pageData$compensation$salary$`25`)
salary_q50 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$salary$`50`),"NA",job_read$props$pageProps$pageData$compensation$salary$`50`)
salary_q75 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$salary$`75`),"NA",job_read$props$pageProps$pageData$compensation$salary$`75`)
bonus_q50 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$bonus$`50`),"NA",job_read$props$pageProps$pageData$compensation$bonus$`50`)
total_q10 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$total$`10`),"NA",job_read$props$pageProps$pageData$compensation$total$`10`)
total_q25 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$total$`25`),"NA",job_read$props$pageProps$pageData$compensation$total$`25`)
total_q50 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$total$`50`),"NA",job_read$props$pageProps$pageData$compensation$total$`50`)
total_q75 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$total$`75`),"NA",job_read$props$pageProps$pageData$compensation$total$`75`)
total_q90 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$total$`90`),"NA",job_read$props$pageProps$pageData$compensation$total$`90`)
satis <- ifelse(is.null(job_read$props$pageProps$pageData$ratings$`Job Satisfaction Overall`$score),"NA",job_read$props$pageProps$pageData$ratings$`Job Satisfaction Overall`$score)
occup <- ifelse(is.null(job_read$props$pageProps$pageData$occupationalDetails$occupationalCategory),"NA",job_read$props$pageProps$pageData$occupationalDetails$occupationalCategory)
lastupdate <- ifelse(is.null(job_read$props$pageProps$pageData$lastUpdated),"NA",str_sub(job_read$props$pageProps$pageData$lastUpdated,1,10))

job_data <- data.frame(name,
                       description,
                       occup,
                       satis,
                       lastupdate,
                       hourly_wage_q50,
                       salary_q25,
                       salary_q50,
                       salary_q75,
                       bonus_q50,
                       total_q10,
                       total_q25,
                       total_q50,
                       total_q75,
                       total_q90)

return(job_data)

}


# extract data for all jobs --> done
get_all_jobs <- rbindlist(pblapply(all_jobs_urls, get_one_job))


# clean the occupational categories column
get_all_jobs$occup_name <- sub("\\d{2}-\\d{4}.\\d{2} - ", "", get_all_jobs$occup)
get_all_jobs$occup_code <- trimws(gsub(" - ", "", gsub("[^0-9 | .-]", "", get_all_jobs$occup)))
get_all_jobs <- get_all_jobs %>% select(name, occup_name, occup_code, everything()) %>% select(-occup)


# save cleaned dataset
saveRDS(get_all_jobs, "payscale/jobs.rds")
write_csv(get_all_jobs, "payscale/jobs.csv")
View(get_all_jobs)
str(get_all_jobs)


### JOIN MAIN INDUSTRIES TO JOB LEVELS

### TODO 

### ANALYSIS


jobs_data <- read_csv("payscale/jobs.csv")
str(jobs_data)
View(jobs_data)
jobs_no_na <- jobs_data %>% filter(!is.na(salary_q50))


# average median salary by occupation group, and by job level
occup_medians <- jobs_no_na %>% group_by(occup_name) %>% summarize(median_salary = mean(salary_q50),
                                                                   count_occupations = n()) %>% arrange(desc(median_salary))

job_medians <- jobs_no_na %>% 
  group_by(name) %>% 
  summarize(median_salary = salary_q50) %>% 
  arrange(desc(median_salary))

job_medians #cardiologist, anesthesiologist, ..., investment bank and venture firm partners earn most money


# Distribution of median salaries in US
library(scales)

(hist_level <- jobs_no_na %>% ggplot(aes(salary_q50)) +
  geom_histogram(binwidth = 5000, color = "darkblue", fill = "blue") +
  scale_x_continuous(labels = dollar, breaks = c(0,50000,100000,150000,200000,250000,300000)) +
  labs(title = "Right-skewed distribution of salaries in US market",
       x = "Salary in USD",
       y = "Frequency") +
  theme_bw())

ggsave("payscale/histogram_level_salaries.png", device = "png", plot = hist_level)

(hist_log <- jobs_no_na %>% ggplot(aes(log(salary_q50))) +
    geom_histogram(binwidth = 0.05, color = "darkblue", fill = "blue") +
    scale_x_continuous() +
    labs(title = "Taking log of salaries proves log-normal distribution of wages",
         x = "Salary in USD, ln scale",
         y = "Frequency") +
    theme_bw())

ggsave("payscale/histogram_log_salaries.png", device = "png", plot = hist_log)


(occup_medians_plot <- occup_medians %>% 
  filter(!is.na(occup_name)) %>% 
  ggplot(aes(median_salary, reorder(occup_name,median_salary))) +
  geom_point(color = "blue", size = 1) +
  labs(x = "Median salary for occupations",
       y = "Occupation categories") +
  geom_vline(xintercept = mean(occup_medians$median_salary), color = "green", size = 1.1) +
  scale_x_continuous(labels = dollar) +
  theme_bw() +
  theme(axis.text.y = element_text(color = "grey20", size = 4, angle = 0, hjust = 1, vjust = 0, face = "plain")))
  
ggsave("payscale/occup_medians.png", 
       device = "png", 
       plot = occup_medians_plot, 
       width = 15, 
       height = 30, 
       units = "cm")


  
