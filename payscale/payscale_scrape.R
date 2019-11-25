### GET ALL JOB DATA FROM PAYSCALE

### CURRENTLY ONLY A-I JOBS ARE BEING STORED
### WILL HAVE TO LOOK INTO SOME BROKEN LINKS

library(rvest)
library(jsonlite)
library(data.table)
library(tidyverse)
library(pbapply)

main_url <- "https://www.payscale.com/research/US/Job"
main_url_html <- read_html(main_url)

industries <- main_url_html %>% html_nodes(".related-content-card__title") %>% html_text() #get all industries covered on website

urls <- unlist(lapply(seq(1,length(industries)), function (x){
  paste0("https://www.payscale.com/research/US/Job/", industries[x], "/", LETTERS)
})) # got a list containing all possible links on payscale

create_salary_urls <- function(url) {

tryurl <- read_html(url)
tryurl_jobs <- tryurl %>% html_nodes(".subcats__links__item") %>% html_attr("href")
tryurl_jobs <- gsub("/research/","",tryurl_jobs)
tryurl_url <- paste0("https://www.payscale.com/research/", tryurl_jobs) #browseURL(tryurl_url[6]) links work
tryurl_url <- tryurl_url %>% .[!grepl("hourly_rate",tolower(.))] #sorting out temporary jobs - jobs that calculate with hourly wage instead of salary

return(tryurl_url)

}

all_jobs_urls <- unlist(pblapply(urls[1:10], create_salary_urls))

get_one_job <- function(url) {

job <- read_html(url)
job_json <- job %>% html_nodes(xpath = "//script[@type='application/json']") %>% html_text()
job_read <- fromJSON(job_json)

name <- ifelse(is.null(job_read$props$pageProps$pageData$dimensions$job),"NA",job_read$props$pageProps$pageData$dimensions$job)
salary_median <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$salary$`50`),"NA",job_read$props$pageProps$pageData$compensation$salary$`50`)
hourly_wage_median <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$hourlyRate$`50`),"NA",job_read$props$pageProps$pageData$compensation$hourlyRate$`50`)
bonus <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$bonus$`50`),"NA",job_read$props$pageProps$pageData$compensation$bonus$`50`)
total_q25 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$total$`25`),"NA",job_read$props$pageProps$pageData$compensation$total$`25`)
total_q50 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$total$`50`),"NA",job_read$props$pageProps$pageData$compensation$total$`50`)
total_q75 <- ifelse(is.null(job_read$props$pageProps$pageData$compensation$total$`75`),"NA",job_read$props$pageProps$pageData$compensation$total$`75`)
satis <- ifelse(is.null(job_read$props$pageProps$pageData$ratings$`Job Satisfaction Overall`$score),"NA",job_read$props$pageProps$pageData$ratings$`Job Satisfaction Overall`$score)
occup <- ifelse(is.null(job_read$props$pageProps$pageData$occupationalDetails$occupationalCategory),"NA",job_read$props$pageProps$pageData$occupationalDetails$occupationalCategory)
lastupdate <- ifelse(is.null(job_read$props$pageProps$pageData$lastUpdated),"NA",str_sub(job_read$props$pageProps$pageData$lastUpdated,1,10))

job_data <- data.frame(name, 
                       occup,
                       lastupdate,
                       salary_median, 
                       hourly_wage_median,
                       bonus,
                       total_q25,
                       total_q50,
                       total_q75,
                       satis)

return(job_data)

}

get_all_jobs <- rbindlist(pblapply(all_jobs_urls, get_one_job))

View(get_all_jobs)

saveRDS(get_all_jobs, "payscale/jobs.rds")
