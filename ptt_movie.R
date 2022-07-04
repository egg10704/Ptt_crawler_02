packages = c("jiebaR", "tm", "tmcn", "dplyr", "tidytext", "ggplot2", "wordcloud2", "rvest","stringr","dygraphs","knitr")
existing = as.character(installed.packages()[,1])
for(pkg in packages[!(packages %in% existing)]) install.packages(pkg)

library(rvest)
library(httr)
library(jiebaR)
library(tm)
library(tmcn)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(dygraphs)
library(wordcloud2)
library(knitr)
library(kableExtra)

all_url_page = paste0('https://www.ptt.cc/bbs/movie/index',9502:9503,'.html') 
all_url_data = c(); recom=c(); title=c(); date=c()

#爬每一篇文章的url，每一頁的推文數、標題及日期
for(i in 1:length(all_url_page)){
  all_url_data = c(all_url_data, read_html (all_url_page[i]) %>% 
                     html_nodes(css = ".title a") %>% html_attr('href'))
  recom = c(recom,read_html(all_url_page[i]) %>% html_nodes(css = ".nrec") %>% html_text())
  title = c(title, read_html(all_url_page[i]) %>% html_nodes(css = ".title a") %>% html_text())
  date = c(date, read_html(all_url_page[i]) %>% html_nodes(css = ".date") %>% html_text())
  if(i %% 30 == 0) Sys.sleep(runif(1,2,5))
}

#資料清理
length(title) <- length(date)
my_data<- data.frame(recom,date,title)

title_clear <-  my_data$title %>%
  gsub(pattern = "\\[.+?\\]",., replacement = "") %>% 
  gsub(pattern = "Fw",., replacement = "") %>%
  gsub(pattern = "Re",., replacement = "") %>% 
  gsub(pattern = "[0-9]{1,2}/[0-9]{1,2}",., replacement = "") %>%
  gsub(pattern = "[0-9]{2}:[0-9]{2}",., replacement = "") %>%
  removePunctuation (.,ucp=T) %>% 
  stripWhitespace() 

#計算各種文章類別數
title_1 <- regmatches(my_data$title,regexec(pattern = "\\[.+?\\]",my_data$title))
title_topic=c()
for(i in 1:80){
  title_topic <- c(title_topic,title_1[[i]])
}
title_topic <- title_topic %>% gsub(pattern = " ",., replacement = "") 
title_topic <- as.factor(title_topic)
sort(summary(title_topic), decreasing = TRUE)

#單獨文章類別數查詢
length(grep("Re:", my_data$title))
length(grep("Fw:", my_data$title))
length(grep("好雷", my_data$title))