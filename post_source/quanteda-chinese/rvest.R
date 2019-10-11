library(rvest)

post_list <- read_html("https://yihui.name/cn/") %>% 
    html_nodes(".archive") %>% 
    html_nodes("p") %>% 
    html_nodes("a") %>% html_attr("href")


all <- read_html("index.html") %>% html_nodes("article") 
title <- all %>% html_nodes("h1")
text <- all %>% html_nodes("p")
links <- all %>% html_nodes("a") %>% html_attr("href")

