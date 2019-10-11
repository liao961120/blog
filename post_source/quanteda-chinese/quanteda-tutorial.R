### Source Code pulled from "quanteda-tutorial.Rmd" by  ###
### purl("quanteda-tutorial.Rmd", documentation = 2L)   ###

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	fig.dim = c(7, 4),
	message = FALSE,
	warning = FALSE,
	comment = "",
	out.width = "100%"
)
library(knitr)

## ----echo=FALSE----------------------------------------------------------
library(htmltools)
HTML('
<div class="mermaid">
graph LR
html("HTML")
html -.->|"rvest"| df0
subgraph 前處理
df1("斷詞 data_frame")
df0("data_frame")
df0 -.->|"<br>     jiebaR <br>   (保留標點)<br>"| df1
df1 -.->|"ropencc <br> 簡轉繁"| df1
end
corp("Corpus")
token("Tokens")
subgraph quanteda
df1 -.->|"quanteda <br> corpus()"| corp
corp -.->|"quanteda <br> tokenize()"| token
end
html -.- bls(" ")
style bls fill:none,stroke:none
style html fill:#ccbdb9
style df1 fill:#92ff7f
linkStyle 5 stroke-width:0px,fill:none;
</div>
')

## ------------------------------------------------------------------------
library(dplyr)
library(rvest)

list_of_posts <- read_html("https://yihui.name/cn/") %>% 
    html_nodes(".archive") %>% # 列表在 div.archive 之下
    html_nodes("p") %>% # 文章標題在 <div> 下之 <p>
    html_nodes("a") %>% html_attr("href") # 文章連結在 <p> 下之 <a> 

readr::write_lines(list_of_posts, "yihui/list_of_post.txt")

## ------------------------------------------------------------------------
head(list_of_posts, 2)
tail(list_of_posts, 2)
length(list_of_posts)

## ------------------------------------------------------------------------
library(stringr)
set.seed(2018) # 設隨機種子 固定隨機函數的結果

idx <- str_detect(list_of_posts, "2018|2015|2010")
sub_list <- list_of_posts[idx]
sub_list <- sub_list[sample(seq_along(sub_list), 20)]  %>% # 抽出 20 篇
    str_replace_all(pattern = "^/", # 將站內連結改為完整 url
                    replacement = "https://yihui.name/") %>%
    str_replace_all(pattern = "/$", "/index.html")

readr::write_lines(sub_list, "yihui/sublist.txt")

# 給 Bash 用的
sub_list %>%
    str_replace_all("https://yihui.name/cn/", "") %>%
    str_replace_all("/index.html", "") %>%
    str_replace_all("/", "-") %>% 
    str_replace_all("-$", "") %>%
    readr::write_lines("yihui/sublist_name.txt")

## cd yihui/html

## #!/bin/bash

## ------------------------------------------------------------------------
path <- "https://yihui.name/cn/2015/11/peer-review/"
all <- read_html(path) %>%
    html_nodes("article")
header <- all %>% html_nodes("header")

title <- header %>%      # 文章標題
    html_nodes("h1") %>% html_text()

post_date <- header %>%  # 發文日期
    html_node("h3") %>% html_text() %>%
    str_extract("201[0-9]-[0-9]{2}-[0-9]{2}")

article <- all %>%       # 內文
    html_nodes("p") %>% 
    html_text() %>% paste(collapse = "\n") 
    # 這裡將 chr vector collapse 至 1 個字串，
    # 簡化資料結構，並以分行符號保留段落資訊

num_sec <- all %>%      # 內文段落數
    html_nodes("p") %>% length

links <- all %>% html_nodes("p") %>% # 內文連結  
    html_nodes("a") %>% html_attr("href")
link_text <- all %>% html_nodes("p") %>% # 內文連結標題
    html_nodes("a") %>% html_text()

## ------------------------------------------------------------------------
library(tibble)
df <- data_frame(title = title,
           date = post_date,
           content = article,
           num_sec = num_sec,
           links = list(links),
           link_text = list(link_text)
           )
df %>%
    mutate(title = str_trunc(title, 8),
           content = str_trunc(content, 8),
           links = str_trunc(links, 8),
           link_text = str_trunc(link_text, 8)) %>%
    kable("markdown", align = "c")

## ----fc-post_data--------------------------------------------------------
post_data <- function (path) {
    all <- read_html(path) %>%
        html_nodes("article")
    header <- all %>% html_nodes("header")
    
    title <- header %>%      # 文章標題
        html_nodes("h1") %>% html_text()
    
    post_date <- header %>%  # 發文日期
        html_node("h3") %>% html_text() %>%
        str_extract("201[0-9]-[0-9]{2}-[0-9]{2}")
    
    article <- all %>%       # 內文
        html_nodes("p") %>% 
        html_text() %>% paste(collapse = "\n")
        # 這裡將 chr vector collapse 至 1 個字串，
        # 簡化資料結構，並以分行符號保留段落資訊
        
    num_sec <- all %>%      # 內文段落數
        html_nodes("p") %>% length
    
    links <- all %>% html_nodes("p") %>% # 內文連結  
        html_nodes("a") %>% html_attr("href")
    link_text <- all %>%     # 內文連結標題
        html_nodes("p") %>% 
        html_nodes("a") %>% html_text()
    
    df <- tibble::data_frame(title = title,
                             date = post_date,
                             content = article,
                             num_sec = num_sec,
                             links = list(links),
                             link_text = list(link_text)
                             )
}

## ------------------------------------------------------------------------
library(dplyr)
library(tidyr)

html_list <- list.files("yihui/html/") # 列出資料夾下的檔案
all_post <- vector("list", length(html_list))

for (i in seq_along(html_list)) {
    path <- paste0("yihui/html/", html_list[i])
    all_post[[i]] <- post_data(path)
}

all_post <- bind_rows(all_post) %>% arrange(desc(date))

head(all_post) %>%
    mutate(title = str_trunc(title, 8),
           content = str_trunc(content, 8),
           links = str_trunc(links, 8),
           link_text = str_trunc(link_text, 8)) %>%
    kable("markdown", align = "c")

## ----eval=FALSE----------------------------------------------------------
## html_list <- read_lines("yihui/sublist.txt") # 讀取 url
## all_post <- vector("list", length(html_list))
## 
## for (i in seq_along(html_list)) {
##     path <- html_list[i]
##     all_post[[i]] <- post_data(path)
## }
## 
## all_post <- bind_rows(all_post) %>% arrange(desc(date))

## ------------------------------------------------------------------------
library(jiebaR)
seg <- worker(symbol = T, bylines = F)
segment(c("妳很漂亮", "我不喜歡你"), seg)

## ------------------------------------------------------------------------
seg <- worker(symbol = T, bylines = T)
segment(c("妳很漂亮", "我不喜歡你"), seg)

## ------------------------------------------------------------------------
library(jiebaR)
all_post_seg <- all_post
seg <- worker(symbol = T, bylines = F)

all_post_seg$content[1] <- all_post$content[1] %>%
    segment(seg) %>% paste(collapse = " ")

all_post$content[1] %>% str_trunc(20)
all_post_seg$content[1] %>% str_trunc(30)

## ------------------------------------------------------------------------
all_post_seg <- all_post
seg <- worker(symbol = T, bylines = F)

idx <- seq_along(all_post$content)
for (i in idx){
    all_post_seg$content[i] <- all_post$content[i] %>%
        segment(seg) %>% paste(collapse = " ")
}

head(all_post$content, 3) %>% str_trunc(20)
head(all_post_seg$content, 3) %>% str_trunc(30)

## ----eval=FALSE----------------------------------------------------------
## devtools::install_github("qinwf/ropencc")

## ------------------------------------------------------------------------
library(ropencc)
trans <- converter(TW2SP) # 臺灣用法轉大陸用法
run_convert(trans, "開放中文轉換軟體")

trans <- converter(T2S)   # 單純繁轉簡
run_convert(trans, "開放中文轉換軟體")

trans <- converter(S2TWP) # 簡轉臺灣用法
run_convert(trans, "开放中文转换软件")

## ------------------------------------------------------------------------
library(ropencc)
all_post_seg$content <- run_convert(converter(S2TWP),
                                    all_post_seg$content)
all_post_seg$title <- run_convert(converter(S2T),
                                  all_post_seg$title)

head(all_post_seg) %>%
    mutate(title = str_trunc(title, 8),
           content = str_trunc(content, 8),
           links = str_trunc(links, 8),
           link_text = str_trunc(link_text, 8)) %>%
    kable("markdown", align = "c")

## ------------------------------------------------------------------------
library(quanteda)
corp <- corpus(all_post_seg, 
               docid_field = "title", 
               text_field = "content") 

corp %>% summary() %>% as_data_frame() %>% 
    head(3) %>%
    mutate(links = str_trunc(links, 8),
           link_text = str_trunc(link_text, 8)) %>%
    kable("markdown", align = "c")

## ----echo=FALSE----------------------------------------------------------
HTML('
<div class="mermaid">
    graph TD
    C(Corpus)
    token(Tokens)
    AP["Positional analysis"]
    AN["Non-positional analysis"]
    dfm(DFM)
    tidy("Tidy Text Format")
    vis("Visualize")
    C --> token 
    token --> dfm
    token -.-> AP
    dfm -.-> AN
    tidy -->|"cast_dfm()"| dfm
    dfm -->|"tidy()"| tidy
    dfm -.- vis
    tidy -.-> vis
    AP -.- vis
    style C stroke-width:0px,fill:#6bbcff
    style token stroke-width:0px,fill:#6bbcff
    style dfm stroke-width:0px,fill:#6bbcff
    style tidy stroke-width:0px,fill:orange
    linkStyle 6 stroke-width:0px,fill:none;
    linkStyle 8 stroke-width:0px,fill:none;
</div>
')

