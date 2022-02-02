
rm(list = ls())


# R CODE MASTER THESIS:


# Libraries
library(rvest)  # To retrieve wikipedia info
library(BatchGetSymbols)  # To retrieve stock prices from yahoo
library(httr) # For function user_agent?


# Extract URLs and dates for each article
articles <- 1:20
html <- read_html("DN.html")  # HTML code from DN
URLs <- list()
Dates <- list()

for (article in articles) {
  URLs <- html %>% # find URLs for each article
    html_nodes("h3") %>% 
    html_nodes("a") %>%  
    html_attr("href")
  Dates <- html %>% 
    html_nodes("time") %>% # find dates for each article
    html_attr("datetime")
  url.list <- data.frame(Dates, URLs)
}

# Log in to DN subscription
url <- "https://www.dn.no/auth/login"
uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"
session <- session(url, user_agent(uastring))
form <- html_form(session)[[1]]
fill <- html_form_set(form, 
                      username = "livewt@live.no",
                      password = "masterthesis123")
session_submit(session, fill, submit = NULL, config(referer = session$url))

# Jump to page with news articles (Still not working btw, gonna work into extract loop)
jump <- session %>% session_jump_to("https://www.dn.no/sok/?topics=Finans%2CPrivat%25C3%25B8konomi%2CB%25C3%25B8rs%2COslo%2520B%25C3%25B8rs%2CMakro%25C3%25B8konomi%2C%25C3%2598konomi&date=[01.01.2010-31.12.2019]&fbclid=IwAR24oNsM3yfEJb-jpAQXVNkg3qOLVT2QK7HfXX9_exZBEjUG43cslyR1anE") 
follow <- session %>% session_follow_link(css = "p a")






# STOCK PRICE RETRIEVAL

# List of OBX companies (from June 2020, maybe we will edit this manually to reflect 2019 info?)
url = "https://en.wikipedia.org/wiki/OBX_Index" 
firms = as.data.frame(read_html(url) %>% 
                       html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% #table containing the information
                       html_table()) #retrieve table

# Rename one wrong ticker, add ".OL" to each ticker so it can be retrieved later
firms$ticker = paste0(gsub("AKERBP","AKRBP",firms$Ticker.symbol),".OL")

# Remove unneccessary information from data frame
firms = select(firms,"company" = Company,ticker)

# Create vector of tickers
tickers.obx = as.vector(firms$ticker)

#Using tickers vector to obtain stock data from Yahoo Finance
obx.stocks = BatchGetSymbols(tickers = tickers.obx,
                              first.date = "2010-01-01",
                              last.date = "2019-12-31",
                              freq.data = "daily",
                              do.cache = FALSE,
                              thresh.bad.data = 0 # Only skip ticker if no data
)

# Check number of rows
# We originally had 25 tickers, now we have 23 rows
print(obx.stocks$df.control)

# Convert stock information into a data frame
stocks = obx.stocks$df.tickers

# Add company name (for searching purposes maybe?)
stocks = left_join(stocks,firms,by="ticker")

# Keep only the information we will use (ticker, date, opening+closing price)
stocks = select(stocks,company,ticker,ref.date,price.open,price.close)

# Calculate a daily price measure
for(i in 1:length(stocks)){
  stocks$av.price[i] = (stocks$price.open[i]+stocks$price.close[i])/2
}







# Example from https://www.listendata.com/2020/12/web-scrape-google-news-with-r.html
# Don't know if we can apply any of it, my R is too slow to even run it

news <- function(term) {
  
  require(dplyr)
  require(xml2)
  require(rvest)
  
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=en-IN&gl=IN&ceid=US%3Aen"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  
  news_dat <- data.frame(
    Title = html_dat %>%
      html_nodes('.DY5T1d') %>% 
      html_text(),
    Link = dat$Link,
    Description =  html_dat %>%
      html_nodes('.Rai5ob') %>% 
      html_text()
  ) 
  
  # Extract Source and Time (To avoid missing content)
  prod <- html_nodes(html_dat, ".SVJrMe")
  Source <- lapply(prod, function(x) {
    norm <- tryCatch(html_node(x, "a") %>% html_text() ,
                     error=function(err) {NA})
  })
  
  time <- lapply(prod, function(x) {
    norm <- tryCatch(html_node(x, "time") %>% html_text(),
                     error=function(err) {NA})
  })
  
  mydf <- data.frame(Source = do.call(rbind, Source), Time = do.call(rbind, time), stringsAsFactors = F)
  dff <- cbind(news_dat, mydf) %>% distinct(Time, .keep_all = TRUE)
  
  return(dff)
}

newsdf <- news('indian"%20economy')

