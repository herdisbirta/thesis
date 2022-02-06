

# R CODE MASTER THESIS:


# Libraries
library(rvest)
library(RSelenium)
library(httr)
library(stringr)
library(BatchGetSymbols)
library(lexicon)
library(translateR)

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
# Only run after having closed R/cleaned environment!
url <- "https://www.dn.no/auth/login"
uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"
session <- session(url, user_agent(uastring))
form <- html_form(session)[[1]]
fill <- html_form_set(form, 
                      username = "livewt@live.no",
                      password = "masterthesis123")
session_submit(session, fill, submit = NULL, config(referer = session$url))

# Extract text from each article
text <- list()

for (url in URLs) {
  jump <- session %>% 
    session_jump_to(url)  # Jump to each URL logged in
  html <- read_html(jump) %>% 
    html_nodes("article") %>% 
    html_nodes("section") %>% 
    html_nodes("p")
  text <- rbind(text, toString(html))
}

# Remove HTML code and everything but letters
text <- text %>%  
  str_remove_all("<span.*?p>") %>% 
  str_replace_all("<p", "") %>% 
  str_replace_all("</p>", "") %>% 
  str_remove_all("<a.*?>") %>%
  str_replace_all("[^[[:alpha:]][[:space:]]]", "") %>% 
  str_remove("classcarouselitemtxt carouseljobbsearchnarrowitemtxt\n                                a\n")

# Make a data frame with dates, URLs and text from each article
text = as.data.frame(text)

text$date = url.list$Dates

text$url = url.list$URLs


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







# Translating Loughran and McDonald dictionary 
#dict = lexicon::hash_sentiment_loughran_mcdonald

#LM.norsk <- translateR::translate(dataset = dict,
#                        content.field = "x",
#                        source.lang = "en",
#                        target.lang = "no",
#                        google.api.key = "AIzaSyA5q6Or5MEVBJNofdmUmiHOdquj4WZHwxw")

#save(LM.norsk, file = "LMNorsk.RData")

load("LMNorsk.RData")
