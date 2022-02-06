

# R CODE MASTER THESIS:


# Libraries
library(rvest)
library(RSelenium)
library(httr)
library(stringr)
library(BatchGetSymbols)
library(lexicon)
library(stopwords)

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

text = as.data.frame(text)

text$date = url.list$Dates

text$url = url.list$URLs

# Save as Rdata
save(text,file = "data.Rdata")

rm(list = ls())
load("data.Rdata")


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

# Change the company names so they match with how they are most frequently referred to
# (example: "Yara International" is most often referred to as simply "Yara")
firms$company = gsub("Yara International", "Yara",firms$company)
# MORE EXAMPLES OF THIS?


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



# Connect articles and companies we're interested in


# Get the names of the companies we have stock price data for (23 companies)
companies = unique(stocks$company)

# Create corpus for each article and each company
# (We can use this loop to remove stopwords of relevant articles etc.)
for(i in 1:nrow(text)){
  for(j in 1:length(companies)){
    ifelse(companies[j]%in%text$text,
           assign(paste(companies[j],text$date[i],sep="-"),text$text[i]),
           assign(paste("NA",companies[j],text$date[i],sep="-"),text$text[i])    # Will change this line later, this is just to see if there are no results or if code is wrong
    )
  }
}

# Code to retrieve stopwords with package "stopwords"
stopwords(language = "no")


# Dictionary stuff
dict = lexicon::hash_sentiment_loughran_mcdonald
head(dict)

