
rm(list = ls())


# R CODE MASTER THESIS:


# Libraries
library(rvest)
library(BatchGetSymbols)

# Read in stock data
OSB <- read.csv("OSB.csv", sep = ";")

# Log in to DN subcription
url <- "https://www.dn.no/auth/login"
uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"
session <- session(url, user_agent(uastring))
form <- html_form(session)[[1]]
fill <- html_form_set(form, 
                             username = "livewt@live.no",
                             password = "masterthesis123")
session_submit(session, fill, submit = NULL, config(referer = session$url))

# Jump to page with news articles 
jump <- session %>% session_jump_to("https://www.dn.no/sok/?topics=Finans%2CPrivat%25C3%25B8konomi%2CB%25C3%25B8rs%2COslo%2520B%25C3%25B8rs%2CMakro%25C3%25B8konomi%2C%25C3%2598konomi&date=[01.01.2010-31.12.2019]&fbclid=IwAR24oNsM3yfEJb-jpAQXVNkg3qOLVT2QK7HfXX9_exZBEjUG43cslyR1anE") 
follow <- session %>% session_follow_link(css = "p a")


# Retrieve dates and text






# Example from exam
for (year in years) {
  base.url <- "https://www.federalreserve.gov"
  web.url <- paste(base.url, "/monetarypolicy/fomchistorical",
                   year, ".htm", sep ="")
  html <- read_html(web.url)
  raw_list <- html %>% # takes the page above for which we've read the html
    html_nodes("a") %>%  # find all links in the page
    html_attr("href") %>% # get the url for these links
    str_subset("\\meeting.pdf") %>% # find those that end in meeting.pdf only
    str_c(base.url, .) # prepend the website to the url
  
  pdfs <- append(pdfs, raw_list)
}







# STOCK PRICE RETRIEVAL

# List of OBX companies (from June 2020, maybe we will edit this manually to reflect 2019 info?)
url = "https://en.wikipedia.org/wiki/OBX_Index" 
firms = as.data.frame(read_html(url) %>% 
                       html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% #table containing the information
                       html_table()) #retrieve table

# Rename one wrong ticker, add ".OL" to each ticker so it can be retrieved later
firms$Ticker.symbol = paste0(gsub("AKERBP","AKRBP",firms$Ticker.symbol),".OL")

# Create vector of tickers
tickers.obx = as.vector(firms$Ticker.symbol)

#Using tickers vector to obtain stock data from Yahoo Finance
obx.stocks = BatchGetSymbols(tickers = tickers.obx,
                              first.date = "2010-01-01",
                              last.date = "2019-12-31",
                              freq.data = "daily",
                              do.cache = FALSE
)

# Check number of rows
# We originally had 25 tickers, now we have 23 rows
print(obx.stocks$df.control)

# Convert stock information into a data frame
stocks = obx.stocks$df.tickers

# Add company name (for searching purposes maybe?)
# stocks$firm = 

# Keep only the information we will use (ticker, date, opening+closing price)
stocks = select(stocks,ticker,ref.date,price.open,price.close)
