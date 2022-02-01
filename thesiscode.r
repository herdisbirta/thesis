rm(list = ls())
# R CODE MASTER THESIS:
require(rvest)  # to get wikipedia info about companies+tickers
require(BatchGetSymbols)  # to get stock prices from yahoo


# List of OBX companies (from June 2020, maybe we will edit this manually to reflect 2019 info?)
url <- "https://en.wikipedia.org/wiki/OBX_Index" 
firms <- as.data.frame(read_html(url) %>% 
                       html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% #table containing the information
                       html_table()) #retrieve table

# Create vector of tickers
tickers.obx = as.vector(paste0(firms$Ticker.symbol,".OL"))

# Rename one wrong ticker
tickers.obx = gsub("AKERBP.OL","AKRBP.OL",tickers.obx)

#Using tickers vector to obtain stock data from Yahoo Finance
obx.stocks <- BatchGetSymbols(tickers = tickers.obx,
                              first.date = "2010-01-01",
                              last.date = "2019-12-31",
                              freq.data = "daily",
                              do.cache = FALSE
)

# How many rows do we have?
# We originally had 25 tickers
print(obx.stocks$df.control)

