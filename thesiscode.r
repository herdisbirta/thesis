

# R CODE MASTER THESIS:


# Libraries
library(rvest)
library(RSelenium)
library(httr)
library(stringr)
library(BatchGetSymbols)
library(lexicon)
library(translateR)
library(stopwords)
library(lubridate)


# STOCK PRICE RETRIEVAL

# List of ALL listed companies
url <- "https://en.wikipedia.org/wiki/List_of_companies_listed_on_the_Oslo_Stock_Exchange"
all.firms <- as.data.frame(read_html(url) %>% 
                             html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>% #table containing the information
                             html_table()) #retrieve table

# Change format of first day of listing column
all.firms$First.day.of.listing = 
  as.Date(all.firms$First.day.of.listing, format= "%d %B %Y")

# Filter to relevant time period
all.firms = 
  all.firms %>% 
  filter(First.day.of.listing <= "2019-12-31")

# Were any companies delisted?
url2 = "https://en.wikipedia.org/wiki/List_of_companies_delisted_from_Oslo_Stock_Exchange"
delisted.firms = as.data.frame(read_html(url2) %>% 
                                 html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>% #table containing the information
                                 html_table()) #retrieve table


inner_join(all.firms,delisted.firms,by = "Company")

# Seems like only Elkem was delisted in or after 2005, the code will automatically 
# exclude it since the stock price info will not be available after delisting

# Get tickers

# Manually rename wrong tickers
all.firms$Ticker = 
  all.firms$Ticker %>% 
  gsub("AKERBP","AKRBP",.) %>% 
  gsub("AKA","AKAST",.) %>% 
  gsub("ARCHER","ARCH",.) %>% 
  gsub("ASETEK","ASTK",.) %>% 
  gsub("AVANCE","AGAS",.) %>% 
  gsub("BEL","BELCO",.) %>% 
  gsub("BON","BONHR",.) %>% 
  gsub("BDRILL","BORR",.) %>% 
  gsub("BOUVET","BOUV",.) %>% 
  gsub("COV","CONTX",.) %>% 
  gsub("CRAYON","CRAYN",.) %>% 
  gsub("FKRAFT","FKRFT",.) %>% 
  gsub("ITE","ITERA",.) %>% 
  gsub("NANO","NANOV",.) %>% 
  gsub("PROTCT","PROT",.) %>% 
  gsub("REC","RECSI",.) %>% 
  gsub("SALMON","SACAM",.) %>% 
  gsub("SBX","GEG",.) %>% 
  gsub("SRBANK","SRBNK",.) %>% 
  gsub("STRONG","STRO",.) %>% 
  gsub("VISTIN","VISTN",.)

# Manually change company names to their more "referred-to" versions
# (Subsea 7 becomes Subsea, Yara international becomes Yara, etc.) (more examples?)
# Remove "*" and ".", remove "ASA"
all.firms$Company = 
  all.firms$Company %>% 
  gsub("Yara International", "Yara",.) %>% 
  gsub("Subsea 7","Subsea",.) %>% 
  gsub("Scatec Solar","Scatec",.) %>% 
  gsub("Lerøy Seafood Group","Lerøy",.) %>% 
  gsub("Gjensidige Forsikring","Gjensidige",.) %>% 
  gsub("\\*","",.) %>% 
  gsub("\\.","",.) %>% 
  gsub(" ASA","",.)

# Edit ticker to be on the format "TICKER.OL"
all.firms$ticker = paste0((gsub("OSE: ","",all.firms$Ticker)),".OL")

# Select relevant columns
all.firms = 
  all.firms %>% 
  select(Company,ticker,First.day.of.listing)


# Create vector of tickers
all.tickers = as.vector(all.firms$ticker)

# Using tickers vector to obtain stock data from Yahoo Finance
all.stocks <- BatchGetSymbols(tickers = all.tickers,
                              first.date = "2005-01-01",
                              last.date = "2019-12-31",
                              freq.data = "daily",
                              do.cache = FALSE
)

# How many companies do we have? (95)
# We originally had 183 tickers for companies, some only had price info for
# <75% of the time period (and was therefore skipped), some tickers didn't have
# any info (deregistered or acquired by other companies and therefore no info)
sum(ifelse(all.stocks$df.control$threshold.decision=="KEEP",1,0))

# Convert stock information into a data frame
stocks = all.stocks$df.tickers

# Add company name (for searching purposes)
stocks = left_join(stocks,all.firms,by="ticker")

# Calculate a daily price measure
for(i in 1:length(stocks)){
  stocks$av.price[i] = (stocks$price.open[i]+stocks$price.close[i])/2
}

# Only select columns we are interested in
stocks = 
  stocks %>% 
  select(Company,ticker,"date"=ref.date,av.price)





# NEWS ARTICLE RETRIEVAL 

# Extract URLs and dates for each article
html <- read_html("DN1.html")  # HTML code from DN
articles <- 1
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

# It's including more links than articles (will look more into this)
grep("https://www.dn.no/marked/notis", URLs)

URLs <- URLs[-c(1015, 1016, 4709, 4728, 5812, 6029, 6077)]



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

text$date = as.Date(url.list$Dates, "%d.%m.%Y")

text$url = url.list$URLs



# CONNECT ARTICLES AND COMPANIES

# Get the names of the companies we have stock price data for
companies = unique(stocks$Company)

# Create a nice data frame for the results
mycols = c("text","date","url",companies)
df = data.frame(matrix(ncol = length(mycols),nrow = nrow(text)))
colnames(df) = mycols
df$date = text$date
df$text = text$text
df$url = text$url


# An idea I had was to try to find out which companies are mentioned and which are
# not, so we can exclude the companies that aren't mentioned at all and then search
# for how many times the remaining companies are mentioned (maybe easier to work with
# fewer companies), this doesn't work yet for some reason but DNB example below works
for(i in 1:nrow(df)){
  for(j in 1:length(companies)){
    for(k in 4:ncol(df)){
      df[i,k] = ifelse(str_detect(df$text[i],companies[j]),1,0)
    }
  }
}


# DNB test (same concept as above but only for DNB)
dnbcols = c("text","date","url","DNB")
dnb.df = data.frame(matrix(ncol = length(dnbcols),nrow = nrow(text)))
colnames(dnb.df) = dnbcols
dnb.df$date = text$date
dnb.df$text = text$text
dnb.df$url = text$url

dnb = c("DNB")
for(i in 1:nrow(dnb.df)){
  for(j in 1:length(dnb)){
    for(k in 4:ncol(dnb.df)){
      dnb.df[i,k] = ifelse(str_detect(dnb.df$text[i],dnb[j]),1,0)
    }
  }
}




# Previous attempt below



# Which articles have company mentions?
# Creates an object for each article that has a company mention
# I know this works. Also if for example DNB and Equinor are both mentioned in
# an article, it creates 2 objects for each company.
for(i in 1:nrow(text)){
  for(j in 1:length(companies)){
    ifelse(str_detect(text$text[i],companies[j]),    # If the company name j is in article i
           assign(paste(companies[j],text$date[i],sep="-"),text$text[i]),    # Yes: Create an object with the article
           assign(paste("NA",companies[j],text$date[i],sep="-"),text$text[i]))    # No: Create an object with the article with NA in the front
    rm(list = ls(pattern = "^NA"))    # Delete the NA objects
  }
} 


# Code to retrieve stopwords with package "stopwords"
stopwords(language = "no")







# TRANSLATION OF LOUGHRAN AND MCDONALD DICTIONARY

#dict = lexicon::hash_sentiment_loughran_mcdonald

#LM.norsk <- translateR::translate(dataset = dict,
#                        content.field = "x",
#                        source.lang = "en",
#                        target.lang = "no",
#                        google.api.key = "AIzaSyA5q6Or5MEVBJNofdmUmiHOdquj4WZHwxw")

#save(LM.norsk, file = "LMNorsk.RData")

load("LMNorsk.RData")


