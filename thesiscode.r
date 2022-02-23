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
library(RCurl)
library(dplyr)


# STOCK PRICE RETRIEVAL

# List of ALL listed companies
all.firms <- as.data.frame(read_html("https://en.wikipedia.org/wiki/List_of_companies_listed_on_the_Oslo_Stock_Exchange") %>% 
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
delisted.firms = as.data.frame(read_html("https://en.wikipedia.org/wiki/List_of_companies_delisted_from_Oslo_Stock_Exchange") %>% 
                                 html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>% #table containing the information
                                 html_table()) #retrieve table


inner_join(all.firms,delisted.firms,by = "Company")

# Seems like only Elkem was delisted in or after 2005
all.firms = 
  all.firms %>% 
  filter(Company != "Elkem")

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
  gsub("InterOil Exploration and Production","InterOil",.) %>% 
  gsub("Orkla Group","Orkla",.) %>% 
  gsub(" International","",.) %>% 
  gsub(" Ltd","",.) %>% 
  gsub("\\*","",.) %>% 
  gsub("\\.","",.) %>% 
  gsub(" ASA","",.)

# Remove company names that sound too similar (searching for "Aker" will give
# results of "Aker BP" and "Aker Solutions" for example, "Wilh Wilhelmsen Holding"
# has both "ser A" and "ser B")

all.firms = 
  all.firms[all.firms$Company != "Aker",]

all.firms = 
  all.firms %>% 
  subset(Company != "Aker" & Company != "Odfjell ser A" &
         Company != "Odfjell ser B" & Company != "Schibsted ser A" & 
         Company != "Schibsted ser B" & Company != "Wilh Wilhelmsen Holding ser A" &
         Company != "Wilh Wilhelmsen Holding ser B")

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
                              first.date = "2014-01-01",
                              last.date = "2019-12-31",
                              freq.data = "daily",
                              do.cache = FALSE
)

# How many companies do we have? (126)
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

save(stocks,file="stocks.Rdata")

##############################################################################


# NEWS ARTICLE RETRIEVAL 

# Extract URLs and dates for each article
articles <- 1
url.list <- as.character()
date.list <- as.character()

files = sort(list.files(pattern="\\.(html)$"), decreasing = T) # get list of .html files

for (file in files) {
  URLs <- read_html(file) %>% # find URLs for each article
    html_nodes("h3") %>% 
    html_nodes("a") %>%  
    html_attr("href")
  Dates <- read_html(file) %>% 
    html_nodes("time") %>% # find dates for each article
    html_attr("datetime")
  url.list <- append(url.list, URLs)
  date.list <- append(date.list, Dates)
}

# Are there duplicate urls?
nrow((distinct(as.data.frame(url.list))))

which(duplicated(url.list) == TRUE)

# Removing wrong/not working URLs)
grep("notis", url.list)
grep("https://www.dn.no/marked/2-1-", url.list)
grep("https://www.dn.no/arbeidsliv/2-1", url.list)
grep("https://www.dn.no/personvern/handel/slar-alarm-om-personvern/1-1-5397744", url.list)
grep("https://www.dn.no/borsbarna/finans/mener-aksjer-larer-ungene-om-frykt-og-gradighet/1-1-5331779", url.list)
grep("https://www.dn.no/dagligvare/handel/coop-vil-selge-103-butikker/1-1-5308961", url.list)

url.list <- url.list[-c(500, 1015, 1016, 4709, 4728, 4819, 5290, 5551, 5655, 5812, 6029, 6042, 6077, 7125, 10823, 15118, 15802, 16111)]

date.list <- date.list[-c(500, 1015, 1016, 4709, 4728, 4819, 5290, 5551, 5655, 5812, 6029, 6042, 6077, 7125, 10823, 15118, 15802, 16111)]

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

for (url in url.list) {
  jump <- session %>% 
    session_jump_to(url)  # Jump to each URL logged in
  html <- read_html(jump) %>% 
    html_nodes("article") %>% 
    html_nodes("section") %>% 
    html_nodes("p")
  text <- rbind(text, toString(html))
}

# save(text, file = "text.RData")

# load("text.RData")

# Remove HTML code and everything but letters (not completely finished)
text <- text %>% 
  str_remove_all("class.*?\\n") %>%
  str_remove_all("<span.*?p>") %>% 
  str_remove_all("<a.*?>") %>% 
  str_remove_all("class=\"carousel__item-txt carousel--jobbsearch-narrow__item-txt") %>% 
  str_remove_all("<aside.*?<\\aside") %>% 
  str_replace_all("<p", " ") %>% 
  str_replace_all("</p>", " ") %>% 
  str_replace_all("\n", " ") %>% 
  str_replace_all("[^[[:alpha:]][[:space:]]]", " ") 
  
# Make a data frame with dates, URLs and text from each article
text = as.data.frame(text)

text$date = as.Date(date.list, "%d.%m.%Y")

text$url = url.list

names(text)[1] <- "text"

# Remove duplicated text
which(duplicated(text$text))

text <- text[!duplicated(text$text), ]



################################################################################

# Start here 
rm(list = ls())
load("text.Rdata")
load("stocks.Rdata")

# CONNECT ARTICLES AND COMPANIES

# Get the names of the companies we have stock price data for
companies = unique(stocks$Company)

# Add Statoil to companies
companies = c(companies,"Statoil")

# 1. Which companies are never mentioned?
# Create data frame with "companies" column and "mentioned" column
comp.df = data.frame(companies,"mentioned" = 0)

# Create string with text from all articles (easier to search in than in each row)
articles = toString(text$text)   # Takes a minute or two

# Loop: for i in each row of comp.df, assign 1 to the "mentioned" column if a
# company name is found in the "articles" string and 0 if not
for(i in 1:nrow(comp.df)){
  comp.df$mentioned[i] = 
    ifelse(str_detect(string = articles,
                      pattern = comp.df$companies[i])==TRUE,
           1,NA)    # If company i is not detected in the "articles" string ,
}

# Remove companies that are never mentioned from the "companies" list
comp.df.new = na.omit(comp.df)

# Create new list with company names that are mentioned
companies = comp.df.new$companies

# New column in text data frame for company names
text$Company <- ""

for (company in 1:length(companies)) {
  for (t in 1:length(text$text)) {
    if (grepl(companies[company], text[t,1], fixed = TRUE)) {
      m <- gregexpr(companies[company], text[t,1])
      ct <- text[t,4]
      name <- toString(regmatches(text[t,1], m)[[1]])
      text[t,4] <- paste(ct, name, sep = ", ")
    }
  }
}

# 2. Which articles have no companies mentioned?
# Create column that will count how many companies are mentioned in each article
text$mentions = 0    # Always run this line before running loop so it doesn't double-count

# Loop: for each row in text and each element in companies, add 1 to 
# "mentions" column for each company name that is found in the article
for(i in 1:nrow(text)){
  for(j in 1:length(companies)){
  text$mentions[i] = 
    text$mentions[i] + ifelse(str_detect(string = text$text[i],
                                      pattern = companies[j])==TRUE,
                           1,0)
  }
  # Remove articles with no company mentions
 text$mentions[i] = ifelse(text$mentions[i] == 0,NA,text$mentions[i])
 text2 = na.omit(text)
}

# Tested results by looking at articles 1 (1 mention) and 2 (2 mentions),
# could find a mention of Veidekke in article 1 and DNB+Storebrand in article 2
text2[1,]
text2[2,]

# 3. "Assign" articles to a company
# (if an article mentions DNB once and Storebrand once (article 5), it probably
# shouldn't be assigned to either one)

text2$company <- ""

for (company in 1:length(companies)) {
  for (t in 1:length(text2$text)) {
    if (grepl(companies[company], text2[t,1], fixed = TRUE)) {
      ct <- text2[t,4]
      text2[t,4] <- paste(ct, companies[company])
    } else {
      # Company name not found in text
    }
  }
}

text <- text[!text$Company=="",] # Remove rows with no company names

text[stri_(text$Company)==T,]

save(text, file = "text3.RData")

load("text3.RData")

for (i in 1:length(text)) {
  c <- text[i,4]
  text <- text[distinct(c)==T,]
}

!duplicated(text$Company)
df <- merge(text, stocks, by=c("date","Company")) # Merge text and stocks df's



# Previous attempts below - would like to keep them until I figure this out 100%

# Create a nice data frame for the results
mycols = c("text","date","url",companies)
df = data.frame(matrix(ncol = length(mycols),nrow = nrow(text)))
colnames(df) = mycols
df$date = text$date
df$text = text$text
df$url = text$url

# Test run - article 5 mentions at least one company (DNB)
test = df[5,]

c.test = companies[1:50]

for(i in 1:length(c.test)){
  for(j in 1:nrow(test)){
    for(k in 4:ncol(test)){
      test[j,k] = 
    str_count(test$text[j],c.test[i])
    }
  }
}

for(j in 1:length(c.test)){
  for(i in 1:nrow(test)){
  ifelse(grep(c.test[j],test$text[i]),
         assign(paste(c.test[j]),grep(c.test[j],test$text[i])),
         assign(paste(NA),grep(c.test[j],test$text[i])))
  }
}


for(i in 1:length(c.test)){
  print(grep(c.test[i],df[5,]))
}

# Test for DNB in article 5

length(grep("DNB", df[5,]))

companies.df = data.frame(companies,"mention"=as.numeric(0))

for(i in 1:nrow(companies.df)){
  for(j in 1:nrow(test)){
    companies.df$mention[i] = 
      companies.df$mention[i] + 
      length(grep(companies.df$companies[i],test$text[j]))
  }
}

for(i in 1:nrow(companies.df)){
  companies.df$mention[i] = 
    companies.df$mention[i] + 
    str_count(companies.df$companies[i],test$text)
}



as.numeric(ifelse(grepl(companies.df$companies[i],test$text[j]),
                      1,0))


test$mention = NA

for(i in 1:nrow(test)){
  for(j in 1:length(companies)){
    test$mention = as.numeric(str_detect(test$text[i],companies[j]))
  }
}





for(i in 1:nrow(test)){
  for(j in companies){
    for(k in 4:ncol(test)){
    test[i,k] =  
      as.numeric(str_detect(test$text[i],companies[j]))
  }
  }
}
  


for(i in 1:nrow(test)){
  for(j in companies){
    for(k in 4:ncol(test)){
      df[i,k] = ifelse(str_detect(test$text[i],companies[j]),1,0)
    }
  }
}





# Try to find out which companies are mentioned and which are
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

dnb = c("DNB")
for(i in 1:nrow(dnb.df)){
  for(j in 1:length(dnb)){
    for(k in 4:ncol(dnb.df)){
      dnb.df[i,k] = 
        as.numeric(str_detect(dnb.df$text[i],dnb[j]))
    }
  }
}




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