# R CODE MASTER THESIS:


# Libraries
library(rvest)
library(RSelenium)
library(httr)
library(stringr)
library(BatchGetSymbols)
library(lexicon)
library(translateR)
library(lubridate)
library(RCurl)
library(dplyr)
library(tm)
library(stopwords)
library(quanteda)




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
  gsub("Oceanteam Shipping", "Oceanteam",.) %>% 
  gsub("Solon Eiendom", "Solon",.) %>% 
  gsub(" International","",.) %>% 
  gsub(" Ltd","",.) %>% 
  gsub("\\*","",.) %>% 
  gsub("\\.","",.) %>% 
  gsub(" ASA","",.) %>% 
  gsub(" Limited","",.)

# Remove company names that sound too similar (searching for "Aker" will give
# results of "Aker BP" and "Aker Solutions" for example, "Wilh Wilhelmsen Holding"
# has both "ser A" and "ser B")

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
# any info (deregistered or acquired by other companies and therefore no info),
# some we removed to make searching easier later)
sum(ifelse(all.stocks$df.control$threshold.decision=="KEEP",1,0))

# Convert stock information into a data frame
stocks = all.stocks$df.tickers

# Add company name (for searching purposes)
stocks = left_join(stocks,all.firms,by="ticker")

# Calculate a daily price measure
for(i in 1:nrow(stocks)){
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

# url <- "https://www.dn.no/auth/login"
# uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"
# session <- session(url, user_agent(uastring))
# form <- html_form(session)[[1]]
# fill <- html_form_set(form, 
#                       username = "livewt@live.no",
#                       password = "masterthesis123")
# session_submit(session, fill, submit = NULL, config(referer = session$url))

# Extract text from each article
# text <- list()

# for (url in url.list) {
#   jump <- session %>% 
#     session_jump_to(url)  # Jump to each URL logged in
#   html <- read_html(jump) %>% 
#     html_nodes("article") %>% 
#     html_nodes("section") %>% 
#     html_nodes("p")
#   text <- rbind(text, toString(html))
# }

#url <- "https://www.dn.no/auth/login"
#uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"
#session <- session(url, user_agent(uastring))
#form <- html_form(session)[[1]]
#fill <- html_form_set(form, 
#                      username = "livewt@live.no",
#                      password = "masterthesis123")
#session_submit(session, fill, submit = NULL, config(referer = session$url))

# Extract text from each article
#text <- list()

#for (url in url.list) {
#  jump <- session %>% 
#    session_jump_to(url)  # Jump to each URL logged in
#  html <- read_html(jump) %>% 
#    html_nodes("article") %>% 
#    html_nodes("section") %>% 
#    html_nodes("p")
#  text <- rbind(text, toString(html))
#}

# save(text, file = "text.RData")

load("text.RData")

# Remove HTML code and everything but letters
text$text <- text$text %>% 
  str_remove_all("<span.*?p>") %>% 
  str_remove_all("<a.*?>") %>% 
  str_remove_all("class=\"carousel__item-txt carousel--jobbsearch-narrow__item-txt") %>%
  str_remove_all("<aside.*?<\\aside") %>% 
  str_remove_all("<p|</p>|\n|<a|a>|<b|b>|<i|i>") %>% 
  str_remove_all("[^[[:alpha:]][[:space:]]]") %>% 
  str_remove_all("class.*?intro") %>% 
  str_remove_all("infoboxcontent|classinfoboxcontent.*?aside") %>% 
  str_remove_all("class|article|author|name|span|separator|strong|rel|div|button") %>% 
  str_remove_all("Les også.*$") %>% 
  str_remove_all("figure figure jsfigure.*?figcaptionfigure") %>% 
  str_remove_all("Tapsanslagene.*?rentenem") %>% 
  str_remove_all("figure") %>% 
  str_remove_all("dnationblockwrapper") %>% 
  str_remove_all("[[:space:]]a[[:space:]]")

# Make a data frame with text, dates and URLs from each article
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

# Get the names of the companies we have stock price data for
companies = unique(stocks$Company)

# Add company names more used/or changed during time period
companies = c(companies,c("Statoil", "Marine Harvest", "Det Norske Oljeselskap",
                          "Apptix", "Clavis Pharma", "AqualisBraemar", 
                          "Bergen Group", "Vik Sparebank", "Aurland Sparebank",
                          "Indre Sogn Sparebank", "Noreco", "Team Bane", 
                          "Namsos Traffikkselskap", "PGS", "Solstad",
                          "PSI", "Ocean Technology"))

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

# Loop to paste company names into new Company column
for (company in 1:length(companies)) {
  for (t in 1:length(text$text)) {
    if (grepl(companies[company], text[t,1], fixed = TRUE)) {
      m <- gregexpr(companies[company], text[t,1])
      ct <- text[t,4]
      name <- toString(regmatches(text[t,1], m)[[1]])
      name <- str_replace_all(name, " ", "-") 
      text[t,4] <- paste(ct, name)
    }
  }
}

text <- text[!text$Company=="",] # Remove rows with no company names

# Loop to remove company names that only get mentioned once per article
for (t in 1:length(text$Company)) {
  for (c in companies) {
    if (str_count(text[t,4], c) <= 1) {
      text[t,4] <- gsub(c, "", text[t,4])
    }
  }
}

# Remove rows where only companies were mentioned once, commas and spaces at the start of rows
text$Company <- str_replace_all(text$Company, ",", " ")

text$Company <- text$Company %>% 
  gsub("^[[:space:]]+$", NA, .) %>% 
  gsub("^[[:space:]]+", "", .)

text <- text[!(is.na(text$Company)),]

# Loop to choose the most mentioned company for each article
for (t in 1:length(text$Company)) {
  for (c in companies) {
    words <- strsplit(text[t,4], "[[:space:]]+")[[1]]
    most <- names(sort(table(words), decreasing = TRUE))[1]
    text[t,4] <- text[t,4] %>% gsub(text[t,4], "", text[t,4]) %>% 
      gsub("", most, text[t,4])
  }
}

# Replace - with spaces and remove spaces before company names
text$Company <-  text$Company %>% 
  gsub("-", " ",.) %>% 
  gsub("^[[:space:]]+", "",.)

# Change old company names to new company names 
text$Company <- text$Company %>% 
  gsub("Statoil", "Equinor", .) %>% 
  gsub("Marine Harvest", "Mowi", .) %>% 
  gsub("Det Norske Oljeselskap", "Aker BP", .) %>% 
  gsub("Apptix", "Carasent", .) %>% 
  gsub("Clavis Pharma|AqualisBraemar", "Aqualis", .) %>% 
  gsub("Bergen Group", "Endúr", .) %>% 
  gsub("'Vik Sparebank'|'Aurland Sparebank'|'Indre Sogn Sparebank'", 
       "Sogn Sparebank", .) %>% 
  gsub("Noreco", "Norwegian Energy Company", .) %>% 
  gsub("Team Bane", "NRC Group", .) %>% 
  gsub("Namsos Trafikkselskap", "NTS", .) %>% 
  gsub("PGS", "Petroleum Geo-Services", .) %>% 
  gsub("Solstad", "Solstad Farstad", .) %>% 
  gsub("PSI", "StrongPoint", .) %>% 
  gsub("Ocean Technology", "Subsea", .)

# Create row with date and company to identify instances where there are more than 1
# article about a company on a specific date:
text$datecomp = paste(text$date, text$Company)

# Select relevant columns
text2 = text %>% 
  select(datecomp,text,date,Company) %>% 
  group_by(datecomp)

# Split text data frame into df1 (non-duplicated rows) and d1b (duplicated rows)
df1 = text2[!duplicated(text2$datecomp),]
df1b = text2[duplicated(text2$datecomp),]

# Check that all rows have been assigned to either d1 or d1b
nrow(df1)+nrow(df1b)-nrow(text)

# Split duplicated rows (d1fb) into df2 (non-duplicated rows) and df2b (duplicated rows)
# This is done to take into account 2 articles per company per day
df2 = df1b[!duplicated(df1b$datecomp),]
df2b = df1b[duplicated(df1b$datecomp),]

# Combine df1 (non-duplicated rows of 1 article per day) with df2 (non-duplicated
# rows of 2 articles per day)
df12 = full_join(df1,df2,by="datecomp")

# Split duplicated rows (df2b) into df3 (non-duplicated rows) and df3b (duplicated rows)
# This is done to take into account 3 articles per company per day
df3 = df2b[!duplicated(df2b$datecomp),]
df3b = df2b[duplicated(df2b$datecomp),]

# Combine df12 (non-duplicated rows of 1 or 2 articles per day) with df3 (non-duplicated
# rows of 3 articles per day)
df23 = full_join(df12,df3,by="datecomp")

# Split duplicated rows (df3b) into df4 (non-duplicated rows) and df4b (duplicated rows)
# This is done to take into account 4 articles per company per day
df4 = df3b[!duplicated(df3b$datecomp),]
df4b = df3b[duplicated(df3b$datecomp),]

# Combine df23 (non-duplicated rows of 1, 2 or 3 articles per day) with df4 (non-duplicated
# rows of 4 articles per day)
df34 = full_join(df23,df4,by="datecomp")

# Split duplicated rows (df4b) into df5 (non-duplicated rows) and df5b (duplicated rows)
# This is done to take into account 5 articles per company per day
df5 = df4b[!duplicated(df4b$datecomp),]
df5b = df4b[duplicated(df4b$datecomp),]

# Combine df34 (non-duplicated rows of 1, 2, 3 or 4 articles per day) with df5 (non-duplicated
# rows of 5 articles per day)
df45 = full_join(df34,df5,by="datecomp")

# Split duplicated rows (df5b) into df6 (non-duplicated rows) and df6b (duplicated rows)
# This is done to take into account 6 articles per company per day
df6 = df5b[!duplicated(df5b$datecomp),]
df6b = df5b[duplicated(df5b$datecomp),]

# Combine df45 (non-duplicated rows of 1, 2, 3, 4 or 5 articles per day) with df6 (non-duplicated
# rows of 6 articles per day)
df56 = full_join(df45,df6,by="datecomp")


# Split duplicated rows (df6b) into df7 (non-duplicated rows) and df7b (duplicated rows)
# This is done to take into account 7 articles per company per day
df7 = df6b[!duplicated(df6b$datecomp),]
df7b = df6b[duplicated(df6b$datecomp),]

# Combine df56 (non-duplicated rows of 1, 2, 3, 4, 5 or 6 articles per day) with df7 (non-duplicated
# rows of 7 articles per day)
df67 = full_join(df56,df7,by="datecomp")

# Split duplicated rows (df7b) into df8 (non-duplicated rows) and df8b (duplicated rows)
# This is done to take into account 8 articles per company per day
df8 = df7b[!duplicated(df7b$datecomp),]
df8b = df7b[duplicated(df7b$datecomp),]

# Combine df67 (non-duplicated rows of 1, 2, 3, 4, 5, 6 or 7 articles per day) with df8 (non-duplicated
# rows of 8 articles per day)
df78 = full_join(df67,df8,by="datecomp")

# Split duplicated rows (df8b) into df9 (non-duplicated rows) and df9b (duplicated rows)
# This is done to take into account 9 articles per company per day
df9 = df8b[!duplicated(df8b$datecomp),]
df9b = df8b[duplicated(df8b$datecomp),]

# Combine df78 (non-duplicated rows of 1, 2, 3, 4, 5, 6, 7 or 8 articles per day) with df9 (non-duplicated
# rows of 9 articles per day)
df89 = full_join(df78,df9,by="datecomp")

# Are there still duplicate dates?
length(which(duplicated(df9b$datecomp)==TRUE))    # We have reached the end

# Combine df89 (non-duplicated rows of 1,2,3,4,5,6,7,8 or 9 articles per day)
# with df9b (final non-duplicated rows)
df.end = full_join(df89,df9b,by="datecomp")

# Combine columns (note that combining without changing NA columns results in some
# elements in the text-column end with "NA" or "NANANANANA" or something similar)
df.end = 
  df.end %>% 
  mutate("text" = 
           paste0(text.x,text.y,text.x.x,text.y.y,text.x.x.x,text.y.y.y,
                  text.x.x.x.x,text.y.y.y.y,text.x.x.x.x.x,text.y.y.y.y.y,
                  sep=" ")) %>% 
  select(datecomp,text,"date" = date.x,"Company" = Company.x)

# Change date: date + 1
df.end$date <- as.Date(df.end$date) +1

# Merge df.end with stocks by date and company
end.df <- merge(df.end, stocks, by=c("date","Company")) # Merge text and stocks df's

# Remove NA at end of text
end.df$text <- str_remove_all(end.df$text, "NA")

# Remove all my unnecessary dfs
rm(list = ls(pattern = "^df"))

# Final data frame is "df"
df = end.df

# Save
save(df,file = "df.Rdata")

###############################################################################

rm(list = ls())
load("df.Rdata")
  

# Stopwords
# Stopwords do not have capital letters or punctuation - remove
corpus = 
  df$text %>% 
  tolower(.) %>% 
  gsub("[[:punct:]]","",.)

# Change the articles to a corpus format
corpus = 
  corpus(corpus)

# Tokenize and remove stopwords
stopw = stopwords::stopwords(language = "no")

toks = corpus %>% 
  tokens() %>% 
  tokens_remove(stopw)

###############################################################################


# TRANSLATION OF LOUGHRAN AND MCDONALD DICTIONARY

#dict = lexicon::hash_sentiment_loughran_mcdonald

#LM.norsk <- translateR::translate(dataset = dict,
#                        content.field = "x",
#                        source.lang = "en",
#                        target.lang = "no",
#                        google.api.key = "AIzaSyA5q6Or5MEVBJNofdmUmiHOdquj4WZHwxw")

#save(LM.norsk, file = "LMNorsk.RData")

load("LMNorsk.RData")

# There is a Norwegian alternative of a sentiment dictionary from UiO
# which is an alternative if we need it

# Sum negative/positive words

sum(LM.norsk$y == 1)  # Number of positive words

sum(LM.norsk$y == -1) # Number of negative words

# Remove duplicates and more than one word translations
nrow((distinct(as.data.frame(LM.norsk$translatedContent))))

which(duplicated(LM.norsk$translatedContent))

LM.norsk <- LM.norsk[!duplicated(LM.norsk$translatedContent), ]

which(sapply(strsplit(LM.norsk$translatedContent, " "), length)>1)

LM.norsk <- LM.norsk[!sapply(strsplit(LM.norsk$translatedContent, " "), length)>1, ]

# Change format of columns so LM.norsk looks exactly like the untranslated dictionary
LM.norsk = 
  LM.norsk %>% 
  select("x" = translatedContent, y)


# SENTIMENT SCORE
# It is important that the sentiment dictionary and stopwords dictionary do not
# overlap, that would result in an unreliable sentiment score.
inner_join(data.frame(x = stopw),LM.norsk,by="x")


# Create actual sentiment score (sum of +1 and -1 values of positive/negative words
# divided by the total number of words)
score = vector()
for(t in 1:length(toks)){
  score = append(score,
                 sum(filter(LM.norsk, x %in% toks[[t]])$y) / length(toks[[t]]))
}

# Test
sum(filter(LM.norsk, x %in% toks[[1]])$y) / length(toks[[1]]) == score[1]
sum(filter(LM.norsk, x %in% toks[[3184]])$y) / length(toks[[3184]]) == score[3184]

# Insert into df
df$sentiment = score

###############################################################################


# REGRESSION:

# Split data:

set.seed(123)

ind <- df$date <= 2018-12-31

train <- df[ind,]

test <- df[-ind,]

# Logistic Regression:

# with full data
simplelog <- glm(av.price~sentiment, data = df, family = binomial())

summary(simplelog)

plot(simplelog)

# with train data
logreg <- glm(av.price~sentiment, data = train, family = binomial())

pred <- predict(logreg, test, type = "response")

conf.mat <- table(test$av.price, pred > 0.5)

conf.mat

accuracy <- sum(diag(conf.mat2))/sum(conf.mat2)

accuracy

val.set.err <- (confmat[1,2]+confmat[2,1])/(n/2)

val.set.err

plot(df)

lines(pred)

# k-fold cross-validation
all.cv = rep(NA, 10)

for (i in 1:10) {
  logfit = glm(av.price~sentiment, data=train, family = binomial())
  all.cv[i] = cv.glm(train, logfit, K=10)$delta[2]
}

plot(1:10, all.cv[-c(1, 2)], lwd=2, type="l", xlab="df", ylab="CV error")


