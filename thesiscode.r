# R CODE MASTER THESIS:


# Libraries
library(readxl)
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
library(boot)
library(e1071)
library(ROCR)
library(caret)
library(class)
library(gam)
library(tree)
library(randomForest)
library(kernlab)

# STOCK PRICE RETRIEVAL
# List of all registered companies

# Get list of all .xlsx files
file.list = list.files(pattern='*.xlsx')

# Read all files
y2014 = read_excel(path = file.list[1], skip = 9)   # Read 2014 file
y2014 = rbind(y2014[1:25,],    # OBX
              y2014[29:150,],  # OB Match
              y2014[155:185,]) # OB Standard and New

y2015 = read_excel(path = file.list[2], skip = 9)   # Read 2015 file
y2015 = rbind(y2015[1:25,],    # OBX
              y2015[29:148,],  # OB Match
              y2015[153:181,]) # OB Standard and New

y2016 = read_excel(path = file.list[3], skip = 9)   # Read 2016 file
y2016 = rbind(y2016[1:25,],    # OBX
              y2016[29:151,],  # OB Match
              y2016[155:180,]) # OB Standard and New

y2017 = read_excel(path = file.list[4], skip = 9)   # Read 2017 file
y2017 = rbind(y2017[1:25,],    # OBX
              y2017[28:163,],  # OB Match
              y2017[167:187,]) # OB Standard and New

y2018 = read_excel(path = file.list[5], skip = 9)   # Read 2018 file
y2018 = rbind(y2018[1:25,],    # OBX
              y2018[28:158,],  # OB Match
              y2018[162:186,]) # OB Standard and New

y2019 = read_excel(path = file.list[6], skip = 9)   # Read 2019 file
y2019 = rbind(y2019[1:25,],    # OBX
              y2019[28:162,],  # OB Match
              y2019[166:188,]) # OB Standard and New

# Combine
all.firms = rbind(y2014,y2015,y2016,y2017,y2018,y2019)

# Select relevant columns
all.firms = 
  all.firms %>% 
  select("Company" = OBX, ticker = ...20)

# Find unique company names
all.firms = all.firms[!duplicated(all.firms$ticker),]

# Remove 'y20XX' data frames
rm(list = ls(pattern = "^y20"))

# Manually rename wrong tickers
all.firms$ticker = 
  all.firms$ticker %>% 
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
  gsub("NANO","NANOV",.) %>% 
  gsub("PROTCT","PROT",.) %>% 
  gsub("REC","RECSI",.) %>% 
  gsub("SALMON","SACAM",.) %>% 
  gsub("SBX","GEG",.) %>% 
  gsub("WWASA","WWI",.) %>% 
  gsub("STRONG","STRO",.) %>% 
  gsub("VISTIN","VISTN",.) %>% 
  gsub("ASC","ABG",.) %>% 
  gsub("AXA","ACR",.) %>% 
  gsub("NORBIT","NORBT",.) %>% 
  gsub("OTELLO","OTEC",.) %>% 
  gsub("SAS NOK","SASNO",.) %>%     
  gsub("SSO","SCATC",.) %>% 
  gsub("ULTIMO","ULTI",.) %>% 
  gsub("WALWIL","WAWI",.) %>% 
  gsub("SCH","SCHA",.) %>% 
  gsub("WBULK","WEST",.)

# Edit ticker to be on the format "TICKER.OL"
all.firms$ticker = paste0(all.firms$ticker, ".OL")

# Create vector of tickers
all.tickers = as.vector(all.firms$ticker)

# Manually change company names to their more "referred-to" versions
# (Yara international becomes Yara, etc.), remove "," and "."
all.firms$Company = 
  all.firms$Company %>% 
  gsub("Norwegian Air Shuttle", "Norwegian Air",.) %>% 
  gsub("Yara International","Yara",.) %>% 
  gsub("AKVA Group", "AKVA",.) %>% 
  gsub("ABG Sundal Collier Holding","ABG Sundal Collier",.) %>% 
  gsub("Avance Gas Holding","Avance Gas",.) %>% 
  gsub("BW Offshore Limited","BW Offshore",.) %>% 
  gsub("Electromagnetic Geoservices", "EMGS",.) %>% 
  gsub("InterOil Exploration and Production","Interoil",.) %>% 
  gsub("Lerøy Seafood Group","Lerøy Seafood",.) %>%
  gsub("Questerre Energy Corporation", "Questerre",.) %>% 
  gsub("SAS AB", "SAS", .) %>% 
  gsub("Subsea 7","Subsea",.) %>% 
  gsub("Petroleum Geo-Services", "Petroleum Geo Services",.) %>% 
  gsub("Q-Free", "Q Free",.) %>% 
  gsub("Tomra Systems", "Tomra",.) %>% 
  gsub("Voss Veksel- og Landmandsbank","Voss Veksel og Landmandsbank",.) %>% 
  gsub("Link Mobility Group","Link Mobility",.) %>% 
  gsub("Crayon Group Holding", "Crayon",.) %>% 
  gsub("Insr Insurance Group", "Insr Insurance",.) %>% 
  gsub("NEXT Biometrics Group", "Next Biometrics",.) %>% 
  gsub("Questerre Energy Corporation","Questerre",.) %>% 
  gsub("Jinhui Shipping and Transportation","Jinhui Shipping",.) %>% 
  gsub("TGS-NOPEC Geophysical Company","TGS Nopec",.) %>% 
  gsub("Golden Ocean Group","Golden Ocean",.) %>%
  gsub("Otello Corporation","Otello",.) %>% 
  gsub("Fjordkraft Holding", "Fjordkraft", .) %>% 
  gsub("PCI Biotech Holding","PCI Biotech",.) %>% 
  gsub("S.D. Standard Drilling", "SD Standard Drilling",.) %>% 
  gsub("TietoEVRY","Tieto",.) %>% 
  gsub("Oceanteam Shipping","Oceanteam",.) %>% 
  gsub("Gaming Innovation Group", "Gaming Innovation",.) %>% 
  gsub("Panoro Energy","Panoro",.) %>% 
  gsub("Havyard Group","Havyard",.) %>% 
  gsub("American Shipping Company","American Shipping",.) %>% 
  gsub("Vistin Pharma", "Vistin",.) %>% 
  gsub("Gjensidige Forsikring","Gjensidige",.) %>% 
  gsub("NTS","NTS Group",.) %>% 
  gsub("\\.","",.) %>% 
  gsub("\\,","",.)


# Remove company names that sound too similar (searching for "Aker" will give
# results of "Aker" and "Aker BP" for example, "Wilh Wilhelmsen Holding"
# has both "ser A" and "ser B")
all.firms = 
  all.firms %>% 
  subset(Company != "Aker" &
         Company != "Hafslund ser A" & Company != "Hafslund ser B" &
         Company != "Reach Subsea" & Company != "SpareBank 1" & 
           Company != "Wilh Wilhelmsen Holding ser A" &
           Company != "Wilh Wilhelmsen Holding ser B" &
           Company != "Schibsted ser A" &
           Company != "Schibsted ser B" &
           Company != "Odfjell ser A" &
           Company != "Odfjell ser B" &
           Company != "B2Holding" &
           Company != "Solstad Offshore ser B" & 
           Company != "Adevinta ser A")

# Using tickers vector to obtain stock data from Yahoo Finance
all.stocks <- BatchGetSymbols(tickers = all.tickers,
                              first.date = "2014-01-01",
                              last.date = "2019-12-31",
                              freq.data = "daily",
                              do.cache = FALSE,
                              thresh.bad.data = 0)


# How many companies do we have? (162)
# We originally had 258 companies and tickers, some tickers didn't have
# any info (deregistered or acquired by other companies and therefore no info),
# some companies had to be removed to eliminate possible confusion when searching
sum(ifelse(all.stocks$df.control$threshold.decision=="KEEP",1,0))

# Convert stock information into a data frame
stocks = all.stocks$df.tickers

# Finalizing the company list
# Add company name (for searching purposes)
stocks = left_join(stocks,all.firms,by="ticker")
stocks$Company = tolower(stocks$Company)

# Get the names of the companies we have stock price data for
companies = unique(stocks$Company)

# Add company names more used/or changed during time period
companies = c(companies,c("statoil", "marine harvest",
                          "af group","solstad farstad",
                          "vekselbanken","bergen group",
                          "vardia insurance",
                          "skandiabanken","psi",
                          "opera software",
                          "apptix","noreco",
                          "tts group","pgs",
                          "namsos traffikkselskap"))

# Calculate a daily price measure
for(i in 1:nrow(stocks)){
  stocks$av.price[i] = (stocks$price.open[i]+stocks$price.close[i])/2
}

# Only select columns we are interested in
stocks = 
  stocks %>% 
  select(Company,ticker,"date"=ref.date,price.close,price.open,av.price)


# Create direction-column
stocks$diff = stocks$price.close - stocks$price.open
stocks$dir = ifelse(stocks$diff == 0, "no change", ifelse(stocks$diff > 0, "up", "down"))

# Make sure that the last price observation of Akastor and the first observation
# of Aker BP (for example) are not calculated together, first observation of each
# company gives NA in the dir-column
# Note: gives error, still works
#for(i in 2:nrow(stocks)){
#  if(stocks$Company[i] != stocks$Company[i-1])
#    stocks$dir[i] = NA
#}        # UNNECCESSARY WHEN USING PRICE.CLOSE-PRICE.OPEN

save(stocks,companies,file="stocks.Rdata")

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

# Are there duplicated URLs?
nrow((distinct(as.data.frame(url.list))))

which(duplicated(url.list) == TRUE)

# Removing wrong/not working URLs
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
drafttext <- list()

for (url in url.list) {
  jump <- session %>% 
    session_jump_to(url)  # Jump to each URL logged in
  html <- read_html(jump) %>% 
    html_nodes("article") %>% 
    html_nodes("section") %>% 
    html_nodes("p") %>% 
    html_text()
  drafttext <- rbind(drafttext, toString(html))
}

#save(drafttext, file = "drafttext.RData")

load("drafttext.RData")

# Remove everything but letters
drafttext <- drafttext %>% 
  str_remove_all("\n") %>%
  str_remove_all("(Vilkår).*$") %>% 
  str_remove_all("[^[[:alpha:]][[:space:]]]")

# Make a data frame with dates, URLs and text from each article
text = as.data.frame(drafttext)

text$date = as.Date(date.list, "%d.%m.%Y")

text$url = url.list

names(text)[1] <- "text"

# Remove duplicated text
which(duplicated(text$text))

text <- text[!duplicated(text$text), ]

save(text, file = "text.Rdata")


################################################################################

# Start here 
rm(list = ls())
load("text.Rdata")
load("stocks.Rdata")

# Change articles to lowercase
text$text = str_to_lower(text$text)

# 1. Which companies are never mentioned?
# Create data frame with "companies" column and "mentioned" column
comp.df = data.frame(companies,"mentioned" = 0)

# Create string with text from all articles (easier to search in than in each row)
articles = toString(text$text)  

# Loop: for i in each row of comp.df, assign 1 to the "mentioned" column if a
# company name is found in the "articles" string and NA if not
for(i in 1:nrow(comp.df)){
  comp.df$mentioned[i] = 
    ifelse(str_detect(string = articles,
                      pattern = comp.df$companies[i])==TRUE,
           1,NA)  
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
      wb <- paste0("\\b", companies[company], "\\b")
      m <- gregexpr(wb, text[t,1])
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
  gsub("statoil", "equinor", .) %>% 
  gsub("marine harvest", "mowi", .) %>% 
  gsub("pgs", "petroleum geo services", .) %>% 
  gsub("noreco", "norwegian energy company", .) %>% 
  gsub("af group","af gruppen",.) %>% 
  gsub("solstad farstad","solstad offshore",.) %>% 
  gsub("vekselbanken","voss veksel og landmandsbank",.) %>% 
  gsub("bergen group", "endúr", .) %>% 
  gsub("vardia insurance","insr insurance",.) %>% 
  gsub("skandiabanken","sbanken",.) %>% 
  gsub("psi", "strongpoint", .) %>% 
  gsub("opera software","otello",.) %>% 
  gsub("apptix", "carasent", .) %>% 
  gsub("tts group","nekkar",.) %>% 
  gsub("samsos trafikkselskap", "nts group", .)

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

# Are there still duplicate dates?
length(which(duplicated(df7b$datecomp)==TRUE))    # We have reached the end

# Combine df67(non-duplicated rows of 1,2,3,4,5,6,7 articles per day)
# with df7b (final non-duplicated rows)
df.end = full_join(df67,df7b,by="datecomp")

# Combine columns (note that combining without changing NA columns results in some
# elements in the text-column end with "NA" or "NANANANANA" or something similar)
df.end = 
  df.end %>% 
  mutate("text" = 
           paste0(text.x,text.y,text.x.x,text.y.y,text.x.x.x,text.y.y.y,
                  sep=" ")) %>% 
  select(datecomp,text,"date" = date.x,"Company" = Company.x)

# Save+load
save(df.end, stocks, file = "dftemp.Rdata")

load("dftemp.Rdata")

# Change date: date
df.end$date <- as.Date(df.end$date)

# Merge df.end with stocks by date and company
end.df <- merge(df.end, stocks, by=c("date","Company")) # Merge text and stocks df's

# Remove NA at end of text
end.df$text <- str_remove_all(end.df$text, "NA")

# Remove all unnecessary dfs
rm(list = ls(pattern = "^df"))

# Remove NAs
end.df$text = na.omit(end.df$text)

# Final data frame is "df"
df = end.df

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

# Calculate sentiment score as the ratio between neg and pos words
LM.pos = LM.norsk %>% 
  filter(y == 1)

LM.neg = LM.norsk %>% 
  filter(y == -1)

score = vector()
for(t in 1:length(toks)){
  pos = sum(filter(LM.pos, x %in% toks[[t]])$y)
  neg = abs(sum(filter(LM.neg, x %in% toks[[t]])$y))
  score = append(score,
                 pos-neg)
}

# Insert into df
df$sentiment = score

save(df, file = "df.RData")

###############################################################################

rm(list = ls())
load("df.RData")

# CLASSIFICATION:

# Remove days with no change (better accuracy)

df <- df[!df$dir=="no change",]

# This creates a NA row - remove
df = na.omit(df)

# change y value to class factor
df$dir <- as.factor(df$dir)

# Split data:
set.seed(1)

n = nrow(df)

n.train = floor(0.9*n)

n.test = n.train+1

train = df[1:n.train,]

test = df[n.test:n,]

# Cross validation method: 10-fold cross validation
ctrl <- trainControl(method = "cv", number = 10)


# Logistic regression:
set.seed(1)

logreg <- train(dir~sentiment, data = train, method = "glm", family = binomial, 
                trControl = ctrl)

logpred <- predict(logreg, test)

confusionMatrix(logpred, test$dir)

conf.mat <- confusionMatrix(logpred, test$dir)[[2]]

accuracy <- sum(diag(conf.mat))/sum(conf.mat)

accuracy

val.set.err <- (conf.mat[1,2]+conf.mat[2,1])/(n/2)

val.set.err


rocpred1 <- prediction(as.numeric(logpred), test$dir)

rocperf1 <- performance(rocpred1, "tpr", "fpr")

plot(rocperf1, main = "ROC curve", col = "red")

AUC1 <- confusionMatrix(logpred, test$dir)[[4]][11]

text(0.4, 0.65, "AUC =")

text(0.4, 0.6, format(round(AUC1, 4)))

lines(x = c(0,100), y = c(0,100), type = "l", lty = 2)

legend(0, 1, legend=c("ROC curve", "Random"),
       col=c("red", "black"), lty=1:2, cex=0.8)

graphics.off()


# SVM classification:
set.seed(1)

svmreg <- train(dir~sentiment, data = train, method = "svmLinear",
                 trControl = ctrl)

svmpred <- predict(svmreg, test)

confusionMatrix(svmpred, test$dir)[[4]][1]

conf.mat2 <- confusionMatrix(svmpred, test$dir)[[2]]

accuracy2 <- sum(diag(conf.mat2))/sum(conf.mat2)

accuracy2

val.set.err2 <- (conf.mat2[1,2]+conf.mat2[2,1])/(n/2)

val.set.err2


rocpred2 <- prediction(as.numeric(svmpred), test$dir)

rocperf2 <- performance(rocpred2, "tpr", "fpr")

plot(rocperf2, main = "ROC curve", col = "green")

AUC2 <- confusionMatrix(svmpred, test$dir)[[4]][11]

text(0.4, 0.65, "AUC =")

text(0.4, 0.6, format(round(AUC2, 4)))

lines(x = c(0,100), y = c(0,100), type = "l", lty = 2)

legend(0, 1, legend=c("ROC curve", "Random"),
       col=c("green", "black"), lty=1:2, cex=0.8)

graphics.off()

# GBM classification:
set.seed(1)

xtrain = train[,10:11]

ytrain = train$dir

xtest = test[,10:11]

ytest = test$dir

x = cbind(xtrain, ytrain)

set.seed(1)

gbmfit = train(dir~sentiment, data=xtrain, method="gbm", 
               distribution = "bernoulli", trControl=ctrl)

gbmpred = predict(gbmfit, xtest)

confusionMatrix(gbmpred, ytest)

conf.mat3 <- confusionMatrix(gbmpred, ytest)[[2]]

accuracy3 <- sum(diag(conf.mat3))/sum(conf.mat3)

accuracy3

val.set.err3 <- (conf.mat3[1,2]+conf.mat3[2,1])/(n/2)

val.set.err3 


rocpred3 <- prediction(as.numeric(gbmpred), test$dir)

rocperf3 <- performance(rocpred3, "tpr", "fpr")

plot(rocperf3, main = "ROC curve", col = "blue")

AUC3 <- confusionMatrix(gbmpred, test$dir)[[4]][11]

text(0.4, 0.65, "AUC =")

text(0.4, 0.6, format(round(AUC3, 4)))

lines(x = c(0,100), y = c(0,100), type = "l", lty = 2)

legend(0, 1, legend=c("ROC curve", "Random"),
       col=c("blue", "black"), lty=1:2, cex=0.8)

graphics.off()


# K-Nearest Neighbors: 
set.seed(1)

knn <- train(dir~sentiment, data = train, method = "knn", trControl = ctrl)

knnpred <- predict(knn, test)

confusionMatrix(knnpred, test$dir)

conf.mat4 <- confusionMatrix(knnpred, test$dir)[[2]]

accuracy4 <- sum(diag(conf.mat4))/sum(conf.mat4)

accuracy4

val.set.err4 <- (conf.mat4[1,2]+conf.mat4[2,1])/(n/2)

val.set.err4


rocpred4 <- prediction(as.numeric(knnpred), test$dir)

rocperf4 <- performance(rocpred4, "tpr", "fpr")

plot(rocperf4, main = "ROC curve", col = "orange")

AUC4 <- confusionMatrix(knnpred, test$dir)[[4]][11]

text(0.4, 0.65, "AUC =")

text(0.4, 0.6, format(round(AUC4, 4)))

lines(x = c(0,100), y = c(0,100), type = "l", lty = 2)

legend(0, 1, legend=c("ROC curve", "Random"),
       col=c("orange", "black"), lty=1:2, cex=0.8)

graphics.off()


# Overview of results from logistic, SVM, GBM and KNN:
method = c("Logistic","SVM", "GBM","KNN")

acc.all = c(accuracy, accuracy2, accuracy3, accuracy4)

vse.all = c(val.set.err, val.set.err2, val.set.err3, val.set.err4)

final.table = data.frame(method,
                         "Accuracy" = acc.all,
                         "Validation set error" = vse.all)

plot(rocperf1, main = "ROC curve", col = "red")

plot(rocperf2, add = TRUE, col = "green")

plot(rocperf3, add = TRUE, col = "blue")

plot(rocperf3, add = TRUE, col = "orange")

lines(x = c(0,100), y = c(0,100), type = "l", lty = 2)

legend(0, 1, legend=c("Logistic regression", "K-nearest neighbors", 
                      "Gradient boosted trees", "Support vector machine", 
                      "Random"),
       col=c("red", "orange", "blue", "green", "black"), lty=c(1,1,1,1,2), cex=0.8)


###############################################################################

# EXTRA CLASSIFICATION METHODS IF NEEDED:

# Naive Bayes:
nbfit <- naiveBayes(dir~sentiment, data = train)

nbpred <- predict(nbfit, test)

conf.mat5 <- table(test$dir, nbpred)

conf.mat5

accuracy5 <- sum(diag(conf.mat5))/sum(conf.mat5)

accuracy5

val.set.err5 <- (conf.mat5[1,2]+conf.mat5[2,1])/(n/2)

val.set.err5

confusionMatrix(nbpred, test$dir)

# Generalized additive models:
gamfit <- gam(as.numeric(dir)~s(sentiment, 4), data = train)

summary(gamfit)

gampred <- predict(gamfit, test)

confusionMatrix(factor(ifelse(gampred > 1.5, "up", "down")), test$dir)

conf.mat6 = confusionMatrix(factor(ifelse(gampred > 1.5, "up", "down")), test$dir)$table
conf.mat6

accuracy6 = sum(diag(conf.mat6))/sum(conf.mat6)
accuracy6

val.set.err6 = (conf.mat6[1,2]+conf.mat6[2,1])/(n/2)

# Tree (randomForest):
rffit <- randomForest(dir~sentiment, data = train, mtry = 1)

rfpred <- predict(rffit, test)

conf.mat7 <- table(test$dir, rfpred)

conf.mat7

accuracy7 <- sum(diag(conf.mat7))/sum(conf.mat7)

accuracy7

val.set.err7 <- (conf.mat7[1,2]+conf.mat7[2,1])/(n/2)

val.set.err7

confusionMatrix(rfpred, test$dir)$table


# Linear discriminant analysis (LDA)
lda.fit = MASS::lda(dir~sentiment, data = train)

# Plot
plot(lda.fit)

# Predict
lda.pred = predict (lda.fit, test)
lda.class = lda.pred$class

# Confusion matrix
conf.mat8 = table(test$dir, lda.class)

# Accuracy
accuracy8 = sum(diag(conf.mat8))/sum(conf.mat8)

# Val set error
val.set.err8 <- (conf.mat8[1,2]+conf.mat8[2,1])/(n/2)

confusionMatrix(lda.class, test$dir)


# Quadratic discriminant analysis (QDA)
qda.fit = MASS::qda(dir~sentiment, data = train)
qda.fit

# Predict
qda.class = predict(qda.fit, test)$class

# Confusion matrix
conf.mat9 = table(test$dir, qda.class)

# Accuracy
accuracy9 = sum(diag(conf.mat9))/sum(conf.mat9)

# Val set error
val.set.err9 <- (conf.mat9[1,2]+conf.mat9[2,1])/(n/2)

confusionMatrix(qda.class, test$dir)

# Overview of results from all methods:
method = c("Logistic","SVM", "GBM","KNN", "Naive bayes", "GAM", 
           "RandomForest", "LDA", "QDA")

acc.all = c(accuracy, accuracy2, accuracy3, accuracy4, accuracy5,
            accuracy6, accuracy7, accuracy8, accuracy9)

vse.all = c(val.set.err, val.set.err2, val.set.err3, val.set.err4, 
            val.set.err5, val.set.err6, val.set.err7, val.set.err8,
            val.set.err9)

final.table = data.frame(method,
                         "Accuracy" = acc.all,
                         "Validation set error" = vse.all)
###############################################################################

