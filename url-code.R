# URL code (we can try to find a better method later if we want)

articles <- 1
URLs <- list()
Dates <- list()

files = list.files(pattern="\\.(html)$") # get list of .html files

for(file in 1:length(files)){
  assign(paste0(gsub(".html","",files[file])),read_html(files[file]))
}

for(article in articles){
  URLs = Q1.2014 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q1.2014 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q1.2014 = data.frame(Dates,URLs)
  
  URLs = Q2.2014 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q2.2014 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q2.2014 = data.frame(Dates,URLs)
  
  URLs = Q3.2014 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q3.2014 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q3.2014 = data.frame(Dates,URLs)
  
  URLs = Q4.2014 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q4.2014 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q4.2014 = data.frame(Dates,URLs)
  
  URLs = Q1.2015 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q1.2015 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q1.2015 = data.frame(Dates,URLs)
  
  URLs = Q2.2015 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q2.2015 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q2.2015 = data.frame(Dates,URLs)
  
  URLs = Q3.2015 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q3.2015 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q3.2015 = data.frame(Dates,URLs)
  
  URLs = Q4.2015 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q4.2015 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q4.2015 = data.frame(Dates,URLs)
  
  URLs = Q1.2016 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q1.2016 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q1.2016 = data.frame(Dates,URLs)
  
  URLs = Q2.2016 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q2.2016 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q2.2016 = data.frame(Dates,URLs)
  
  URLs = Q3.2016 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q3.2016 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q3.2016 = data.frame(Dates,URLs)
  
  URLs = Q4.2016 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q4.2016 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q4.2016 = data.frame(Dates,URLs)
  
  URLs = Q1.2017 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q1.2017 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q1.2017 = data.frame(Dates,URLs)
  
  URLs = Q2.2017 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q2.2017 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q2.2017 = data.frame(Dates,URLs)
  
  URLs = Q3.2017 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q3.2017 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q3.2017 = data.frame(Dates,URLs)
  
  URLs = Q4.2017 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q4.2017 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q4.2017 = data.frame(Dates,URLs)
  
  URLs = Q1.2018 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q1.2018 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q1.2018 = data.frame(Dates,URLs)
  
  URLs = Q2.2018 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q2.2018 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q2.2018 = data.frame(Dates,URLs)
  
  URLs = Q3.2018 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q3.2018 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q3.2018 = data.frame(Dates,URLs)
  
  URLs = Q4.2018 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q4.2018 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q4.2018 = data.frame(Dates,URLs)
  
  URLs = Q1.2019 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q1.2019 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q1.2019 = data.frame(Dates,URLs)
  
  URLs = Q2.2019 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q2.2019 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q2.2019 = data.frame(Dates,URLs)
  
  URLs = Q3.2019 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q3.2019 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q3.2019 = data.frame(Dates,URLs)
  
  URLs = Q4.2019 %>% 
    html_nodes("h3") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Dates = Q4.2019 %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  urls.Q4.2019 = data.frame(Dates,URLs)
  
  url.list = rbind(urls.Q1.2014,urls.Q2.2014,urls.Q3.2014,urls.Q4.2014,
                   urls.Q1.2015,urls.Q2.2015,urls.Q3.2015,urls.Q4.2015,
                   urls.Q1.2016,urls.Q2.2016,urls.Q3.2016,urls.Q4.2016,
                   urls.Q1.2017,urls.Q2.2017,urls.Q3.2017,urls.Q4.2017,
                   urls.Q1.2018,urls.Q2.2018,urls.Q3.2018,urls.Q4.2018,
                   urls.Q1.2019,urls.Q2.2019,urls.Q3.2019,urls.Q4.2019)
  
  URLs = as.character(url.list$URLs)
  save(url.list,URLs,file="urls.Rdata")
  rm(list = ls())
  load("urls.Rdata")

}






# Log in to DN subscription
# Only run after having closed R/cleaned environment!
url <- "https://www.dn.no/auth/login"
uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"
session <- session(url, user_agent(uastring))
form <- html_form(session)[[1]]
fill <- html_form_set(form, 
                      username = "herdisbirta1@gmail.com",
                      password = "Herdiserbest")
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

# testing some stuff

# Manually emove urls that don't exist
# URLs = URLs[-1969,-2278]

URLs2 = URLs[2000:2050]

url.exists = url.exists(URLs2)

text <- list()

for (url in URLs2) {
  jump <- session %>% 
    session_jump_to(url)  # Jump to each URL logged in
  if(url.exists(url)==T){ # Check if url exists
  html <- read_html(jump) %>% 
    html_nodes("article") %>% 
    html_nodes("section") %>% 
    html_nodes("p")
  text <- rbind(text, toString(html))
  }
}







