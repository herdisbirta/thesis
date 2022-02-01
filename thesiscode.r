
# R CODE MASTER THESIS:

# Libraries
library(rvest)

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