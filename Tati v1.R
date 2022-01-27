#---------------------------------------------------------------------#
# Who are the followers and what do they tweet about?
#---------------------------------------------------------------------#

##### Setting the environment and load tokens

setwd("C:/Users/tdutrabruno/OneDrive - IESEG/IESEG/Social Media Analytics/Group Project")
source("C:/Users/tdutrabruno/OneDrive - IESEG/IESEG/Social Media Analytics/Group Project/tokens.R")

##### Downloading required libraries

if(!require("httr")) install.packages("httr"); library("httr")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

# Library to extract followers/user information
if(!require("academictwitteR")) install.packages("academictwitteR"); library("academictwitteR")

##### Importing datasets

## Profile to be analyzed

company <- "TatiDuttra"

## Get the Profile ID

url_for_id <- modify_url(
  url = "https://api.twitter.com",
  path = c("2", "users","by","username",company)
)
resUser <- GET(url = url_for_id,add_headers(authorization = paste0("Bearer ",BearerToken)))
userid<- fromJSON(httr::content(resUser, "text"))

# ID Value
id <- get_user_id(company,bearer = BearerToken)

## Get followers List (ID, Name, Username) of the Profile

get_followers_url <- modify_url(
  url = "https://api.twitter.com",
  path = c("2", "users",userid$data$id,"followers"),
  query= list(
    max_results=1000
  )
)
resFoll <- GET(url = get_followers_url,add_headers(authorization = paste0("Bearer ",BearerToken)))
userfollowers <- fromJSON(httr::content(resFoll, "text"))
View(userfollowers$data)

## Get followers List of the Profile (with more information)

followers <- get_user_followers(id,bearer=BearerToken)
