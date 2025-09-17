---
title: "Data Collection"
---

**Assignment 1**

Q2.

a.  

The survey is made up of a variety of blocks under which the questions encompass customer preferences, behaviours and demographics. It follows in a structured format where the questions are categorized by different categories of thew ell respondents such as movies, DVD, software and demographics.

b.  

The structure of the questionnaire includes questions scale based on LLikert scale, questions selection based on types of multiple choice and open ended. Similarly, the questionnaire topics include demographics, societal views on the media content, ideas for technologies, the patterns of purchase in terms of tools and how the respondents describe themselves in terms of age, sex and income.

c.  

The questions pose the problem of media users and their preference, then raise the issue of software interest and concern before shifting to the subject of buying behavior back and later turning to the demographic questions. This clear sequence helps in encouraging the respondents with the survey successful and ensures the most confidential information has been captured at such stage.

Q4.

The Diversity and Inclusiveness Survey features specific questions designed to assess various aspects of diversity and inclusivity, focusing on topics such as race, ethnicity, and gender. In contrast, the Movie Rental Survey is more general, allowing for the addition of a wide range of questions that may not directly relate to diversity or inclusiveness. This flexibility enables customization based on different research objectives, highlighting the tailored nature of the Diversity and Inclusiveness Survey compared to the open-ended approach of the Movie Rental Survey.

**Assignment 2**

Q2. a) Analyzing Google Trends Data for "Trump," "Kamala Harris," and "Election" Using the Google Trends Website: To analyze interest in "Trump," "Kamala Harris," and "Election," first visit the Google Trends website. Enter the keywords in the search bar and select a time frame for analysis. Once the data is displayed, you can download it as a CSV file. This method provides an intuitive interface for viewing trends, allowing for a manual inspection of dates and interest levels. You can visualize the data using software like Excel.

b)  

Using the gtrendsR Package in R: Alternatively, you can utilize the gtrendsR package in R for automated data retrieval. After installing and loading the package, use the gtrends function to collect data for the keywords. The resulting data frame includes detailed information on interest over time, making it easier to perform statistical analyses and visualizations directly in R.

```{r}
options(repos = c(CRAN = "https://cran.rstudio.com/"))
## Installing package
install.packages("gtrendsR")


## Loading library and run gtrends
library(gtrendsR)
library(dplyr)
library(tidyr)

HarrisTrumpElection = gtrends(c("Trump","Harris","election"), time = "all")

## Selecting the data for plotting
HarrisTrumpElection_interest <- HarrisTrumpElection$interest_over_time
HarrisTrumpElection_interest <- HarrisTrumpElection_interest[!is.na(HarrisTrumpElection_interest$hits), ]

## Plotting the data
par(family="Georgia")
str(HarrisTrumpElection)

plot(HarrisTrumpElection_interest$hits, type="l")


## Collecting data by timeframe
Sys.sleep(10)  # Wait 10 seconds between calls

gtrends(c("Harris", "Trump"), time = "now 1-H") # last hour

gtrends(c("Harris", "Trump"), time = "today 1-m") # last 30 days

## Collecting data by country

tg_gb <- gtrends(c("immigrants"), geo = c("GB", "US"), time = "all") 

## Checking the country codes
data("countries")

```

c)  Differences Between the Two Methods: The Google Trends website offers a user-friendly experience suitable for quick searches, but it requires manual data handling. In contrast, the gtrendsR package allows for automated collection of data, enabling more extensive analysis and visualization capabilities. Additionally, the package can provide finer granularity in data collection, making it preferable for users comfortable with coding and looking for deeper insights into trends over time.

**Assignment 3**

Q2.

```{r}
# Sample program for using quanteda for text modeling and analysis
# Documentation: vignette("quickstart", package = "quanteda")
# Website: https://quanteda.io/


library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(readr)
library(ggplot2)

# Twitter data about President Biden and Xi summit in Novemeber 2021
# Do some background search/study on the event
# 
summit <- read_csv("https://raw.githubusercontent.com/datageneration/datamethods/master/textanalytics/summit_11162021.csv")
View(summit)

sum_twt = summit$text
toks = tokens(sum_twt)
sumtwtdfm <- dfm(toks)
class(toks)

# Latent Semantic Analysis 
## (https://quanteda.io/reference/textmodel_lsa.html)

sum_lsa <- textmodel_lsa(sumtwtdfm, nd=4,  margin = c("both", "documents", "features"))
summary(sum_lsa)

head(sum_lsa$docs)
class(sum_lsa)
tweet_dfm <- tokens(sum_twt, remove_punct = TRUE) %>%
  dfm()
head(tweet_dfm)

tag_dfm <- dfm_select(tweet_dfm, pattern = "#*")
toptag <- names(topfeatures(tag_dfm, 50))
head(toptag, 10)

library("quanteda.textplots")
tag_fcm <- fcm(tag_dfm)
head(tag_fcm)

topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
textplot_network(topgat_fcm, min_freq = 50, edge_alpha = 0.8, edge_size = 1)

user_dfm <- dfm_select(tweet_dfm, pattern = "@*")
topuser <- names(topfeatures(user_dfm, 50))
head(topuser, 20)

user_fcm <- fcm(user_dfm)
head(user_fcm, 20)

user_fcm <- fcm_select(user_fcm, pattern = topuser)
textplot_network(user_fcm, min_freq = 20, edge_color = "firebrick", edge_alpha = 0.8, edge_size = 1)
```

Q3. a)

```{r}
# Sample program for using quanteda for text modeling and analysis
# Website: https://quanteda.io/

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(readr)
library(ggplot2)

# Wordcloud
# based on US presidential inaugural address texts, and metadata (for the corpus), from 1789 to present.
dfm_inaug <- corpus_subset(data_corpus_inaugural, Year <= 1826) %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_remove(stopwords('english')) %>% 
  dfm() %>%
  dfm_trim(min_termfreq = 10, verbose = FALSE)

set.seed(100)
textplot_wordcloud(dfm_inaug)
inaug_speech = data_corpus_inaugural

corpus_subset(data_corpus_inaugural, 
              President %in% c("Trump", "Obama", "Bush")) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm() %>%
  dfm_group(groups = President) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
  textplot_wordcloud(comparison = TRUE)


textplot_wordcloud(dfm_inaug, min_count = 10,
                   color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))


data_corpus_inaugural_subset <- 
  corpus_subset(data_corpus_inaugural, Year > 1949)
kwic(tokens(data_corpus_inaugural_subset), pattern = "american") %>%
  textplot_xray()


textplot_xray(
  kwic(tokens(data_corpus_inaugural_subset), pattern = "american"),
  kwic(tokens(data_corpus_inaugural_subset), pattern = "people"),
  kwic(tokens(data_corpus_inaugural_subset), pattern = "communist")
  
)

## Why is the "communist" plot missing?
## The word communist is missing in the plot because they didn't use this word in there document or speech

theme_set(theme_bw())
g <- textplot_xray(
  kwic(tokens(data_corpus_inaugural_subset), pattern = "american"),
  kwic(tokens(data_corpus_inaugural_subset), pattern = "people"),
  kwic(tokens(data_corpus_inaugural_subset), pattern = "communist")
)
g + aes(color = keyword) + 
  scale_color_manual(values = c("blue", "red", "green")) +
  theme(legend.position = "none")


library(quanteda.textstats)
features_dfm_inaug <- textstat_frequency(dfm_inaug, n = 100)

# Sort by reverse frequency order
features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))

ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Get frequency grouped by president
freq_grouped <- textstat_frequency(dfm(tokens(data_corpus_inaugural_subset)), 
                                   groups = data_corpus_inaugural_subset$President)

# Filter the term "american"
freq_american <- subset(freq_grouped, freq_grouped$feature %in% "american")  

ggplot(freq_american, aes(x = group, y = frequency)) +
  geom_point() + 
  scale_y_continuous(limits = c(0, 14), breaks = c(seq(0, 14, 2))) +
  xlab(NULL) + 
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


dfm_rel_freq <- dfm_weight(dfm(tokens(data_corpus_inaugural_subset)), scheme = "prop") * 100
head(dfm_rel_freq)

rel_freq <- textstat_frequency(dfm_rel_freq, groups = dfm_rel_freq$President)

# Filter the term "american"
rel_freq_american <- subset(rel_freq, feature %in% "american")  

ggplot(rel_freq_american, aes(x = group, y = frequency)) +
  geom_point() + 
  scale_y_continuous(limits = c(0, 0.7), breaks = c(seq(0, 0.7, 0.1))) +
  xlab(NULL) + 
  ylab("Relative frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


dfm_weight_pres <- data_corpus_inaugural %>%
  corpus_subset(Year > 2000) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm() %>%
  dfm_weight(scheme = "prop")

# Calculate relative frequency by president
freq_weight <- textstat_frequency(dfm_weight_pres, n = 10, 
                                  groups = dfm_weight_pres$President)

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")


# Only select speeches by Obama and Trump
pres_corpus <- corpus_subset(data_corpus_inaugural, 
                             President %in% c("Obama", "Trump"))

# Create a dfm grouped by president
pres_dfm <- tokens(pres_corpus, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_group(groups = President) %>%
  dfm()

# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(pres_dfm, target = "Trump")

# Plot estimated word keyness
textplot_keyness(result_keyness) 

# Plot without the reference text (in this case Obama)
textplot_keyness(result_keyness, show_reference = FALSE)


library(quanteda.textmodels)

# Irish budget speeches from 2010 (data from quanteda.textmodels)
# Transform corpus to dfm
data(data_corpus_irishbudget2010, package = "quanteda.textmodels")
ie_dfm <- dfm(tokens(data_corpus_irishbudget2010))

# Set reference scores
refscores <- c(rep(NA, 4), 1, -1, rep(NA, 8))

# Predict Wordscores model
ws <- textmodel_wordscores(ie_dfm, y = refscores, smooth = 1)

# Plot estimated word positions (highlight words and print them in red)
textplot_scale1d(ws,
                 highlighted = c("minister", "have", "our", "budget"), 
                 highlighted_color = "red")


# Get predictions
pred <- predict(ws, se.fit = TRUE)

# Plot estimated document positions and group by "party" variable
textplot_scale1d(pred, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))

# Plot estimated document positions using the LBG transformation and group by "party" variable

pred_lbg <- predict(ws, se.fit = TRUE, rescaling = "lbg")

textplot_scale1d(pred_lbg, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))


# Estimate Wordfish model
library("quanteda.textmodels")
wf <- textmodel_wordfish(dfm(tokens(data_corpus_irishbudget2010)), dir = c(6, 5))

# Plot estimated word positions
textplot_scale1d(wf, margin = "features", 
                 highlighted = c("government", "global", "children", 
                                 "bank", "economy", "the", "citizenship",
                                 "productivity", "deficit"), 
                 highlighted_color = "red")


# Plot estimated document positions
textplot_scale1d(wf, groups = data_corpus_irishbudget2010$party)


# Transform corpus to dfm
ie_dfm <- dfm(tokens(data_corpus_irishbudget2010))

# Run correspondence analysis on dfm
ca <- textmodel_ca(ie_dfm)

# Plot estimated positions and group by party
textplot_scale1d(ca, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))

```

b). The lexical dispersion plot reveals shifts in U.S. presidential rhetoric from Eisenhower to Biden, with the terms "American" and "people" appearing consistently but with varying emphasis. Presidents like Reagan, Clinton, Obama, and Trump frequently used both terms, reflecting a focus on national identity and populist themes. More recent presidents, especially Biden, show increased usage of "people," suggesting a shift toward inclusive language. Earlier presidencies like Eisenhower and Johnson have fewer instances, indicating a leaner use of direct appeals to citizens in speeches, potentially due to changes in cultural and rhetorical trends over time.

Q4. Wordfish was created for text data analysis, and it is particularly helpful for determining where documents fall on a latent scale based on word usage. By quantifying variations in word frequency among documents using a Poisson-based method, it enables researchers to deduce the ideological or policy stances of texts without being aware of those stances beforehand. Wordfish can be used to estimate these locations in policy documents, political speeches, or any corpus where positional scaling is important. Wordfish is useful for social science and political science research because it can reveal hidden themes or points of view in texts by spotting unique word patterns.

**Assignment 4 (Data Collection)**

Q1.

```{r}
## Workshop: Scraping webpages with R rvest package
# Prerequisites: Chrome browser, Selector Gadget
options(repos = c(CRAN = "https://cran.rstudio.com/"))
install.packages("rvest")

# install.packages("tidyverse")
library(tidyverse)
install.packages("rvest")
library(rvest)
library(readr)

url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_foreign-exchange_reserves'
#Reading the HTML code from the Wiki website
wikiforreserve <- read_html(url)
class(wikiforreserve)

## Get the XPath data using Inspect element feature in Safari, Chrome or Firefox
## At Inspect tab, look for <table class=....> tag. Leave the table close
## Right click the table and Copy --> XPath, paste at html_nodes(xpath =)

foreignreserve <- wikiforreserve %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[1]') %>%
  html_table()
class(foreignreserve) 

# Why the first column is not scrapped?
# Answer: This could happen due to table headers or inconsistent formatting in the HTML structure, 
# or if the first column is a row header or merged cell, which may not be captured in the table structure as expected by html_table.

fores = foreignreserve[[1]][,c(1, 2,3,4,5,6,7,8) ] # [[ ]] returns a single element directly, without retaining the list structure.


# 
names(fores) <- c("Country", "Forexreswithgold", "Date1", "Change1","Forexreswithoutgold", "Date2","Change2", "Sources")
colnames(fores)

head(fores$Country, n=10)

# Sources column useful?
# Answer: The Column names are large so it could cause problem in code.That is the reson we changed these names. 
# Now I don't think they are useful

## Clean up variables
## What type is Date?
str(fores) # Dates are character in this case

# Convert Date1 variable
fores$Date1 = as.Date(fores$Date1, format = "%d %b %Y")
class(fores$Date1)

write.csv(fores, "fores.csv", row.names = FALSE) 
 
# use fwrite?
install.packages("data.table")

# Load the data.table package
library(data.table)
fwrite(fores, "fores.csv",row.names = FALSE)

################################################## New Table #####################################

# Prerequisites: Chrome browser, Selector Gadget

# install.packages("tidyverse")
library(tidyverse)
install.packages("rvest")
library(rvest)
library(readr)

url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_foreign-exchange_reserves'


wikiforreserve <- read_html(url)
class(wikiforreserve)


currency <- wikiforreserve %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table()
class(foreignreserve) 

# Why the first column is not scrapped?
# Answer: This could happen due to table headers or inconsistent formatting in the HTML structure, 
# or if the first column is a row header or merged cell, which may not be captured in the table structure as expected by html_table.

fores = currency[[1]][,c(2,3,4,5,6,7,8) ] # [[ ]] returns a single element directly, without retaining the list structure.

# 
names(fores) <- c("Year", "Quater", "USD", "EUR","JPY", "GBP","CAD")
colnames(fores)

head(fores, n=10)

fores <- fores[-c(1), ]

## Clean up variables
## What type is Date?
str(fores)

# use fwrite?
install.packages("data.table")

# Load the data.table package
library(data.table)

fwrite(fores, "currency.csv",row.names = FALSE)

```

Q2.

```{r}
## Scraping Government data
## Website: GovInfo (https://www.govinfo.gov/app/search/)
## Prerequisite: Download from website the list of files to be downloaded
## Designed for background job

# Start with a clean plate and lean loading to save memory
 
gc(reset=T)

# install.packages(c("purrr", "magrittr")
library(purrr)
library(magrittr) # Alternatively, load tidyverse

## Set path for reading the listing and home directory
## For Windows, use "c:\\directory\\subdirectory\\"
## For Mac, "/Users/YOURNAME/path/"

setwd("/Users/vinay/Downloads")
library(rjson)
library(jsonlite)
library(data.table)
library(readr)

## CSV method
govfiles= read.csv(file="https://github.com/datageneration/datamethods/raw/refs/heads/master/webdata/govinfo-search-results-2024-10-13T07_10_42.csv", skip=2)

## JSON method
### rjson
gf_list <- rjson::fromJSON(file ="https://github.com/datageneration/datamethods/raw/refs/heads/master/webdata/govinfo-search-results-2024-10-13T07_18_29.json")
govfile2=dplyr::bind_rows(gf_list$resultSet)

### jsonlite
gf_list1 = jsonlite::read_json("https://github.com/datageneration/datamethods/raw/refs/heads/master/webdata/govinfo-search-results-2024-10-13T07_18_29.json")

### Extract the list
govfiles3 <- gf_list1$resultSet

### One more step
govfiles3 <- gf_list1$resultSet |> dplyr::bind_rows()


# Preparing for bulk download of government documents
govfiles$id = govfiles$packageId
pdf_govfiles_url = govfiles$pdfLink
pdf_govfiles_id <- govfiles$id

# Directory to save the pdf's
save_dir <- "/Users/vinay/Downloads"

# Function to download pdfs
download_govfiles_pdf <- function(url, id) {
  tryCatch({
    destfile <- paste0(save_dir, "govfiles_", id, ".pdf")
    download.file(url, destfile = destfile, mode = "wb") # Binary files
    Sys.sleep(runif(1, 1, 3))  # Important: random sleep between 1 and 3 seconds to avoid suspicion of "hacking" the server
    return(paste("Successfully downloaded:", url))
  },
  error = function(e) {
    return(paste("Failed to download:", url))
  })
}

# Simple timer, can use package like tictoc

## Try downloading one document
start.time <- Sys.time()
message("Starting downloads")
results <- 1:1 %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Try all five
start.time <- Sys.time()
message("Starting downloads")
results <- 1:length(pdf_govfiles_url) %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Print results
print(results)


## Exercise: Try downloading 118th Congress Congressional Hearings in Committee on Foreign Affairs?

# Start with a clean plate and lean loading to save memory

gc(reset=T)

# install.packages(c("purrr", "magrittr")
library(purrr)
library(magrittr) # Alternatively, load tidyverse

## Set path for reading the listing and home directory
## For Windows, use "c:\\directory\\subdirectory\\"
## For Mac, "/Users/YOURNAME/path/"

setwd("/Users/vinay/Downloads")
library(rjson)
library(jsonlite)
library(data.table)
library(readr)


### jsonlite
gf_list1 = jsonlite::read_json("/Users/vinay/Downloads/govinfo-search-results-2024-12-04T05_24_52.json")

### Extract the list
govfiles3 <- gf_list1$resultSet

### One more step
govfiles3 <- gf_list1$resultSet |> dplyr::bind_rows()


# Preparing for bulk download of government documents
govfiles$id = govfiles$packageId
pdf_govfiles_url = govfiles$pdfLink
pdf_govfiles_id <- govfiles$id

# Directory to save the pdf's
save_dir <- "/Users/vinay/Downloads"

# Function to download pdfs
download_govfiles_pdf <- function(url, id) {
  tryCatch({
    destfile <- paste0(save_dir, "govfiles_", id, ".pdf")
    download.file(url, destfile = destfile, mode = "wb") # Binary files
    Sys.sleep(runif(1, 1, 3))  # Important: random sleep between 1 and 3 seconds to avoid suspicion of "hacking" the server
    return(paste("Successfully downloaded:", url))
  },
  error = function(e) {
    return(paste("Failed to download:", url))
  })
}

# Download files, potentially in parallel for speed
# Simple timer, can use package like tictoc
# 

## Try downloading one document
start.time <- Sys.time()
message("Starting downloads")
results <- 1:1 %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Try all five
start.time <- Sys.time()
message("Starting downloads")
results <- 1:10 %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Print results
print(results)


```

Q3.

a.  The data scraping has provided a topicality of difficulties, including data scraping set up so that the websites are inconsistent in structure, dynamic content requiring JavaScript execution, and limitations due to anti-scraping mechanisms. Such challenges resulted in an incomplete or messy dataset with missing or duplicated entries and entries with irregular formatting. Although it is very messy, after so much cleaning and preprocessing, the data can still be partially usable and may not completely meet projection requirements as to reliability and completeness.

b.  Improving the functionality of scraping, advanced technologies, for example, running a headless browser or APIs to deal with dynamic content, would help. Building strong error management and retry functions can also help limit lost data, while automated data quality checks will continue to yield better results. Secure permission and ethical scraping guidelines will also help reduce the chances of being blocked.

**Assignment 5 (Data Collection)**

Q1

a)  

```{r}
## Collecting Social Media data: YouTube

# Required Libraries
# Install if necessary
# install.packages("tuber")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("stringi")
# install.packages("wordcloud")
# install.packages("gridExtra")
# install.packages("httr")
# install.packages("tm")

library(tuber)
library(tidyverse)
library(lubridate)
library(stringi)
library(wordcloud)
library(gridExtra)
library(httr)
library(tm)


# Replace with your actual Client ID and Client Secret
yt_oauth("", "", token = "")

### Step 3: Download YouTube Data

#### Here’s an example of collecting data on the “US election 2024.”

#### Search for videos related to "CNN"
yt_cnn2024 <- yt_search(term = "CNN")


#### Display the first few rows

head(yt_cnn2024)


### Step 4: Basic Analytics on CNN Data

#### Most Frequent Words in Video Titles

# Extract titles and clean up

# Extract titles and clean up
titles <- yt_cnn2024$title
titles_clean <- tolower(titles) %>%
  stri_replace_all_regex("[[:punct:]]", "") %>%
  str_split(" ") %>%
  unlist()

# Create a word frequency table
word_freq <- table(titles_clean)
word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
colnames(word_freq_df) <- c("word", "freq")

# Filter common words (stop words) and plot a word cloud
word_freq_df <- word_freq_df %>% filter(!word %in% tm::stopwords("en"))
set.seed(123)
wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, max.words = 50)


### Plotting Video Publish Dates

# Format to publish dates and aggregate data

yt_cnn <- yt_cnn2024 %>%
  mutate(publish_date = as.Date(publishedAt)) %>%
  count(publish_date)

# Plot the frequency of videos published over time
ggplot(yt_cnn, aes(x = publish_date, y = n)) +
  geom_line(color = "green") +
  labs(title = "Videos Published Over Time", x = "Date", y = "Number of Videos") +  
  theme_bw()

### Top Channels by Video Count

# Summarize by channel
top_channels <- yt_cnn2024 %>%
  count(channelTitle, sort = TRUE) %>%
  top_n(10)

# Plot top channels
ggplot(top_channels, aes(x = reorder(channelTitle, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top Channels with CNN", x = "Channel", y = "Number of Videos")
```

Q2.

The graph portraying Videos Published Over Time indicates that there is an upsurge in the number of videos with respect to the CNN and throughout the year 2024. It means increased interest generated and drift towards these elections closer to the date. This emphasizes its excellent growing significance at present during the run-up to key moments in political campaigns and other major happenings that prompt news-gathering organizations and media channels to produce content.

The Word Cloud of Video Titles holds a bunch of keywords such as "CNN," "Trump," "Harris," and "election; " they reflect the prominence given to these people during important events and also at times of debate. But "CNN" assumes much importance in the analysis for the reason that this entity gathers almost all the complete information with regard to elections. Terms like 'Trump' or 'Harris' indicate the amount of interest generated among the public and media during elections around such people.

The Chart, 'Top Channels by No of Videos' reflects the supremacy held by CNN with respect to the American elections; its average channel plus branches such as CNN Brasil and CNN Türk present great numbers of videos. With such a thesis, the leading references present the global character that CNN possesses when it comes to its capability to discourse in the subjectivity or personal tangibility of the election. Other actors like The Rubin Report and Sky News Australia somewhat contribute to the count of contents that are being produced in relation to the elections, thus showing such diversity in national and independent perspectives of the elections.

Q3.

Yes, we can summarize it with quatenda. Here is the code:

```{r}
# Load required libraries
install.packages("quanteda.textstats")
library(quanteda.textstats)

library(quanteda)
library(dplyr)
library(ggplot2)

# Extract and clean titles
titles <- yt_cnn2024$title

# Create a corpus from titles
corpus <- corpus(titles)

# Tokenize and clean text (remove punctuation, numbers, and stop words)
tokens <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("en"))

# Create a Document-Feature Matrix (DFM)
dfm <- dfm(tokens)

# Analyze word frequencies
word_freq <- textstat_frequency(dfm, n = 50)

# Plot the top 20 most frequent words
ggplot(word_freq[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 20 Words in Video Titles",
       x = "Words",
       y = "Frequency")

# Sentiment Analysis
# Add a sentiment dictionary
sentiment <- tokens_lookup(tokens, dictionary = data_dictionary_LSD2015)

# Summarize sentiment by video titles
sentiment_summary <- dfm(sentiment) %>%
  convert(to = "data.frame") %>%
  summarise(positive = sum(positive), negative = sum(negative))

print(sentiment_summary)



```
