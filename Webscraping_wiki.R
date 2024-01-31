# DCP
# Data Collection
# Master of Data Science
# Bharath Shivakumar

###############################################################

# PART 1

# 1. Retrieve and load all the data from the url into R. (1 point)

#We install the necessary libraries. We need rvest to read html data
library(tidyverse)
library(rvest)

#We store the website detail in the variable called url
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

#We scrape the url information using read_html function by rvest
mydata <- read_html(url)


# 2. Obtain the table legend (Figure 1) and store all its elements. You can use any data
#    structure to store the data. (6 points)

#This function scrapes "dl" data from the website and retrieves text information only
table_legend <- mydata %>% 
  html_nodes("dl") %>% 
  html_text()

#In this function, we get rid of some variables as they are unnecessary to us like "citations" and 
# "general"
table_legend <- table_legend[c(-3,-4)]

#Once we remove the variables we don't need, we store the variables in a list.
#I have used strsplit to split the variables at "\n" which is the noise in the data from the website. 
table_legend <- strsplit(table_legend, "\\n")

#The above function creates us a list of 2. But it is easier to store information on one list.
#So we use the unlist function to put everything under one vector.
table_legend <- unlist(table_legend)

#We print to see if we have the right set of data.
print(table_legend)


# 3. Scrape the endangered list, which contains the current listed sites. You can use any
#    data structure to store the table. (6 points)

#We scrape the table which is under the "Currently listed sites".
#We use a selector as our identifier to retrieve only that particular table
endangered_list <- mydata %>% 
  html_nodes("#mw-content-text > div.mw-parser-output > table:nth-child(16)") %>% 
  html_table()

#The table which we have obtained is in a tibble format but in a list, so we use
#bind rows function to have them as one data frame or tibble which is easier to access.
endangered_list <- bind_rows(endangered_list)

#Now by printing this, we can see the table information
head(endangered_list)


# 4. Scrape all available hyperlinks in the url. (3 points)

#This provides us all the links which are available in the website.
hyperlinks <- mydata %>% 
  html_nodes("a") %>% 
  html_attr("href")

#We print to see if the right set of links that we need.
head(hyperlinks)

# 5. Using computational methods, obtain the hyperlink that you can click on to jump to
#    a new page that contains the selection criteria (image below) used to classify a site as 
#    cultural or natural heritage. (8 points)


#This function gives us anything which is under the selection criteria as that 
#is what we are trying to search
selection_criteria <- grep(pattern = "Selection_criteria", hyperlinks)

#Once we have the location of the links, we try to extract the links in them
selection_criteria_link <- hyperlinks[selection_criteria]

#We have reached a point where we have two same links as there are two tables in the website.
#We need only one link, so we use a simple method to index and we take the first link.
selection_criteria_link <- selection_criteria_link[1]

#We print to see if we have the right data.
print(selection_criteria_link)

#The link what we have is incomplete, to have the full website link we to use paste0 to
#to paste our link to the wikipedia website, as that is where are trying to get our data from.
selection_criteria_link <- paste0("https://en.wikipedia.org", selection_criteria_link)

#Now that we have our link ready, we just scrape all the data in the website using read_html.
selection_criteria_data <- read_html(selection_criteria_link)


# 6. Use the hyperlink obtained in the previous step and scrape the two lists 
#    (cultural and natural) and store them in two separated data structures within a list. (6 points)

#We retrieve the list contents using this function
Cultural_list <- selection_criteria_data %>% 
  html_nodes("#mw-content-text > div.mw-parser-output > ol:nth-child(28)") %>% 
  html_text()

#Then we remove the "\" from the data using gsub function
Cultural_list <- gsub('[\\\\"]','', Cultural_list)

#We still have "\n" in the data, but we use that to split the strings
#and have a list The data retrieved is in one line, it is important for us to split the data
#to understand our information.
Cultural_list <- strsplit(Cultural_list, "\\n")

#We put the list in a data frame as asked in the question.
#We use [[1]] in this function as we have created a list by using string split, we cannot create
# a dat frame without using indexing or it throws up an error.
Cultural_list <- as_data_frame(Cultural_list[[1]])

#There is no column name to the data frame, so we name the column.
colnames(Cultural_list) <- "Cultural List"

#We could also retrive the column name by scraping the data into another variable, and then combing
#two vectors. The process is uncessarily long. That is why I have chosen to just name the column manually.


#We have retrieved the cultural list, now we extract the natural list
Natural_list <- selection_criteria_data %>% 
  html_nodes("#mw-content-text > div.mw-parser-output > ol:nth-child(32)") %>% 
  html_text()

#Then we remove the "\" from the data using gsub function
Natural_list <- gsub('[\\\\"]','', Natural_list)

#We still have "\n" in the data, but we use that to split the strings
#and have a list. The data retrieved is in one line, it is important for us to split the data
#to understand our information.
Natural_list <- strsplit(Natural_list, "\\n")

#We put the list in a data frame as asked in the question.
#We use [[1]] in this function as we have created a list by using string split, we cannot create
# a dat frame without using indexing or it throws up an error.
Natural_list <- as_data_frame(Natural_list[[1]])

#There is no column name to the data frame, so we name the column.
colnames(Natural_list) <- "Natural List"

#We could also retrive the column name by scraping the data into another variable, and then combing
#two vectors. The process is uncessarily long. That is why I have chosen to just name the column manually.

#As per the question we are required to put the data frames under one list, so we put them in a list.
selected_criteria_list <- list(Cultural_list, Natural_list)

#We print to see if the data is right.
selected_criteria_list[[2]]


##################################################################

# PART 2

# 1. From the table containing the endangered sites, 
#    remove the undesired variables: Image and Refs. (2 points)

#I have used a flag variable to store the neccessary information.
endangered_sites <- endangered_list %>% 
  select(-Image, -Refs)


# 2. Then, obtain the country from the “Location” variable. Using computational 
#    methods (e.g., Regex) fix any inconsistencies in the variable and then 
#    extract the country only. (14 points)

#In this problem, we use a series of gsub function to remove unwanted data based on their pattern
#"^" this matches the start of the string, [^,]+ this matches more than one which are not commas
#\\s matches white spaces and ([^0-9]+) matches one or more characters but not digits
endangered_sites$Country <- gsub("^[^,]+,\\s*([^0-9]+).*", "\\1", endangered_sites$Location)

#matches with any number of digits
endangered_sites$Country <- gsub(pattern = "\\d", "",endangered_sites$Country)

#This matches with following symbol itself \\( \\) such as parenthesis.
endangered_sites$Country <- gsub(pattern = "\\..*|\\°.*|\\[.*|\\(.*","",endangered_sites$Country)

#This specifically removes the * which is in the country "Côte d'Ivoire & Guinea " and then it replaces
#it with an and.
endangered_sites$Country <- sub(pattern = "\\*", " &",endangered_sites$Country)
endangered_sites$Country <- sub(pattern = "\\*", "",endangered_sites$Country)

#This removes $ symbol from the string.
endangered_sites$Country <- str_remove(endangered_sites$Country,",$")
endangered_sites$Country <- sub(".*\\,", "", endangered_sites$Country)

#In the problem where we have the country Jerusalem, it removes the first the unwanted letters in them.
endangered_sites$Country <- gsub("^Jer+", "", endangered_sites$Country)

#This matches with white spaces and removes them
endangered_sites$Country <- gsub("\\s", " ", endangered_sites$Country)

#This trims the white space before the names of the country.
endangered_sites$Country <- trimws(endangered_sites$Country)


# 3. Using computational methods (Regex), 
#    split the variable that contains the criteria (“Criteria”) 
#    into two variables: “Type” (cultural/natural) and “Criteria” 
#    (containing roman numbers). (6 points)


#We are supposed the character variables from the roman variables.
#In this case we have a separator which is ":"
#While using the simple regex function "sep", we separate the data.
endangered_sites <- endangered_sites %>% 
  separate(Criteria, into = c("Type", "Criteria"), sep = "\\:", remove = FALSE)

#We print to see if we have the right set of data.
head(endangered_sites)


# 4. Then, maintain only the data in acres and remove the hectares (ha) from the 
#    “Area” variable. Remove any extra characters from the numbers. (8 points)

#We create new variables to separate area(ha) into HA and acres.
#We have a white space between them which we can use to our advantage.
endangered_sites <- endangered_sites %>% 
  separate(`Areaha (acre)`, into = c("Area(Ha)", "Area(acres)"), sep = "\\s", remove = FALSE)

#Now that we have separated our HA and acres, our acre values have parenthesis in them
#which we have to remove. We gsub function to remove the parenthesis.
endangered_sites$`Area(acres)` <- gsub("\\(|\\)", "", endangered_sites$`Area(acres)`)

#Now we just drop the unwanted variables which is area with HA and acre and we drop HA.
endangered_sites <- endangered_sites %>% 
  select(-`Areaha (acre)`, -`Area(Ha)`)

#When we print the data, we do see some NAs in them. This was because that data was not available
#in the website itself.
head(endangered_sites)


# 5. Using computational methods (Regex), clean the variable Endangered and 
#    maintain only the very last year. Remove any unwanted characters. (8 points)

#First we remove "–" from our data, with this almost 90% of our data is clean.
endangered_sites$Endangered <- gsub("–", "", endangered_sites$Endangered)

#Next we have some observations with multiple years in them,
#according to the question we remain the very last year.
#So looking at the pattern, we just use the sub function to remove the observation behind the separator.
endangered_sites$Endangered <- sub("^[^,]+,\\s*", "", endangered_sites$Endangered)


# 6. Make sure that you have numeric vectors and characters vectors only. (2 points)

#First, we check our datatypes of all the columns to understand which of them needs an updation.
str(endangered_sites)

#We have comma "," symbol in the data, because of which we are not able to convert them
#to numeric variables directly. We use the gsub function to remove the comma from the data. 
endangered_sites$`Area(acres)` <- gsub("\\,", "", endangered_sites$`Area(acres)`)

#We use as.numeric function to convert them from char to numeric variables.
endangered_sites$`Area(acres)` <- as.numeric(endangered_sites$`Area(acres)`)

#Like the last problem we have double quotations in the variables which we are trying to remove
#using the gsub function.
endangered_sites$Endangered <- gsub('\\"\\"', "", endangered_sites$Endangered)

#Then, we just convert them to integer as it is Year values.
endangered_sites$Endangered <- as.numeric(endangered_sites$Endangered)

#At last, we make sure that our Year variable is also numeric according to the question.
endangered_sites$`Year (WHS)` <- as.numeric(endangered_sites$`Year (WHS)`)


###############################################################################

# PART 3

# 1. What type of site (cultural or natural) is the most common in the 
#    endangered list and how many does each type of site have? (3 points)

#In this function we try to find the count of the types that we have at hand.
#Once we find the count we know which one is higher/more common.
Count_common_site <- endangered_sites %>% 
  group_by(Type) %>% 
  summarise(Count_of_types = n())

#We print to see the data.
#By printing the data we understand that Cultural is more common as it has a higher count.
print(Count_common_site)

#Once understood that Cultural is the most common. We filter the data under Cultural and display it.
common_endangered_list <- endangered_sites %>% 
  filter(Type == "Cultural")

#We print to see if we have the right set of data.
print(common_endangered_list)


# 2. What site has the largest area (in m2) and what site has the smallest area (in m2)? (3 points)

#We try to find the area in meter squares using mutate function.
endangered_sites <- endangered_sites %>% 
  mutate(Area_m2 = `Area(acres)` * 4046.86)

#Using which.max we find the location of the maximum value
max_index <- which.max(endangered_sites$Area_m2)

#Then using the location we try to find the site name which has the highest area in m2.
max_area_site <- endangered_sites$Name[max_index]

#We print the name of the site.
print(max_area_site)

#In this point, we trying to find the location of the minimum value.
min_index <- which.min(endangered_sites$Area_m2)

#Then using the location we try to find the site name which has the lowest area in m2.
min_area_site <- endangered_sites$Name[min_index]

#We print the name of the site.
print(min_area_site)


# 3. What is the frequency (in years) with which sites were put on the endangered list?
#    For example, how many were put on the list between 2010 and 2015? Use a plot 
#    to answer this question (e.g., histogram), remember to label and title you plot 
#    correctly. (6 points)


#We just print a basic histogram, based on the frequency of the column Endangered.
#In frequency it just takes the count or the number of occurrences of the variables for the histogram.
hist(endangered_sites$Endangered,
     freq = TRUE,
     xlab = "Endangered Year",
     ylab = "Frequency",
     main = "Histogram for the frequency of sites, 
     distributed through the years.",
     col = "Orange")


# 4. What country has more sites in the list? and how many sites has each country in the list? (6 points)

#To find the frequency or occurrences, we group the country and we arrange the frequency in descending order.
endangered_sites_freq <- endangered_sites %>% 
  group_by(Country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

#We print to see the frequency table.
#In the table we can clearly see that Libya and Syria are the countries with most sites.
print(endangered_sites_freq)

#Now that we have seen the table, we just need the country information.
#We try to find the location with higher counts.
max_indices <- which(endangered_sites_freq$count == max(endangered_sites_freq$count))

#We print to see if we have the right data.
print(max_indices)

#We have the location of the data, now using the location we try to find the country information.
country_with_max_sites <- endangered_sites_freq$Country[max_indices]


#We print the countries with highest sites as required in the question.
print(country_with_max_sites)


# 5. How long took each site to be in the endangered list? (6 points)

#We need to understand the difference between endangered and year (WHS) to find how long
#the site has taken to be in the endangered list.
#For this operation we need our old data, to take the initial years in the endangered columns.
endangered_list_new_df <- endangered_list

#The year data has to be cleaned, so we use regex using str_extract.
#\\d{4}. this is the patter which removes the digits and {4} removes the 4 digit number.
endangered_list_new_df$Endangered <- str_extract(endangered_list_new_df$Endangered, "\\d{4}")

#Now that we have corrected the data. We need to make sure that our colmns are in numeric format.
endangered_list_new_df$`Year (WHS)` <- as.numeric(endangered_list_new_df$`Year (WHS)`)
endangered_list_new_df$Endangered <- as.numeric(endangered_list_new_df$Endangered)

#We have corrected the data, we mutate a new columns to understand the number of years.
Endangered_site_years <- endangered_list_new_df %>% 
  select(Name, `Year (WHS)`, Endangered) %>% 
  mutate(Num_of_years = Endangered - `Year (WHS)`)

#We print to see if the data is right.
print(Endangered_site_years)


# 6. What is the frequency with which sites were put on the endangered list after they
#    were inscribed in the World Heritage List? For instance, how many sites were in 
#    the endangered list after 3 years in the World Heritage list. Use a plot to 
#    answer this question. (6 points)

#We just print a basic histogram, based on the frequency of the column Number of years.
#In frequency it just takes the count or the number of occurrences of the variables for the histogram.
hist(Endangered_site_years$Num_of_years,
     freq = TRUE,
     xlab = "Nuumber of year occurences",
     ylab = "Frequency",
     main = "Histogram for the frequency of the sites 
     which were put on the endangered list after they
     were inscribed in the World Heritage List",
     col = "Yellow")
