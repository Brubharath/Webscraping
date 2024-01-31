#DCP Final Assignment
#Bharath Shivakumar
#Master of data science

#Question 17

#We load the necessary data
library(tidyverse)
library(rvest)

#We store the website link in the variable url
url <- "https://www.amazon.com.au/s?k=Apple+iPhone+X+Space+Grey+256GB&i=electronics&ref=nb_sb_noss"

#The scraping page variable, reads the html page
scraping_page <- read_html(url)

#This function takes a substring of the URL, which is useful for us later to accesss links through it.
sub_data <- substr(url, start = 1, stop = 26)

#In this question we are supposed to any five items from the website. 
#So we take out the hyperlinks for everything
links <- scraping_page %>% 
  html_nodes("a") %>% 
  html_attr("href")

#Once we have taken out all the hyperlinks, we find the links where there are items.
#So we take the index positions of the items using a grep function
#In this case we are after iPhones. We would be looking at Renewed iPhones.
items_pos <- grep(pattern = "Renewed", links)

#We check for the customer reviews with links
review_pos <-  grep(pattern = "eviews", links[items_pos])

#We check the length of it
length(review_pos)

#We use this to remove the customer reviews with links, as they are not items.
items_pos <- items_pos[-review_pos]

#We check our data once
links[items_pos]

#This chooses the 5 links position or index values.
hyperlinks_5_items <- sort(sample(length(items_pos), 5))
print(hyperlinks_5_items)

#Once the index has been chosen, we use these positions to access those links
random_links_5 <- links[items_pos[hyperlinks_5_items]]
print(random_links_5)

#We create an empty data frame that we would be filling in later.
iphone_data_table <- data_frame(Phone_name = character(),
                                Phone_rating = character(),
                                Phone_questions_answered = character(),
                                Phone_price = character(),
                                Phone_description = character())

print(iphone_data_table)


#We use a for loop, as we are scrapping high amounts of data
#as per the question, we store the data in dataframes using loops
for (i in 1:length(hyperlinks_5_items)) {
  #We use this function to create the url link
  item_link <- paste0(sub_data, random_links_5[i])
  #We read the html link
  item_link_data <- read_html(item_link)
  
  #First we scrape the name of the phone.
  #str_squish is to remove excess space in the data
  Phone_name <- item_link_data %>% 
    html_node("h1") %>% 
    html_text() %>% 
    str_squish()
  print(Phone_name)
 
  #Next we scrape the phone ratings review
  Phone_rating <- item_link_data %>% 
    html_nodes("a") %>% 
    html_text()
  #We take the index values first using the noise in the data and then remove these noise
  #or delimiters out of the data
  indices <- grep(pattern = "[0-9]\\.[0-9]\\sout\\sof\\s5\\sstars", Phone_rating)    
  rating <- str_extract(Phone_rating[indices[1]], pattern = "[0-9]\\.[0-9]\\sout\\sof\\s5\\sstars")
  print(rating) 
 
  #We also add in a condition in case if the rating is not available.
  if (length(rating) == 0) {                                            
    rating <- "NA"
  }
  
  #We use this function to get the answered questions data.
  Phone_questions_answered <- item_link_data %>% 
    html_nodes("span") %>% 
    html_text()
  
  #We use this function as we have received data with noise and we have received huge amounts of data as well.
  #We use the grep function to only call the data that we require.
  #the gsub is used for the string manipulation to clean the string
  questions_answered <- Phone_questions_answered[grep(pattern = "answered questions", Phone_questions_answered)] %>%
    gsub(pattern = "\\\n", replacement = "") %>% 
    trimws(which = "both")
  #The code was printing multiple same strings, we just need one string and that is why we
  #extract the string using the unique function using regex.
  questions_answered <- str_extract(unique(questions_answered), pattern = "\\d+ answered questions")
  print(questions_answered)
  
  if (length(questions_answered) == 0) {
    questions_answered <- "NA"
  }

  #Next we scrape, the phone price.
  #which is under a table, we use regex to find the price of the phone
  Phone_price <- item_link_data %>% 
    html_node("tr") %>% 
    html_text()                          
  iphone_price <- gsub(pattern = "[[:blank:]]", replacement = "", Phone_price) %>% 
    str_extract(pattern = "Price:\\$[0-9,]+\\.[0-9,]+")  
  print(iphone_price)  
  
  #if there is no price, then it displays NA
  if (length(iphone_price) == 0) {                                                    
    iphone_price <- "NA"
  }
  

  #For the "about this item" I have used "odel" or "iPhone" of the model to detect from the string as a keyword.
  #Not all the items have keyword common between them which can be used.
  Phone_description <- item_link_data %>% 
    html_nodes("ul") %>% 
    html_text() %>% 
    str_squish()
  description <- Phone_description[grep(pattern = "display | iPhone | Lightning", Phone_description)]
  
  print(description)
  
  #If there is no description value, it shows NA
  if(length(description) == 0) {
    description = "NA"
  }

  #Once the loop comes to the end, we take in all the information and bind them to the data frame
  #Then the loop repeats again and gives us different answers which we bind again until
  #the codition is satisfied.
  row_vector <- c(Phone_name, rating, questions_answered, iphone_price, description)
  iphone_data_table <- rbind(iphone_data_table, as.list(row_vector))
}  

#Since the data has been added to the table, the column names keep changing.
#So once the loop is done, we rename the columns back to the same name.
colnames(iphone_data_table) <- c("Phone name",
                                  "Phone rating",
                                  "Phone questions answered",
                                  "Phone price",
                                  "Phone description")

#We print to check the data
print(iphone_data_table)



