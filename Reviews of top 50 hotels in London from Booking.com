The task is structured in 4 parts. 
The first part (pre-processing part) - extract the data of top 50 hotels in London from Booking.com
Part A - Construction of Corpus 
Part B - Text features and Sentiment association with rating score
Part C - Topic Modelling and Latent Dirichlet allocation 


Pre-processing part
Step1: Library Initialization
library(stringr)
library(rvest)
library(purrr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(RSelenium)
library(lubridate)
library(plyr)
library(cld2)
library(htmlwidgets)
library(qdapRegex)
library(qdap)
library(tm)
library(sentimentr)
library(textstem)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(wordcloud)
library(quanteda.textplots)
library(textclean)
library(igraph)
library(viridis)
library(wordcloud2)
library(textdata)
library(caTools)
library(groupdata2)
library(ggpubr)
library(corrplot)
library(stargazer)
library(caret)
library(reshape2)
library(splitstackshape)
library(ggraph)
library(udpipe)
library(doParallel)
library(stm)
library(FactoMineR)
library(factoextra)
library(furrr)
library(FSelector)

Step2: Read URL
# Read the content from a html page
hotel_1 <- read_html("https://www.booking.com/searchresults.html?label=gen173nr-1FCAEoggI46AdIM1gEaFCIAQGYATG4ARfIAQzYAQHoAQH4AQKIAgGoAgO4Arbr-JEGwAIB0gIkYzkzODdmMWUtYWU0MC00Y2FhLTg5NjMtN2Y2YjE4NGI1MTUw2AIF4AIB&sid=ee2dfaa95be553ef9dd8390329775870&aid=304142&ss=London&ssne=London&ssne_untouched=London&lang=en-us&src=searchresults&dest_id=-2601889&dest_type=city&group_adults=2&no_rooms=1&group_children=0&sb_travel_purpose=leisure&nflt=ht_id%3D204")

hotel_2 <- read_html("https://www.booking.com/searchresults.html?label=gen173nr-1FCAEoggI46AdIM1gEaFCIAQGYATG4ARfIAQzYAQHoAQH4AQKIAgGoAgO4Arbr-JEGwAIB0gIkYzkzODdmMWUtYWU0MC00Y2FhLTg5NjMtN2Y2YjE4NGI1MTUw2AIF4AIB&sid=ee2dfaa95be553ef9dd8390329775870&aid=304142&ss=London&ssne=London&ssne_untouched=London&lang=en-us&src=searchresults&dest_id=-2601889&dest_type=city&group_adults=2&no_rooms=1&group_children=0&sb_travel_purpose=leisure&nflt=ht_id%3D204&offset=25")

# print URL details
# print(hotel_1)
# print(hotel_2)
Step3: Import htmls
# Find name of hotels (top 50)
name_hotel_1 <- hotel_1 %>% html_nodes("h3 a") %>% html_text(".fde444d7ef _c445487e2") %>% data.frame()
name_hotel_2 <- hotel_2 %>% html_nodes("h3 a") %>% html_text(".fde444d7ef _c445487e2") %>% data.frame()

# Find hotel's link
link_hotel_1 <- hotel_1 %>% html_nodes("h3 a") %>% html_attr("href") %>% data.frame()
link_hotel_2 <- hotel_2 %>% html_nodes("h3 a") %>% html_attr("href") %>% data.frame()

# Create a dataframe for hotels
name_hotel <- rbind(name_hotel_1,name_hotel_2)
name_hotel$Hotel_name <- name_hotel$. 
name_hotel$. <-NULL

# Name of hotels
name_hotel$Hotel_name <- gsub("Opens in new window","",name_hotel$Hotel_name)

link_hotel <- rbind(link_hotel_1,link_hotel_2)
link_hotel$Hotel_link <- link_hotel$. 
link_hotel$. <-NULL

# Review links
hotel <- tibble(name_hotel,link_hotel)
hotel$Hotel_review <- hotel$Hotel_link
hotel$Hotel_review <- gsub("hotelTmpl","tab-reviews",hotel$Hotel_review)
Step4: Hotels’ detail
# Connect to server
  rD <- rsDriver(browser = "chrome",port=1393L, chromever="99.0.4844.35")
  remDr <- rD[["client"]]
# Create a dataframe to store hotels' detail
Hotel_df <- data.frame()

for (i in 1:nrow(hotel)){ 
  url <- hotel$Hotel_link[i]

  remDr$navigate(url)
  
    # Scrape in HTML Objects
     html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
    
    # Name
     Hotel_name <- hotel$Hotel_name[i]
   
    # Address
     Address <- html_obj %>% html_nodes("[data-node_tt_id='location_score_tooltip']") %>% html_text() %>% gsub("\n","",.)
   
    # Star rating
     Star <- html_obj %>% html_nodes("[data-testid='rating-stars']") %>% html_nodes("svg") %>% length() %>% as.numeric()

    # Popular facilities
     Facilities <- html_obj %>% html_nodes("[class='hp_desc_important_facilities clearfix hp_desc_important_facilities--bui ']") %>% html_nodes("div") %>% html_text("svg") %>% unique() %>% gsub("\n","",.) 
     Facilities <- paste(as.character(Facilities), collapse=", ")
     
    # Room Type
     RoomType <- html_obj %>% html_nodes("tbody a") %>% html_attr("data-room-name-en")
     RoomType <- paste(as.character(RoomType), collapse=", ")
     
    # Check-in time
     Checkin <- html_obj %>% html_nodes("[data-component='prc/timebar']") %>% html_attr("data-from") %>% head(length(1))
     
    # Check-out time
     Checkout <- html_obj %>% html_nodes("[data-component='prc/timebar']") %>% html_attr("data-until") %>% tail(length(1))
     
    # Smoking allowance
     Smoking <- html_obj %>% html_nodes("[class='description description--house-rule']") %>% html_nodes("p") %>% html_text() 
     Smoking <- ifelse(grepl("Smoking is not allowed.", Smoking),"No","Yes") 
     Smoking <- "No" %in% Smoking
     Smoking <- ifelse(Smoking=="TRUE","No","Yes")
     
    # Pet allowance
     Pet <- html_obj %>% html_nodes("[class='description description--house-rule']") %>% html_nodes("p") %>% html_text()
     Pet <- ifelse(grepl("Pets are not allowed", Pet),"No","Yes") 
     Pet <- "No" %in% Pet
     Pet <- ifelse(Pet=="TRUE","No","Yes")
     
    # Score - Staff
     Score_hotel <- html_obj %>% html_nodes("[class='c-score-bar__score']") %>% html_text() 
     Score_staff <- Score_hotel[1]
    # Score - Facilities
     Score_facilities <- Score_hotel[2]
    # Score - Cleanliness
     Score_cleanliness <- Score_hotel[3]
    # Score - Comfort
     Score_comfort <- Score_hotel[4]
    # Score - Value for money
     Score_value_money <- Score_hotel[5]
    # Score - Location
     Score_location <- Score_hotel[6]
    # Score - Free WiFi
     Score_free_WiFi <- Score_hotel[7]
     
     c <- tibble(Hotel_name, Address,Star, Facilities,RoomType,Checkin,Checkout,Smoking,Pet,Score_staff,Score_facilities,Score_cleanliness,Score_comfort,Score_value_money,Score_location,Score_free_WiFi)
     Hotel_df = rbind.fill(Hotel_df, c)
     
      print(i)}
   
 remDr$close()
 rD$server$stop()
 
 hotel <- left_join(hotel,Hotel_df)
Step5: Reviewers’ information
# Connect to server
rD <- rsDriver(browser = "chrome",port=1388L, chromever="99.0.4844.35")
remDr <- rD[["client"]]
# Create a dataframe to store reviewers' detail
Hotel_review_df <- data.frame()

for (i in 1:nrow(hotel)){
  url <- hotel$Hotel_review[i]

  remDr$navigate(url)
  
    # Scrape in HTML Objects
     html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
    
    # Find total numbers of page review
     page_num <- html_obj %>% html_nodes("[class='bui-pagination__pages']") %>% html_nodes(".bui-pagination__link") %>% html_nodes("[aria-hidden='true']") %>% html_text() %>% as.numeric()
     max_num <- ifelse(is_empty(page_num),1,max(page_num))
    
    for (j in 1:max_num){
     html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
         
     # Reviewer  
     Reviewer <- html_obj %>% html_nodes("[class='c-review-block__row c-review-block__guest']") %>% html_nodes("[class='bui-avatar-block__text']") %>% html_node("[class='bui-avatar-block__title']") %>% html_text() %>% {if(length(.) == 0) NA else .}
     
     # Country
     Country <- html_obj %>% html_nodes("[class='c-review-block__row c-review-block__guest']") %>% html_node(".bui-avatar-block__subtitle") %>% html_text() %>% {if(length(.) == 0) NA else .}

     Country <- gsub("\n                                    ","",Country)
  
     # Room type
     Room_type <- html_obj %>% html_nodes("[class='bui-grid__column-3 c-review-block__left']") %>% html_node("[class='bui-list__body']") %>% html_text() %>% {if(length(.) == 0) NA else .}
     Room_type <- gsub("\n","",Room_type)
     
     # Number of nights and stay date
     Nightdate <- html_obj %>% html_nodes(".c-review-block__stay-date") %>% html_node(".bui-list__body") %>% html_text() %>% {if(length(.) == 0) NA else .} 
     Nightdate <- gsub("\n","",Nightdate)
     Number_night <- gsub("(night).*", "\\1",Nightdate)
     Stay_Date <- gsub(".*·","",Nightdate)
     
     # Review score
     Score <- html_obj %>% html_nodes("[class='bui-grid__column-2 bui-u-text-right']") %>% html_nodes(".bui-review-score__badge") %>% html_text(".Scored")
     Score <- gsub(" ","",Score)
     
     # Review date
     Review_date <- html_obj %>% html_nodes(".c-review-block__right") %>% html_nodes(".c-review-block__row") %>% html_nodes(".c-review-block__date") %>% html_text("class")
     Review_date <- gsub("\nReviewed: ","",Review_date)
     Review_date <- gsub("\n","",Review_date)
     
     # Review text
     Review <- html_obj %>% html_nodes("[class='bui-grid__column-10']") %>% html_text() %>% {if(length(.) == 0) NA else .}%>% head(length(Review_date))
     Review <- gsub("\n","",Review)
    
     # Review language   
     Review_language <- html_obj %>% html_nodes("[class='bui-grid__column-10']") %>% html_node("h3") %>% html_attr("lang") %>% {if(length(.) == 0) NA else .} %>% head(length(Review_date))

      
     # Positive and negative reviews
     Like_Dislike <- html_obj %>% html_nodes(".c-review-block__right") %>% html_text("c-review__body") %>% as.data.frame()
     Like_Dislike$. <- gsub(".*0    \n\n\n\n\n\n\n\n","",Like_Dislike$.)
     Like_Dislike$. <- gsub("\n\n\n\n\n\n\n\n\n\n\n\nHelpful\n\n\n\n\n\nNot helpful\n\n\n\n\n\n\n\n","",Like_Dislike$.)
     
     ## Positive reviews
     Like_Dislike$Like <- as.data.frame(ifelse(grepl("Liked",Like_Dislike$.),1,0))
     Like_Dislike$Like_Review <- ifelse(Like_Dislike$Like=="1", gsub("\n.*","",Like_Dislike$.), NA)
     Like_Dislike$Like_Review <- gsub("Liked","",Like_Dislike$Like_Review)
     Like_Dislike$Like_Review <- gsub("·","",Like_Dislike$Like_Review)
     Like <- as.vector(Like_Dislike$Like_Review)
    
      ## Negative reviews
     Like_Dislike$Dislike <- as.data.frame(ifelse(grepl("Disliked",Like_Dislike$.),1,0))
     Like_Dislike$Dislike_Review <- ifelse(Like_Dislike$Dislike=="1", gsub(".*Disliked","",Like_Dislike$.), NA)
     Like_Dislike$Dislike_Review <- gsub("·","",Like_Dislike$Dislike_Review)
     Like_Dislike$Dislike_Review <- gsub("\n.*","",Like_Dislike$Dislike_Review)
     Dislike <- as.vector(Like_Dislike$Dislike_Review)
    
     ## Combine all detail
     Detail <- tibble(Reviewer,Country,Room_type,Number_night,Stay_Date,Score,Review_date,Review,Review_language,Like,Dislike)
     Detail$Hotel_Name <- hotel$Hotel_name[i]
     
     Hotel_review_df = rbind.fill(Hotel_review_df, Detail)

     
# Next Page
     down <- remDr$findElement("css selector","[class='sliding-panel-widget-content review_list_block one_col']")
     down$sendKeysToElement(list(key="end"))
     Next <- remDr$findElement("css selector","[class='bk-icon -iconset-navarrow_right bui-pagination__icon']")
     Next$clickElement()
     Sys.sleep(1)
     
     print(j)}

     print(i)}
   
 remDr$close()
 rD$server$stop()
Step6: Check data structure
# Check the structure of the dataset
glimpse(Hotel_review_df)
# Normalizing data by creating id for each review
Hotel_review_df$review_id <- 1:nrow(Hotel_review_df)
# as numeric Score
Hotel_review_df$Score <- as.numeric(Hotel_review_df$Score)
# Remove "night" and change to factor
Hotel_review_df$Number_night <- gsub(" night","",Hotel_review_df$Number_night)
Hotel_review_df$Number_night <- as.factor(Hotel_review_df$Number_night)
# Change "xu" to "en"
Hotel_review_df$Review_language <- gsub("xu","en",Hotel_review_df$Review_language)
# Check if all rows are different
nrow(Hotel_review_df)== nrow(distinct(Hotel_review_df))

# Check if all rows contain text in Review
length(Hotel_review_df$Review)==length(!is.na(Hotel_review_df$Review))
a <- Hotel_review_df %>% dplyr::filter(Review=="")
#There are 97 rows that contain NA values 

# Remove these rows as they also contain NA values in column Like and Dislike
Newreview <- Hotel_review_df %>% dplyr::filter(!Review=="")

# Check if all rows contain value in overall
length(Newreview$Score)==length(!is.na(Newreview$Score))

# Change format Stay_Date
Newreview$Stay_Date <- as.Date(Newreview$Stay_Date,format = "%m %Y")
As the length of summary review is short, positive and negative reviews are used to analyse instead.
# Combine Like and Dislike
Newreview$Reviewfinal <- paste(Newreview$Like,Newreview$Dislike,sep = ".")
Newreview$Reviewfinal <- gsub("[.][.]",".",Newreview$Reviewfinal)

# Remove "NA", "NA.", "N/A", "N/A.", "Nothing", "Nothing.", "None", "None."
Newreview$Reviewfinal <- gsub(c("NA|NA.|N/A|N/A.|.NA|Nothing|Nothing.|None|None."),"",Newreview$Reviewfinal,ignore.case=TRUE)

# Replace NA in Reviewfinal with sentences in column "review" instead
Newreview$Reviewfinal <- ifelse(is.na(Newreview$Reviewfinal),Newreview$Review,Newreview$Reviewfinal)
Newreview$Reviewfinal <- ifelse(Newreview$Reviewfinal=="",Newreview$Review,Newreview$Reviewfinal)

# Compute the length Reviewfinal
Newreview$review_length_chars <- str_length(Newreview$Reviewfinal)
Newreview$Reviewfinal <- ifelse(Newreview$review_length_chars=="1",Newreview$Review,Newreview$Reviewfinal)
Newreview$review_length_chars <- nchar(Newreview$Reviewfinal)

Newreview_hist <- hist(Newreview$review_length_chars, breaks = 100, main = "Review Length(Original)", xlab = "Review Length")
Newreview$Score_rounded <- Newreview$Score
Newreview$Score_rounded <- round(Newreview$Score_rounded)
# Use ggplot() and geom_jitter() to plot the jitter plot of review length against over all rating
ggplot(Newreview,aes(x=Score_rounded,y=review_length_chars,color=as.factor(Score_rounded))) + geom_jitter() + labs(x="Score (rounded)",y="Review Length(Words)", fill="Type") + theme_classic() + scale_fill_brewer(palette = "Spectral") + theme(panel.background = element_rect(colour='black'), plot.title = element_text(hjust = 0.5), legend.position = "None", strip.background =element_rect(fill="cornsilk")) + ggtitle("Jitter Plot of Review Length against overall rating (rounded)"))

# Use boxplot() and filter() to remove outliers from the dataset 
Newreview <- Newreview %>% filter(review_length_chars<min(boxplot(Newreview$review_length_chars, plot=FALSE)$out))

# Remove the abnormal reviews
Newreview <- Newreview %>%
  filter(review_length_chars > 12) %>%
  filter(review_length_chars < 250)
# Plot the data again
Newreview_hist2 <- hist(Newreview$review_length_chars, breaks = 100, main = "Review Length(Adjusted)", xlab = "Review Length")

# Use ggplot() and geom_jitter() to plot the jitter plot of review length against over all rating again
ggplot(Newreview,aes(x=Score_rounded,y=review_length_chars,color=as.factor(Score_rounded))) + geom_jitter() + labs(x="Score (rounded)",y="Review Length(Words)", fill="Type") + theme_classic() + scale_fill_brewer(palette = "Spectral") + theme(panel.background = element_rect(colour='black'), plot.title = element_text(hjust = 0.5), legend.position = "None", strip.background =element_rect(fill="cornsilk")) + ggtitle("Jitter Plot of Review Length against overall rating (rounded) (Adjusted)")
# Check the number of reviews for each hotel
numreview_hotel <- Newreview %>% group_by(Hotel_Name) %>% dplyr::summarise(total = n())
# Remove any reviews that are not English
Newreview <- Newreview %>% filter(Review_language=="en")
Newreview <- Newreview %>% filter(!review_id=="21451")
Newreview <- Newreview %>% filter(!review_id=="82635")
Newreview <- Newreview %>% filter(!review_id=="86413")
Newreview <- Newreview %>% filter(!review_id=="20681")
Newreview <- Newreview %>% filter(!review_id=="111083")

# Recheck language by using function
Newreview <- Newreview %>% 
  mutate(review_language = cld2::detect_language(Reviewfinal))

# Keep the english reviews
Newreview <- Newreview %>% 
  filter(review_language == "en")
Step7: Data Cleaning
# Remove URL explain reason
searchword <- str_detect(Newreview$Reviewfinal, "\\.com")

# Check the URLs
str_view_all(Newreview$Reviewfinal[searchword], "\\.com") 
Newreview$Reviewfinal <- gsub("booking.com","",Newreview$Reviewfinal, ignore.case=TRUE)
# Remove excess whitespace
Newreview$Reviewfinal = str_squish(Newreview$Reviewfinal)

# Convert contractions back to their base words 
Newreview$Reviewfinal = replace_contraction(Newreview$Reviewfinal)
searchword <- str_detect(Newreview$Reviewfinal,"'")
str_view_all(Newreview$Reviewfinal[searchword],"'",match=TRUE)

# Earl's Court --> no need to fix
# There's still a few left which needs to be manually replaced, i.e. haven't to have not, lot's to lots.
Newreview$Reviewfinal <- str_replace_all(Newreview$Reviewfinal, "haven't", "have not")
Newreview$Reviewfinal <- str_replace_all(Newreview$Reviewfinal, "lot's", "lots")
Newreview$Reviewfinal <- str_replace_all(Newreview$Reviewfinal, "re'", "re")
Newreview$Reviewfinal <- str_replace_all(Newreview$Reviewfinal, "hadn't", "had not")
 
# Remove excess whitespace
Newreview$Reviewfinal <- stripWhitespace(Newreview$Reviewfinal)
# Replace symbol
Newreview$Reviewfinal <- replace_symbol(Newreview$Reviewfinal)

# Replace number
Newreview$Reviewfinal <- replace_number(Newreview$Reviewfinal)

# Use str_replace_all and regex to remove words with numbers
Newreview$Reviewfinal <- str_replace_all(Newreview$Reviewfinal,"\\w*[0-9]+\\w*\\s*", " 
")

# Remove reviews within brackets()
Newreview$Reviewfinal <- bracketX(Newreview$Reviewfinal)

# Remove word elongation
Newreview$Reviewfinal <- replace_word_elongation(Newreview$Reviewfinal, impart.meaning = TRUE)

# Replace abbreviation
Newreview$Reviewfinal <-  replace_abbreviation(Newreview$Reviewfinal)

# Replace emoticons
Newreview$Reviewfinal <- replace_emoticon(Newreview$Reviewfinal)

# Replace emojis 
Newreview$Reviewfinal <- replace_emoji(Newreview$Reviewfinal, emoji_dt = lexicon::hash_emojis)

# Replace internet slangs with long forms
Newreview$Reviewfinal <- replace_internet_slang(Newreview$Reviewfinal, slang = paste0(
"\\b",lexicon::hash_internet_slang[[1]], "\\b"), replacement = lexicon::hash_internet_slang[[2]], ignore.case = TRUE)

# Replace non-ASCII characters with space
Newreview$Reviewfinal <- replace_non_ascii(Newreview$Reviewfinal, replacement = "", remove.nonconverted = TRUE)

# Remove words having less than three characters
Newreview$Reviewfinal <- gsub('\\b\\w{1,2}\\b','',Newreview$Reviewfinal)
saveRDS(Newreview,file="Newreview_afterreplace.rds")
#Newreview <- readRDS("Newreview_afterreplace.rds")
Separate each sentence in reviewText into different rows
review <- Newreview %>% dplyr::select(review_id, Reviewfinal)
sentence <- review %>% 
  mutate(reviewTextnew = strsplit(Reviewfinal, "[.]")) %>% 
  unnest(cols = c(reviewTextnew)) %>% 
  dplyr::select(-c("Reviewfinal"))

Newreview_sentence <- Newreview %>% full_join(sentence, by="review_id")
Newreview_sentence$reviewTextnew <- str_squish(Newreview_sentence$reviewTextnew)
Tokenization
text_token <- Newreview_sentence  %>% unnest_tokens(word,reviewTextnew)
# Check misspelled words
unique <- text_token %>%
  dplyr::select(word) %>% 
  distinct(word)
   
unique_nostopwords <- unique %>% 
  anti_join(stop_words) 
# unique words without stop words

# Remove all punctuation marks
unique_nostopwords$word <- removePunctuation(unique_nostopwords$word)
unique_nostopwords <- distinct(unique_nostopwords)
    
#Find misspelled words
collapse_word <- unique_nostopwords %>% 
  paste(collapse=" ")  
      
misspelled_words <- qdap::check_spelling(collapse_word,assume.first.correct=FALSE)
     
#Replace some misspelled words with correct ones
wrong <- c("couldn"," mins "," didn ","boyfriends"," wasn "," tech ","wi ","couldnt","wasnt","wintercould","aswell","weren","reasoly","reasole"," bkf "," amd ","againalso","kariwho","locationgood"," ive ","unhygienic"," iam ","breakfastwould"," nw "," hr ","londons","über","buffett","exceptio","professio","unfortuely","comftable","exceptioly","loveley","lLocation","hallmuseumembassys","resuarant","decór","occasio","recieved","museums","resuarant","helpfull","traditio","mediately","tranport","occasioly","décor","persoly","perso","proffisio","essentials","essentially","facilites","undergroun","resturants","comfotble","wouldn","breakfastgood","restuarant","doesn","proffessio","hadn","noisey"," aren ","qaulity"," esp ","definitly","hospitaliting","couldve","ammenities","confortable","shouldn","dont","definitelly","réception","différents","accomodatingl","helpfullocated"," grea ","wonderfull","faviourte"," ve ","extremly","restauants","emails","duvets","buisness","dispite","bedroo","Kennsington","lication","reseption","staffvery","neccessary","apperance","smoming","ciggarett","cigaret","cigarettes","hottel","disrput","exacellent"," staf ","acessible","brakfest"," bfast "," bf ","ktichen","vovid","unfortunetly"," acess ","extraordiily","recesption","kicten","resteruant"," aug "," dr ","nothinh","unhapoy","maerial","exclellent","accessable","cetral","surroundins","cigarettesalthough","thelandmarklondon","parkingexcellent","donsnt","cleanlinessfriendly","stafflovely","goodbreakfast","ambiencewonderful","nightsbut","beakfast","drsign","clealiness","enviornment","disapointing","petfect","disappointingdisappointing","excellentexcellent","wonderfulwonderful","goodvery"," romms "," wen ","samll","complimantary","manager charles"," charles ","paied"," tripadv "," didnt ","cealing","allover","ridicullous","insuffisant"," dint "," hve ","delox"," luic "," hea ","maament","althougth"," wast ","adverised","no comeback"," dosnt "," cmon ","adequatel","dingey","noizy"," defely "," defenatly ","irresponsable","unnaceptable","uneptable","extreemly"," dnt ","caization","hôtel","bookedadon","airconditionin","aboslutely"," couse ","cockroache","calcification","beyound","attendents","all's","aftican","aircraftcabine","borken","repeartedly"," deco ","availabld","Barth room"," awayl ","cafetiere","brusk","bessheets","youve","accetable"," air cond "," poole "," cha ","kir royale"," shin "," wich "," awre ","vlaidate","aersary","aduequte","acomodate","altho","towells","camere","airconditon","aweful","awaketil","addaptors"," atay "," satay ","arear's","arears"," bailys ","areangement","beenbetter","actully","anxiouse","caes"," preauth "," thy "," barey "," acs "," anty "," matras "," cond "," wasn‘t "," wasnt "," utractive "," didnt ","corridiors"," clearned ","discrimie"," alatm ","agrred"," afor "," aelderly "," wast "," aak "," lovly "," ereince ") 

correct <- c("could not"," minutes "," did not ","boyfriend"," was not "," technology ","wifi","could not","was not","winter could","as well","were not","reasonably","reasonable"," breakfast "," and ","again also","kari who","location good"," I have ","not hygeine"," I am ","breakfast would"," north-west "," hour ","london","uber","buffet","exception","profession","unfortunate","comfortable","exceptional","lovely","location","hall museum embassys","restaurant","decor","occasional","receive","museum","restaurant","helpful","tradition","immediately","transport","occasional","decor","personal","personal","professional","essential","essential","facilities","underground","restaurant","comfortable","would not","breakfast good","restaurant","does not","prefession","had not","noisy"," are not ","quality"," especially ","definitely","hospitalizing","could have","amenities","comfortable","should not","do not","definitely","reception","different","accomdating","helpful located"," great ","wonderful","favourite"," have ","extremely","restaurant","email","duvet","business","despite","bedroom","Kensington","location","reception","staff very","necessary","appearance","smoking","cigarette","cigarette","cigarette","hotel","disrupt","excellent"," staff ","accessible","breakfast"," breakfast "," breakfast ","kitchen", "covid","unfortunately"," access ","extraordinary","reception","kitchen","restaurant"," August "," driver ","nothing","not happy","material","excellent","accessible","central","surroundings","cigarette although","the landmark london","parking excellent","does not","cleanliness friendly","staff lovely","good breakfast","ambience wonderful","nights but","breakfast","design","cleanliness","environment","disappointing","perfect","disappointing","excellent","wonderful","good very"," rooms ", "when ","small","complimentary","manager"," manager ","paid"," tripadvisor"," did not ","ceiling","all over","ridiculous","insufficient"," did not "," have ","deluxe"," lunatic"," head ","management","although"," what ","advertised","nocomeback"," does not "," come on ","adequate","dingy","noisy"," definitely "," definitely ","irresponsible","unacceptable","unacceptable","extremely"," do not ","causation","hotel","booked","airconditioning","absolutely"," course ","cockroach","clarification","beyond","attendant","all","african","aircraftcabin","broken","repeatedly"," decoration ","available","bathroom"," away ","cafeteria","brush","bedsheets","you have","acceptable"," airconditioner "," pool "," call ","kirroyale"," chin "," which "," aware ","validate","anniversary","adequate","accommodate","although","towel","camera","airconditioner","awful","awake until","adaptors"," stay "," stay ","area","area"," baileys ","arrangement","been better","actually","anxious","cases"," preauthorisation "," they "," barley "," airconditioning "," anything "," mattress "," conditioner "," was not "," was not "," unattractive "," did not ","corridor"," cleaned ","discriminate"," alarm ","agree"," for "," elderly "," was not "," ask "," lovely "," experience ")


Newreview_sentence$reviewTextnew <- tolower(Newreview_sentence$reviewTextnew)
Newreview_sentence$reviewTextnew <- qdap::replace_abbreviation(Newreview_sentence$reviewTextnew, wrong, correct, ignore.case = TRUE)

# Replace consecutive duplicate words with a single word 
Newreview_sentence$reviewTextnew  <- str_replace(Newreview_sentence$reviewTextnew ,"\\b(\\w+)(\\s+\\1\\b)", '\\1')
  
Newreview_sentence$consecutive_words <-  ifelse(grepl("\\b(\\w+)(\\s+\\1\\b)", Newreview_sentence$reviewTextnew), 1,0)

#Still have consecutive duplicate words, remove them (try to do 50 times and check a result)
for (i in 1:4){
  print(i)
  if(sum(Newreview_sentence$consecutive_words)>0){
    Newreview_sentence$reviewTextnew  <- str_replace(Newreview_sentence$reviewTextnew, "\\b(\\w+)(\\s+\\1\\b)", '\\1')}
  else {Newreview_sentence$reviewTextnew <- Newreview_sentence$reviewTextnew}
  }
    
Newreview_sentence$consecutive_words <-  ifelse(grepl("\\b(\\w+)(\\s+\\1\\b)", Newreview_sentence$reviewTextnew), 1,0) 
check <- Newreview_sentence %>% filter(consecutive_words=="1")
saveRDS(Newreview_sentence, file="Newreview_sentence1.rds")
#Newreview_sentence <- readRDS("Newreview_sentence1.rds")
Final data (word, sentence, review)
#Cleaned review
TextFinal <- Newreview_sentence %>% dplyr::select(review_id,reviewTextnew)
Hotel_TextFinal <- TextFinal %>% group_by(review_id) %>% 
    dplyr::summarise(reviewTextFinal=str_c(reviewTextnew, collapse = ". ")) %>% ungroup()

Newreview <- left_join(Newreview,Hotel_TextFinal)

# Word
word <- unnest_tokens(Newreview,word,reviewTextFinal)

# Sentence
sentence <- Newreview_sentence %>% dplyr::select(-c("Reviewfinal"))
sentence$reviewTextnew <- removePunctuation(sentence$reviewTextnew)

#Cleaned review
TextFinal <- Newreview_sentence %>% dplyr::select(review_id,reviewTextnew)
Hotel_TextFinal <- TextFinal %>% group_by(review_id) %>% 
    dplyr::summarise(reviewTextFinal=str_c(reviewTextnew, collapse = ". ")) %>% ungroup()

Newreview <- left_join(Newreview,Hotel_TextFinal)



Part A: Construction of Corpus – Creating a dataset 

Tokenization
wordA <- word
plot(freq_terms(wordA$word), plot=FALSE) + labs(title = "Before Lemmatization")
Lemmatization
wordA$word <- lemmatize_words(wordA$word)
Remove stopwords
# Remove stopwords in dic given
word_clean <- wordA %>% anti_join(stop_words)
# Compare words frequency before and after removing stop words to see whether further adjustments are needed

 p1 <- plot(freq_terms(wordA$word)) + labs(title = "Before Stop Words Removed")
 p2 <- plot(freq_terms(word_clean$word), plot = FALSE) + labs(title = "After Stop Words Removed")
 print(p1+p2)
 

# Create own stopwords dictionary
my_words <- c("hotel","stay","check","london", ignore.case=TRUE)

my_dic <- tibble(word = my_words,lexicon = "my,his,her" )
my_stopwords <- stop_words %>%  bind_rows(my_dic)

# Re-removing own stopwords
word_clean <- word_clean %>% anti_join(my_stopwords)

plot(freq_terms(word_clean$word), plot = FALSE) + labs(title = "After Stop Words Removed")
Dominant Word
TF-IDF And Word Count
# Token length
word_clean$token_length <- nchar(word_clean$word)
word_clean %>% group_by(token_length) %>% dplyr::summarise(total =n())
word_clean <- word_clean %>% filter(token_length>2)
word_token_filter <- word_clean %>% group_by(token_length) %>% dplyr::summarise(total =n()) %>% arrange(desc(token_length))
word_clean <- word_clean %>% filter(token_length <= 16)

# Word counting
word_count <- word_clean %>% dplyr::count(word,Score_rounded,review_id,sort=TRUE) %>% ungroup() %>% dplyr::rename(count=n)

hotel_tf_idf <- word_count %>%
    bind_tf_idf(word,Score_rounded,count) %>%
    dplyr::filter(tf_idf>0) 
# Show graph
hotel_tf_idf  %>%
  group_by(Score_rounded) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder(word, tf_idf), fill = Score_rounded)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Score_rounded, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = "word", title = "TF-IDF of words by Ratings")
# Create DTM
hotel_dtm_tf_idf <- hotel_tf_idf %>% 
  cast_dtm(Score_rounded, word, tf_idf) %>% 
  removeSparseTerms(0.99)

hotel_dtm <- colSums(as.matrix(hotel_dtm_tf_idf)) %>% as.data.frame()

hotel_dtm  <- hotel_dtm %>% mutate(document = row.names(hotel_dtm))

hotel_dtm$document <- removeNumbers(hotel_dtm$document)

colnames(hotel_dtm)=c("frequency","document")

# Word cloud using tfidf values
set.seed(5)
wordcloud(hotel_dtm$document, hotel_dtm$frequency,   
          scale = c(4, 0.5),     
          0,                    
          max.words = 200,      
          colors = brewer.pal(8, "Dark2"))  
title("TF-IDF-ReviewText", col.main = "grey14") 
# Plot out the count of words by Ratings
set.seed(5)
word_count %>%
  group_by(Score_rounded) %>%
  arrange(desc(count)) %>%
  slice(1:10)  %>% 
  ungroup() %>% 
  mutate(word1 = reorder_within(word, count, Score_rounded)) %>%
  ggplot(aes(word1, count, fill = Score_rounded)) +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Score_rounded, ncol = 3, scales = "free") +
  labs(x = NULL, y = "Count", title = "Count of words by Ratings")

# Word cloud for hotel in London
set.seed(5)
wordcloud(word_count$word,word_count$count,colors = brewer.pal(8, "Dark2"))
title("Count-Review", col.main = "grey14")
As it can represent no more than 8 categories, score (1&2),(3&4),(5&6),(7&8) and (9&10) are grouped together.
# Comparison cloud
col <- rev(RColorBrewer::brewer.pal(8, "Dark2"))[4:8]

# Group scores together
group <- word_count
group$Score_group <- ifelse(group$Score_rounded %in% c("1","2"),"1&2",
                     ifelse(group$Score_rounded %in% c("3","4"),"3&4", 
                     ifelse(group$Score_rounded %in% c("5","6"),"5&6",
                     ifelse(group$Score_rounded %in% c("7","8"),"7&8","9&10"))))
group_A <- group %>% dplyr::group_by(word,Score_group) %>% dplyr::summarise(count=sum(count))

# Cast into document feature matrix
hotel_dfm_count <- cast_dfm(group_A, document = Score_group, term = word, value = count)
set.seed(5)
textplot_wordcloud(hotel_dfm_count, comparison = TRUE, random_order = FALSE, 
                  color = col,
                  labelsize = 1.5,
                  max_words = 500,
                  min_size = 1, ) 
#title("Comparison Wordcloud", col.main = "grey14") 
Word Combination
Bigram
bigram <-  word_clean %>% 
  group_by(review_id) %>% 
  dplyr::summarise(reviewText = paste(word, collapse = " "), 
            totalwords = n()) %>% 
  left_join(word_clean[,c("review_id", "Score_rounded")]) %>%
  filter(totalwords >= 2) %>% 
  unnest_tokens(word,reviewText,token="ngrams", n=2) %>% 
  na.omit() %>% 
  separate(word, c("word1","word2"), sep = " ") %>% 
  # Remove rows with duplicate words
  filter(word1 != word2) %>% 
  mutate(word = paste(word1, word2, sep = " ")) %>% 
  select(-c(word1, word2))

bigram_count <- bigram %>%  
  dplyr::count(word, Score_rounded, sort = TRUE) %>% ungroup()
names(bigram_count)[3] <- "count"

# Plot out the count of words by Ratings
set.seed(5)
bigram_count %>%
  group_by(Score_rounded) %>%
  top_n(10) %>% 
  ungroup() %>% 
  mutate(bigram2 = reorder_within(word, count, Score_rounded)) %>% 
  ggplot(aes(bigram2, count, fill = Score_rounded)) +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Score_rounded, ncol = 3, scales = "free") +
  labs(x = "Count", y = NULL, title = "Count of bigram by Ratings") 

# Word cloud for hotel in London
set.seed(5)
wordcloud(bigram_count$word,bigram_count$count,max.words = 100,colors = brewer.pal(8, "Dark2"))
title("bigram count - Reviews", col.main = "grey14") 

# Comparison cloud (remove same words in other documents)
col <- rev(RColorBrewer::brewer.pal(8, "Dark2"))[4:8] 

# Group scores together
bigram_group <- bigram_count
bigram_group$Score_group <- ifelse(bigram_group$Score_rounded %in% c("1","2"),"1&2",
                     ifelse(bigram_group$Score_rounded %in% c("3","4"),"3&4", 
                     ifelse(bigram_group$Score_rounded %in% c("5","6"),"5&6",
                     ifelse(bigram_group$Score_rounded %in% c("7","8"),"7&8","9&10"))))
bigram_group_A <- bigram_group %>% dplyr::group_by(word,Score_group) %>% dplyr::summarise(count=sum(count))

bigram_corpus <- cast_dfm(bigram_group, document = Score_group, term = word, value = count)
set.seed(5)
textplot_wordcloud(bigram_corpus, comparison = TRUE, random_order = FALSE, 
                  color = col,
                  labelsize = 1.5,
                  max_words = 500,
                  min_size = 1)
# title("Bigram Wordcloud", col.main = "grey14")
# Each score - tfidf
bigram_tfidf <- bigram_count %>% 
    bind_tf_idf(word,Score_rounded,count) %>%
    filter(tf_idf>0)
# Grouped score - tfidf
bigram_tfidf_group <- bigram_group_A %>% 
    bind_tf_idf(word,Score_group,count) %>%
    filter(tf_idf>0)

# Plot out the tf-idf of words by Ratings
bigram_tfidf %>%
  group_by(Score_rounded) %>%
  arrange(desc(tf-idf)) %>% 
  slice(1:10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder(word, tf_idf), fill = Score_rounded)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Score_rounded, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# Word cloud for tfidf values
bigram_dtm_tfidf <- bigram_tfidf_group %>% 
  cast_dtm(Score_group, word, tf_idf) %>% #use which one to be the document?
  removeSparseTerms(0.99)
bigram_dtm_tfidf <- colSums(as.matrix(bigram_dtm_tfidf)) %>% 
  as.data.frame() 
bigram_dtm_tfidf <- bigram_dtm_tfidf %>% 
  mutate(document = row.names(bigram_dtm_tfidf))
bigram_dtm_tfidf$document <- removeNumbers(bigram_dtm_tfidf$document)
colnames(bigram_dtm_tfidf)=c("frequency","document")

wordcloud(bigram_dtm_tfidf$document, bigram_dtm_tfidf$frequency,   
          scale = c(4, 0.5),     
          0,                    
          max.words = 300,      
          colors = brewer.pal(8, "Dark2"),
          main="Title")   
# title("bigram tfidf - Hotel Reviews", col.main = "grey14")

# Word network - visualize word combinations
top_bigram_tfidf <- bigram_tfidf_group %>%
 group_by(Score_group) %>%
 arrange(desc(count)) %>%
 slice(1:20) %>%
 ungroup()
# Top 20 word combinations of each grouped score
bigram.network.plots <- list()

 # Top 10 bigram of each grouped score
## Score_group=1&2
   field_name <- "Grouped score 1&2"
 tmp_tfidf <- top_bigram_tfidf %>% 
     filter(Score_group=="1&2") 
   bigram.network.plots[[field_name]] <- tmp_tfidf %>% 
   separate(word, c("node1", "node2"), sep = " ") %>% 
   # Decouple the bigram into single words
   graph_from_data_frame() %>%
   ggraph(layout = "kk") +
   geom_edge_link(aes(start_cap = label_rect(node1.name),
   end_cap = label_rect(node2.name),
   edge_alpha = tf_idf),
   color = viridis(10)[3],
   edge_width = 1,
   arrow = arrow(length = unit(2, 'mm')))+
   geom_node_text(aes(label = name))+
   scale_edge_alpha(range=c(0.5,1))+
   labs(title = field_name) +
   theme_bw() +
   labs(title=field_name)
   
   ## Score_group=3&4
   field_name <- "Grouped score 3&4"
 tmp_tfidf <- top_bigram_tfidf %>% 
     filter(Score_group=="3&4") 
   bigram.network.plots[[field_name]] <- tmp_tfidf %>% 
   separate(word, c("node1", "node2"), sep = " ") %>% 
   # Decouple the bigram into single words
   graph_from_data_frame() %>%
   ggraph(layout = "kk") +
   geom_edge_link(aes(start_cap = label_rect(node1.name),
   end_cap = label_rect(node2.name),
   edge_alpha = tf_idf),
   color = viridis(10)[3],
   edge_width = 1,
   arrow = arrow(length = unit(2, 'mm')))+
   geom_node_text(aes(label = name))+
   scale_edge_alpha(range=c(0.5,1))+
   labs(title = field_name) +
   theme_bw() +
   labs(title=field_name)
   
    ## Score_group=5&6
   field_name <- "Grouped score 5&6"
 tmp_tfidf <- top_bigram_tfidf %>% 
     filter(Score_group=="5&6") 
   bigram.network.plots[[field_name]] <- tmp_tfidf %>% 
   separate(word, c("node1", "node2"), sep = " ") %>% 
   # Decouple the bigram into single words
   graph_from_data_frame() %>%
   ggraph(layout = "kk") +
   geom_edge_link(aes(start_cap = label_rect(node1.name),
   end_cap = label_rect(node2.name),
   edge_alpha = tf_idf),
   color = viridis(10)[3],
   edge_width = 1,
   arrow = arrow(length = unit(2, 'mm')))+
   geom_node_text(aes(label = name))+
   scale_edge_alpha(range=c(0.5,1))+
   labs(title = field_name) +
   theme_bw() +
   labs(title=field_name)
   
    ## Score_group=7&8
    field_name <- "Grouped score 7&8"
 tmp_tfidf <- top_bigram_tfidf %>% 
     filter(Score_group=="7&8") 
   bigram.network.plots[[field_name]] <- tmp_tfidf %>% 
   separate(word, c("node1", "node2"), sep = " ") %>% 
   # Decouple the bigram into single words
   graph_from_data_frame() %>%
   ggraph(layout = "kk") +
   geom_edge_link(aes(start_cap = label_rect(node1.name),
   end_cap = label_rect(node2.name),
   edge_alpha = tf_idf),
   color = viridis(10)[3],
   edge_width = 1,
   arrow = arrow(length = unit(2, 'mm')))+
   geom_node_text(aes(label = name))+
   scale_edge_alpha(range=c(0.5,1))+
   labs(title = field_name) +
   theme_bw() +
   labs(title=field_name)
   
    ## Score_group=9&10
   field_name <- "Grouped score 9&10"
 tmp_tfidf <- top_bigram_tfidf %>% 
     filter(Score_group=="9&10") 
   bigram.network.plots[[field_name]] <- tmp_tfidf %>% 
   separate(word, c("node1", "node2"), sep = " ") %>% 
   # Decouple the bigram into single words
   graph_from_data_frame() %>%
   ggraph(layout = "kk") +
   geom_edge_link(aes(start_cap = label_rect(node1.name),
   end_cap = label_rect(node2.name),
   edge_alpha = tf_idf),
   color = viridis(10)[3],
   edge_width = 1,
   arrow = arrow(length = unit(2, 'mm')))+
   geom_node_text(aes(label = name))+
   scale_edge_alpha(range=c(0.5,1))+
   labs(title = field_name) +
   theme_bw() +
   labs(title=field_name)

# Combine all the network plots in one.
gridExtra::grid.arrange(grobs = bigram.network.plots, 
                        ncol = 3,
                        top = "Top 20 word combinations of each grouped rating")
Trigram
# Tokenise to bigram
trigram <-  word_clean %>% 
  group_by(review_id) %>% 
  dplyr::summarise(reviewText = paste(word, collapse = " "), 
            totalwords = n()) %>% 
  left_join(word_clean[,c("review_id", "Score_rounded")]) %>%
  filter(totalwords >= 3) %>% 
  unnest_tokens(word,reviewText,token="ngrams", n=3) %>% 
  na.omit() %>% 
  separate(word, c("word1","word2","word3"), sep = " ") %>% 
  # Remove rows with duplicate words
   filter(word1 != word2 & word2 != word3 & word1 != word3) %>% 
  mutate(word = paste(word1, word2, word3, sep = " ")) %>% 
  select(-c(word1, word2, word3))


# Most common used 3-word combination before text cleaning
trigram_count <- trigram %>%  
  dplyr::count(word, Score_rounded, sort = TRUE) %>% ungroup() 
names(trigram_count)[3] <- "count"
# Plot out the count of words by Ratings
set.seed(5)
trigram_count %>%
  group_by(Score_rounded) %>%
  top_n(10) %>% 
  ungroup() %>% 
  mutate(trigram = reorder_within(word, count, Score_rounded)) %>% 
  ggplot(aes(trigram, count, fill = Score_rounded)) +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Score_rounded, ncol = 3, scales = "free") +
  labs(x = "Count", y = NULL, title = "Count of trigram by Ratings") 

# Word cloud for hotel in London
set.seed(5)
wordcloud(trigram_count$word,
          trigram_count$count,
          max.words = 150,
          colors = brewer.pal(8, "Dark2"))
# title("Trigram count - Hotel Reviews", col.main = "grey14")

# Comparison cloud (remove same words in other documents)
col <- rev(RColorBrewer::brewer.pal(8, "Dark2"))[4:8] 

# Group scores together
trigram_group <- trigram_count
trigram_group$Score_group <- ifelse(trigram_group$Score_rounded %in% c("1","2"),"1&2",
                     ifelse(trigram_group$Score_rounded %in% c("3","4"),"3&4", 
                     ifelse(trigram_group$Score_rounded %in% c("5","6"),"5&6",
                     ifelse(trigram_group$Score_rounded %in%c("7","8"),"7&8","9&10"))))
trigram_group_A <- trigram_group %>% dplyr::group_by(word,Score_group) %>% dplyr::summarise(count=sum(count))

trigram_corpus <- cast_dfm(trigram_group, document = Score_group, term = word, value = count)
set.seed(5)
textplot_wordcloud(trigram_corpus, comparison = TRUE, random_order = FALSE, 
                  color = col,
                  labelsize = 1.5,
                  max_words = 500)
# title("Trigram Wordcloud", col.main = "grey14")
# Each score - tfidf
trigram_tfidf <- trigram_count %>% 
    bind_tf_idf(word,Score_rounded,count) %>%
    filter(tf_idf>0)
# Grouped score - tfidf
trigram_tfidf_group <- trigram_group_A %>% 
    bind_tf_idf(word,Score_group,count) %>%
    filter(tf_idf>0)

# Plot out the tf-idf of words by total
trigram_tfidf %>%
  group_by(Score_rounded) %>%
  arrange(desc(tf-idf)) %>% 
  slice(1:10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder(word, tf_idf), fill = Score_rounded)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Score_rounded, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# Word cloud for tfidf values
trigram_dtm_tfidf <- trigram_tfidf_group %>% 
  cast_dtm(Score_group, word, tf_idf) %>% #use which one to be the document?
  removeSparseTerms(0.99)
trigram_dtm_tfidf <- colSums(as.matrix(trigram_dtm_tfidf)) %>% 
  as.data.frame() 
trigram_dtm_tfidf <- trigram_dtm_tfidf %>% 
  mutate(document = row.names(trigram_dtm_tfidf))
trigram_dtm_tfidf$document <- removeNumbers(trigram_dtm_tfidf$document)
colnames(trigram_dtm_tfidf)=c("frequency","document")

wordcloud(trigram_dtm_tfidf$document, trigram_dtm_tfidf$frequency,   
          scale = c(4, 0.5),     
          0,                    
          max.words = 300,      
          colors = brewer.pal(8, "Dark2"),
          main="Title")   
# title("Trigram tfidf - Hotel Reviews", col.main = "grey14")

# Word network - visualize word combinations
top_trigram_tfidf <- trigram_tfidf %>%
 group_by(Score_rounded) %>%
 arrange(desc(count)) %>%
 slice(1:20) %>%
 ungroup()






Part B: Text features and Sentiment association with Target Outcomes

# Import data from pre-processing part
sentence 
NewreviewB <- Newreview
hotel 
Find the features related to the rating score
Feature extraction
# Take a sample
set.seed(100)
NewreviewB_S <- stratified(NewreviewB, "Score", 0.1)

# Combining data
# Merge NewreviewB with hotel(metadata)
hotel$Hotel_Name <- hotel$Hotel_name
NewreviewB_f <- NewreviewB_S %>% left_join(hotel) 
Score.prop<- as.data.frame(prop.table(table(NewreviewB_f$Score)))
head(NewreviewB_f)

# Plot the distribution of the rating score before balancing
sp <- ggplot(data=Score.prop, aes(x=Var1, y=Freq)) +
 geom_bar(stat="identity", fill="steelblue")+
 geom_text(aes(label=round(Freq, 2)), vjust=1.6, color="black", size=3.5)+
 labs(title="The distribution of the rating score", subtitle = "Before Balancing", x 
= "Rating", y = "proportion")

# Balancing the data set
NewreviewB_f %>% group_by(Score) %>% dplyr::summarise(n())
library(groupdata2)
B_rating <- balance(NewreviewB_f, size = "mean", cat_col = "Score") 
Score.prop.balanced <- as.data.frame(prop.table(table(B_rating$Score)))

# Plot the distribution of the rating score before balancing
sp_b <-ggplot(data=Score.prop.balanced, aes(x=Var1, y=Freq)) +
 geom_bar(stat="identity", fill="steelblue")+
 geom_text(aes(label=round(Freq, 2)), vjust=1.6, color="black", size=3.5)+
 labs(title="The distribution of the rating score", subtitle = "After Balancing", x 
= "Rating", y = "proportion")
# Plot the distribution of the rating score after and before balancing
ggarrange(sp, sp_b, widths = c(2,2))
Gender
B_rating$Reviewer <- B_rating$Reviewer  %>%
 stripWhitespace() %>%
 removeNumbers() %>%
 removePunctuation() %>%
 bracketX() %>%
 tolower() 

## extracting the reviewer names in order to match the gender from the SSA database
B_rating$firstname <- stringr::word(B_rating$Reviewer,1)
gender::gender(B_rating$firstname) %>% select(firstname = name,gender) %>% unique(.)-> results_gender

# add gender col
B_rating <- B_rating %>% mutate(gender = "") %>% relocate(gender, .after = Reviewer) 
for (i in 1:nrow(B_rating)) {
 match_row <- stringr::str_detect(B_rating$Reviewer[i],results_gender$firstname) %>%
which(TRUE)
 if(length(match_row)!=0){
 B_rating$gender[i] <- results_gender$gender[match_row[1]]
 }
}
B_rating <- B_rating %>% filter(gender != "")

## Plot
B_rating %>%
 group_by(gender, Score_rounded) %>%
 dplyr::summarise(count = n()) %>%
 na.omit() %>%
 ggplot(aes(x=gender,y=Score_rounded, fill = count))+
 geom_tile()+
 scale_fill_distiller(direction = 1)+
 labs(title = "Number of each rating given by gender", y = "Rating")
Capital words in a review
B_rating <- B_rating %>% mutate(capitalWords=str_count(B_rating$Reviewfinal,"[A-Z]"))

# Calculate capitalizationRatio
B_rating<- B_rating %>% dplyr::mutate(capitalizationRatio=capitalWords/review_length_chars)
summary(B_rating$capitalizationRatio)

# Plot the relationship between the word capitalization ratio of the reviews and the rating score
B_rating %>%
 ggplot(aes(capitalizationRatio, as.factor(Score_rounded))) +
 geom_boxplot(width = 0.5) +
 geom_jitter(aes(col=as.factor(Score_rounded)), alpha = 0.3) +
 scale_color_viridis_d() +
 labs(title = "The relationship between the word capitalization ratio and rating", x = "Readability",y="Rating", color= "Rating")

# Plot the relationship between the word capitalization ratio of the reviews and the rating score (remove outliers)
B_rating %>%
 ggplot(aes(capitalizationRatio, as.factor(Score_rounded))) +
 geom_boxplot(width = 0.5) +
 geom_jitter(aes(col=as.factor(Score_rounded)), alpha = 0.3) +
 scale_color_viridis_d() + xlim(0,0.2)+
 labs(title = "The relationship between the word capitalization ratio and rating", x = "Capitalization ratio",y="Rating", color= "Rating")
Readability
read_review <- qdap::flesch_kincaid(B_rating$reviewTextFinal, B_rating$review_id)
to_join <- read_review$Readability %>% dplyr::select(review_id, FK_grd.lvl)
## Merge NewreviewB with to_join to add readability as a feature
B_rating <-B_rating %>% left_join(to_join)
## Rename FK_grd.lvl to readability 
B_rating$readability <- B_rating$FK_grd.lvl
# Drop the duplicate FK_grd.lvl
B_rating$FK_grd.lvl<-NULL
# Plot the relationship between the readability of the reviews and the rating score
B_rating %>%
 ggplot(aes(readability, as.factor(Score_rounded))) +
 geom_boxplot(width = 0.5) +
 geom_jitter(aes(col=as.factor(Score_rounded)), alpha = 0.3) +
 scale_color_viridis_d() +
 labs(title = "The relationship between readability and rating", x = "Readability",y="Rating", color= "Rating")
Formality
#calculate
formality <- qdap::formality(B_rating$reviewTextFinal, B_rating$review_id)
#Bringing it back as a feature in the original dataset.
formality_calc <- formality$formality %>% dplyr::select(review_id,formality)
formality_calc$review_id <- as.numeric(formality_calc$review_id)
B_rating <- B_rating %>% left_join(formality_calc)
#Visualizing the relationship of formality and score
B_rating %>%
 ggplot(aes(formality, as.factor(Score_rounded))) +
 geom_boxplot(width = 0.5) +
 geom_jitter(aes(col=as.factor(Score_rounded)), alpha = 0.3) +
 scale_color_viridis_d() +
 labs(title = "The relationship between formality and rating",y="Rating", color= "Rating")
Feeling
# The proportion of adj, adv in the reviews
# download the model for the english language 
langmodel_download <- udpipe::udpipe_download_model("english")
langmodel <- udpipe::udpipe_load_model(langmodel_download$file_model)
# annotation process on reviews
postagged <- udpipe::udpipe_annotate(langmodel,
B_rating$reviewTextFinal,
parallel.cores = 8,
 trace = 100) %>%
 as.data.frame(.)
# filter the postagged terms
lematized <- postagged %>% filter(is.na(lemma) == FALSE) %>% 
  filter(upos %in% c("ADJ","ADV")) %>%
  dplyr::select(doc_id,lemma) %>% group_by(doc_id) %>%
  dplyr::summarise(documents_pos_tagged = paste(lemma,collapse = " "))

B_rating <- B_rating %>%
 mutate(doc_id = paste0("doc",row_number())) %>%
 dplyr::left_join(lematized) %>%
 
# count the number of terms
 dplyr::mutate(count_pos_tagged = (stringr::str_count(documents_pos_tagged,"\\S+"))/(stringr::str_count(reviewTextFinal,"\\S+")))

# Visualizing the relationship of feeling and overall
B_rating %>%
 ggplot(aes(count_pos_tagged, as.factor(Score_rounded))) +
 geom_boxplot(width = 0.5) +
 geom_jitter(aes(col=as.factor(Score_rounded)), alpha = 0.3) +
 scale_color_viridis_d() +
 labs(title = "The relationship between feeling and rating",y="Rating", color= "Rating")
Polarity
#calculate
polarity <- qdap::polarity(removePunctuation(B_rating$reviewTextFinal),B_rating$review_id)

# Bringing it back as a feature in the original dataset.
polarity_calc <- polarity$all %>% dplyr::select(review_id,polarity)
polarity_calc$review_id <- as.numeric(polarity_calc$review_id)
B_rating <- B_rating %>% left_join(polarity_calc)

# Visualizing the relationship of formality and score
B_rating %>%
 ggplot(aes(polarity, as.factor(Score_rounded))) +
 geom_boxplot(width = 0.5) +
 geom_jitter(aes(col=as.factor(Score_rounded)), alpha = 0.3) +
 scale_color_viridis_d() +
 labs(title = "The relationship between polarity and rating",y="Rating", color= "Rating")
Estimate Regression Model - all variables
hotel_reg <-B_rating %>% dplyr::select(Score, gender, capitalizationRatio, readability,formality,polarity, count_pos_tagged,review_id) %>% dplyr::rename(feeling = count_pos_tagged) %>% na.omit()

# Create the ordered logit models
model_readability <- lm(Score~readability, data=hotel_reg)
# use as base model
model_gender <- lm(Score~gender, data=hotel_reg)
model_capitalizationRatio <- lm(Score~capitalizationRatio, data=hotel_reg)
model_formality <- lm(Score~formality, data=hotel_reg)
model_feeling <- lm(Score~feeling, data=hotel_reg)
model_polarity <- lm(Score~polarity, data=hotel_reg)

# Relative risk ratios allow an easier interpretation of the logit coefficients. They are the exponentiated value of the logit coefficients.
model_readability.or=exp(coef(model_readability))

stargazer::stargazer(model_readability, model_gender,model_capitalizationRatio,model_readability,model_formality,model_feeling,model_polarity,type="text")

Sentiment Analysis
Load dictionaries
# Bing Liu
bing_dictionary <- tidytext::get_sentiments("bing")

# AFINN 
afinn_dictionary <- tidytext::get_sentiments("afinn")

# NRC dictionary (feelings)
nrc_dictionary <- tidytext::get_sentiments("nrc")

#  Loughran-McDonald dictionary
loughran_dictionary <- tidytext::get_sentiments("loughran")
Dictionary coverage
tokens <- unnest_tokens(NewreviewB,word,reviewTextFinal)

# Use Bing dictionary to calculate coverage
bingCoverage <- tokens %>% left_join(bing_dictionary) %>%  
  dplyr::count(Score,review_id,sentiment) %>%
  mutate(sentiment=ifelse(is.na(sentiment),'untagged',sentiment)) %>%
  pivot_wider(names_from = sentiment,values_from = n) %>%
  mutate(negative=ifelse(is.na(negative),0,negative),positive=ifelse(is.na(positive),0,
    positive)) %>%
  mutate(sentiment = ifelse((positive+negative)==0,0,(positive-negative)/(positive+negative)),coverage=(positive+negative)/(positive+negative+untagged)) %>% select(
coverage)
# Assign the coverage to NewreviewB
NewreviewB$bingCoverage  <- bingCoverage$coverage 
NewreviewB$bingadj <- NewreviewB$bingCoverage*NewreviewB$bingsentiment

# Use comparison cloud to view the most negative and positive terms based on Bing dictionary
tokens %>%
 inner_join(get_sentiments("bing")) %>%
 dplyr::count(word, sentiment, sort = TRUE) %>%
 acast(word ~ sentiment, value.var = "n", fill = 0) %>%
 comparison.cloud(colors = c("red", "lightgreen"),
 max.words = 300,title.bg.colors="white")


# Use AFINN dictionary to calculate coverage
afinnCoverage <- tokens  %>% left_join(afinn_dictionary) %>% mutate(value=ifelse(is.na(value),"Untagged","Tagged")) %>% dplyr::count(Score,review_id,value) %>% pivot_wider(names_from = value,values_from = n) %>% mutate(coverage=Tagged/(Tagged+Untagged)) %>% select(coverage)
# Assign the coverage to reviewData
NewreviewB$afinnCoverage  <- afinnCoverage$coverage   
NewreviewB$afinnadj <- NewreviewB$afinnCoverage*NewreviewB$afinnsentiment

# Use NRC dictionary to calculate coverage
nrcCoverage <- tokens %>% left_join(nrc_dictionary) %>%  
  dplyr::count(Score,review_id,sentiment) %>%
  mutate(sentiment=ifelse(is.na(sentiment),'untagged',sentiment)) %>%
  pivot_wider(names_from = sentiment,values_from = n) %>%
  mutate(negative=ifelse(is.na(negative),0,negative),positive=ifelse(is.na(positive),0,
    positive)) %>%
  mutate(sentiment = ifelse((positive+negative)==0,0,(positive-negative)/(positive+negative)),coverage=(positive+negative)/(positive+negative+untagged)) %>% select(
coverage)
# Assign the coverage to NewreviewB
NewreviewB$nrcCoverage <- nrcCoverage$coverage 
NewreviewB$nrcadj <- NewreviewB$nrcCoverage*NewreviewB$nrcsentiment

# Use Loughran-McDonald dictionary to calculate coverage
LMCoverage <- tokens %>% left_join(loughran_dictionary) %>%  
  dplyr::count(Score,review_id,sentiment) %>%
  mutate(sentiment=ifelse(is.na(sentiment),'untagged',sentiment)) %>%
  pivot_wider(names_from = sentiment,values_from = n) %>%
  mutate(negative=ifelse(is.na(negative),0,negative),positive=ifelse(is.na(positive),0,
    positive)) %>%
  mutate(sentiment = ifelse((positive+negative)==0,0,(positive-negative)/(positive+negative)),coverage=(positive+negative)/(positive+negative+untagged)) %>% select(
coverage)
# Assign the coverage to NewreviewB
NewreviewB$LMCoverage <- LMCoverage$coverage  
NewreviewB$LMadj <- NewreviewB$LMCoverage*NewreviewB$LMsentiment

Sentiment Score by review_id and sentence (check if there are negations)
# Reviews of first 10 IDs 
review10 <- sentence %>% dplyr::select(reviewTextnew,review_id) %>% filter(review_id %in% (1:10)) %>% separate_rows(reviewTextnew, sep = "!")

# Find sentiment of each sentence and remove NAs
review10$sentiment <- sentiment(review10$reviewTextnew) 
review10 <- review10 %>% filter(sentiment$word_count>0)

# View the terms and the corresponding sentiment
term <- review10$reviewTextnew %>% extract_sentiment_terms()

#There are problems with some sentences. For example, the computed sentiment of the twentyfifth sentence is positive while the underlying sentiment of the text is in fact quite negative. This happens because of how far back or forward you look for valence shifters. These problems can be be adjusted as follows.

# Handle valence shifters
review10$sentiment_adjusted <- sentiment(review10$reviewTextnew, valence_shifters_dt = lexicon::hash_valence_shifters, n.after=10,n.before=10) 
# The results for sentiment Score are slightly better.

# Calculate sentiment Score when using bing dictionary
## Based on sentences
review10_full <- NewreviewB %>% dplyr::select(reviewTextFinal,review_id) %>% filter(review_id %in% (1:10)) 
review10_sentence <- sentimentr::get_sentences(review10_full)
bing_key <- as_key(syuzhet:::bing)

review10_bing_Score <- sentiment_by(review10_sentence,polarity_dt=bing_key, n.after=10,n.before=10)

## Based on only words without stop words
review10_token <- unnest_tokens(review10_full,word,reviewTextFinal) %>% anti_join(stop_words)
review10_token_bing <-  review10_token %>% inner_join(bing_dictionary) %>%
                        dplyr::count(review_id,sentiment) %>%
                        pivot_wider(names_from=sentiment, values_from=n) %>% 
                        replace(is.na(.), 0) %>%
                        mutate(sentiment = (positive-negative)/(positive+negative)) %>%
                        mutate(dictionary="bing")
# Calculating sentiment Score based on sentence is more sensible.

Creat Sentiment by dictionaries
# Bing dictionary
bing_key <- as_key(syuzhet:::bing)
bingsentiment <- sentimentr::sentiment_by(get_sentences(NewreviewB$reviewTextFinal),polarity_dt=bing_key,n.after=10,n.before=10)
NewreviewB$bingsentiment  <- bingsentiment$ave_sentiment

# AFINN dictionary
afinn_key <- as_key(syuzhet:::afinn)
afinnsentiment <- sentimentr::sentiment_by(get_sentences(NewreviewB$reviewTextFinal),polarity_dt=afinn_key,n.after=10,n.before=10)
NewreviewB$afinnsentiment  <- afinnsentiment$ave_sentiment

# NRC dictionary
nrcsentiment <-sentimentr::sentiment_by(get_sentences(NewreviewB$reviewTextFinal),polarity_dt=lexicon::hash_sentiment_nrc,n.after=10,n.before=10)
NewreviewB$nrcsentiment  <- nrcsentiment$ave_sentiment

# Loughran-McDonald dictionary
LMsentiment <- sentimentr::sentiment_by(get_sentences(NewreviewB$reviewTextFinal),polarity_dt=lexicon::hash_sentiment_loughran_mcdonald,n.after=10,n.before=10)
NewreviewB$LMsentiment  <- LMsentiment$ave_sentiment
Plotting (4 dictionaries)
bing_Score_total <- NewreviewB %>% dplyr::select(review_id,bingsentiment,Score) %>% mutate(dictionary="Bing") %>% dplyr::rename(ave_sentiment=bingsentiment)
bing_Score_total$dictionary <- gsub("Bing","Bing Liu",bing_Score_total$dictionary)

afinn_Score_total <- NewreviewB %>% dplyr::select(review_id,afinnsentiment,Score) %>% mutate(dictionary="Afinn") %>% dplyr::rename(ave_sentiment=afinnsentiment)

nrc_Score_total <- NewreviewB %>% dplyr::select(review_id,nrcsentiment,Score) %>% mutate(dictionary="NRC") %>% dplyr::rename(ave_sentiment=nrcsentiment)

LM_Score_total <- NewreviewB %>% dplyr::select(review_id,LMsentiment,Score) %>% mutate(dictionary="Loughran-McDonald") %>% dplyr::rename(ave_sentiment=LMsentiment)

all_results <- bing_Score_total %>% bind_rows(afinn_Score_total) %>% bind_rows(nrc_Score_total) %>% bind_rows(LM_Score_total)

# View graph for the first 30000 reviews
## By total
all_results %>% dplyr::filter(review_id %in% (1:30000)) %>% 
  ggplot(aes(review_id, ave_sentiment, fill = dictionary))+ geom_bar(stat="identity")+facet_wrap(~dictionary,ncol=2,scales="free_y") + labs(x="ID",y="Sentiment") 

# Average sentiment Score by dictionary and rating
avg_sentiment_all <- all_results %>% dplyr::group_by(dictionary,Score) %>% summarise(average=mean(ave_sentiment))
avg_sentiment_all
Correlation between the two dictionaries
all_results %>% dplyr::select(review_id,ave_sentiment,dictionary) %>% pivot_wider(names_from = dictionary,values_from=ave_sentiment) -> tocorr
tocorr <- tocorr %>% replace(is.na(.), 0)

cor(tocorr$`Bing Liu`,tocorr$Afinn)
ggplot(tocorr,aes(`Bing Liu`,Afinn))+geom_point()+geom_smooth(method="lm",se = FALSE)
# Bing dictionary has a positive relationship with AFINN dictionary.
#Single word without stop words
single_word <- unnest_tokens(NewreviewB,word,reviewTextFinal) %>% anti_join(stop_words)
#Let's see how the feelings develop through reviewID
nrc_sentiment      <-   single_word %>% inner_join(nrc_dictionary) %>%
                        dplyr::count(Score,review_id,sentiment) %>% 
                        pivot_wider(names_from=sentiment, values_from=n) %>%
                        mutate(sentiment = (positive-negative)/(positive+negative)) %>%
                        mutate(dictionary = "nrc")
#By total
nrc_sentiment %>% dplyr::select(-c(positive,negative)) %>%
pivot_longer(anger:sentiment,names_to = "feeling",values_to="sentiment") %>%
ggplot(aes(x=review_id,y=sentiment,fill=feeling))+
geom_smooth()+
facet_wrap(~feeling,scales="free_y",ncol=2)

#By rating 
nrc_sentiment %>% dplyr::filter(Score %in% (0:2)) %>% dplyr::select(-c(positive,negative)) %>% 
pivot_longer(anger:sentiment,names_to = "feeling",values_to="sentiment") %>%
ggplot(aes(x=review_id,y=sentiment,fill=feeling))+
geom_smooth()+
facet_wrap(~feeling,scales="free_y",ncol=2)+labs(title="Rating 0-2.0")+theme(plot.title = element_text(hjust = 0.5))

nrc_sentiment %>% dplyr::filter(Score %in% (2:4)) %>% dplyr::select(-c(positive,negative)) %>% 
pivot_longer(anger:sentiment,names_to = "feeling",values_to="sentiment") %>%
ggplot(aes(x=review_id,y=sentiment,fill=feeling))+
geom_smooth()+
facet_wrap(~feeling,scales="free_y",ncol=2)+labs(title="Rating 2.1-4.0")+theme(plot.title = element_text(hjust = 0.5))

nrc_sentiment %>% dplyr::filter(Score %in% (4:6)) %>% dplyr::select(-c(positive,negative)) %>% 
pivot_longer(anger:sentiment,names_to = "feeling",values_to="sentiment") %>%
ggplot(aes(x=review_id,y=sentiment,fill=feeling))+
geom_smooth()+
facet_wrap(~feeling,scales="free_y",ncol=2) +labs(title="Rating 4.1-6.0")+theme(plot.title = element_text(hjust = 0.5))

nrc_sentiment %>% dplyr::filter(Score %in% (6:8)) %>% dplyr::select(-c(positive,negative)) %>% 
pivot_longer(anger:sentiment,names_to = "feeling",values_to="sentiment") %>%
ggplot(aes(x=review_id,y=sentiment,fill=feeling))+
geom_smooth()+
facet_wrap(~feeling,scales="free_y",ncol=2) +labs(title="Rating 6.1-8.0")+theme(plot.title = element_text(hjust = 0.5))

nrc_sentiment %>% dplyr::filter(Score %in% (8:10)) %>% dplyr::select(-c(positive,negative)) %>% 
pivot_longer(anger:sentiment,names_to = "feeling",values_to="sentiment") %>%
ggplot(aes(x=review_id,y=sentiment,fill=feeling))+
geom_smooth()+
facet_wrap(~feeling,scales="free_y",ncol=2) +labs(title="Rating 8.1-10.0")+theme(plot.title = element_text(hjust = 0.5))

nrc_sentiment$positive <-replace_na(nrc_sentiment$positive,0)
nrc_sentiment$negative <-replace_na(nrc_sentiment$negative,0)
Correlation between feelings
library(corrplot)
to_corr2 <- nrc_sentiment %>% dplyr::select(-c(positive,negative)) %>% dplyr::select(anger:sentiment)

knitr::kable(round(cor(na.omit(to_corr2)),2))

corrplot::corrplot(cor(na.omit(to_corr2)))
Simple regression of 4 dictionaries
# Plot the simple regression
gr_bl <- NewreviewB %>% 
 dplyr::select(bingsentiment,Score) %>% 
 na.omit() %>% 
 ggplot(aes(x=bingsentiment,y=log(Score))) + geom_smooth(method="lm") + geom_point(size = 2, shape=1,alpha=0.1) + xlab("Bing Liu")

gr_nrc <- NewreviewB %>% 
 dplyr::select(nrcsentiment,Score) %>% 
 na.omit() %>% 
 ggplot(aes(x=nrcsentiment,y=log(Score))) + geom_smooth(method="lm") + geom_point(size = 2, shape=1,alpha=0.1) + xlab("NRC")

gr_afinn <- NewreviewB %>% 
 dplyr::select(afinnsentiment,Score) %>% na.omit() %>% 
 ggplot(aes(x=afinnsentiment,y=log(Score))) + geom_smooth(method="lm") + geom_point(size = 2, shape=1,alpha=0.1) + xlab("afinn")

gr_loughran <- NewreviewB  %>% 
 dplyr::select(LMsentiment,Score) %>% 
 na.omit() %>% 
 ggplot(aes(x=LMsentiment,y=log(Score))) + geom_smooth(method="lm") + geom_point(size = 2, shape=1,alpha=0.1)+ xlab("Loughran")

grid.arrange(gr_bl, gr_nrc, gr_afinn, gr_loughran, ncol = 2, nrow = 2)
Build an ordinal logistic regression model
# Check distribution to decide whether to use log(Score)
a<- NewreviewB %>% group_by(Score) %>% dplyr::summarise(total=n())
ggplot(a,aes(x=Score,y=total)) + geom_col() + labs(x="Rating")

# Build a model
# Bing dictionary
lm_bing <- lm(log(Score)~ bingsentiment,data=NewreviewB)
lm_bing_adj <- lm(log(Score)~ bingadj,data=NewreviewB)

# AFINN dictionary
lm_afinn <- lm(log(Score)~ afinnsentiment,data=NewreviewB)
lm_afinn_adj <- lm(log(Score)~afinnadj,data=NewreviewB)

# NRC dictionary
lm_nrc <- lm(log(Score)~ nrcsentiment,data=NewreviewB)
lm_nrc_adj <- lm(log(Score)~ nrcadj,data=NewreviewB)

# Loughran-McDonald dictionary
lm_LM <- lm(log(Score)~ LMsentiment,data=NewreviewB)
lm_LM_adj <- lm(log(Score)~ LMadj,data=NewreviewB)
Use stargazer() for evaluating results
library(stargazer)
stargazer::stargazer(lm_bing,lm_bing_adj,
                     lm_afinn,lm_afinn_adj,
                     lm_nrc,lm_nrc_adj,
                     lm_LM,lm_LM_adj,
                     type = "text")
Build LM model and use stargazer() for evaluating feelings
#Running regressions for nrc feelings
nrca <- nrc_sentiment %>% dplyr::select(-c(positive,negative)) %>%
pivot_longer(anger:sentiment,names_to = "feeling",values_to="sentiment")

nrc_feelings_modelling = nrca %>% spread(feeling, sentiment, fill = 0)
nrc_feelings_modelling[is.na(nrc_feelings_modelling)] <- 0

#Check distrbution to see if log should be used
hist(nrc_feelings_modelling$Score)

#Build a model
modelfeel <- lm(log(Score)~joy+sadness+trust+anger+anticipation+disgust+fear+surprise, data=nrc_feelings_modelling)

#Evaluate the model
stargazer::stargazer(modelfeel, type = "text")
Information Gain
Only dictionaries
set.seed(123)
#Select only interested variables
NewreviewB_B <- NewreviewB %>% dplyr::select(Score, bingsentiment, afinnsentiment, nrcsentiment,LMsentiment)

#Use function information.gain to compute information gain values of the attributes
NewreviewB_B_attr_weights <- information.gain(Score ~., NewreviewB_B)

#sort information gain
NewreviewB_B_sorted_weights <- NewreviewB_B_attr_weights[order(-NewreviewB_B_attr_weights$attr_importance), , drop = F]
barplot(unlist(NewreviewB_B_sorted_weights), 
        names.arg = rownames(NewreviewB_B_sorted_weights), las = "2", cex.names=0.7,
        space = 0.5, main = "Information gain (dictionaries)")
Dictionaries and extracted variables
a <- B_rating
b <- NewreviewB
Bjoin <- full_join(a,b)

#Select only interested variables
set.seed(123)
NewreviewB_B2 <- Bjoin %>% dplyr::select(Score, bingsentiment, afinnsentiment, nrcsentiment,LMsentiment,readability,formality,polarity,gender,capitalizationRatio,count_pos_tagged) %>% dplyr::rename(feeling=count_pos_tagged)
NewreviewB_B2 <- unique(NewreviewB_B2)

#Use function information.gain to compute information gain values of the attributes
NewreviewB_B_attr_weights2 <- information.gain(Score ~., NewreviewB_B2)

#sort information gain
NewreviewB_B_sorted_weights2 <- NewreviewB_B_attr_weights2[order(-NewreviewB_B_attr_weights2$attr_importance), , drop = F]

#Plot the information gain
barplot(unlist(NewreviewB_B_sorted_weights2), 
names.arg = rownames(NewreviewB_B_sorted_weights2), las = "2", cex.names=0.7,
space = 0.5, main = "information gain (dictionaries and extracted variables)")










Part C: Topic Modelling and Latent Dirichlet allocation 

Pre-Processing
#store the data from previous sections 
NewreviewC <- readRDS("Newreview_final.rds")
hotelC<- readRDS(file="hotel.rds")
hotelC$Hotel_Name <- hotelC$Hotel_name
hotelC$Hotel_name <- NULL

#Remove the stop words
NewreviewC_nostop <- NewreviewC %>%
  unnest_tokens(word,reviewTextFinal) %>%
  anti_join(stop_words) %>%
  group_by(review_id) %>%
  dplyr::summarise(reviewText_nostop = paste(word,collapse = " "),
            totalwords = n()) %>%
  filter(totalwords > 2) %>%
  filter(totalwords < 150)

# Combine the two datasets and only select the observations with recorded in the meta data
hotel_review <- NewreviewC %>%
  inner_join(NewreviewC_nostop, by = "review_id") %>%
  inner_join(hotelC, by = "Hotel_Name")

# Select the necessary variables for topic modelling
hotel_topic <- hotel_review %>% 
  dplyr::select(review_id,reviewText_nostop) %>%
  dplyr::rename(document = reviewText_nostop) %>%
  na.omit()

# Convert review date into date object
abvm <- c("January","February","March","April","May","June","July","August","Septemeber","October","November","December")

repm <- c("1-","2-","3-","4-","5-","6-","7-","8-","9-","10-","11-","12-")
hotel_review$Review_date <- replace_abbreviation(hotel_review$Review_date, abvm,repm)
hotel_review$Review_date  <- gsub(" ","",hotel_review$Review_date)
hotel_review$Review_date  <- gsub(",","-",hotel_review$Review_date)
hotel_review$Review_date  <- as.Date(hotel_review$Review_date,format = "%m-%d-%Y")

# Calculate date to number
  data_day <- hotel_review %>%
  dplyr::mutate(day2 = as.numeric(Review_date)) %>%
  dplyr::mutate(day2 = ifelse(is.na(day2), mean(day2, na.rm = T), day2)) %>%
  dplyr::mutate(day2 = day2 + 1 - min(day2)) %>%
  dplyr::select(review_id, day2)
hotel_review <- hotel_review %>% left_join(data_day, by = "review_id")
# Download and load the model for the English language
langmodel_download <- udpipe::udpipe_download_model(language="english")
langmodel <- udpipe::udpipe_load_model(langmodel_download$file_model)

Part-of-speech tagging
#normal approach
hotel_postagged <- udpipe_annotate(langmodel, hotel_topic$document, parallel.cores = 8, trace = 1000)

hotel_postagged <- as.data.frame(hotel_postagged)
#take a glimpse on the result
head(hotel_postagged)
#lemmatization
hotel_lematized <- hotel_postagged %>% 
  filter(is.na(lemma) == FALSE) %>%
  filter(upos %in% c("NOUN", "ADJ", "ADV")) %>%
  dplyr::select(doc_id,lemma) %>% 
  group_by(doc_id) %>%
  dplyr::summarise(documents_pos_tagged = paste(lemma,collapse = " "))

hotel_model <- hotel_review %>% dplyr::select(review_id, Score,Room_type,Star,day2)

hotel_model <- hotel_model %>% mutate(doc_id = paste0("doc",row_number())) %>%
  dplyr::left_join(hotel_lematized)

hotel_model <- na.omit(hotel_model)
STM topic modelling
#STM topic modelling perparation
hotel_processed <- textProcessor(hotel_model$documents_pos_tagged,customstopwords = c("tongue","lovly","london","bit","lot","hotel","erience","birthday","day","location","staff","wasn"), removestopwords = TRUE,metadata = hotel_model,stem = F)

#establish threshold
hotel_threshold <- round(1/100 * length(hotel_processed$documents),0)

hotel_out <- prepDocuments(documents = hotel_processed$documents,vocab = hotel_processed$vocab,meta = hotel_processed$meta, lower.thresh = hotel_threshold)
Finding the optimal number of topics
Search K approach
hotel_numtopics <- searchK(hotel_out$documents,hotel_out$vocab, K=seq(from=4, to=10, by=1))

# Visualize the topic selection
plot(hotel_numtopics)


Time_data <- data.frame("k" = unlist(hotel_numtopics$results$K),
  "Semcoh" = unlist(hotel_numtopics$results$semcoh),
  "Exclus" = unlist(hotel_numtopics$results$exclus)) 
Time_data %>% ggplot2::ggplot(aes(x=Semcoh, y=Exclus)) +
 geom_label(aes(x = Semcoh, y = Exclus), label = Time_data$k)

Semantic coherence approach
# Check at k=6,7,8
many_models <- tibble(K = c(6,7, 8)) %>% dplyr::mutate(hotel_model = future_map(K, ~stm(hotel_out$documents,vocab=hotel_out$vocab, K = ., verbose = FALSE))) 

many_models 

heldout <- make.heldout(hotel_out$documents,vocab=hotel_out$vocab)

k_result <- many_models %>% dplyr::mutate(exclusivity = purrr::map(hotel_model, exclusivity), semantic_coherence = purrr::map(hotel_model, semanticCoherence, hotel_out$documents), eval_heldout = purrr::map(hotel_model, eval.heldout, heldout$missing), residual = purrr::map(hotel_model, checkResiduals, hotel_out$documents), bound = map_dbl(hotel_model, function(x) max(x$convergence$bound)), lfact = map_dbl(hotel_model, function(x) lfactorial(x$settings$dim$K)), lbound = bound + lfact, iterations = map_dbl(hotel_model, function(x) length(x$convergence$bound))) 

k_result 

k_result %>% transmute(K, `Lower bound` = lbound, Residuals = map_dbl(residual, "dispersion"), `Semantic coherence` = map_dbl(semantic_coherence, mean), `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>% gather(Metric, Value, -K) %>% ggplot(aes(K, Value, color = Metric)) + geom_line() + facet_wrap(~Metric, scales = "free_y") 

k_result %>% dplyr::select(K, exclusivity, semantic_coherence) %>% filter(K %in% c(6,7,8)) %>% unnest(cols = c(exclusivity, semantic_coherence)) %>% ggplot(aes(semantic_coherence, exclusivity, color = factor(K))) + geom_point() 
# k=7 is likely to be the most suitable K as having low residuals and good semantic coherence and Held-out likelihood
STM execution
# Define K = 7 and execute the model
# Unsupervised approach
set.seed(200)
hotel_fit <- stm(documents = hotel_out$documents,
               vocab = hotel_out$vocab,
               K = 7,
               max.em.its = 75, 
               data = hotel_out$meta,
               reportevery=10,
               # gamma.prior = "L1",
               sigma.prior = 0.7,
               init.type = "LDA")

# Supervised approach
set.seed(200)
hotel_fit1 <- stm(documents = hotel_out$documents,
               vocab = hotel_out$vocab,
               K = 7,
               prevalence = ~Score+Star,
               max.em.its = 75, 
               data = hotel_out$meta,
               reportevery=10,
               # gamma.prior = "L1",
               sigma.prior = 0.7,
               init.type = "LDA")

# generate topic solution
( hotel_summary <- summary(hotel_fit) )
( hotel_summary1 <- summary(hotel_fit1) )

# Plot expected topic proportion
plot(hotel_fit)
plot(hotel_fit1)


hotel_proportions <- colMeans(hotel_fit$theta)
hotel_proportions1 <- colMeans(hotel_fit1$theta)
# Select supervised approach

Word cloud for topics
# Visualize the dominating words for each topic 
mycolors <- brewer.pal(8, "Dark2")
stm::cloud(hotel_fit1,topic = 1,color=mycolors)
stm::cloud(hotel_fit1,topic = 2,color=mycolors)
stm::cloud(hotel_fit1,topic = 3,color=mycolors)
stm::cloud(hotel_fit1,topic = 4,color=mycolors)
stm::cloud(hotel_fit1,topic = 5,color=mycolors)
stm::cloud(hotel_fit1,topic = 6,color=mycolors)
stm::cloud(hotel_fit1,topic = 7,color=mycolors)

# Implement beta matrix
hotel_topics <- tidy(hotel_fit1,matrix="beta")
# Extract top 10 terms for each topic
hotel_terms <- hotel_topics %>%
group_by(topic) %>%
slice_max(beta,n=10) %>%
ungroup() %>%
arrange(topic,desc(beta))
# View the top 10 terms for each topic
hotel_terms
Topic labeling
# Based on the output assign topic names to topic_lables
topic_labels <- c("Hotel Surroundings","Food Quality", "Convenience and Location", "Quality of Room","Hotel Facilities", "Staff service","Overall Experience")

# Assign Topic labels to main data frame.
hotel_terms <- hotel_terms %>%
mutate(topic_label = case_when((topic == 1) ~ topic_labels[1],
(topic == 2) ~ topic_labels[2],
(topic == 3) ~ topic_labels[3],
(topic == 4) ~ topic_labels[4],
(topic == 5) ~ topic_labels[5],
(topic == 6) ~ topic_labels[6],
(topic == 7) ~ topic_labels[7]))
PCA Visualization
library(FactoMineR)
hotel_labels1 <- as.matrix(c(paste0("topic_",1:7)))
hotel_gamma1 <- tidy(hotel_fit1,matrix="gamma")
hotel_gamma1 <- hotel_gamma1 %>%
  pivot_wider(names_from = topic, values_from = gamma)
colnames(hotel_gamma1) <- c("document",hotel_labels1)

rownames(hotel_gamma1) <- hotel_gamma1$document
hotel_gamma1$document <- NULL 

hotel_pca1 <- FactoMineR::PCA(hotel_gamma1,graph = FALSE)
factoextra::fviz_pca_var(hotel_pca1)
# analysis visualization
hotel_terms %>%
mutate(term = reorder_within(term, beta, topic)) %>%
ggplot(aes(beta, term, fill = factor(topic))) +
geom_col(show.legend = FALSE) +
facet_wrap(~ topic_label, scales = "free") +
scale_y_reordered()


stm::plot.STM(hotel_fit1, "perspectives", topics = c(5, 7))
Topic correlation
hotel_topicCor1 <- topicCorr(hotel_fit1,
                          method = "simple",
                          cutoff = 0.5) 
# Plot topic correlation – type 1
set.seed(195)
plot(hotel_topicCor1,
     topics = c(1:7),
     vlabels = hotel_labels1,
     layout = NULL,
     vertex.color = "Red",
     vertex.label.cex = 0.8,
     vertex.label.color = "Black",
     vertex.size = 40,
     main = "Topic Correlation"
     )
# Plot topic correlation – type 2
corrplot::corrplot(cor(hotel_gamma1))
Effect estimation
hotel_effects <- estimateEffect(~Score+Star+day2, 
                              stmobj = hotel_fit1,
                              metadata = hotel_out$meta)

# Plot the marginal effect of topics on rating
plot(hotel_effects, 
     covariate = "Score",
     topics = c(1:7),
     model = hotel_fit1, 
     method = "difference",
     cov.value1 = "100", 
     cov.value2 = "0",
     xlab = "Low Rating ... High Rating",
     xlim = c(-0.01,0.01),
     ci.level = 0.05,
     main = "Marginal Effects of Rating",
     custom.labels = topic_labels,
     labeltype = "custom")

# Plot the effect of each topic on rating
for(i in 1:length(hotel_labels1)){
plot(hotel_effects, covariate = "Score",
 topics = i,
 model = hotel_fit1, 
 method = "continuous",
 xlab = "Ratings",
 # xlim = c(0,800),
 main = topic_labels[i],
 printlegend = FALSE,
 custom.labels =topic_labels[i],
 labeltype = "custom")
}
# Calculate the upper and lower bound margin for star
margin1 <- as.numeric(quantile(hotel_out$meta$Star)[2])
margin2 <- as.numeric(quantile(hotel_out$meta$Star)[4])




# Plot the marginal effect of topics on star
plot(hotel_effects, covariate = "Star",
     topics = c(1:7),
     model = hotel_fit1, 
     method = "difference",
     cov.value1 = margin2, 
     cov.value2 = margin1,
     xlab = "Low Star ... High Star",
     xlim = c(-0.01,0.01),
     main = "Marginal change on topic probabilities for low and high star",
     custom.labels =topic_labels,
     ci.level = 0.05,
     labeltype = "custom")

# Plot the effect of each topic on star
for(i in 1:length(hotel_labels1)){
plot(hotel_effects, covariate = "Star",
 topics = i,
 model = hotel_fit1, 
 method = "continuous",
 xlab = "Star Rating",
 # xlim = c(0,800),
 main = topic_labels[i],
 printlegend = FALSE,
 custom.labels =topic_labels[i],
 labeltype = "custom")
}
Important topics across time
# Plot depending on day
plottrend <- function(n){
tit <- paste(topic_labels[n])
plot(hotel_effects,"day2", method = "continuous", topics = n, model = hotel_fit2,
printlegend = FALSE, xaxt = "n", xlab="Time (month)", main =tit)
yearseq <- seq(from = as.Date("2019-03-28"), to = as.Date("2022-03-28"), by = "month")
yearnames <- month(yearseq) 
axis(1,at = as.numeric(yearseq) - min(as.numeric(yearseq)), labels = yearnames)}


plottrend(1)
plottrend(2)
plottrend(3)
plottrend(4)
plottrend(5)
plottrend(6)
plottrend(7)
Prepare the regression
hotel_object <- hotel_fit$theta

colnames(hotel_object) <- topic_labels[1:7]

hotel_reg_C <- cbind(hotel_out$meta,hotel_object)

hotel_reg_C <- hotel_reg_C %>%
  dplyr::select(-c(doc_id, documents_pos_tagged))

hotel_reg_C$Score <- as.factor(hotel_reg_C$Score)
#Select only important variables
hotel_reg_C2 <- hotel_reg_C
hotel_reg_C2$Score <- NULL
hotel_reg_C2$Room_type <- NULL
hotel_reg_C2$Star <- NULL

hotel_reg_C2 <- Bjoin %>%
  left_join(hotel_reg_C2, by = "review_id") %>%
  dplyr::rename(sent_afinn = afinnsentiment, Day=day2) %>% dplyr::select(review_id, sent_afinn,`Hotel Surroundings`,`Food Quality`,`Convenience and Location`,`Quality of Room`,`Hotel Facilities`,`Staff service`,`Overall Experience`,Score,Day,Room_type,Star) %>% na.omit()

#Remove review_id to find information gain
hotel_reg_C3 <- hotel_reg_C2 %>% select(-review_id)

#Use function information.gain to compute information gain values of the attributes
hotel_attr_weights <- information.gain(Score ~., hotel_reg_C3)
#sort information gain
hotel_sorted_weights <- hotel_attr_weights[order(-hotel_attr_weights$attr_importance), , drop = F]
barplot(unlist(hotel_sorted_weights), 
        names.arg = rownames(hotel_sorted_weights), las = "2", cex.names=0.7,
        space = 0.5, main = "Information Gain")
# Check distribution to decide whether to use log(Score)
a<- hotel_reg_C2 %>% group_by(Score) %>% dplyr::summarise(total=n())
ggplot(a,aes(x=Score,y=total)) + geom_col()
# Create lm models 
# Use Afinn sentiment to be a base model 
model_0 <- lm(log(Score) ~ sent_afinn, data = hotel_reg_C2)
model_1 <- lm(log(Score) ~ sent_afinn + `Hotel Surroundings`, data = hotel_reg_C2)
model_2 <- lm(log(Score) ~ sent_afinn + `Food Quality`, data = hotel_reg_C2)
model_3 <- lm(log(Score) ~ sent_afinn + `Convenience and Location`, data = hotel_reg_C2)
model_4 <- lm(log(Score) ~ sent_afinn + `Quality of Room`, data = hotel_reg_C2)
model_5 <- lm(log(Score) ~ sent_afinn + `Hotel Facilities`, data = hotel_reg_C2)
model_6 <- lm(log(Score) ~ sent_afinn + `Staff service`, data = hotel_reg_C2)
model_7 <- lm(log(Score) ~ sent_afinn + `Overall Experience`, data = hotel_reg_C2)
Model Evaluation
stargazer::stargazer(model_1,
                     model_2,
                     model_3,
                     model_4,
                     model_5,
                     model_6,
                     model_7,
                     type = "text")

