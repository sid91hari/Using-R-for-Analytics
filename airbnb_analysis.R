################################################################################
######### Berlin Airbnb Analysis code #########
################################################################################


################################################################################
# Airbnb Word Cloud code
################################################################################

#Set directory

setwd("E:\\Purdue\\Fall 1\\R for Analytics\\R Shiny Project")


###Word cloud:

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Load data

airbnb <- read.csv('listings_summary.csv', stringsAsFactors = F)

#Wordcloud for transit

rulesCorpus <- Corpus(VectorSource(airbnb$transit))
rulesCorpus <- tm_map(rulesCorpus, content_transformer(tolower))
rulesCorpus <- tm_map(rulesCorpus, removePunctuation)
rulesCorpus <- tm_map(rulesCorpus, removeWords, c("berlin","und","ist","die","the","is","mit","der","das","zum","ein","can","pleas","will","smoke")) 

#revCorpus <- tm_map(revCorpus, PlainTextDocument)
rulesCorpus <- tm_map(rulesCorpus, removeWords, stopwords('english'))

rulesCorpus <- tm_map(rulesCorpus, stemDocument)
wordcloud(rulesCorpus, max.words = 200, random.order = FALSE,colors=brewer.pal(8, "Dark2"), rot.per=0.35)


#Wordcloud for transit

rulesCorpus <- Corpus(VectorSource(airbnb$house_rules))
rulesCorpus <- tm_map(rulesCorpus, content_transformer(tolower))
rulesCorpus <- tm_map(rulesCorpus, removePunctuation)
rulesCorpus <- tm_map(rulesCorpus, removeWords, c("berlin","und","ist","die","the","is","mit","der","das","zum","ein","can","pleas","will","smoke")) 

#revCorpus <- tm_map(revCorpus, PlainTextDocument)
rulesCorpus <- tm_map(rulesCorpus, removeWords, stopwords('english'))

rulesCorpus <- tm_map(rulesCorpus, stemDocument)
wordcloud(rulesCorpus, max.words = 200, random.order = FALSE,colors=brewer.pal(8, "Dark2"), rot.per=0.35)


################################################################################
# Airbnb Price Analysis code
################################################################################

library(shiny)
library(shinythemes)
library(ggplot2)
library (sqldf)
library(caret)
library(Metrics)
library(randomForest)
library(gbm)

library(dplyr)
library(lubridate) 


#Set directory

# setwd("E:\\Purdue\\Fall 1\\R for Analytics\\R Shiny Project")

#Load data

# airbnb <- read.csv('listings_summary.csv', stringsAsFactors = F)

#List of columns to be dropped

drop_cols <-
  c(
    'summary',
    'space',
    'description',
    'experiences_offered',
    'neighborhood_overview',
    'notes',
    'transit',
    'access',
    'interaction',
    'house_rules',
    'host_name',
    'host_about',
    'host_total_listings_count',
    'neighbourhood',
    'market',
    'smart_location',
    'country_code',
    'country',
    'requires_license',
    'license',
    'is_business_travel_ready',
    'require_guest_profile_picture',
    'require_guest_phone_verification',
    'weekly_price',
    'monthly_price',
    'has_availability',
    'listing_url',
    'scrape_id',
    'last_scraped',
    'thumbnail_url',
    'medium_url',
    'picture_url',
    'xl_picture_url',
    'host_url',
    'host_thumbnail_url',
    'host_picture_url',
    'calendar_last_scraped',
    'jurisdiction_names'
  )



#Drop columns from data

for (i in drop_cols){
  airbnb[i] <- NULL
}

names <- names(airbnb)

for (i in 1:length(names)){
  
  airbnb[which(airbnb[,names[i]]=='N/A'),names[i]] <- NA 
  
}

for (i in 1:length(names)){
  
  airbnb[which(airbnb[,names[i]]==''),names[i]] <- NA 
  
}

#Load Data Quality to inspect NAs

# source("DataQualityReport.R")
 
# DataQualityReport(airbnb)



#                        Attributes    Type NumberMissing PercentComplete   Min         Avg   Median       Max NumberLevels
# 1                              id numeric             0          100.00  2015 15715604.77 16866381  29867352            -
# 2                            name UNKNOWN            59           99.74     -           -        -         -            - # Unknown
# 3                         host_id numeric             0          100.00  2217 54033548.02 31267110 224508134            -
# 4                      host_since UNKNOWN            26           99.88     -           -        -         -            - # Date (Median)
# 5                   host_location UNKNOWN           116           99.49     -           -        -         -            -
# 6              host_response_time UNKNOWN         12894           42.83     -           -        -         -            - # NA and blank UNKNOWN
# 7              host_response_rate UNKNOWN         12895           42.82     -           -        -         -            - # Drop (58% NAs and 31% are 100%)
# 8            host_acceptance_rate UNKNOWN         22552            0.00     -           -        -         -            - # Drop
# 9               host_is_superhost UNKNOWN            26           99.88     -           -        -         -            - # False (Mode)
# 10             host_neighbourhood UNKNOWN          5094           77.41     -           -        -         -            - # Unknown
# 11            host_listings_count numeric            26           99.88     0        2.33        1      1676            - # Drop since calculated_host_listings_count gives the result
# 12             host_verifications UNKNOWN             0          100.00     -           -        -         -            -
# 13           host_has_profile_pic UNKNOWN            26           99.88     -           -        -         -            - # True (Mode)
# 14         host_identity_verified UNKNOWN            26           99.88     -           -        -         -            - # False (Mode)
# 15                         street UNKNOWN             0          100.00     -           -        -         -            -
# 16         neighbourhood_cleansed UNKNOWN             0          100.00     -           -        -         -            - # Drop
# 17   neighbourhood_group_cleansed UNKNOWN             0          100.00     -           -        -         -            -
# 18                           city UNKNOWN             5           99.98     -           -        -         -            - # Drop
# 19                          state UNKNOWN            84           99.63     -           -        -         -            - # Drop
# 20                        zipcode UNKNOWN           656           97.09     -           -        -         -            - # row_number impute
# 21                       latitude numeric             0          100.00 52.35       52.51    52.51     52.65            -
# 22                      longitude numeric             0          100.00  13.1       13.41    13.42     13.76            -
# 23              is_location_exact UNKNOWN             0          100.00     -           -        -         -            -
# 24                  property_type UNKNOWN             0          100.00     -           -        -         -            -
# 25                      room_type UNKNOWN             0          100.00     -           -        -         -            -
# 26                   accommodates numeric             0          100.00     1        2.64        2        16            -
# 27                      bathrooms numeric            32           99.86     0        1.09        1       8.5            - # Median
# 28                       bedrooms numeric            18           99.92     0        1.16        1        12            - # Median
# 29                           beds numeric            40           99.82     0        1.62        1        22            - # Median
# 30                       bed_type UNKNOWN             0          100.00     -           -        -         -            -
# 31                      amenities UNKNOWN             0          100.00     -           -        -         -            -
# 32                    square_feet numeric         22106            1.98     0       465.4    403.5      4639            - # Drop
# 33                          price UNKNOWN             0          100.00     -           -        -         -            -
# 34               security_deposit UNKNOWN          9361           58.49     -           -        -         -            - # 0
# 35                   cleaning_fee UNKNOWN          7146           68.31     -           -        -         -            - # 0
# 36                guests_included numeric             0          100.00     1        1.33        1        16            -
# 37                   extra_people UNKNOWN             0          100.00     -           -        -         -            -
# 38                 minimum_nights numeric             0          100.00     1        7.16        2      5000            -
# 39                 maximum_nights numeric             0          100.00     1   103050.46     1124 999999999            -
# 40               calendar_updated UNKNOWN             0          100.00     -           -        -         -            - # Drop
# 41                availability_30 numeric             0          100.00     0        4.94        0        30            -
# 42                availability_60 numeric             0          100.00     0       11.15        0        60            -
# 43                availability_90 numeric             0          100.00     0       20.02        0        90            -
# 44               availability_365 numeric             0          100.00     0       79.85        4       365            -
# 45              number_of_reviews numeric             0          100.00     0       17.84        5       498            -
# 46                   first_review UNKNOWN          3914           82.64     -           -        -         -            - # host_since (and rename it to listing_start_date)
# 47                    last_review UNKNOWN          3908           82.67     -           -        -         -            - # Drop
# 48           review_scores_rating numeric          4389           80.54    20       94.41       97       100            - # Median
# 49         review_scores_accuracy numeric          4414           80.43     2        9.66       10        10            - # Median
# 50      review_scores_cleanliness numeric          4411           80.44     2        9.32       10        10            - # Median
# 51          review_scores_checkin numeric          4432           80.35     2        9.71       10        10            - # Median
# 52    review_scores_communication numeric          4418           80.41     2        9.73       10        10            - # Median
# 53         review_scores_location numeric          4431           80.35     2        9.51       10        10            - # Median
# 54            review_scores_value numeric          4435           80.33     2        9.41       10        10            - # Median
# 55               instant_bookable UNKNOWN             0          100.00     -           -        -         -            -
# 56            cancellation_policy UNKNOWN             0          100.00     -           -        -         -            -
# 57 calculated_host_listings_count numeric             0          100.00     1        1.92        1        45            -
# 58              reviews_per_month numeric          3914           82.64  0.01        1.14     0.54     36.67            - # Median


#Removing further columns from data after making NAs

drop_cols2 <- c('host_location','host_response_rate','host_acceptance_rate','host_listings_count','city','state','square_feet','last_review','neighbourhood_cleansed')

for (i in drop_cols2){
  airbnb[i] <- NULL
}

########### Numeric Cleaning ###########

# extra_people

# price

# security_deposit
# cleaning_fee


num_cols <-
  c(
    'extra_people'
    ,'price'
    ,'security_deposit'
    ,'cleaning_fee'
  )

for (i in num_cols){
  airbnb[[i]] <- gsub('[$]','',airbnb[[i]])
  airbnb[[i]] <- as.numeric(gsub(',','',airbnb[[i]]))
}

#Imputing NAs with 0 for security_deposit and cleaning_fee variables

airbnb$security_deposit[is.na(airbnb$security_deposit)] <- 0

airbnb$cleaning_fee[is.na(airbnb$cleaning_fee)] <- 0

#Imputing NAs with median for other numerical variables



median_cols <-
c(
 'bathrooms'
,'bedrooms'
,'beds'
,'review_scores_rating'
,'review_scores_accuracy'
,'review_scores_cleanliness'
,'review_scores_checkin'
,'review_scores_communication'
,'review_scores_location'
,'review_scores_value'
,'reviews_per_month'

)

for (i in median_cols){
  airbnb[[i]][is.na(airbnb[[i]])] <- median(airbnb[[i]], na.rm = T)
}


########### Factor ###########

### Cleaning

# 2                            name UNKNOWN            59           99.74     -           -        -         -            - # Unknown
# 6              host_response_time UNKNOWN         12894           42.83     -           -        -         -            - # Unknown
# 10             host_neighbourhood UNKNOWN          5094           77.41     -           -        -         -            - # Unknown

# 9               host_is_superhost UNKNOWN            26           99.88     -           -        -         -            - # False (Mode)
# 13           host_has_profile_pic UNKNOWN            26           99.88     -           -        -         -            - # True (Mode)
# 14         host_identity_verified UNKNOWN            26           99.88     -           -        -         -            - # False (Mode)

##### Imputing NAs in character vectors using 'Unknown' #####

unknown_cols <-
  c(
    'name'
    ,'host_response_time'
    ,'host_neighbourhood'
  )


for (i in unknown_cols){
  airbnb[[i]][is.na(airbnb[[i]])] <- 'Unknown'
}


##### Imputing NAs in character vectors using Mode #####

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_cols <-
c(
 'host_is_superhost'
,'host_has_profile_pic'
,'host_identity_verified'
)


for (i in mode_cols){
  airbnb[[i]][is.na(airbnb[[i]])] <- getmode(airbnb[[i]])
}

##### Clean cancellation_policy data #####

airbnb$cancellation_policy <- ifelse(airbnb$cancellation_policy == "strict_14_with_grace_period", "Strict_14_days", airbnb$cancellation_policy)

airbnb$cancellation_policy <- ifelse(airbnb$cancellation_policy == "super_strict_30", "Strict_30_days", airbnb$cancellation_policy)

airbnb$cancellation_policy <- ifelse(airbnb$cancellation_policy == "super_strict_60", "Strict_60_days", airbnb$cancellation_policy)


###### Convert factors ######

# host_response_time (ordered)
# neighbourhood_group_cleansed
# property_type
# room_type
# bed_type
# cancellation_policy

factor_cols <-
c(
 'neighbourhood_group_cleansed'
,'property_type'
,'room_type'
,'bed_type'
,'cancellation_policy'
)


for (i in factor_cols){
  airbnb[[i]] <- as.factor(airbnb[[i]])
}

#### Converting host_response_time into ordered factor ####

airbnb$host_response_time <- factor(airbnb$host_response_time, 
                                       levels = c("within an hour","within a few hours","within a day","a few days or more","Unknown"),
                                       ordered = T)

#### Breakdown top amenities into separate columns ####

# library(sqldf)

airbnb <- sqldf("select *,
							case when amenities like '%Wifi%' then 'Yes' else 'No' end as Wifi,
							case when amenities like '%TV%' then 'Yes' else 'No' end as tv,
							case when amenities like '%Internet%' then 'Yes' else 'No' end as Internet,
							case when amenities like '%Kitchen%' then 'Yes' else 'No' end as Kitchen,
							case when amenities like '%Heating%' then 'Yes' else 'No' end as Heating,
							case when amenities like '%Washer%' then 'Yes' else 'No' end as Washer,
							case when amenities like '%Refrigerator%' then 'Yes' else 'No' end as Refrigerator,
							case when amenities like '%Dishwasher%' then 'Yes' else 'No' end as Dishwasher,
							case when amenities like '%Dryer%' then 'Yes' else 'No' end as Dryer,
							case when amenities like '%Microwave%' then 'Yes' else 'No' end as Microwave,
							case when amenities like '%Stove%' then 'Yes' else 'No' end as Stove,
							case when amenities like '%Paid parking on premises%' then 'Yes' else 'No' end as Premise_Part,
							case when amenities like '%Paid parking off premises%' then 'Yes' else 'No' end as Off_Premise_Part
						from airbnb")


str(airbnb)

#### Factor amenities ####

amenities <-
c(
'Wifi'
,'tv'
,'Internet'
,'Kitchen'
,'Heating'
,'Washer'
,'Refrigerator'
,'Dishwasher'
,'Dryer'
,'Microwave'
,'Stove'
,'Premise_Part'
,'Off_Premise_Part'
)


for (i in amenities){
  airbnb[[i]] <- as.factor(airbnb[[i]])
}




#### Convert True/False into 1s and 0s ####

true_false_cols <- 
c(
 'host_is_superhost'
,'host_has_profile_pic'
,'host_identity_verified'
,'is_location_exact'
,'instant_bookable'
)

for (i in true_false_cols) {
airbnb[[i]] <- (ifelse(airbnb[[i]]=="t",1,0))
}

airbnb$host_since <- as.Date(airbnb$host_since, origin = "1970-01-01")
airbnb$first_review <- as.Date(airbnb$first_review,origin = "1970-01-01")

max_date <- as.numeric(max(airbnb$host_since, na.rm = T))

min_date <- as.numeric(min(airbnb$host_since, na.rm = T))

final_date_val <- (max_date+min_date)/2

final_date <- as.Date(final_date_val, origin = "1970-01-01")

airbnb[["host_since"]][is.na(airbnb[["host_since"]])] <- final_date


airbnb$first_review <- as.Date(ifelse(is.na(airbnb$first_review), airbnb$host_since, airbnb$first_review), origin = "1970-01-01")


######## Final data for visualization ########

airbnb2 <- airbnb[1:49]

for (i in true_false_cols){
  airbnb2[[i]] <- as.factor(airbnb2[[i]])
}

airbnb2 <- sqldf("select *,
							case when amenities like '%Wifi%' then 1 else 0 end as Wifi,
							case when amenities like '%TV%' then 1 else 0 end as tv,
							case when amenities like '%Internet%' then 1 else 0 end as Internet,
							case when amenities like '%Kitchen%' then 1 else 0 end as Kitchen,
							case when amenities like '%Heating%' then 1 else 0 end as Heating,
							case when amenities like '%Washer%' then 1 else 0 end as Washer,
							case when amenities like '%Refrigerator%' then 1 else 0 end as Refrigerator,
							case when amenities like '%Dishwasher%' then 1 else 0 end as Dishwasher,
							case when amenities like '%Dryer%' then 1 else 0 end as Dryer,
							case when amenities like '%Microwave%' then 1 else 0 end as Microwave,
							case when amenities like '%Stove%' then 1 else 0 end as Stove,
							case when amenities like '%Paid parking on premises%' then 1 else 0 end as Premise_Part,
							case when amenities like '%Paid parking off premises%' then 1 else 0 end as Off_Premise_Part
						from airbnb2")

#### Drop amenities and host_verifications ####

airbnb$amenities <- NULL
airbnb$host_verifications <- NULL

airbnb2$amenities <- NULL
airbnb2$host_verifications <- NULL


for (i in names(airbnb2)[48:60]){
airbnb2[[i]] <- as.factor(airbnb2[[i]])
}



write.csv(airbnb2, "airbnb_cleaned_data.csv", row.names = F)

###### EDA and graphs

dev.off()

par(mfrow=c(3,3), bg="white", fg="black",cex.lab=1.2, cex.axis=.9, cex.main=1.5)
amenities<-c("Wifi", "tv", "Internet", "Kitchen", "Heating", "Washer", "Refrigerator", "Dishwasher", "Dryer")
for(i in amenities){
  plot(airbnb2[,i], main=paste0("Plot of ", names(airbnb2[i])), xlab=names(airbnb2[i]), col="gold")
}


###Geo charts:

par(mfrow=c(1,1))

dev.off()

ggplot(data=subset(airbnb,price<200))+geom_point(aes(x=latitude, y=longitude, color=price)) + scale_colour_gradientn(colours = terrain.colors(10))+ ggtitle("Price per night across Berlin Airbnb listings") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data=subset(airbnb,price<200))+geom_point(aes(x=latitude, y=longitude, color=neighbourhood_group_cleansed))+ ggtitle("Distribution of listings across Berlin") + theme(plot.title = element_text(hjust = 0.5))


#### Select X predictors and Y ####

# [1] "id"                             "name"                          
# [3] "host_id"                        "host_since"                    
# [5] "host_response_time"             "host_is_superhost"             
# [7] "host_neighbourhood"             "host_has_profile_pic"          
# [9] "host_identity_verified"         "street"                        
# [11] "neighbourhood_group_cleansed"   "zipcode"                       
# [13] "latitude"                       "longitude"                     
# [15] "is_location_exact"              "property_type"                 
# [17] "room_type"                      "accommodates"                  
# [19] "bathrooms"                      "bedrooms"                      
# [21] "beds"                           "bed_type"                      
# [23] "price"                          "security_deposit"              
# [25] "cleaning_fee"                   "guests_included"               
# [27] "extra_people"                   "minimum_nights"                
# [29] "maximum_nights"                 "calendar_updated"              
# [31] "availability_30"                "availability_60"               
# [33] "availability_90"                "availability_365"              
# [35] "number_of_reviews"              "first_review"                  
# [37] "review_scores_rating"           "review_scores_accuracy"        
# [39] "review_scores_cleanliness"      "review_scores_checkin"         
# [41] "review_scores_communication"    "review_scores_location"        
# [43] "review_scores_value"            "instant_bookable"              
# [45] "cancellation_policy"            "calculated_host_listings_count"
# [47] "reviews_per_month"              "Wifi"                          
# [49] "tv"                             "Internet"                      
# [51] "Kitchen"                        "Heating"                       
# [53] "Washer"                         "Refrigerator"                  
# [55] "Dishwasher"                     "Dryer"                         
# [57] "Microwave"                      "Stove"                         
# [59] "Premise_Part"                   "Off_Premise_Part"   

#### Y and X data for Regression ####

d <- airbnb

d <-d[,c(23, 5, 6, 8, 9,11, 15, 16, 17, 19, 20, 21, 22, 25, 45, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58)]

names(d)[1] <- "y"

capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

# for (i in names(d[sapply(d, is.numeric)])) {

  # d[[i]]=capOutlier(d[[i]])

# }

d$y <- capOutlier(d$y)

summary(d$y)

boxplot(d$y)

library(caret)

################################################################################
# Data partitioning
################################################################################

set.seed(42) # set a seed so you can replicate your results

# identify records that will be used in the training set. Here we are doing a
# 80/20 train-test split
inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .8,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set

################################################################################
# Modeling
################################################################################

##### Linear Model

lm_model = lm(formula = y ~ .,data = train)

summary(lm_model)


y_lm_train = predict(lm_model, train)

library(Metrics)

rmse.lm<-rmse(train$y, y_lm_train)
print(rmse.lm)

# 22.64719

y_lm_pred = predict(lm_model, test)

rmse.lm<-rmse(test$y, y_lm_pred)
print(rmse.lm)

# 22.89644

library(randomForest)
set.seed(42)

rf = randomForest(x = train[-1],
                         y = train$y,
                         ntree = 500)


rf_imp<-data.frame(rf$importance)

rf_imp$variable <- rownames(rf_imp)

rownames(rf_imp) <- NULL

names(rf_imp) <- c("importance","variable")

rf_imp<-rf_imp[order(-rf_imp$importance),]


View(rf_imp)

##### Taking only important variables for Shiny #####

cols <- c("y","room_type","cleaning_fee",
              "bedrooms","neighbourhood_group_cleansed",
              "beds","bathrooms","cancellation_policy",
              "tv","Internet")

d2 <- d[cols]


set.seed(42) # set a seed so you can replicate your results

# identify records that will be used in the training set. Here we are doing a
# 80/20 train-test split
inTrain <- createDataPartition(y = d2$y,   # outcome variable
                               p = .8,   # % of training data you want
                               list = F)
# create your partitions
train <- d2[inTrain,]  # training data set
test <- d2[-inTrain,]  # test data set


set.seed(42)

rf = randomForest(x = train[-1],
                  y = train$y,
                  ntree = 500)

y_rf_train = predict(rf, train)


rmse.rf<-rmse(train$y, y_rf_train)
print(rmse.rf)

# 18.67953

y_rf_pred = predict(rf, test)

rmse.rf<-rmse(test$y, y_rf_pred)
print(rmse.rf)

# 22.62052


##################### GBM #####################

library(gbm)

set.seed(42)
gbm.gbm <- gbm(y ~ .
               , data=train
               , distribution="gaussian"
               , n.trees=1000
               , interaction.depth=3
               , n.minobsinnode=10
               , shrinkage=0.1
               , bag.fraction=0.75
               , cv.folds=10
               , verbose=FALSE
)
best.iter <- gbm.perf(gbm.gbm, method="cv")

print(best.iter)

# 633

train.predict <- predict.gbm(object=gbm.gbm, newdata=train, best.iter)


rmse.gbm<-rmse(train$y, train.predict)
print(rmse.gbm)

# 22.05521


y_gbm_pred <- predict.gbm(object=gbm.gbm, newdata=test, best.iter)

rmse.gbm<-rmse(test$y, y_gbm_pred)
print(rmse.gbm)

# 22.57989

#### Sample prediction ####

test2 <- list(room_type = "Private room",
              cleaning_fee = 30,
              bedrooms = 1.0,
              neighbourhood_group_cleansed = "Mitte",
              beds = 2,
              bathrooms = 1.0,
              cancellation_policy = "strict_14_with_grace_period",
              tv = "Yes",
              Internet = "No")

test_data2 <- data.frame(lapply(test2, function(x) t(data.frame(x))))

y_pred_final = predict.gbm(gbm.gbm,test_data2, best.iter)

y_pred_final

#59.7552