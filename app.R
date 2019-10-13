#Load data

library(shiny)
library(shinythemes)
library(ggplot2)
library (sqldf)
library(caret)
library(Metrics)
library(randomForest)
library(gbm)

# setwd("E:\\Purdue\\Fall 1\\R for Analytics\\R Shiny Project\\Shiny")
# airbnb <- read.csv('E:\\Purdue\\Fall 1\\R for Analytics\\R Shiny Project\\Shiny\\listings_summary.csv', stringsAsFactors = F)
airbnb <- read.csv('listings_summary.csv', stringsAsFactors = F)

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

#Removing further columns from data after making NAs

drop_cols2 <- c('host_location','host_response_rate','host_acceptance_rate','host_listings_count','city','state','square_feet','last_review','neighbourhood_cleansed')

for (i in drop_cols2){
  airbnb[i] <- NULL
}

########### Numeric Cleaning ###########

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


#### Drop amenities and host_verifications ####

airbnb$amenities <- NULL
airbnb$host_verifications <- NULL

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

d$y <- capOutlier(d$y)

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

lm_model = lm(formula = y ~ .,data = train)

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

# train.predict <- predict.gbm(object=gbm.gbm, newdata=train, best.iter)
# 
# y_gbm_pred <- predict.gbm(object=gbm.gbm, newdata=test, best.iter)

# unique(d2$cancellation_policy)

# cal <- read.csv('E:\\Purdue\\Fall 1\\R for Analytics\\R Shiny Project\\Shiny\\calendar_summary.csv',stringsAsFactors = F)

cal <- read.csv('calendar_summary.csv',stringsAsFactors = F)


################################################################################

cal <- subset(cal,available =="t")

cal$date <- as.Date(cal$date)

cal['month'] <- months(cal$date)

cal_agg <- sqldf("select listing_id,month,count(*) as num_book from cal where month not in ('November','October') group by listing_id,month ")

cal_agg['occupancy'] <- cal_agg$num_book / 31

cal_cat <- sqldf("select cal_agg.*,room_type,neighbourhood_group_cleansed,cancellation_policy
      from cal_agg join airbnb on cal_agg.listing_id = airbnb.id")

month_agg <- sqldf("select neighbourhood_group_cleansed,month,avg(occupancy) as occupancy
          from cal_cat group by neighbourhood_group_cleansed,month
       union
         select room_type,month,avg(occupancy) 
        from cal_cat group by room_type,month")

month_agg$month =factor(month_agg$month,levels=c("December","January","February","March","April","May","June","July","August","September"))

################################################################################

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Berlin Airbnb Listings Price Estimator"),
  fluidRow(
    column(
      width = 2, 
      selectInput('nbhood', 'Neighbourhood',choices = unique(d2$neighbourhood_group_cleansed)),
      hr()),
    column(
      width=1,
      radioButtons("tv", label = ("TV"),choices = c('Yes','No'), selected = "Yes"),
      hr()),
    column(
      width=1,
      radioButtons("Internet", label = ("Internet"),choices = c('Yes','No'), selected = "Yes"),
      hr()),
    column(
      width=2,  
      sliderInput('cleaning_fee', 'Cleaning Fee',min = 0, max = 30,value=10),
      hr()),
    column(
      width = 2,  
      sliderInput('bedrooms', 'Number of Bedrooms',min = 0, max = 5,value=2),
      hr()),
    column(
      width=2,
      sliderInput('beds', 'Number of Beds',min = 0, max = 4,value=2),
      hr()),
    column(
      width = 2,
      sliderInput('bathrooms', 'Number of Bathrooms',min = 0, max = 3,value=1),
      hr())
  ),
  
  fluidRow( 
    
    column(
      width = 2,
      selectInput('room_type', 'Room Type',choices = unique(d2$room_type)),
      hr()),
    column(
      width = 2,
      selectInput('cancellation_policy', 'Cancellation Policy',choices = unique(d2$cancellation_policy)),
      hr()),
    column(
      width = 2,
      actionButton("click", "Update"),
      hr()),
    
    column(
      width = 6,
      verbatimTextOutput("Prediction"),
      hr())
    
  ),
  
  fluidRow(
    column(width = 4, plotOutput("gmap_all",height="350")),
    column(width = 4, plotOutput("gmap_neigh",height="350")),
	column(width = 4, plotOutput("gbar",height="350"))
	)
)


server <- function(input, output,session){
  
  test_data2 <- eventReactive(input$click,{
    data.frame(
  room_type = input$room_type,
  cleaning_fee = input$cleaning_fee,
  bedrooms = input$bedrooms,
  neighbourhood_group_cleansed = input$nbhood,
  beds = input$beds,
  bathrooms = input$bathrooms,
  cancellation_policy = input$cancellation_policy,
  tv = input$tv,
  Internet = input$Internet)})


  y_pred_final <- eventReactive(input$click,{ predict.gbm(object=gbm.gbm,test_data2(),best.iter)})
  
  airbnb_d <- eventReactive(input$click, {subset(airbnb,airbnb$neighbourhood_group_cleansed==input$nbhood)})
  
  lat <- eventReactive(input$click, {as.numeric(unlist(subset(airbnb["latitude"],airbnb$neighbourhood_group_cleansed==input$nbhood)))})
  
  long <- eventReactive(input$click, {as.numeric(unlist(subset(airbnb["longitude"],airbnb$neighbourhood_group_cleansed==input$nbhood)))})
  
  heading <- eventReactive(input$click, {input$nbhood})
  
  data_gg_data <- eventReactive(input$click, {subset(month_agg,neighbourhood_group_cleansed==input$nbhood)})
  
  data_gg_x <- eventReactive(input$click, {subset(month_agg,neighbourhood_group_cleansed==input$nbhood)[["month"]]})
  
  data_gg_y <- eventReactive(input$click, {subset(month_agg,neighbourhood_group_cleansed==input$nbhood)[["occupancy"]]})

  output$Prediction <- renderText(print(paste("Estimated Price:",round(y_pred_final(),2),"Euros")))

  output$gmap_all <- renderPlot ({
    ggplot(data=subset(airbnb,price<400))+
      geom_point(aes(x=latitude, y=longitude, color=neighbourhood_group_cleansed)) + 
      xlim(52.4,52.65) + 
      ylim(13.1,13.75) +
      theme(legend.position = "none") +
      ggtitle('Berlin City listings across Neighbourhood') +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$gmap_neigh <- renderPlot({
    ggplot(data= airbnb_d())+
      geom_point(aes(x= lat(),y=long()),colour="blue") +
      ggtitle(paste("Existing listings in Neighbourhood -",heading())) +
      xlim(52.4,52.65) +
      ylim(13.1,13.75) +
      xlab('latitude') + ylab('longitude') +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$gbar <- renderPlot({
  ggplot(data=data_gg_data(),aes(x=data_gg_x(),y=data_gg_y())) + geom_bar(stat="identity", fill = "blue") + 
      scale_x_discrete(limits = c("December","January","February","March","April","May","June","July","August","September")) +
      ggtitle(paste("Occupancy rates for",heading())) + xlab("Month") + ylab ("Occupancy Rate") +
      theme(plot.title = element_text(hjust = 0.5))
  })

}

shinyApp(ui = ui, server = server)
