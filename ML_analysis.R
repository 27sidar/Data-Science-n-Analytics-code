library(ggplot2)
library(reshape2)
library(data.table)
library(RPostgres)
library(lubridate)
library(dplyr)
library(tidyr)
library(bit64)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(ggplot2)
library(lattice)
library(caret)
library(smotefamily)
library(NeuralNetTools)
library(carData)
library(car)
library(corrplot)
library(kernlab)
library(e1071)
library(naivebayes)
library(randomForest)
library(RSNNS)
library(RColorBrewer)
library(pdp)
library(plotly)
library(withr)




#### preprocess our tax filings data
# i pull tax historic filings in the us from: https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi

# the different data comes in different formats and columns each year; need to deal with that
# for the sake of clarity and overview, set the wd to the folder with the respective tax filings data csv files
setwd("~/Desktop/Uni/Master/SoSe 24/Data Science/code/final/submit/Tax stats data")
files <- list.files(pattern = "*.csv")
interested_columns <- c('zipcode', 'agi_stub', 'n1', 'mars1', 'mars2', 'elderly', 'n02300', 'a02300')
columns <- list()
# now lets see which variables we are interested are to be found in which file / year of filings
for (file in files) {
  first_line <- readLines(file, n = 1)
  column_names <- strsplit(first_line, ",")[[1]]
  column_names <- tolower(column_names) # i standardize every column so lower / upper case does not make a diff
  columns_in_file <- interested_columns %in% column_names# witht his i check which variable is found in which file
  columns[[file]] <- setNames(columns_in_file, interested_columns) # this stores the results 
}

print(columns)




# now we can graphically see which variables are to be found in which file

df <- data.frame(do.call(rbind, columns))
df$file <- rownames(df) # getting the filenames
df_long <- reshape2::melt(df, id.vars = "file") # long format; i use melt to get the long format

ggplot(df_long, aes(x = variable, y = file, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = c("red", "cadetblue3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Variable", y = "File", fill = "Present", title = "Which variables are missing")


# so now we found out that mars1 (the filings of single people) is missing in our 2011 data
# and the variable elderly (the filings made by elderly people in a zipcode are) is not available until 2014

# paired with the statistic for smartphone market penetration, 2018 seems as a further legitimate step to encolse the data. Used data for tax filings will range therefore from 2018 - 2021 due to computaional restrictions and the nature of the paper to examplary show the process of deploying ML models




# for now we will use from 2018 on data
#### consolidate all files together ####
if(1){ 
  all_years_data <- list()
  for(year in 2018:2021) {
    file_name <- paste0(substr(year, 3, 4), "zpallagi.csv")
    header <- fread(file_name, nrows = 0)# first i identify the headers to later on only read those headers in. Since not every file has the same upper / lower case syntax for each variable i insert some pre steps
    names(header) <- tolower(names(header))
    needed_cols <- c('zipcode', 'agi_stub', 'n1', 'mars1', 'mars2', 'elderly', 'n02300')
    cols_positions <- match(needed_cols, names(header)) # find the positions of the columns we need in the speicific file
    data <- fread(file_name, select = cols_positions, col.names = needed_cols)
    # transform our data to a way we can use in our ML models. Here I calculate e.g. the mean wealthiness of a zip code or the amount of elderly people in a zip code etc.
    data <- data %>%
      group_by(zipcode) %>%
      summarise(
        wealthiness_mean = sum(agi_stub * n1) / sum(n1),
        wealthiness_median = weightedMedian(agi_stub, w = n1),
        elderly_people = sum(elderly) / sum(n1),
        married = sum(mars2) / sum(n1),
        singles = sum(mars1) / sum(n1),
        unemployment = sum(n02300) / sum(n1),
        year = year  
      )
    all_years_data[[as.character(year)]] <- data
  }
  all_years_data <- bind_rows(all_years_data)
}




# then check for missing values and impute if necessary
missing_values <- is.na(all_years_data)
any_missing <- any(missing_values)
print(any_missing)
# so we indeed have no missing values from the tax filings (which makes sense since the ones needed were the ones collected)







### SQL Queries
# for overview purposes i set the wd again to the entry level where the code is running
setwd('~/Desktop/Uni/Master/SoSe 24/Data Science/code/final/submit')

# to establish a connection i had to run the code on the Posit R environment provided by the universities servers; not possible on own laptop
con <- dbConnect(RPostgres::Postgres(),dbname="remoteuser",host="localhost",port="5432",
                 user = "dsma_student", password = "DSMA_Stud23")



execute_sql_file <- function(file_name) {
  file_path <- paste0("./sql queries/", file_name, ".sql")
  sql_query <- readChar(file_path, file.info(file_path)$size)
  data <- dbGetQuery(con, statement = sql_query)
  return(data)
}

business_data <- execute_sql_file("business")
reviews_data <- execute_sql_file('reviews')
tips_data <- execute_sql_file('tips')
users_data <- execute_sql_file('users')


#merge business x reviews
final_data <- merge(business_data, reviews_data, by = 'business_id')

#merge this now with tips data
final_data <- merge(final_data, tips_data, by.x = c("business_id", "review_year"), by.y = c("business_id", "tip_year"), all.x = TRUE) # this is where the NAs in total_compoliments & total_tips come from (since i use the whole dataset of themerged busines x reviews and map whats possible from tips. So NAs are effectively 0)
# this merged data will have non values in it since i keep all the rows from the initial merged frame of business x reviews. Therefore we still need to set these na s to 0 to not let them play into our models later or treat them as missings. it just means that there is no tip for that specific restaurant or year in that row-
final_data <- merge(final_data, users_data, by.x = c("business_id", "review_year"), by.y = c("business_id", "review_year"))

# for checkins: see the other file; i do the calcualtions directly in R since it takes too long to run on the SQL server itself
checkin_data <- dbGetQuery(con,
                           "SELECT 
                           checkin.j->>'business_id' AS business_id, 
                           checkin.j->>'date' AS checkin_date
                           FROM 
                           public.checkin
                           WHERE 
                           checkin.j->>'business_id' IN (
                           SELECT 
                           business.j->>'business_id' AS business_id
                           FROM 
                           public.business
                           WHERE 
                           business.j->>'categories' LIKE '%Restaurant%')")



checkin_data <- checkin_data %>% # stretch out checkin data by seperating each checkin
  separate_rows(checkin_date, sep = ",") %>% 
  mutate(checkin_date = ymd_hms(checkin_date)) 

checkin_data$year <- year(checkin_data$checkin_date)
# Group by business_id and year, and calculate the total check-ins
checkin_data <- checkin_data %>%
  group_by(business_id, year) %>%
  summarise(check_ins_this_year = n(), .groups = "drop") %>%
  replace_na(list(check_ins_this_year = 0))


# now one final merge
final_data <- merge(final_data, checkin_data, by.x = c('business_id', 'review_year'), by.y = c('business_id', 'year'), all.x = TRUE)





### missing values ###
# lets check for missing values in our data now
missing_values <- apply(final_data, 2, function(x) any(is.na(x)))
na_columns <- names(final_data)[missing_values]
if(length(na_columns) > 0) {
  print(na_columns)
} else {
  print('No NAs in your data')
}


# are na really 0??
# in our final frame we still have NAs which result from the mapping process (see above). These are effectively 0s though, since they represent totals of non existent sums (e.g. tips or compliments)  so i set all NAs to 0.
# for check_ins_this_year na means 0 check ins this year. lets turn na's into zeros (since we map and keep also data w/o ch_ins):
na_cols <- c('check_ins_this_year', 'total_tips', 'total_compliments')
final_data[na_cols][is.na(final_data[na_cols])] <- 0


# also need to transform the dtypes to be able to do calculations later on
str(final_data)
integer64_cols <- names(final_data)[sapply(final_data, is.integer64)]
final_data[integer64_cols] <- lapply(final_data[integer64_cols], function(x) as.integer(as.numeric(x)))


#sanity check again:
missing_values <- apply(final_data, 2, function(x) any(is.na(x)))
na_columns <- names(final_data)[missing_values]
if(length(na_columns) > 0) {
  print(na_columns)
} else {
  print('No NAs in your data')
}
# as we will continue without attributes as predictors, we will ignore the missing values in attributes. I still let it in the final dataframe for potential checks later on







#### Analysis 
setwd('/Users/Sidar/Desktop/Uni/Master/SoSe 24/Data Science/code/final/submit')

# now lets merge the 2 files from our 2 seperate preprocessings together
data <- merge(final_data, all_years_data, by.x = c('postal_code', 'review_year'), by.y = c('zipcode', 'year'))
missing_values <- apply(data, 2, function(x) any(is.na(x)))
print(missing_values)

# a bit of clean up
rm(all_years_data, final_data)


# transforming the dependent variable
# the performance of the restaurant will be messured in check ins
# therefore we want the check ins to be meassured in 0 (-> no check in) and 1 (-> check in) - in terms of good or bad check in behavior according to our threshold value
data$check_in_factor <- as.factor(ifelse(data$check_ins_this_year >= 1, "check_in", "no_check_in"))
data$check_in_factor <- relevel(data$check_in_factor,ref="no_check_in")


#####
# descriptive statistics
# those should be done before applying SMOTE

# overview over the data
names(data)
dim(data)
head(data)
tail(data)
summary(data)


# graph on star ratings
# make sure to only count each business once
unique_businessid <- distinct(data, business_id, stars)
mean_stars <- mean(unique_businessid$stars)
median_stars <- median(unique_businessid$stars)

hist_vector <- hist(unique_businessid$stars,
                    main = "distribution of ratings (by all users)",
                    xlab = "star rating",
                    yaxt = "n",
                    col = "aquamarine3",
                    breaks = seq(floor(min(unique_businessid$stars)), ceiling(max(unique_businessid$stars)), by = 0.5),
                    freq = TRUE)
# labeling
text(hist_vector$mids, hist_vector$counts, labels = hist_vector$counts, adj = c(0.5, -0.5))
segments(mean_stars, 0, mean_stars, max(hist_vector$counts), col = "brown4", lty = 2)
text(mean_stars, max(hist_vector$counts), paste("Mean:", round(mean_stars, 2)), pos = 2.2, col = "brown4")
segments(median_stars, 0, median_stars, max(hist_vector$counts), col = "blue", lty = 2)
text(median_stars, max(hist_vector$counts), paste("Median:", round(median_stars, 2)), pos = 4, col = "blue")



# graph on star ratings - elite users only
unique_businessid_elite <- distinct(data, business_id, elite_stars_avg)
mean_stars_elite <- mean(unique_businessid_elite$elite_stars_avg)
median_stars_elite <- median(unique_businessid_elite$elite_stars_avg)

hist_vector_elite <- hist(unique_businessid_elite$elite_stars_avg,
                          main = "distribution of ratings (by elite users only)",
                          xlab = "star rating",
                          yaxt = "n",
                          col = "aquamarine3",
                          breaks = seq(floor(min(unique_businessid_elite$elite_stars_avg)), ceiling(max(unique_businessid_elite$elite_stars_avg)), by = 0.5),
                          freq = TRUE)
# labeling
text(hist_vector_elite$mids, hist_vector_elite$counts, labels = hist_vector_elite$counts, adj = c(0.5, -0.5))
segments(mean_stars_elite, 0, mean_stars_elite, max(hist_vector_elite$counts), col = "brown4", lty = 2)
text(mean_stars_elite, max(hist_vector_elite$counts), paste("Mean:", round(mean_stars_elite, 2)), pos = 2, col = "brown4")
segments(median_stars_elite, 0, median_stars_elite, max(hist_vector_elite$counts), col = "blue", lty = 2)
text(median_stars_elite, max(hist_vector_elite$counts), paste("Median:", round(median_stars_elite, 2)), pos = 1, col = "blue")




#####
# correlation
# to check, which varibales cannot both be taken into consideration
selected_for_corr_check <- data[, c("stars", "review_count", "avg_review_rating", "total_useful_votes",
                                    "total_funny_votes", "total_cool_votes", "total_reviews", "total_tips",
                                    "total_compliments", "elite_reviews", "n_fans_reviews", "total_friends",
                                    "elite_stars_avg", "wealthiness_mean", "wealthiness_median",
                                    "elderly_people", "married", "singles", "unemployment")]
cor_matrix <- cor(selected_for_corr_check, method = "pearson")
print(cor_matrix)

col <- colorRampPalette(c('darkgoldenrod', 'white', 'aquamarine4'))(20)

# Plot the correlation heatmap with the new color scheme
corrplot(cor_matrix, method = "color", type = "lower", na.col = "white", 
         tl.col = "black", tl.cex = 0.9, addrect = TRUE,
         stat = "r", stat.cex = 0.5, is.diag = TRUE, col = col
)


# findings
# several attributes are strongly correlated postive or negative
# decision needed which one to include in the analysis
# avg_review_rating & elite_stars_avg --> going forward with elite_stars_avg
# total_reviews & elite_reviews --> going forward with elite_reviews
# wealthiness_mean & wealthiness_median --> going forward with wealthiness_median
# married & singles --> going forward with married


#####
# regression analysis
# just as a starting analysis to get a feeling for the behavior of our data; this will not be subject of the actual analysis later on
reg_anal = glm(check_in_factor ~ stars + review_count + total_useful_votes + total_funny_votes + 
                 total_cool_votes + total_tips + total_compliments + elite_reviews +
                 n_fans_reviews + total_friends + elite_stars_avg + wealthiness_median +
                 elderly_people + married + unemployment, data = data, family = "binomial")
car::vif(reg_anal)
summary(reg_anal)




#####
# defintion of variables to be used in ML algorithms
# only a subset of the available variables are going to be used
# therfore we store the "old" data as another dataframe
# and only keep the relevant attributes in the "working" dataframe
# create a list of attributes, that should be "kept"
relevant_attributes <- c("check_in_factor", "check_ins_this_year","stars", "review_count", "total_useful_votes", "total_funny_votes",
                         "total_cool_votes", "total_tips", "total_compliments", "elite_reviews",
                         "n_fans_reviews", "total_friends", "elite_stars_avg", "wealthiness_median",
                         "elderly_people", "married", "unemployment")
# store all available data under new name
data_not_used <- data
# only keep data as stated in list "relevant_attributes"
data <- data[, relevant_attributes]
# check to see if desired subset was created
print(relevant_attributes)
print(names(data))


#####
# divide dataset into training-data and test-data
# for reproducibility reasons we need to define a "seed"
set.seed(66)
# TODO
# adjust the datasize
datasetsize <- nrow(data)/2
# random sampling without replacement
working_sample <- data[sample(1:nrow(data), datasetsize, replace = F),]
# split data into 75% training and 25% testing data
sample.train <- working_sample[1:floor(nrow(working_sample)*.75), ]
sample.test <- working_sample[(floor(nrow(working_sample)*.75)+1):nrow(working_sample), ]


#####
# defining the formula as a placeholder for our ML training expressions later on
BaseFormular_factor <- as.formula("check_in_factor ~ stars + review_count + total_useful_votes +
                          total_funny_votes + total_cool_votes + total_tips + total_compliments +
                          elite_reviews + n_fans_reviews + total_friends + elite_stars_avg + unemployment + wealthiness_median + elderly_people")
BaseFormular_normal <- as.formula("check_ins_this_year ~ stars + review_count + total_useful_votes +
                          total_funny_votes + total_cool_votes + total_tips + total_compliments +
                          elite_reviews + n_fans_reviews + total_friends + elite_stars_avg + unemployment + wealthiness_median + elderly_people")



#####
# deal with class imbalance

# SMOTE
# Check the size of the minority class
imbalance_table_pre_smote = table(sample.train[,"check_in_factor"])
print(imbalance_table_pre_smote)
# pie chart to demonstrate imbalance pre SMOTE
pie(imbalance_table_pre_smote,
    labels = c("no_check_in","check_in"),
    col = c("burlywood2", "aquamarine4"),
    border = "black",
    main = "data imbalance pre 'SMOTE'")



if(1){
  sample.trainsmote = SMOTE(sample.train[,-c(1,2)], sample.train[,1])$data
  names(sample.trainsmote)[ncol(sample.trainsmote)] = "check_in_factor"
  sample.trainsmote$check_ins_this_year = ifelse(sample.trainsmote$check_in_factor == "check_in", 1, 0)
  sample.trainsmote$check_in_factor = as.factor(sample.trainsmote$check_in_factor)
  sample.train = sample.trainsmote
  rm(sample.trainsmote)
}

imbalance_table_post_smote = table(sample.train[,"check_in_factor"])
print(imbalance_table_post_smote)


# pie chart to demonstrate imbalance post SMOTE
pie(imbalance_table_post_smote,
    labels = c("no_check_in","check_in"),
    col = c("burlywood2", "aquamarine4"),
    border = "black",
    main = "data imbalance post 'SMOTE'")


#####
# normalize data
sample.trainnorm = predict(preProcess(sample.train, method = "range"), newdata=sample.train)
sample.testnorm = predict(preProcess(sample.test, method = "range"), newdata=sample.test)

# check if everything worked out
summary(sample.trainnorm)
summary(sample.testnorm)

# redo the releveling:
sample.trainnorm$check_in_factor = relevel(sample.trainnorm$check_in_factor, ref="no_check_in") 
sample.testnorm$check_in_factor = relevel(sample.testnorm$check_in_factor, ref="no_check_in")

# check if everything worked out
summary(sample.trainnorm)
summary(sample.testnorm)




#####
# ML algorithms
# threshold probability will be set to the portion of 1s (check ins) 
probthreshold = mean(sample.trainnorm$check_ins_this_year)


# this function will be used with each ML model - i take the given one from the lecture to keep the syntax if problems may occur later on 
makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  # plots the liftplot, and computes the GINI coefficient.
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$check_in_factor[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate$check_in_factor == "check_in") #total number of real churners in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted=="check_in"),2,cumsum)/SumChurnReal #cumulative fraction of churners
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="check_in")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
  ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of check-ins (sorted by predicted check-in probability)",ylab="Cumulative fraction of check-ins")
  lines(c(0,1),c(0,1),col="brown4",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","brown4"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}






##### ML models

#####
# LOGIT
# model
start_time <- Sys.time()
sample.modelLogit <- glm(BaseFormular_factor, data = sample.trainnorm, family = "binomial") # estimating the probability of "checkin"

summary(sample.modelLogit)

# performance of model
sample.test$predictionlogit = predict(sample.modelLogit, newdata = sample.testnorm, type = "response") # predict the prob of check ins witht given vector of input variables (characteristics). if > prob of threshold then check in; if not no check in
sample.test$predictionlogitclass[sample.test$predictionlogit > probthreshold] <- "check_in" # classifiy the predicted probability into classes
sample.test$predictionlogitclass[sample.test$predictionlogit <= probthreshold] <- "no_check_in"# ""

sample.test$correctlogit <- sample.test$predictionlogitclass == sample.test$check_in_factor # count how often we predcited right
print(paste("% of predicted classifications correct", mean(sample.test$correctlogit)))
LogitOutput <- makeLiftPlot(sample.test$predictionlogit,sample.test,"Logit")

end_time <- Sys.time()
elapsed_time <- end_time - start_time
LogitOutput$PercCorrect <- mean(sample.test$correctlogit) * 100
LogitOutput$training_time <- round(elapsed_time, digits = 2)


#####
# Naive Bayes
# model
##### enables parallel running if supported by the package
cl <- makeCluster(detectCores())
registerDoParallel(cl)
start_time <- Sys.time()
sample.modelNB <- train(BaseFormular_factor, data = sample.trainnorm, method="naive_bayes")

# performance of model
sample.test$predictionNB <- predict(sample.modelNB, newdata = sample.testnorm, type = "prob")

sample.test$predictionNBclass[sample.test$predictionNB[, "check_in"] > probthreshold] = "check_in"
sample.test$predictionNBclass[sample.test$predictionNB[, "check_in"] <= probthreshold] = "no_check_in"

sample.test$correctNB <- sample.test$predictionNBclass == sample.test$check_in_factor
print(paste("% of predicted classifications correct", mean(sample.test$correctNB)))

# the variable importance
print(varImp(sample.modelNB))

# Extract the class probabilities.
sample.test$predictionNB <- sample.test$predictionNB[, "check_in"]

NBOutput <- makeLiftPlot(sample.test$predictionNB, sample.test, "NB")

end_time <- Sys.time()
elapsed_time <- end_time - start_time
NBOutput$PercCorrect <- mean(sample.test$correctNB) * 100
NBOutput$training_time <- round(elapsed_time, digits = 2)
stopCluster(cl)



#####
# KNN
# model
cl <- makeCluster(detectCores())
registerDoParallel(cl)
start_time <- Sys.time()
sample.modelKNN <- train(BaseFormular_factor, data = sample.trainnorm, method = "knn")

# performance of model
sample.test$predictionKNN <- predict(sample.modelKNN, newdata = sample.testnorm, type = "prob")

sample.test$predictionKNNclass[sample.test$predictionKNN[, "check_in"] > probthreshold] = "check_in"
sample.test$predictionKNNclass[sample.test$predictionKNN[, "check_in"] <= probthreshold] = "no_check_in"

sample.test$correctKNN <- sample.test$predictionKNNclass == sample.test$check_in_factor
print(paste("% of predicted classifications correct", mean(sample.test$correctKNN)))

# the variable importance
print(varImp(sample.modelKNN))

# Extract the class probabilities.
sample.test$predictionKNN <- sample.test$predictionKNN[, "check_in"]

KNNOutput <- makeLiftPlot(sample.test$predictionKNN, sample.test, "KNN")

end_time <- Sys.time()
elapsed_time <- end_time - start_time
KNNOutput$PercCorrect <- mean(sample.test$correctKNN) * 100
KNNOutput$training_time <- round(elapsed_time, digits = 2)
stopCluster(cl)



#####
# Neural Networks 
# model
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# fast trainer using parallel computations
start_time <- Sys.time()
# now i define the grid for hyperparameter tuning, i.e. the different combinations of layers in our neural net later on
mlp_grid = expand.grid(layer1 = c(4, 8),
                       layer2 = c(0, 4),
                       layer3 = c(0, 4))
sample.modelNNet <- train(BaseFormular_factor, data=sample.trainnorm, method='mlpML', tuneGrid=mlp_grid) 

# performance of model
sample.test$predictionNNet <- predict(sample.modelNNet, newdata = sample.testnorm, type="prob")

sample.test$predictionNNetclass[sample.test$predictionNNet[, "check_in"] > probthreshold] = "check_in"
sample.test$predictionNNetclass[sample.test$predictionNNet[, "check_in"] <= probthreshold] = "no_check_in"


sample.test$correctNNet <- sample.test$predictionNNetclass == sample.test$check_in_factor
print(paste("% of predicted classifications correct", mean(sample.test$correctNNet)))

print(varImp(sample.modelNNet))
# plot NNet
if(0){
  NeuralNetTools::plotnet(sample.modelNNet$finalModel)
}
sample.test$predictionNNet <- sample.test$predictionNNet[, "check_in"]

NNetOutput <- makeLiftPlot(sample.test$predictionNNet, sample.test, "Neural Network")

end_time <- Sys.time() 
elapsed_time <- end_time - start_time
#NNetOutput$summary=varImp(sample.modelNNet)
NNetOutput$PercCorrect <- mean(sample.test$correctNNet) * 100
NNetOutput$training_time <- round(elapsed_time, digits = 2)

stopCluster(cl)



#####
# TREE
# model
# fast model using parallel computation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start_time <- Sys.time()
sample.modelTree <- train(BaseFormular_factor, data=sample.trainnorm, method = 'ctree') 

# performance of model
sample.test$predictionTree <- predict(sample.modelTree, newdata = sample.testnorm, type = "prob")

sample.test$predictionTreeClass[sample.test$predictionTree[, "check_in"] > probthreshold] = "check_in"
sample.test$predictionTreeClass[sample.test$predictionTree[, "check_in"] <= probthreshold] = "no_check_in"

sample.test$predictionTreeClass <- factor(sample.test$predictionTreeClass, levels=c("no_check_in", "check_in"))

sample.test$correctTree <- sample.test$predictionTreeClass == sample.test$check_in_factor
print(paste("% of predicted classifications correct", mean(sample.test$correctTree)))

sample.test$predictionTree <- sample.test$predictionTree[, "check_in"]

# to see the importance of the variables
print(varImp(sample.modelTree))

# plot tree, if desired 
if(0){
  plot(sample.modelTree$finalModel, cex = 0.3)
}

TreeOutput <- makeLiftPlot(sample.test$predictionTree, sample.test, "Tree")

end_time <- Sys.time()
elapsed_time <- end_time - start_time
#TreeOutput$summary <- varImp(sample.modelTree)
TreeOutput$PercCorrect <- mean(sample.test$correctTree) * 100
TreeOutput$training_time <- round(elapsed_time, digits = 2)

stopCluster(cl)



#####
# Bagging
# model
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start_time <- Sys.time()
# fast training using parallel computation
sample.modelBagging  <- train(BaseFormular_factor, data=sample.trainnorm, method = "treebag", importance=T)

# performance of model
sample.test$predictionBagging <- predict(sample.modelBagging, newdata=sample.testnorm, type="prob")

sample.test$predictionBaggingClass[sample.test$predictionBagging[, "check_in"] > probthreshold] = "check_in"
sample.test$predictionBaggingClass[sample.test$predictionBagging[, "check_in"] <= probthreshold] = "no_check_in"

sample.test$predictionBaggingClass <- factor(sample.test$predictionBaggingClass, levels = c("no_check_in", "check_in"))

# Calculate the overall accuracy.
sample.test$correctBagging <- sample.test$predictionBaggingClass == sample.test$check_in_factor
print(paste("% of predicted classifications correct", mean(sample.test$correctBagging)))

# Extract the class probabilities.
sample.test$predictionBagging <- sample.test$predictionBagging[, "check_in"]

# to see the importance of the variables
print(varImp(sample.modelBagging))

BaggingOutput <- makeLiftPlot(sample.test$predictionBagging, sample.test, "Bagging")

end_time <- Sys.time()
elapsed_time <- end_time - start_time
#BaggingOutput$summary <- varImp(sample.modelBagging)
BaggingOutput$PercCorrect <- mean(sample.test$correctBagging) * 100
BaggingOutput$training_time <- round(elapsed_time, digits = 2)

stopCluster(cl)



#####
# Boosting
# model
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start_time <- Sys.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
sample.modelBoosting  <- train(BaseFormular_factor, data=sample.trainnorm, method = 'blackboost')#,  method = 'bstTree')

# performance of model
sample.test$predictionBoosting <- predict(sample.modelBoosting, newdata = sample.testnorm, type = "prob")

sample.test$predictionBoostingClass[sample.test$predictionBoosting[, "check_in"] > probthreshold] = "check_in"
sample.test$predictionBoostingClass[sample.test$predictionBoosting[, "check_in"] <= probthreshold] = "no_check_in"

sample.test$predictionBoostingClass <- factor(sample.test$predictionBoostingClass, levels = c("no_check_in","check_in"))

# Calculate the overall accuracy.
sample.test$correctBoosting <- sample.test$predictionBoostingClass == sample.test$check_in_factor
print(paste("% of predicted classifications correct", mean(sample.test$correctBoosting)))

# Extract the class probabilities.
sample.test$predictionBoosting <- sample.test$predictionBoosting[, "check_in"]

# to see the importance of the variables
print(varImp(sample.modelBoosting))

# Make a lift curve
BoostingOutput <- makeLiftPlot(sample.test$predictionBoosting, sample.test, "Boosting")

end_time <- Sys.time()
elapsed_time <- end_time - start_time
#BoostingOutput$summary <- varImp(sample.modelBoosting)
BoostingOutput$PercCorrect <- mean(sample.test$correctBoosting) * 100
BoostingOutput$training_time <- round(elapsed_time, digits = 2)

stopCluster(cl)



#####
# RANDOM FOREST
# model
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start_time <- Sys.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
sample.modelRF <- train(BaseFormular_factor, data = sample.trainnorm, method = "parRF") 

# performance of model
sample.test$predictionRF <- predict(sample.modelRF, newdata=sample.testnorm, type = "prob")

sample.test$predictionRFClass[sample.test$predictionRF[, "check_in"] > probthreshold] = "check_in"
sample.test$predictionRFClass[sample.test$predictionRF[, "check_in"] <= probthreshold] = "no_check_in"

sample.test$predictionRFClass <- factor(sample.test$predictionRFClass, levels=c("no_check_in", "check_in"))

# Calculate the overall accuracy.
sample.test$correctRF <- sample.test$predictionRFClass == sample.test$check_in_factor
print(paste("% of predicted classifications correct", mean(sample.test$correctRF)))

# Extract the class probabilities.
sample.test$predictionRF <- sample.test$predictionRF[, "check_in"]

# to see the importance of the variables
print(varImp(sample.modelRF))

RFOutput <- makeLiftPlot(sample.test$predictionRF, sample.test, "Random Forest")

end_time <- Sys.time()
elapsed_time <- end_time - start_time
#RFOutput$summary <- varImp(sample.modelRF)
RFOutput$PercCorrect <- mean(sample.test$correctRF) * 100
RFOutput$training_time <- round(elapsed_time, digits = 2)

stopCluster(cl)




#####
# graphics to summarize the findings
OverallTDL <- c(LogitOutput$TDL, KNNOutput$TDL, NBOutput$TDL, #SVMOutput$TDL,
                TreeOutput$TDL, BaggingOutput$TDL, BoostingOutput$TDL,
                RFOutput$TDL, NNetOutput$TDL)
OverallGINI <- c(LogitOutput$GINI, KNNOutput$GINI, NBOutput$GINI, #SVMOutput$GINI,
                 TreeOutput$GINI, BaggingOutput$GINI, BoostingOutput$GINI,
                 RFOutput$GINI, NNetOutput$GINI)

ForGraph <- data.frame(OverallTDL, OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs / max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs / max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL / max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI / max(ForGraph$OverallGINI)

op <- par(mar = c(5,4,4,4) + 0.1)


barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])),
        beside = TRUE, yaxt = "n", names.arg =
          c("Logit","KNN", "NB", #"SVM",
            "Tree","Bagg","Boost","RF","NNet"),
        ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), 
        ylab =	"Top Decile Lift", legend = c("TDL","GINI"),
        main="Performance of the Machine Learning Algorithms",
        col = ifelse(rownames(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1") ]))) == "OverallTDL1", "bisque2", "cornflowerblue"),
        cex.names = 1)


axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))


mtext(sapply(c(LogitOutput$training_time, KNNOutput$training_time, NBOutput$training_time,
               # Include SVMOutput if needed
               TreeOutput$training_time, BaggingOutput$training_time, BoostingOutput$training_time,
               RFOutput$training_time, NNetOutput$training_time),
             function(x) {
               if (x > 60) {
                 paste(round(x/60, digits=2), "min", sep="")  
               } else {
                 paste(round(x, digits=2), "sec", sep="") 
               }
             }),
      side = 1, line = 3, cex = 0.9, at = c(2,5,8,11,14,17,20,23))



mtext(c(paste(round(LogitOutput$PercCorrect,digits=1),"%"),
        paste(round(KNNOutput$PercCorrect,digits=1),"%"),
        paste(round(NBOutput$PercCorrect,digits=1),"%"),
        #paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=1),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=1),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=1),"%"),
        paste(round(RFOutput$PercCorrect,digits=1),"%"),
        paste(round(NNetOutput$PercCorrect,digits=1),"%")), side = 1, line = 4, cex = 0.9, at = c(2,5,8,11,14,17,20,23))

mtext("Calc. time", side = 1, line = 3, cex = 0.9, at = -.9)
mtext("% correct", side = 1, line = 4, cex = 0.9, at = -.9)


mtext(c(paste(round(LogitOutput$GINI,digits=3)),
        paste(round(KNNOutput$GINI,digits=3)),
        paste(round(NBOutput$GINI,digits=3)),
        #paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$GINI,digits=3)),
        paste(round(BaggingOutput$GINI,digits=3)),
        paste(round(BoostingOutput$GINI,digits=3)),
        paste(round(RFOutput$GINI,digits=3)),
        paste(round(NNetOutput$GINI,digits=3))), side = 1, line = -18.5, cex = 0.9, at = c(2.5,5.5,8.5,11.5,14.5,17.5,20.5,23.5))






# drawing the TDL/Gini Graphs for each output
par(mfrow=c(2,2))
LogitOutput <- makeLiftPlot(sample.test$predictionlogit, sample.test, "Logit")
KNNOutput <- makeLiftPlot(sample.test$predictionKNN, sample.test, "Nearest Neighbour")
NBOutput <- makeLiftPlot(sample.test$predictionNB, sample.test, "Naive Bayes")
#SVMOutput <- makeLiftPlot(sample.test$predictionSVM, sample.test, "SVM")
windows()
par(mfrow=c(3,2))
TreeOutput <- makeLiftPlot(sample.test$predictionTree, sample.test, "Tree")
BaggingOutput <- makeLiftPlot(sample.test$predictionBagging, sample.test, "Bagging")
BoostingOutput <- makeLiftPlot(sample.test$predictionBoosting, sample.test, "Boosting")
RFOutput <- makeLiftPlot(sample.test$predictionRF, sample.test, "Random Forest")
NNetOutput <- makeLiftPlot(sample.test$predictionNNet, sample.test, "Neural Network")


lift_obj = lift(check_in_factor ~ predictionBagging + predictionBoosting + predictionTree +
                  predictionNNet + #predictionSVM + 
                  predictionKNN + predictionNB + predictionlogit,
                data = sample.test, class = "check_in")

ggplot(lift_obj)





#### 
#variable importances
# i calculate the relative importances to sum up to 1

# for logit we would just take the coefficients (relative)
logit_importance <- abs(coef(sample.modelLogit)[2:length(coef(sample.modelLogit))])  # skip the intercept
logit_importance <- logit_importance / sum(logit_importance)

importance_df_logit <- data.frame(variable = names(logit_importance), importance = logit_importance)
ggplot(importance_df_logit, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", fill = '#458B74') + coord_flip() + labs(title = "Variable Importance - Logit", x = "Variable", y = "Relative Importance") + theme_bw()
importance_df_logit


# for all the models besides logit we will use the built in feature of caret:
models <- list(
  NB = sample.modelNB,
  KNN = sample.modelKNN,
  NNet = sample.modelNNet,
  Tree = sample.modelTree,
  Bagging = sample.modelBagging,
  Boosting = sample.modelBoosting,
  RF = sample.modelRF
)


var_importance_list <- list()

for (model_name in names(models)) {
  model <- models[[model_name]]
  
  try({
    var_imp <- varImp(model)$importance
    if (!is.null(var_imp) && ncol(var_imp) > 0) {
      var_imp <- var_imp / sum(var_imp) # normalize to get the relative importance which sums to 1
      var_importance_list[[model_name]] <- var_imp
    }
  }, silent = TRUE)
}

for (model_name in names(var_importance_list)) {
  var_importance <- var_importance_list[[model_name]]
  if (!is.null(var_importance) && nrow(var_importance) > 0) {  # check if empty
    var_importance_df <- data.frame(
      Variable = rownames(var_importance),
      Importance = var_importance[,1]
    )
    var_importance_df <- var_importance_df[order(var_importance_df$Importance, decreasing = TRUE), ]
    
    print(
      ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = '#458B74') +
        coord_flip() +
        ggtitle(paste("Variable Importance -", model_name)) +
        xlab("Variable") +
        ylab("Relative Importance") +
        theme_bw()
    )
  }
}








# i asked ChatGPT to create a heatmap of all variable importances in once. this is what i got
# 1. Logit Model
logit_importance <- abs(coef(sample.modelLogit)[-1])
logit_importance <- logit_importance / sum(logit_importance)
importance_df_logit <- data.frame(variable = names(logit_importance), importance = logit_importance, model = "Logit")

# 2. Naive Bayes Model
var_imp_nb <- varImp(sample.modelNB)$importance
var_imp_nb <- var_imp_nb / sum(var_imp_nb)
importance_df_nb <- data.frame(variable = rownames(var_imp_nb), importance = var_imp_nb[, 1], model = "NB")

# 3. KNN Model
var_imp_knn <- varImp(sample.modelKNN)$importance
var_imp_knn <- var_imp_knn / sum(var_imp_knn)
importance_df_knn <- data.frame(variable = rownames(var_imp_knn), importance = var_imp_knn[, 1], model = "KNN")

# 4. Neural Network Model
var_imp_nnet <- varImp(sample.modelNNet)$importance
var_imp_nnet <- var_imp_nnet / sum(var_imp_nnet)
importance_df_nnet <- data.frame(variable = rownames(var_imp_nnet), importance = var_imp_nnet[, 1], model = "NNet")

# 5. Decision Tree Model
var_imp_tree <- varImp(sample.modelTree)$importance
var_imp_tree <- var_imp_tree / sum(var_imp_tree)
importance_df_tree <- data.frame(variable = rownames(var_imp_tree), importance = var_imp_tree[, 1], model = "Tree")

# 6. Bagging Model
var_imp_bagging <- varImp(sample.modelBagging)$importance
var_imp_bagging <- var_imp_bagging / sum(var_imp_bagging)
importance_df_bagging <- data.frame(variable = rownames(var_imp_bagging), importance = var_imp_bagging[, 1], model = "Bagging")

# 7. Boosting Model
var_imp_boosting <- varImp(sample.modelBoosting)$importance
var_imp_boosting <- var_imp_boosting / sum(var_imp_boosting)
importance_df_boosting <- data.frame(variable = rownames(var_imp_boosting), importance = var_imp_boosting[, 1], model = "Boosting")

# 8. Random Forest Model
var_imp_rf <- varImp(sample.modelRF)$importance
var_imp_rf <- var_imp_rf / sum(var_imp_rf)
importance_df_rf <- data.frame(variable = rownames(var_imp_rf), importance = var_imp_rf[, 1], model = "RF")

# Combine all importances into one data frame
all_importances <- bind_rows(
  importance_df_logit,
  importance_df_nb,
  importance_df_knn,
  importance_df_nnet,
  importance_df_tree,
  importance_df_bagging,
  importance_df_boosting,
  importance_df_rf
)
all_importances

overall_importance <- all_importances %>%
  group_by(variable) %>%
  summarize(overall_importance = sum(importance)) %>%
  arrange(desc(overall_importance))

# Reorder the variables in the data frame based on the sorted importance
all_importances$variable <- factor(all_importances$variable, levels = rev(overall_importance$variable))
all_importances$model <- factor(all_importances$model, levels = c("Logit", 'KNN', 'NB', 'Tree', 'Bagging', 'Boosting', 'RF', 'NNet')) 

# Create the heatmap
ggplot(all_importances, aes(x = model, y = variable, fill = importance)) +
  geom_tile(color = "white", width = 0.96, height = 0.9) +
  scale_fill_gradient(low = "white", high = "blueviolet") +
  labs(title = "Variable Importance Across Models",
       x = NULL, 
       y = NULL, 
       fill = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(6, 0, 0, 0), "cm")
  ) 







# inspect the best tuning parameters
best_params <- sample.modelNNet$bestTune
print(best_params)




# 2 way pdp
sample.trainnorm_subset <- sample.trainnorm[sample(1:nrow(sample.trainnorm), 1000), ] 

cl <- makeCluster(detectCores())
registerDoParallel(cl)
pdp_plot <- partial(sample.modelNNet, pred.var = c("check_ins_this_year", 'unemployment'), 
                    plot = TRUE, 
                    train = sample.trainnorm_subset,  
                    parallel = TRUE, 
                    progress = "text")
print(pdp_plot)
stopCluster(cl)




# single lines
pdp_plot <- partial(sample.modelNNet, pred.var = "elite_reviews", 
                    plot = TRUE, 
                    train = sample.trainnorm_subset,
                    parallel = TRUE, 
                    progress = "text")
print(pdp_plot)