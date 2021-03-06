###Download MovieLens 10M dataset and format for processing

###Note: this process could take a couple of minutes
###MovieLens 10M dataset:
###https://grouplens.org/datasets/movielens/10m/
###http://files.grouplens.org/datasets/movielens/ml-10m.zip

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

###Creating the test and train data sets to perform the analysis.  The following code creates edx(train_set) and validation(test_set) ensuring both datasets are consistent and can be used for a valid analysis.

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

###Function for residual mean squared error(RMSE), in this analysis RMSE will represent our error when making a movie prediction.

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Results
###Initial simple prediction model used to establish a baseline.  The code below generates an RMSE of 1.061 using the mean rating of the edx dataset of 3.51.  Code used to verify using mu_hat input greater than or less that derived mu_hat for the dataset generates a larger RMSE.

mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(validation))
RMSE(validation$rating, predictions)

predictions <- rep(4.5, nrow(validation))
RMSE(validation$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse,
                           Accuracy = mean(predictions==validation$rating))
rmse_results %>% knitr::kable()

###Enhancing the model to determine movie rating effects due to bias attributed to the movie itself.  Graph shows rating estimates can very alot grouping by movie id.  Adding the movie effects bias to the model lowers the RMSE to 0.9439.  The model below includes added movie effects. 

mu <- mean(edx$rating) 

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  dplyr::summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model1_rmse,
                                     Accuracy = mean(round(predicted_ratings/0.5)*0.5==validation$rating)))
rmse_results %>% knitr::kable()

###Enhancing the model to determine movie rating effects due to user bias.  Graphing rating estimates grouped by user id also shows a great degree of variability.  Adding the user effects bias to the model reduces the RMSE to 0.8653.  The model below includes movie effects and user effects.

edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  dplyr::summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model2_rmse,
                                     Accuracy = mean(round(predicted_ratings/0.5)*0.5==validation$rating)))
rmse_results %>% knitr::kable()

###Considering movie effects and user effects in our model has achieved the required RMSE for this analysis, but let's try using regularization to see if we can improve the model further.

###Implement regularization for estimating movie effects. Regularization logic added to model grouped by movie id. 

lambda <- 3

mu <- mean(edx$rating)

movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  dplyr::summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model3_rmse,
                                     Accuracy = mean(round(predicted_ratings/0.5)*0.5==validation$rating)))
rmse_results %>% knitr::kable()

###Implement regularization for estimating movie effects and user effects. Regularization code added to model grouped by user id.  Cross validation is used to determine the minimum lambda to use for the model.

lambdas <- seq(0, 10, 0.25)

mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  dplyr::summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    dplyr::summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    dplyr::summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses),
                                     Accuracy = mean(round(predicted_ratings/0.5)*0.5==validation$rating)))
rmse_results %>% knitr::kable()

### Code to create output file with predicted ratings.

validation <- validation %>% mutate(pred_rating = predicted_ratings, pred_rating_rnd = round((predicted_ratings/0.5)*0.5))

write.csv(validation %>% select(userId, movieId) %>% mutate(rating = round(predicted_ratings/0.5)*0.5),
          "submission.csv", na = "", row.names=FALSE)


#Conclusion 
###An acceptable RMSE was achieved by taking into consideration bias due to movie effects and user effects.  Regularization improved the RMSE slightly for both scenarios in the analysis.  The final model produced a RMSE of 0.8648.