#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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

#############################################################
#Creat edx(train_set)and validation(test_set)
#############################################################

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#############################################################
#Movie effects
#############################################################

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(validation))
RMSE(validation$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse,
                           Accuracy = mean(predictions==validation$rating))

mu <- mean(edx$rating) 

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  dplyr::summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse,
                                     Accuracy = mean(round(predicted_ratings/0.5)*0.5==validation$rating)))
rmse_results %>% knitr::kable()

#############################################################
#Movie & User effects

#############################################################
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

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse,
                                     Accuracy = mean(round(predicted_ratings/0.5)*0.5==validation$rating)))
rmse_results %>% knitr::kable()

#############################################################
#Movie Regularization

#############################################################
lambda <- 3

mu <- mean(edx$rating)

movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  dplyr::summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse,
                                     Accuracy = mean(round(predicted_ratings/0.5)*0.5==validation$rating)))
rmse_results %>% knitr::kable()

#############################################################
#Movie & User Regularization

#############################################################
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

#############################################################

#############################################################

validation <- validation %>% mutate(pred_rating = predicted_ratings, pred_rating_rnd = round((predicted_ratings/0.5)*0.5))

write.csv(validation %>% select(userId, movieId) %>% mutate(rating = round(predicted_ratings/0.5)*0.5),
          "submission.csv", na = "", row.names=FALSE)

#############################################################