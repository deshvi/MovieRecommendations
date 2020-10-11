movies <- read.csv("movies.csv", stringsAsFactors = FALSE)

movies <- as.data.frame(movies[movies$genres != '(no genres listed)', ], 
                        stringsAsFactors=FALSE)

movies1 <- movies %>% 
        separate_rows(genres,sep = "\\|") %>% 
        mutate(logical = 1) %>% 
        pivot_wider(names_from = genres, values_from = logical, values_fill = 0) %>% 
        replace(is.na(.), 0)

ratMatrix <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)

ratMatrix <- as.matrix(ratMatrix[,-1])

#install.packages("recommenderlab")
library(recommenderlab)

ratMatrix <- as(ratMatrix, "realRatingMatrix")

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")

recommendation_model$IBCF_realRatingMatrix$parameters

similarity_mat <- similarity(ratMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

movie_similarity <- similarity(ratMatrix[, 1:4], method =
                                       "cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies similarity")

rating_values <- as.vector(ratMatrix@data)
unique(rating_values)

Table_of_Ratings <- table(rating_values) 
Table_of_Ratings

#----
library(ggplot2)
movie_views <- colCounts(ratMatrix) 
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) 
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] 
table_views$title <- NA
for (index in 1:10325){
        table_views[index,3] <- as.character(subset(movies,
                                                    movies$movieId == 
                                                            table_views[index,1])$title)
}
table_views[1:10,]

ggplot(table_views[1:10, ], aes(x = title, y = views)) +
        geom_bar(stat="identity", fill = 'steelblue') +
        geom_text(aes(label=views), vjust=-0.3, size=3.5) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle("Total Views of the Top Films")

movie_ratings <- ratMatrix[rowCounts(ratMatrix) > 50,
                              colCounts(ratMatrix) > 50]
movie_ratings

binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
good_rated_films <- binarize(movie_ratings, minRating = 3)

sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)

top_recommendations <- 10 
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

user1 <- predicted_recommendations@items[[1]] 
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
        movies_user2[index] <- as.character(subset(movies,
                                                   movies$movieId == 
                                                           movies_user1[index])$title)
}
movies_user2
