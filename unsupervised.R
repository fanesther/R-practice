#unsupervised
#k mean cluster
install.packages("factoextra")
library(factoextra)
library(plotly)
#import data
data(iris)
#set input column and split data
iris_data<-iris[,1:4]#set the first column for testing inputs
#decide how many clusters needed
#elbow method, silhouette method, gap statistic
fviz_nbclust(iris_data,
             kmeans,
             method = 'wss'#the elbow methods
             )


#create and train model
kmean_model <- kmeans(
      iris_data, #dataset used for training
      center = 3,#number of cluster used
      nstart = 10)#number of times K means algorithm will run to find better solution

#display clusters
clusters <- kmean_model$cluster


#predict
newdata <- data.frame(
  Sepal.Length = c(5.1,5.5,6.0),
  Sepal.Width = c(3.5,3.0,2.7),
  Petal.Length =c(1.4,4.0,5.5),
  Petal.Width = c(0.2,1.0,2.0)
)
#compute distances from new_data to clusters centers
distances <- as.matrix( #convert the distance object to a matrix format
  stats::dist( #calculates the distances between pairs of rows in the matrix
    rbind( #binds the cluster centers with the new_data to create a new matrix
      kmean_model$centers,newdata)))

#Assign cluster labels based on minmum distance
km_predict<-apply(
  distances[, -seq_len(
    nrow(kmean_model$centers))],
    1,
    which.min)
print(km_predict)
  

#evaluate

#plot the cluster
plot_scatter<- plot_ly(
      data =  iris_data,
      x = ~Sepal.Length,
      y = ~Sepal.Width,
      color = ~clusters,
      type= 'scatter'
)%>%
  layout(
    title = "K_Means Clustering",
    xaxis = list(title = "Sepal Length"),
    yaxis = list(title = "Sepal Width")
  )
plot_scatter

  