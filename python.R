install.packages("keras")
library(keras)
install_keras()
installed.packages("tensorflow")
library(tensorflow)
install_tensorflow()


#load the data
mnist<-import("tensorflow.keras.dataset.mnist")
mnist_data<- mnist$load_data()

#[1]access the training dataset
#[1]access the training column
image_train<- mnist_data[[1]][[1]]
decision_train <- mnist_data[[1]][[2]]
image_test<-mnist_data[[2]][[1]]

decision_test<- mnist_data[[2]][[2]]
# data wrangling and exploration
image_dim <- c(28,28)
image_train<- array_reshape(image_train,
                            c(dim(image_train)[1],
                              prod(image_dim)))
image_test<- array_reshape(image_test,
                            c(dim(image_train)[1],
                              prod(image_dim)))
#plot a frew image
par( #set graphical
  mfrow = c(3,3))
  

for(i in 1:9)
{
  img<-matrix(
    image_train[i,],
    nrow = image_dim[1],
    byrow = TRUE
  )
  image(
    1:image_dim[1],
    1:image_dim[2],
    img,
    col = gray((0:255)/255),
    axes = FALSE
    
  )
}
print(img)

#normalization to set the values to range between 0 and 1
image_train<-array_reshape(image_train,c(nrow(image_train),784))
image_train <- image_train/255

image_test<- array_reshape(image_test,c(nrow(image_test),784))
image_test<- image_test/255

decision_train<- to_categorical(decision_train,
                                num_classes = 10
)
decision_test<-to_categorical(decision_test,num_classes = 10)
#build the NN model
model<- keras_model_sequential()%>%
  layer_dense(units = 256,
              activation = 'relu',
              input_shape = c(784)
              )%>%
  layer_dropout(rate = 0.25)%>%
  layer_dense(units = 128,activation = 'relu')%>%
  layer_dropout(rate = 0.25)%>%
  layer_dense(units = 64,activation = 'relu')%>%
  layer_dropout(rate = 0.25)%>%
  layer_dense(units =10, activation = "softmax")
#compile the model
model %>% compile(
  loss= 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)


#train the model 
train_model <- model %>% fit(
  image_train, decision_train,
  epochs = 10,
  batch_size = 64,
  validation_split = 0.2

)
#prediction
predicitons <- model%>% predict(image_test)%>% k_argmax()
#evaluate the model