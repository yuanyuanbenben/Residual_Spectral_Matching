# pre process 

# read data from original dataset
setwd('/home/yuanyuanbenben/project_dmc/matrix_test/real_data_example')
data_total <- read.csv("netflix_data.csv")
names(data_total) <- c("movies","id","score","time")
dims <- dim(data_total)[1]
data_total$time <- as.Date(data_total$time)
movies <- as.data.frame(table(data_total$movies))

# select movies for recommendation systems (movies that rated more than 25000)
used_movies <- as.numeric(levels(droplevels(movies[movies$Freq>25000,1])))
data <- data_total[data_total$movies %in% used_movies,]
ids <- as.data.frame(table(data$id))

# users rating more than 700 times
# used_id <- as.numeric(levels(droplevels(ids[ids$Freq>700,1])))

# select a subgroup users for rating more than 30 times (randomly select 3000 users)
used_id <- as.numeric(levels(droplevels(ids[ids$Freq>30,1])))
data <- data[data$id %in% used_id,]
set.seed(20240504)
used_id <- sample(used_id,3000,replace=FALSE)


data <- data[data$id %in% used_id,]

m = length(used_id)
n = length(used_movies)
sample_size = length(data$movies)
X1_total = rep(0,sample_size)
X2_total = rep(0,sample_size)
Y_total = rep(0,sample_size)

for (k in 1:sample_size) {
  X1_total[k] <- which(used_id == data$id[k])
  X2_total[k] <- which(used_movies == data$movies[k])
  Y_total[k] <- data$score[k]
}


# train, validation and test set
train_index = sample.int(sample_size,round(sample_size*4/6),replace = FALSE)

X1_train = X1_total[train_index]
X2_train = X2_total[train_index]
Y_train = Y_total[train_index]


X1_rest = X1_total[-train_index]
X2_rest = X2_total[-train_index]
Y_rest = Y_total[-train_index]

validation_index = sample.int(length(X1_rest),round(length(X1_rest)/2),replace = FALSE)

X1_validation = X1_rest[validation_index]
X2_validation = X2_rest[validation_index]
Y_validation = Y_rest[validation_index]

X1_test = X1_rest[-validation_index]
X2_test = X2_rest[-validation_index]
Y_test = Y_rest[-validation_index]

save.image("real_data_example1.RData")

write.csv(data.frame(X1_train,X2_train,Y_train),'data/case1_traindata.csv')
write.csv(data.frame(X1_validation,X2_validation,Y_validation),'data/case1_validationdata.csv')
write.csv(data.frame(X1_test,X2_test,Y_test),'data/case1_testdata.csv')


######################################################################################################
# select movies for recommendation systems (movies that rated more than 25000)
used_movies <- as.numeric(levels(droplevels(movies[movies$Freq>25000,1])))
data <- data_total[data_total$movies %in% used_movies,]
ids <- as.data.frame(table(data$id))

# select a subgroup users for completion (randomly select 3000 users)
used_id <- as.numeric(levels(droplevels(ids[ids$Freq>700,1])))
data <- data[data$id %in% used_id,]
set.seed(20240504)
# used_id <- sample(used_id,3000,replace=FALSE)
# data <- data[data$id %in% used_id,]

m = length(used_id)
n = length(used_movies)
sample_size = length(data$movies)
X1_total = rep(0,sample_size)
X2_total = rep(0,sample_size)
Y_total = rep(0,sample_size)

for (k in 1:sample_size) {
  X1_total[k] <- which(used_id == data$id[k])
  X2_total[k] <- which(used_movies == data$movies[k])
  Y_total[k] <- data$score[k]
}


# train, validation and test set
train_index = sample.int(sample_size,round(sample_size*4/6),replace = FALSE)

X1_train = X1_total[train_index]
X2_train = X2_total[train_index]
Y_train = Y_total[train_index]


X1_rest = X1_total[-train_index]
X2_rest = X2_total[-train_index]
Y_rest = Y_total[-train_index]

validation_index = sample.int(length(X1_rest),round(length(X1_rest)/2),replace = FALSE)

X1_validation = X1_rest[validation_index]
X2_validation = X2_rest[validation_index]
Y_validation = Y_rest[validation_index]

X1_test = X1_rest[-validation_index]
X2_test = X2_rest[-validation_index]
Y_test = Y_rest[-validation_index]

save.image("real_data_example2.RData")

write.csv(data.frame(X1_train,X2_train,Y_train),'data/case2_traindata.csv')
write.csv(data.frame(X1_validation,X2_validation,Y_validation),'data/case2_validationdata.csv')
write.csv(data.frame(X1_test,X2_test,Y_test),'data/case2_testdata.csv')

#####################################################################################################

# select movies for recommendation systems (movies that rated more than 25000)
used_movies <- as.numeric(levels(droplevels(movies[movies$Freq>25000,1])))
data <- data_total[data_total$movies %in% used_movies,]
ids <- as.data.frame(table(data$id))

# select a subgroup users for rating more than 200 times (randomly select 3000 users)
used_id <- as.numeric(levels(droplevels(ids[ids$Freq>200,1])))
data <- data[data$id %in% used_id,]
set.seed(20240504)
used_id <- sample(used_id,3000,replace=FALSE)

data <- data[data$id %in% used_id,]

m = length(used_id)
n = length(used_movies)
sample_size = length(data$movies)
X1_total = rep(0,sample_size)
X2_total = rep(0,sample_size)
Y_total = rep(0,sample_size)

for (k in 1:sample_size) {
  X1_total[k] <- which(used_id == data$id[k])
  X2_total[k] <- which(used_movies == data$movies[k])
  Y_total[k] <- data$score[k]
}


# train, validation and test set
train_index = sample.int(sample_size,round(sample_size*4/6),replace = FALSE)

X1_train = X1_total[train_index]
X2_train = X2_total[train_index]
Y_train = Y_total[train_index]


X1_rest = X1_total[-train_index]
X2_rest = X2_total[-train_index]
Y_rest = Y_total[-train_index]

validation_index = sample.int(length(X1_rest),round(length(X1_rest)/2),replace = FALSE)

X1_validation = X1_rest[validation_index]
X2_validation = X2_rest[validation_index]
Y_validation = Y_rest[validation_index]

X1_test = X1_rest[-validation_index]
X2_test = X2_rest[-validation_index]
Y_test = Y_rest[-validation_index]

save.image("real_data_example3.RData")

write.csv(data.frame(X1_train,X2_train,Y_train),'data/case3_traindata.csv')
write.csv(data.frame(X1_validation,X2_validation,Y_validation),'data/case3_validationdata.csv')
write.csv(data.frame(X1_test,X2_test,Y_test),'data/case3_testdata.csv')
 

###################################################################################################

# select movies for recommendation systems (movies that rated more than 25000)
used_movies <- as.numeric(levels(droplevels(movies[movies$Freq>25000,1])))
data <- data_total[data_total$movies %in% used_movies,]
ids <- as.data.frame(table(data$id))

# select a subgroup users for rating more than 75 times (randomly select 3000 users)
used_id <- as.numeric(levels(droplevels(ids[ids$Freq>75,1])))
data <- data[data$id %in% used_id,]
set.seed(20240504)
used_id <- sample(used_id,3000,replace=FALSE)

data <- data[data$id %in% used_id,]

m = length(used_id)
n = length(used_movies)
sample_size = length(data$movies)
X1_total = rep(0,sample_size)
X2_total = rep(0,sample_size)
Y_total = rep(0,sample_size)

for (k in 1:sample_size) {
  X1_total[k] <- which(used_id == data$id[k])
  X2_total[k] <- which(used_movies == data$movies[k])
  Y_total[k] <- data$score[k]
}


# train, validation and test set
train_index = sample.int(sample_size,round(sample_size*4/6),replace = FALSE)

X1_train = X1_total[train_index]
X2_train = X2_total[train_index]
Y_train = Y_total[train_index]


X1_rest = X1_total[-train_index]
X2_rest = X2_total[-train_index]
Y_rest = Y_total[-train_index]

validation_index = sample.int(length(X1_rest),round(length(X1_rest)/2),replace = FALSE)

X1_validation = X1_rest[validation_index]
X2_validation = X2_rest[validation_index]
Y_validation = Y_rest[validation_index]

X1_test = X1_rest[-validation_index]
X2_test = X2_rest[-validation_index]
Y_test = Y_rest[-validation_index]

save.image("real_data_example4.RData")

write.csv(data.frame(X1_train,X2_train,Y_train),'data/case4_traindata.csv')
write.csv(data.frame(X1_validation,X2_validation,Y_validation),'data/case4_validationdata.csv')
write.csv(data.frame(X1_test,X2_test,Y_test),'data/case4_testdata.csv')
