setwd('/home/yuanyuanbenben/project_dmc/matrix_test/real_data_example2')

#######################################
# Scenario 1
#######################################
# library(rjson)
# library(jsonlite)

# read the data is time consuming...

# data<-jsonlite::stream_in(file("Books.jsonl/Books.jsonl"),pagesize = 100)
# data_used <- data.frame(data$parent_asin,data$asin,data$user_id,data$rating)
# names(data_used) <- c('books_parent_id','books_id','id','score')
# rm(data)
# gc()
# save.image("amazon_data.RData")

# directly load the used raw data
# load("amazon_data.RData")
# 
# dims <- dim(data_used)[1]
# books <- as.data.frame(table(data_used$books_parent_id))
# books_order <- order(books$Freq,decreasing = TRUE)
# books_sorted <- books[books_order,]
# 
# # select books for recommendation systems 
# used_books <- books_sorted[c(1:1000),1]
# data_used <- data_used[data_used$books_parent_id %in% used_books,]
# ids <- as.data.frame(table(data_used$id))
# ids_order <- order(ids$Freq,decreasing = TRUE)
# ids_sorted <- ids[ids_order,]
# save.image("amazon_data_preprocess_books.RData")
# 

load("amazon_data_preprocess_books.RData")
# select a subgroup users for rating (select 3000 users)
used_id <- levels(droplevels(ids[ids$Freq>12,1]))
data_used <- data_used[data_used$id %in% used_id,]
set.seed(20240504)
used_id <- sample(used_id,3000,replace=FALSE)
data_used <- data_used[data_used$id %in% used_id,]

m = length(used_id)
n = length(used_books)
sample_size = length(data_used$books_parent_id)
print(sample_size/m/n)
X1_total = rep(0,sample_size)
X2_total = rep(0,sample_size)
Y_total = rep(0,sample_size)

for (k in 1:sample_size) {
  X1_total[k] <- which(used_id == data_used$id[k])
  X2_total[k] <- which(used_books == data_used$books_parent_id[k])
  Y_total[k] <- data_used$score[k]
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

save.image("amazon_data_test1.RData")

write.csv(data.frame(X1_train,X2_train,Y_train),'data/case1_traindata.csv')
write.csv(data.frame(X1_validation,X2_validation,Y_validation),'data/case1_validationdata.csv')
write.csv(data.frame(X1_test,X2_test,Y_test),'data/case1_testdata.csv')


#######################################
# Scenario 2
#######################################
# library(rjson)
# library(jsonlite)
# 
# # read the data is time consuming...
# 
# data<-jsonlite::stream_in(file("Electronics.jsonl/Electronics.jsonl"),pagesize = 100)
# data_used <- data.frame(data$parent_asin,data$asin,data$user_id,data$rating)
# names(data_used) <- c('asins_parent_id','asins_id','id','score')
# rm(data)
# gc()
# save.image("amazon_data2.RData")

# directly load the used raw data
# load("amazon_data2.RData")
# 
# dims <- dim(data_used)[1]
# asins <- as.data.frame(table(data_used$asins_parent_id))
# asins_order <- order(asins$Freq,decreasing = TRUE)
# asins_sorted <- asins[asins_order,]
# 
# # select items for recommendation systems 
# used_asins <- asins_sorted[c(1:1000),1]
# data_used <- data_used[data_used$asins_parent_id %in% used_asins,]
# ids <- as.data.frame(table(data_used$id))
# ids_order <- order(ids$Freq,decreasing = TRUE)
# ids_sorted <- ids[ids_order,]
# save.image("amazon_data_preprocess_electronics.RData")


load("amazon_data_preprocess_electronics.RData")
# select a subgroup users for rating (select 3000 users)
used_id <- levels(droplevels(ids[ids$Freq>12,1]))
data_used <- data_used[data_used$id %in% used_id,]
set.seed(20240504)
used_id <- sample(used_id,3000,replace=FALSE)
data_used <- data_used[data_used$id %in% used_id,]

m = length(used_id)
n = length(used_asins)
sample_size = length(data_used$asins_parent_id)
print(sample_size/m/n)
X1_total = rep(0,sample_size)
X2_total = rep(0,sample_size)
Y_total = rep(0,sample_size)

for (k in 1:sample_size) {
  X1_total[k] <- which(used_id == data_used$id[k])
  X2_total[k] <- which(used_asins == data_used$asins_parent_id[k])
  Y_total[k] <- data_used$score[k]
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

save.image("amazon_data_test2.RData")

write.csv(data.frame(X1_train,X2_train,Y_train),'data/case2_traindata.csv')
write.csv(data.frame(X1_validation,X2_validation,Y_validation),'data/case2_validationdata.csv')
write.csv(data.frame(X1_test,X2_test,Y_test),'data/case2_testdata.csv')


#######################################
# Scenario 3
#######################################
# library(rjson)
# library(jsonlite)
# 
# # read the data is time consuming...
# 
# data<-jsonlite::stream_in(file("Automotive.jsonl/Automotive.jsonl"),pagesize = 100)
# data_used <- data.frame(data$parent_asin,data$asin,data$user_id,data$rating)
# names(data_used) <- c('asins_parent_id','asins_id','id','score')
# rm(data)
# gc()
# save.image("amazon_data3.RData")
# 
# # directly load the used raw data
# load("amazon_data3.RData")
# 
# dims <- dim(data_used)[1]
# asins <- as.data.frame(table(data_used$asins_parent_id))
# asins_order <- order(asins$Freq,decreasing = TRUE)
# asins_sorted <- asins[asins_order,]
# 
# # select items for recommendation systems
# used_asins <- asins_sorted[c(1:1000),1]
# data_used <- data_used[data_used$asins_parent_id %in% used_asins,]
# ids <- as.data.frame(table(data_used$id))
# ids_order <- order(ids$Freq,decreasing = TRUE)
# ids_sorted <- ids[ids_order,]
# save.image("amazon_data_preprocess_automotive.RData")


load("amazon_data_preprocess_automotive.RData")
# select a subgroup users for rating (select 3000 users)
used_id <- levels(droplevels(ids[ids$Freq>7,1]))
data_used <- data_used[data_used$id %in% used_id,]
set.seed(20240504)
used_id <- sample(used_id,3000,replace=FALSE)
data_used <- data_used[data_used$id %in% used_id,]

m = length(used_id)
n = length(used_asins)
sample_size = length(data_used$asins_parent_id)
print(sample_size/m/n)
X1_total = rep(0,sample_size)
X2_total = rep(0,sample_size)
Y_total = rep(0,sample_size)

for (k in 1:sample_size) {
  X1_total[k] <- which(used_id == data_used$id[k])
  X2_total[k] <- which(used_asins == data_used$asins_parent_id[k])
  Y_total[k] <- data_used$score[k]
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

save.image("amazon_data_test3.RData")

write.csv(data.frame(X1_train,X2_train,Y_train),'data/case3_traindata.csv')
write.csv(data.frame(X1_validation,X2_validation,Y_validation),'data/case3_validationdata.csv')
write.csv(data.frame(X1_test,X2_test,Y_test),'data/case3_testdata.csv')


#######################################
# Scenario 4
#######################################
# library(rjson)
# library(jsonlite)
# 
# # read the data is time consuming...
# 
# data<-jsonlite::stream_in(file("Movies_and_TV.jsonl/Movies_and_TV.jsonl"),pagesize = 100)
# data_used <- data.frame(data$parent_asin,data$asin,data$user_id,data$rating)
# names(data_used) <- c('asins_parent_id','asins_id','id','score')
# rm(data)
# gc()
# save.image("amazon_data4.RData")
# 
# # directly load the used raw data
# load("amazon_data4.RData")
# 
# dims <- dim(data_used)[1]
# asins <- as.data.frame(table(data_used$asins_parent_id))
# asins_order <- order(asins$Freq,decreasing = TRUE)
# asins_sorted <- asins[asins_order,]
# 
# # select items for recommendation systems
# used_asins <- asins_sorted[c(1:1000),1]
# data_used <- data_used[data_used$asins_parent_id %in% used_asins,]
# ids <- as.data.frame(table(data_used$id))
# ids_order <- order(ids$Freq,decreasing = TRUE)
# ids_sorted <- ids[ids_order,]
# save.image("amazon_data_preprocess_movies.RData")


load("amazon_data_preprocess_movies.RData")
# select a subgroup users for rating (select 3000 users)
used_id <- levels(droplevels(ids[ids$Freq>14,1]))
data_used <- data_used[data_used$id %in% used_id,]
set.seed(20240504)
used_id <- sample(used_id,3000,replace=FALSE)
data_used <- data_used[data_used$id %in% used_id,]

m = length(used_id)
n = length(used_asins)
sample_size = length(data_used$asins_parent_id)
print(sample_size/m/n)
X1_total = rep(0,sample_size)
X2_total = rep(0,sample_size)
Y_total = rep(0,sample_size)

for (k in 1:sample_size) {
  X1_total[k] <- which(used_id == data_used$id[k])
  X2_total[k] <- which(used_asins == data_used$asins_parent_id[k])
  Y_total[k] <- data_used$score[k]
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

save.image("amazon_data_test4.RData")

write.csv(data.frame(X1_train,X2_train,Y_train),'data/case4_traindata.csv')
write.csv(data.frame(X1_validation,X2_validation,Y_validation),'data/case4_validationdata.csv')
write.csv(data.frame(X1_test,X2_test,Y_test),'data/case4_testdata.csv')

