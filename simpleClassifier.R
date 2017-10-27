# simple training and testing of a basic binary classifier

simpleClassifier = function(train_dat_filename, test_dat_filename){
  train_dat = read.csv(train_dat_filename)
  test_dat = read.csv(test_dat_filename)
  id = (train_dat[,1] == 1)
  center_1 = apply(train_dat[id, -1], 2, mean)
  center_0 = apply(train_dat[!id, -1], 2, mean)
  s_1 = apply(train_dat[id, -1], 2, var) 
  s_0 = apply(train_dat[!id, -1], 2, var)
  dim(center_1) = c(1,length(center_1))
  dim(center_0) = c(1,length(center_0))
  dim(s_1) = c(1, length(s_1))
  dim(s_0) = c(1, length(s_0))
  ones = matrix(1, nrow = nrow(test_dat), ncol = 1)
  dist_1 = apply((test_dat[, -1] - ones %*% center_1) ^ 2 / (ones %*% s_1), 1, sum)
  dist_0 = apply((test_dat[, -1] - ones %*% center_0) ^ 2 / (ones %*% s_0), 1, sum)
  y_pred = as.numeric(dist_1 < dist_0)
  accuracy = mean(test_dat[, 1] == y_pred)
  return(accuracy)
}