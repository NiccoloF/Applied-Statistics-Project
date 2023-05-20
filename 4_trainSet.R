# split data frame into train and test randomly
set.seed(2108)
train <- sample(seq_len(nrow(data_18_19)), 0.8 * nrow(data_18_19))
test <- setdiff(seq_len(nrow(data_18_19)), train)
train <- data_18_19[train, ]
test <- data_18_19[test, ]
