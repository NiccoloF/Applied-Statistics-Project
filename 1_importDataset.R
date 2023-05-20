# Import data_set
data <- read.csv("./Data_set/ml_data.csv",
    header = TRUE,
    row.names = 1
)

# different subsections
data_18 <- data[which((rownames(data) >= "2018-01-01 00:00:00" &
    rownames(data) <= "2018-12-31 23:00:00")), ]
data_19 <- data[which((rownames(data) >= "2019-01-01 00:00:00" &
    rownames(data) <= "2019-12-31 23:00:00")), ]
data_20 <- data[which((rownames(data) >= "2020-01-01 00:00:00" &
    rownames(data) <= "2020-12-31 23:00:00")), ]
data_21 <- data[which((rownames(data) >= "2021-01-01 00:00:00" &
    rownames(data) <= "2021-12-31 23:00:00")), ]
data_18_19 <- data[which((rownames(data) >= "2018-01-01 00:00:00" &
    rownames(data) <= "2019-12-31 23:00:00")), ]
data_20_21 <- data[which((rownames(data) >= "2020-01-01 00:00:00" &
    rownames(data) <= "2021-12-31 23:00:00")), ]
