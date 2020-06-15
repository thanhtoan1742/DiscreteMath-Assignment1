options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(xlsx);

# read data
data <- read.xlsx("0.xlsx", sheetIndex = 1, encoding = "UTF-8", colIndex = 1:16);
data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
names(data) <- namesList;
row.names(data) <- NULL;
data <- as_tibble(data);

# question 5
data <- data %>% filter(total_score >= 0);
data_by_stdid <- data %>% group_by(stdid);
data %>% print(n = nrow(data), width = Inf);




# original data    
data <- as.data.frame(data);
namesList <- c("Mã số ID", "Tình trạng", "Đã bắt đầu vào lúc", "Đã hoàn thành", "Thời gian thực hiện", "Điểm/10,00", "Q. 1 /1,00", "Q. 2 /1,00", "Q. 3 /1,00", "Q. 4 /1,00", "Q. 5 /1,00", "Q. 6 /1,00", "Q. 7 /1,00", "Q. 8 /1,00", "Q. 9 /1,00", "Q. 10 /1,00");
Encoding(namesList) <- "UTF-8";
names(data) <- namesList;
#write.xlsx(data, file = "0.xlsx", row.names = FALSE);