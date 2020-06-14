options(encoding = "utf-8");
library(xlsx);

#Read the Excel file
data = data.frame();
data = read.xlsx("Data\\1.xlsx", sheetIndex = 1);
namesList = c("Mã số ID", "Tình trạng", "Đã bắt đầu vào lúc", "Đã hoàn thành", "Thời gian thực hiện", "Điểm/10,00", "Q. 1 /1,00", "Q. 2 /1,00", "Q. 3 /1,00", "Q. 4 /1,00", "Q. 5 /1,00", "Q. 6 /1,00", "Q. 7 /1,00", "Q. 8 /1,00", "Q. 9 /1,00", "Q. 10 /1,00");
Encoding(namesList) = "UTF-8";
names(data) = namesList;
row.names(data) = NULL;
Encoding(data[, 2]) = "UTF-8";
Encoding(data[, 5]) = "UTF-8";

write.xlsx(data, file = "1_0.xlsx", row.names = FALSE);