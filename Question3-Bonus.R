options(encoding = "utf-8");
library(readxl);
library(xlsx);
library(dplyr);

#Read the Excel file
data = data.frame();
data = read.xlsx("Data\\1.xlsx", sheetIndex = 1);
names(data) = NULL;
row.names(data) = NULL;
Encoding(data[, 2]) = "UTF-8";
Encoding(data[, 5]) = "UTF-8";
print(data);