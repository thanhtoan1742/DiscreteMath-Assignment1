library(xlsx)
library(dplyr)

#Read the Excel file
thienan <- read.xlsx2("1.xlsx", sheetIndex = 1)

#Question 7
thienan$Date <- as.Date(thienan$Đã.bắt.đầu.vào.lúc, "%d %B %Y")
pta <- subset(thienan, thienan$Date >= "2020-04-10")
pta2 <- pta %>% count(Mã.số.ID)
nrow(pta2)
