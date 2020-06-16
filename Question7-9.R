library(dplyr)
library(xlsx)
library(ggplot2)


#Read the Excel file
thienan = data.frame()
thienan = read.xlsx("/Users/Tran Dinh Gia Hai/Desktop/CTRR/1.xlsx", sheetIndex = 1)
namesList = c("Ma so ID", "Tinh trang", "Da bat dau vao luc", "Da hoan thanh", "Thoi gian thuc hien", "Diem/10,00", "Q. 1 /1,00", "Q. 2 /1,00", "Q. 3 /1,00", "Q. 4 /1,00", "Q. 5 /1,00", "Q. 6 /1,00", "Q. 7 /1,00", "Q. 8 /1,00", "Q. 9 /1,00", "Q. 10 /1,00")
Encoding(namesList) = "UTF-8"
names(thienan) = namesList
row.names(thienan) = NULL
Encoding(thienan[, 2]) = "UTF-8"
Encoding(thienan[, 5]) = "UTF-8"


#Refine data from Excel file
thienan$'Da bat dau vao luc' <- strptime(thienan$'Da bat dau vao luc', format = "%d %B %Y  %I:%M %p")
thienan$'Diem/10,00' <- suppressWarnings(as.numeric(sub(",", ".", thienan$'Diem/10,00', fixed = TRUE)))


#Question 7
pta1 <- thienan
pta1 <- subset(pta1, pta1$'Da bat dau vao luc' >= "2020-04-10 00:00:00")
pta1 <- pta1[!duplicated(pta1$'Ma so ID'),]
pta1 %>% ggplot(aes(x = pta1$'Diem/10,00')) + geom_bar(width = 0.2, fill = "#FF6666") + xlab("Mark") + ylab("Count") + ggtitle("Mark Range Q7")
print("So hoc sinh hoc doi pho la: ") 
print(nrow(pta1))


#Question 9
pta2 <- thienan
pta2 <- pta2 %>% group_by(pta2$'Ma so ID') %>% arrange(pta2$'Da bat dau vao luc') %>% slice(1:1)
pta2 <- subset(pta2, pta2$'Diem/10,00' >= 9 & pta2$'Ma so ID' > 1)
pta2 %>% ggplot(aes(x = pta2$'Diem/10,00')) + geom_bar(width = 0.2, fill = "#FF6666") + xlab("Mark") + ylab("Count") + ggtitle("Mark Range Q9")
print("So hoc sinh thong minh la: ") 
print(nrow(pta2))

#Refine data: Chuẩn hóa định dạng ngày tháng và thời gian để tiện cho tính toán về sau. Chuẩn hóa điểm thành dạng số
#Q7: 1. Đưa dữ liệu trong data frame thienan vào pta1 để không làm ảnh hưởng dữ liệu gốc khi tính toán
#    2. Dùng subset lọc những phần tử của column "Da bat dau vao luc" phù hợp với t2 (Muốn lọc với t2 khác thì sửa thời gian trong code)
#    3. Xóa những lần nộp bài sau, chỉ xét lần nộp bài đầu tiên của mỗi sinh viên
#    4. Vẽ biểu đồ phổ điểm dạng cột (với x là các giá trị điểm, y là số lượng sinh viên đạt điểm đó) cho lần nộp đầu của các sinh viên học đối phó
#    5. In ra số lượng sinh viên học đối phó bằng cách đếm số dòng của data frame đã lọc ở bước 3
#Q9: 1. Đưa dữ liệu trong data frame thienan vào pta2 để không làm ảnh hưởng dữ liệu gốc khi tính toán
#    2. 
#    3. Xóa những lần nộp bài sau, chỉ xét lần nộp bài đầu tiên của mỗi sinh viên
#    4. Vẽ biểu đồ phổ điểm dạng cột (với x là các giá trị điểm, y là số lượng sinh viên đạt điểm đó) cho lần nộp đầu của các sinh viên học đối phó
#    5. In ra số lượng sinh viên học đối phó bằng cách đếm số dòng của data frame đã lọc ở bước 3
