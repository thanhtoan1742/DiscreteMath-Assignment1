options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(readxl);
library(xlsx);

file_number = 4;
tid = 6;
file_in = paste("Data\\", as.character(file_number), ".xlsx", sep = "");
file_out = paste("Result\\", as.character(file_number), ".tsv", sep = "");

to_number = function(a) {
    res = as.numeric(sub(",", ".", a, fixed = TRUE));
    return(res);
}

write_data = function(data) {
  write_data_col_names = TRUE;
  if (is.character(data) | is.numeric(data) | is.vector(data))
    write_data_col_names = FALSE;

  if (is.character(data))
    Encoding(data) = "UTF-8";
  data <- as_tibble(data);
  write_tsv(data, path = file_out, append = TRUE, col_names = write_data_col_names);
}

#Read the Excel file    
thienan = read_excel(file_in, sheet = 1)
namesList = c("Ma so ID", "Tinh trang", "Da bat dau vao luc", "Da hoan thanh", "Thoi gian thuc hien", "Diem/10,00", "Q. 1 /1,00", "Q. 2 /1,00", "Q. 3 /1,00", "Q. 4 /1,00", "Q. 5 /1,00", "Q. 6 /1,00", "Q. 7 /1,00", "Q. 8 /1,00", "Q. 9 /1,00", "Q. 10 /1,00")
Encoding(namesList) = "UTF-8"
names(thienan) = namesList
row.names(thienan) = NULL


#Refine data from Excel file
thienan$'Da bat dau vao luc' <- strptime(thienan$'Da bat dau vao luc', format = "%d %B %Y  %I:%M %p")
thienan$'Diem/10,00' <- suppressWarnings(as.numeric(sub(",", ".", thienan$'Diem/10,00', fixed = TRUE)))


#Question 7
write_data("Câu 7");
pta1 <- thienan
pta1 <- subset(pta1, pta1$'Da bat dau vao luc' >= "2020-04-10 00:00:00")
pta1 <- pta1[!duplicated(pta1$'Ma so ID'),]
pta1 %>% ggplot(aes(x = pta1$'Diem/10,00')) + geom_bar(width = 0.2, fill = "#FF6666") + xlab("Mark") + ylab("Count") + ggtitle("Mark Range Q7")
write_data("So hoc sinh hoc doi pho la: ") 
write_data(nrow(pta1))


#Question 9
write_data("Câu 9");
pta2 <- thienan
pta2 <- pta2 %>% group_by(pta2$'Ma so ID') %>% arrange(pta2$'Da bat dau vao luc') %>% slice(1:1)
pta2 <- subset(pta2, pta2$'Diem/10,00' >= 9 & pta2$'Ma so ID' > 1)
pta2 <- pta2[!duplicated(pta2$'Ma so ID'),]
pta2 %>% ggplot(aes(x = pta2$'Diem/10,00')) + geom_bar(width = 0.2, fill = "#FF6666") + xlab("Mark") + ylab("Count") + ggtitle("Mark Range Q9")
write_data("So hoc sinh thong minh la: ") 
write_data(nrow(pta2))

#Refine data: Chuẩn hóa định dạng ngày tháng và thời gian để tiện cho tính toán về sau. Chuẩn hóa điểm thành dạng số
#Q7: 1. Đưa dữ liệu trong data frame thienan vào pta1 để không làm ảnh hưởng dữ liệu gốc khi tính toán
#    2. Dùng subset lọc những phần tử của column "Da bat dau vao luc" phù hợp với t2 (Muốn lọc với t2 khác thì sửa thời gian trong code)
#    3. Xóa những lần nộp bài sau, chỉ xét lần nộp bài đầu tiên của mỗi sinh viên
#    4. Vẽ biểu đồ phổ điểm dạng cột (với x là các giá trị điểm, y là số lượng sinh viên đạt điểm đó) cho lần nộp đầu của các sinh viên học đối phó
#    5. In ra số lượng sinh viên học đối phó bằng cách đếm số dòng của data frame đã lọc ở bước 3
#Q9: 1. Đưa dữ liệu trong data frame thienan vào pta2 để không làm ảnh hưởng dữ liệu gốc khi tính toán
#    2. Xếp các lần nộp bài của cùng một sinh viên vào một nhóm bằng group_by, hàm arrange sẽ sắp xếp thời gian theo thứ tự tăng dần, sau đó dùng lệnh slide(1:n) để lấy n lần nộp đầu tiên
#    3. Lọc ra những sinh viên trong số n lần nộp đó đạt được điểm k. (Điều kiện "Ma so ID > 1" để loại bỏ kết quả trung bình cuối file Excel)
#    4. Xóa những lần nộp bài sau, chỉ xét lần nộp bài đầu tiên đạt điểm k của mỗi sinh viên
#    5. Vẽ biểu đồ phổ điểm dạng cột (với x là các giá trị điểm, y là số lượng sinh viên đạt điểm đó) cho lần nộp đầu của các sinh viên thông minh
#    6. In ra số lượng sinh viên thông minh bằng cách đếm số dòng của data frame đã lọc được

#t2 or File 1: 2020-04-10 00:00:00
#t2 of File 2: 2020-04-20 00:00:00
#t2 of File 3: 2020-04-24 00:00:00
#t2 of File 3: 2020-04-18 00:00:00
