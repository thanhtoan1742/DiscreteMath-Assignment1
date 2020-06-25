options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(readxl);
library(e1071);

file_in = "Data\\1.xlsx";
file_out = "1.tsv";
tid = 6;

reset_file_out = function() {
  write_tsv(as_tibble("tid: 6"), path = file_out, append = FALSE, col_names = FALSE);
}

write_data = function(data) {
  print_col_names = TRUE;
  if (is.character(data) | is.numeric(data) | is.vector(data))
    print_col_names = FALSE;

  if (is.character(data))
    Encoding(data) = "UTF-8";
  data <- as_tibble(data);
  write_tsv(data, path = file_out, append = TRUE, col_names = print_col_names);
}

sum_all = function(data_col,data_size) {
  all_score = 0
  for(i in 1:data_size)
  {
    all_score = all_score + (data_col[i])
  }
  return (all_score)
}

#Get data
DataChart <- read_excel(file_in, sheet = 1);
Data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10");
names(DataChart) <- namesList;
row.names(DataChart) <- NULL;
reset_file_out();

#Question 1
write_data("Câu 1");
write_data("Số sinh viên trong danh sách là:");
write_data(n_distinct(DataChart$stdid));

#Question 2
# 2.a Total Score List
write_data("Câu 2");
for(i in 1:nrow(DataChart))
{
  if(DataChart$total_score[i] == "-")
  {
    DataChart$total_score[i] = "-1,00"
  }
}

DataChart$total_score = gsub(",",".",DataChart$total_score);
DataChart$total_score = as.numeric(DataChart$total_score);
DataChart = subset(DataChart, total_score >= 0)
total_score_list = DataChart$total_score
write_data("a. Tổng số điểm của tất cả các bài nộp:");
write_data(sum(total_score_list));

# 2.b Finding lowest-score
write_data("b. Điểm thấp nhất:");
lowest_score = min(DataChart$total_score);
write_data(lowest_score);

# 2.c) List of student with lowest-score
lowest_list = subset(DataChart, total_score == lowest_score);
lowest_list = lowest_list$stdid;
unique(lowest_list, incomparables = FALSE)
write_data("c. Danh sách sinh viên có bài nộp điểm thấp nhất:");
lowest_list = data.frame(
  "stdid" = lowest_list
)
write_data(lowest_list$stdid);

# 2.d Spectral of submit time from students of lowest score
lowest_list$times = c(0)
for (i in 1:nrow(lowest_list))
{
  stdid_temp = lowest_list$stdid[i];
  for(j in 1:nrow(DataChart))
  {
    stdid_temp_2 = DataChart$stdid[j];
    if(!is.na(stdid_temp_2) && (stdid_temp_2 == stdid_temp))
    {
      lowest_list$times[i] = lowest_list$times[i] + 1;
    }
  }
}
write_data("d. Phổ theo số lần nộp của các sinh viên có ít nhất một nộp đạt điểm thấp nhất:");
lowest_list$times = as.integer(lowest_list$times)
lowest_list_plot = data.frame(
  "times" = unique(lowest_list$times),
  "frequency" = c(0)
)

for(i in 1:nrow(lowest_list_plot))
{
  for(j in 1:nrow(lowest_list))
  if(lowest_list_plot$times[i] == lowest_list$times[j] )
  {
    lowest_list_plot$frequency[i] = lowest_list_plot$frequency[i] + 1
  }
}
write_data(lowest_list_plot);
barplot(lowest_list_plot$frequency,ylim = c(0,10),xlab = 'so lan nop',ylab = 'tan so xuat hien',names.arg = lowest_list_plot$times, main = "Pho tong so lan nop cua nhung hoc sinh co 1 diem thap nhat");

#2.e) Get the lowest student final score
write_data("e. Điểm tổng kết thấp nhất:");
DataChart = subset(DataChart, !(is.na(stdid)) );
stdid_list_e = DataChart;

stdid_list_e_stdid = stdid_list_e$stdid;
stdid_list_e_stdid = unique(stdid_list_e_stdid, incomparables = FALSE)

final_score_chart = data.frame(
  "stdid" = stdid_list_e_stdid,
  "final_score" = c(0)
)

for(i in 1:nrow(final_score_chart))
{
  for(j in 1:nrow(DataChart))
  {
    if(DataChart$stdid[j] == final_score_chart$stdid[i])
    {
      if(DataChart$total_score[j] > final_score_chart$final_score[i])
      {
        final_score_chart$final_score[i] = DataChart$total_score[j];
      }
    }
  }
}
#print(final_score_chart);
write_data(min(final_score_chart$final_score));
lowest_final_score_e = min(final_score_chart$final_score)

#2.f) List of student with lowest final score
write_data("f. Danh sách sinh viên có điểm tổng kết thấp nhất:")
lowest_final_list = subset(final_score_chart, final_score == lowest_final_score_e);
write_data(lowest_final_list$stdid);

#2.g) Spectral of submit time from students of lowest final score
write_data("g. Phổ theo số lần nộp của các sinh viên có điểm tổng kết thấp nhất:")
lowest_final_list$times = c(0)
for (i in 1:nrow(lowest_final_list))
{
  stdid_temp = lowest_final_list$stdid[i];
  for(j in 1:nrow(DataChart))
  {
    stdid_temp_2 = DataChart$stdid[j];
    if(!is.na(stdid_temp_2) && (stdid_temp_2 == stdid_temp))
    {
      lowest_final_list$times[i] = lowest_final_list$times[i] + 1;
    }
  }
}
lowest_final_list$times = as.integer(lowest_final_list$times)
lowest_final_list_plot = data.frame(
  "times" = unique(lowest_final_list$times),
  "frequency" = c(0)
)
for(i in 1:nrow(lowest_final_list_plot))
{
  for(j in 1:nrow(lowest_final_list))
    if(lowest_final_list_plot$times[i] == lowest_final_list$times[j] )
    {
      lowest_final_list_plot$frequency[i] = lowest_final_list_plot$frequency[i] + 1
    }
}
write_data(lowest_final_list_plot);
barplot(lowest_final_list_plot$frequency,ylim = c(0,10),xlab = 'so lan nop',ylab = 'tan so xuat hien',names.arg = lowest_final_list_plot$times, main = "Pho tong so lan nop cua nhung hoc sinh co 1 diem tong ket thap nhat");

#2.h) Find Highest_score
write_data("h. Điểm cao nhất:")
highest_score = max(DataChart$total_score)
write_data(highest_score);

#2.i) List of students with at least 1 highest score
write_data("i. Danh sách sinh viên có bài nộp đạt điểm cao nhất:");
highest_chart = subset(final_score_chart, final_score == highest_score)
write_data(highest_chart$stdid);

#2.j) Spectral of submit time from students have at least a highest score submit
write_data("j. Phổ theo số lần nộp của các sinh viên có bài nộp đạt điểm cao nhất:")
highest_final_list = data.frame(
  "stdid" = highest_chart$stdid,
  "times" = c(0)
)
for (i in 1:nrow(highest_final_list))
{
  stdid_temp = highest_final_list$stdid[i];
  for(j in 1:nrow(DataChart))
  {
    stdid_temp_2 = DataChart$stdid[j];
    if(!is.na(stdid_temp_2) && (stdid_temp_2 == stdid_temp))
    {
      highest_final_list$times[i] = highest_final_list$times[i] + 1;
    }
  }
}
highest_final_list_plot = data.frame(
  "times" = unique(highest_final_list$times),
  "frequency" = c(0)
)
for(i in 1:nrow(highest_final_list_plot))
{
  for(j in 1:nrow(highest_final_list))
    if(highest_final_list_plot$times[i] == highest_final_list$times[j] )
    {
      highest_final_list_plot$frequency[i] = highest_final_list_plot$frequency[i] + 1
    }
}
write_data(highest_final_list_plot);
barplot(highest_final_list_plot$frequency,ylim = c(0,300),xlab = 'so lan nop',ylab = 'tan so xuat hien',names.arg = highest_final_list_plot$times, main = "Pho tong so lan nop cua nhung hoc sinh co 1 diem cao nhat");

#2.k)
write_data("k. Điểm tổng kết cao nhất:")
highest_score = max(DataChart$total_score)
write_data(highest_score);

#2.l)
write_data("l. Danh sách sinh viên có điểm tổng kết cao nhất:");
write_data(highest_chart$stdid);

#2.m)
write_data("m. Phổ theo số lần nộp của các sinh viên có điểm tổng kết cao nhất:");
write_data(highest_final_list);
barplot(highest_final_list_plot$frequency,ylim = c(0,300),xlab = 'so lan nop',ylab = 'tan so xuat hien',names.arg = highest_final_list_plot$times, main = "Pho tong so lan nop cua nhung hoc sinh co diem tong ket cao nhat");

#2.n) Caculate average final_score of all student
write_data("n. Điêm số tổng kết trung bình:");
average_final_score_all_student = sum(final_score_chart$final_score) / nrow(final_score_chart);
average_final_score_all_student = round(average_final_score_all_student * 10) / 10;
write_data(average_final_score_all_student);

final_score_chart$average_score = c(0)
final_score_chart$times = c(0)
for (i in 1:nrow(final_score_chart))
{
  stdid_temp = final_score_chart$stdid[i];
  for(j in 1:nrow(DataChart))
  {
    stdid_temp_2 = DataChart$stdid[j];
    if(!is.na(stdid_temp_2) && (stdid_temp_2 == stdid_temp))
    {
      final_score_chart$average_score[i] = final_score_chart$average_score[i] * final_score_chart$times[i] + DataChart$total_score[j]
      final_score_chart$times[i] = final_score_chart$times[i] + 1
      final_score_chart$average_score[i] = final_score_chart$average_score[i] / final_score_chart$times[i]
    }
  }
}
#average_final_list = subset(final_score_chart, final_score == average_final_score_all_student)
#print("Final score list with average score")
#print(average_final_list);

#2.o) Number of student have a average final score
write_data("o. Danh sách sinh viên có điểm tổng kết bằng điểm trung bình:");
score_match_average_list = subset(final_score_chart, final_score == average_final_score_all_student)
write_data(nrow(score_match_average_list));

#2.p) Median, Max, Min
write_data("p.")
write_data("Trung vị điểm tổng kết:");
Median_final_score = median(final_score_chart$final_score)
write_data(Median_final_score);

write_data("Cực đại mẫu điểm tổng kết:");
Max_final_score = max(final_score_chart$final_score)
write_data(Max_final_score);

write_data("Cực tiểu mẫu điểm tổng kết:");
Min_final_score = min(final_score_chart$final_score)
write_data(Min_final_score);

#2.q)Range, Variance, Standard Deviation
write_data("q. Độ phân tán dữ liệu điểm tổng kết:");
range_final = range(final_score_chart$final_score)
range_final_value = range_final[2] - range_final[1]
write_data("Khoảng dữ liệu:");
write_data(range_final_value);

#Create result_chart save total_submit and numofstudent classified by final score
result_chart = data.frame(
  "score" = NULL,
  "times" = NULL
)
for(i in 1:nrow(final_score_chart))
{
  if(final_score_chart$final_score[i] %in% result_chart$score)
  {
    index = which(result_chart$score == final_score_chart$final_score[i])
    result_chart$times[index] = result_chart$times[index] + 1
  }
  else 
  {
    temp_chart = data.frame(
    "score" = c(final_score_chart$final_score[i]),
    "times" = 1
    )
    result_chart = rbind(result_chart,temp_chart)
  }
}

variance_final_score = var(final_score_chart$final_score)
standard_deviation_final_score = sd(final_score_chart$final_score)
write_data("Phương sai:");
write_data(variance_final_score);
write_data("Độ lệch chuẩn:");
write_data(standard_deviation_final_score);

#2.r)Skewness and Kurtosis of final score
write_data("r.");
write_data("Độ méo lệch của điểm tổng kết:")
write_data(skewness(final_score_chart$final_score));
write_data("Độ nhọn của điểm tổng kết:");
write_data(kurtosis(final_score_chart$final_score));

#2.s)Quantile 1st and 3rd
write_data("s.");
write_data("Tứ phân vị thứ nhất:");
write_data(quantile(final_score_chart$final_score, 0.25));
write_data("Tứ phân vị thứ ba:");
write_data(quantile(final_score_chart$final_score, 0.75));

#2.t)
write_data("t. Số lượng sinh viên có điểm cao nhất hoặc cao nhì:");
result_chart = result_chart[order(-result_chart$score), ,drop = FALSE]
write_data(result_chart$times[1] + result_chart$times[2]);

#2.u)
write_data("u. Phổ theo số lần nộp của các sinh viên có điểm cao nhất hoặc cao nhì:");
final_score_list_first = subset(final_score_chart, final_score == result_chart$score[1])
final_score_list_second = subset(final_score_chart, final_score == result_chart$score[2])
final_score_list_first_second = rbind(final_score_list_second, final_score_list_first)

final_score_list_first_second_plot = data.frame(
  "times" = unique(final_score_list_first_second$times),
  "frequency" = c(0)
)
for(i in 1:nrow(final_score_list_first_second_plot))
{
  for(j in 1:nrow(final_score_list_first_second))
    if(final_score_list_first_second_plot$times[i] == final_score_list_first_second$times[j] )
    {
      final_score_list_first_second_plot$frequency[i] = final_score_list_first_second_plot$frequency[i] + 1
    }
}
write_data(final_score_list_first_second_plot);
barplot(final_score_list_first_second_plot$frequency,ylim = c(0,300),xlab = 'so lan nop',ylab = 'tan so xuat hien',names.arg = final_score_list_first_second_plot$times, main = "Pho so lan nop cua nhung hoc sinh co 1 diem tong ket cao nhat/nhi");

#2.v)
write_data("v. Số lượng sinh viên có điểm tổng kết cao thứ k:");
names(result_chart) = c("k", "number of students");
write_data(result_chart[c(1, 2), ]);
barplot(result_chart$`number of students`,xlab = 'Phan diem tong ket', ylim = c(0,400),ylab = 'So sinh vien',names.arg = result_chart$k, main = "Tong so sinh vien theo phan diem cao thu k");

#2.w)
write_data("w. Phổ theo số lân nộp của các sinh viên có điểm tổng kết cao thứ k:");
result_chart$total_submit = c(0)
for(i in 1:nrow(result_chart))
{
  for(j in 1:nrow(final_score_chart))
  {
    if(result_chart$k[i] == final_score_chart$final_score[j])
    {
      result_chart$total_submit[i] = result_chart$total_submit[i] + final_score_chart$times[j]
    }
  }
}

k = 0;
for (score in final_score_chart %>% group_by(final_score) %>% summarise() %>% arrange(desc(final_score)) %>% pull(final_score)) {
  print(score);
  submit_k_plot = final_score_chart %>% filter(final_score == score) %>% group_by(times) %>% summarise("frequency" = n());
  k = k + 1;
  ggplot(data = submit_k_plot) + 
    geom_col(mapping = aes(x = times, y = frequency));
  ggsave(paste("Screenshot\\Q1-2\\q2w-", as.character(k), "-file1.png", sep = ""));
}

result_chart$k = c(1:nrow(result_chart))
write_data(result_chart);