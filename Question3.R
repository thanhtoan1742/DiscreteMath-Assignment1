options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(xlsx);
library(readxl)
library(e1071);

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


# read data
data <- read_excel(file_in, sheet = 1);
data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10");
names(data) <- namesList;
row.names(data) <- NULL;
data <- as_tibble(data) %>% filter(total_score >= 0);
data$time_begin <- strptime(data$time_begin, format = "%d %B %Y  %I:%M %p");
data$total_score <- to_number(data$total_score);
data <- arrange(data, desc(time_begin));

# Question 3
write_data("Câu 3");
data = data %>% filter(total_score >= 0) %>% group_by(stdid);
size_list = summarise(data, "size" = n());
data = mutate(data, "size" = n());

# a so luong sinh vien co so lan nop it nhat
write_data("a. so luong sinh vien co so lan nop it nhat");
min_number_of_submission = min(size_list$size);
#dung ham min de tim so sv nop it nhat
write_data(min_number_of_submission);

# b in danh sach sinh vien co so lan nop it nhat
write_data("b. in danh sach sinh vien co so lan nop it nhat");
#dung lenh filter de loc danh sach sv so lan nop it nhat
list1=filter(size_list,size == min_number_of_submission);
write_data(list1[ ,1]);

#c in pho diem sinh vien co so lan nop it nhat
write_data("c. in pho diem sinh vien co so lan nop it nhat");
#dung lenh filter loc danh sach sv so lan nop it nhat
least_nob_list  = data %>% filter(size == min_number_of_submission);
#dung ggplot de in ra pho diem can in
least_nob_list = least_nob_list %>% filter(total_score >= 0) %>% group_by(total_score);
point_list1 = summarise(least_nob_list, "size" = n());
least_nob_list = mutate(least_nob_list, "size" = n());
ggplot(data=point_list1)+
geom_col(mapping=aes(y=size,x=total_score));
write_data(point_list1 %>% select(total_score, size));

#d so luong sinh vien co so lan nop nhieu nhat
write_data("d. so luong sinh vien co so lan nop nhieu nhat");
#dung ham max de tim so sv nop nhieu nhat
max_number_of_submission = max(size_list$size);
write_data(max_number_of_submission);

# e in danh sach sinh vien co so lan nop nhieu nhat
write_data("e. in danh sach sinh vien co so lan nop nhieu nhat");
#loc danh sach sv so lan nop it nhat
list2=filter(size_list,size == max_number_of_submission);
#in ra danh sach thoa yeu cau
write_data(list2[ ,1]);

#f in pho diem sinh vien co so lan nop nhieu nhat
write_data("f. in pho diem sinh vien co so lan nop nhieu nhat");
#dung lenh filter loc danh sach sv so lan nop nhieu nhat
most_nob_list  = data %>% filter(size == max_number_of_submission);
#dung ggplot de in ra pho diem can in
most_nob_list = most_nob_list %>% filter(total_score >= 0) %>% group_by(total_score);
point_list2 = summarise(most_nob_list, "size" = n());
least_nob_list = mutate(most_nob_list, "size" = n());
ggplot(data=point_list2)+
geom_col(mapping=aes(y=size,x=total_score));
write_data(point_list2 %>% select(total_score, size))

#g so lan nop trung binh
write_data("g. so lan nop trung binh");
#dung ham round lam tron den so nguyen gan nhat, dung ham mean de tinh trung binh so lan nop trong cai danh sach
write_data((round(mean(size_list$size),0)));

#h so luong sinh vien co so lan nop trung binh
write_data("h. so luong sinh vien co so lan nop trung binh");
count=0;
#kiem tra coi sv co thuoc so lan nop tb, neu co thi cong 1 vao count
for (i in 1: nrow(size_list))
{
    if (size_list[i,2]==(round(mean(size_list$size),0)))
    count=count+1;
}
write_data(count);

#i in pho diem sinh vien co so lan nop trung binh
write_data("i. in pho diem sinh vien co so lan nop trung binh");
#dung lenh filter de loc pho diem sv so lan nop trung binh
mean_nob_list  = data %>% filter(size == round(mean(size_list$size),0));
#dung ggplot de in ra pho diem can in
mean_nob_list = most_nob_list %>% filter(total_score >= 0) %>% group_by(total_score);
point_list3 = summarise(mean_nob_list, "size" = n());
mean_nob_list = mutate(mean_nob_list, "size" = n());
ggplot(data=point_list3)+
geom_col(mapping=aes(y=size,x=total_score));
write_data(point_list3 %>% select(total_score, size));

#j trung vi, cuc dai, cuc tieu
write_data("j. trung vi, cuc dai, cuc tieu");
median_number_of_submission=median(size_list$size);
#lenh median de tinh trung vi
write_data(median_number_of_submission);
#lenh max de tinh cuc dai
write_data(max_number_of_submission);
#lenh min de tinh cuc tieu
write_data(min_number_of_submission);

#k phuong sai, do lech chuan
write_data("k. phuong sai, do lech chuan");
#lenh var de tinh phuong sai
write_data(var(size_list$size));
#lenh sd de tinh do lech chuanw
write_data(sd(size_list$size));


#l do meo lech, do nhon
write_data("l. do meo lech, do nhon");
#lenh skewness de tinh do meo lech
write_data(skewness(size_list$size));
#lent kurtosis de tinh do nhon
write_data(kurtosis(size_list$size));

#m tu phan vi
write_data("m. tu phan vi");
#lenh quantile de tinh tu phan vi, xong lay cai thu nhat va cai thu 3
write_data(quantile(size_list$size));


#n danh sach sv nop bai nhieu nhi
write_data("n. danh sach sv nop bai nhieu nhi");
#lay data2 gom nhung sv it hon so lan nop bai nhieu nhat, dung lenh filter de loc nhung thang nhu vay roi truyen vao data2
data2 = data %>% filter(total_score >= 0, size < max_number_of_submission) %>% group_by(stdid);
size_list2 = summarise(data2, "size" = n());
data2 = mutate(data2, "size" = n());
#dung lenh filter de loc danh sach nhung thang nop bai nhieu nhat trong data2, cung la nop nhieu nhi trong data
max2_number_of_submission = max(size_list2$size);
list3= filter(size_list2,size == max2_number_of_submission);
#in ra danh sach thoa yeu cau
write_data(list3[ ,1]);

#o danh sach sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi
write_data("o. danh sach sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi");
#dung lenh filter loc danh sach sv so lan nop nhieu nhat
x1=size_list %>% filter(size == max_number_of_submission) ;
#dung lenh filter loc danh sach sv so lan nop nhieu nhi
x2=size_list2 %>% filter(size == max2_number_of_submission) ;
#dung lenh union de ket hop 2 cai nhieu nhat va nhieu nhi lai
union1=union(x1,x2);
#in ra danh sach thoa yeu cau
write_data(union1[ ,1]);


#p so sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi
write_data("p. so sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi");
#dung bien count1 de dem so sinh vien thoa yeu cau, neu trong danh sach gap sv thoa thi cong len 1
count1=0;
for (i in 1: nrow(size_list))
if ((size_list[i,2]==max_number_of_submission) | (size_list[i,2]==max2_number_of_submission))
{
    count1=count1+1;
}
write_data(count1);


#q pho diem so luong sv nop bai nhieu nhat hoac nhi
write_data("q. pho diem so luong sv nop bai nhieu nhat hoac nhi");
#dung lenh filter loc danh sach sv so lan nop nhieu nhat
#dung lenh filter loc danh sach sv so lan nop nhieu nhi
most_nob_list  = data %>% filter(size == max_number_of_submission);
most2_nob_list  = data %>% filter(size == max2_number_of_submission);
#dung lenh union de ket hop 2 cai nhieu nhat va nhieu nhi lai
union2=union(most_nob_list,most2_nob_list);
#dung ggplot de in ra pho diem can in

union2 = union2 %>% filter(total_score >= 0) %>% group_by(total_score);

point_list5 = summarise(union2, "size" = n());
union2 = mutate(union2, "size" = n());
ggplot(data=point_list5)+
geom_col(mapping=aes(y=size,x=total_score));
write_data(point_list5 %>% select(total_score, size));


#r 1/3 danh sach sc nop bai nhieu nhat
write_data("r. 1/3 danh sach sc nop bai nhieu nhat");
df <-size_list[order(size_list$size, decreasing= TRUE),];
m = round(1/3*nrow(df), 0);
#ham slice_max la cat tu tren xuong theo y minh muon, o day cat 1/3 so sv dau
one_third <- df %>% slice_max(size, n=m);
#in ra danh sach thoa yeu cau
write_data(one_third[ ,1]);

#s 1/3 so luong sv nop bai nhieu nhat
write_data("s. so luong sv nop bai nhieu nhat");
write_data(round(1/3*nrow(df), 0));

#t pho diem 1/3 so luong sv nop bai nhieu nhat
write_data("t. so luong sv nop bai nhieu nhat");
#dung slice_max de cat ra 1/3 so thăng nop nhieu nhat dau tien
one_third2 <- df %>% slice_max(size, n=m);
#truyen toan bo du lieu cua 1/3 thang do vao one_third2
one_third2 <- data %>% filter(stdid %in% one_third2$stdid);
#dung ggplot de in ra pho diem can in
one_third2 = one_third2 %>% filter(total_score >= 0) %>% group_by(total_score);
point_list6 = summarise(one_third2, "size" = n());
one_third2 = mutate(one_third2, "size" = n());
ggplot(data=point_list6)+
geom_col(mapping=aes(y=size,x=total_score));
write_data(point_list6 %>% select(total_score, size));

#u pho diem voi k cho truoc
write_data("u. pho diem voi k cho truoc");
k = 3;
# nhom nhung thang o k nhom dau lai
k_score <- size_list %>% group_by(size) %>% summarise() %>% slice_max(size, n = k);
k_group <- size_list %>% filter(size %in% k_score$size);
#truyen toan bo du lieu nhung thang o k nhom dau vao k_group
k_group <- data %>% filter(stdid %in% k_group$stdid);
#dung ggplot de in ra pho diem can in
k_group = k_group %>% filter(total_score >= 0) %>% group_by(total_score);
point_list7 = summarise(k_group, "size" = n());
one_third2 = mutate(k_group, "size" = n());
ggplot(data=point_list7)+
geom_col(mapping=aes(y=size,x=total_score));
write_data(point_list7 %>% select(total_score, size));