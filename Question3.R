options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(xlsx);
library(e1071);

# read data
data <- read.xlsx("4.xlsx", sheetIndex = 1, encoding = "UTF-8", colIndex = 1:16);
data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
names(data) <- namesList;
row.names(data) <- NULL;
data <- as_tibble(data);

# Question 3
data = data %>% filter(total_score >= 0) %>% group_by(stdid);
size_list = summarise(data, "size" = n());
data = mutate(data, "size" = n());
print(size_list, n=nrow(size_list));

# a so luong sinh vien co so lan nop it nhat
print("so luong sinh vien co so lan nop it nhat");
min_number_of_submission = min(size_list$size);
#dung ham min de tim so sv nop it nhat
print(min_number_of_submission);

# b in danh sach sinh vien co so lan nop it nhat
print("in danh sach sinh vien co so lan nop it nhat");
#dung lenh filter de loc danh sach sv so lan nop it nhat
list1=filter(size_list,size == min_number_of_submission);
print(list1[ ,1],n=nrow(list1));

#c in pho diem sinh vien co so lan nop it nhat
print("in pho diem sinh vien co so lan nop it nhat");
#dung lenh filter loc danh sach sv so lan nop it nhat
least_nob_list  = data %>% filter(size == min_number_of_submission);
#dung ggplot de in ra pho diem can in
least_nob_list = least_nob_list %>% filter(total_score >= 0) %>% group_by(total_score);
point_list1 = summarise(least_nob_list, "size" = n());
least_nob_list = mutate(least_nob_list, "size" = n());
ggplot(data=point_list1)+
geom_col(mapping=aes(y=size,x=total_score));

#d so luong sinh vien co so lan nop nhieu nhat
print("so luong sinh vien co so lan nop nhieu nhat");
#dung ham max de tim so sv nop nhieu nhat
max_number_of_submission = max(size_list$size);
print(max_number_of_submission);

# e in danh sach sinh vien co so lan nop nhieu nhat
print("in danh sach sinh vien co so lan nop nhieu nhat");
#loc danh sach sv so lan nop it nhat
list2=filter(size_list,size == max_number_of_submission);
#in ra danh sach thoa yeu cau
print(list2[ ,1],n=nrow(list2));

#f in pho diem sinh vien co so lan nop nhieu nhat
print("in pho diem sinh vien co so lan nop nhieu nhat");
#dung lenh filter loc danh sach sv so lan nop nhieu nhat
most_nob_list  = data %>% filter(size == max_number_of_submission);
#dung ggplot de in ra pho diem can in
most_nob_list = most_nob_list %>% filter(total_score >= 0) %>% group_by(total_score);
point_list2 = summarise(most_nob_list, "size" = n());
least_nob_list = mutate(most_nob_list, "size" = n());
ggplot(data=point_list2)+
geom_col(mapping=aes(y=size,x=total_score));

#g so lan nop trung binh
print("so lan nop trung binh");
#dung ham round lam tron den so nguyen gan nhat, dung ham mean de tinh trung binh so lan nop trong cai danh sach
print((round(mean(size_list$size),0)));

#h so luong sinh vien co so lan nop trung binh
print("so luong sinh vien co so lan nop trung binh");
count=0;
#kiem tra coi sv co thuoc so lan nop tb, neu co thi cong 1 vao count
for (i in 1: nrow(size_list))
{
    if (size_list[i,2]==(round(mean(size_list$size),0)))
    count=count+1;
}
print(count);

#i in pho diem sinh vien co so lan nop trung binh
print("in pho diem sinh vien co so lan nop trung binh");
#dung lenh filter de loc pho diem sv so lan nop trung binh
mean_nob_list  = data %>% filter(size == round(mean(size_list$size),0));
#dung ggplot de in ra pho diem can in
mean_nob_list = most_nob_list %>% filter(total_score >= 0) %>% group_by(total_score);
point_list3 = summarise(mean_nob_list, "size" = n());
mean_nob_list = mutate(mean_nob_list, "size" = n());
ggplot(data=point_list3)+
geom_col(mapping=aes(y=size,x=total_score));

#j trung vi, cuc dai, cuc tieu
print("trung vi, cuc dai, cuc tieu");
median_number_of_submission=median(size_list$size);
#lenh median de tinh trung vi
print(median_number_of_submission);
#lenh max de tinh cuc dai
print(max_number_of_submission);
#lenh min de tinh cuc tieu
print (min_number_of_submission);

#k phuong sai, do lech chuan
print("phuong sai, do lech chuan");
#lenh var de tinh phuong sai
print(var(size_list$size));
#lenh sd de tinh do lech chuan
print(sd(size_list$size));


#l do meo lech, do nhon
print("do meo lech, do nhon");
#lenh skewness de tinh do meo lech
print(skewness(size_list$size));
#lent kurtosis de tinh do nhon
print(kurtosis(size_list$size));

#m tu phan vi
print("tu phan vi");
#lenh quantile de tinh tu phan vi, xong lay cai thu nhat va cai thu 3
quantile(size_list$size);


#n danh sach sv nop bai nhieu nhi
print("danh sach sv nop bai nhieu nhi");
#lay data2 gom nhung sv it hon so lan nop bai nhieu nhat, dung lenh filter de loc nhung thang nhu vay roi truyen vao data2
data2 = data %>% filter(total_score >= 0, size < max_number_of_submission) %>% group_by(stdid);
size_list2 = summarise(data2, "size" = n());
data2 = mutate(data2, "size" = n());
#dung lenh filter de loc danh sach nhung thang nop bai nhieu nhat trong data2, cung la nop nhieu nhi trong data
max2_number_of_submission = max(size_list2$size);
list3= filter(size_list2,size == max2_number_of_submission);
#in ra danh sach thoa yeu cau
print(list3[ ,1], n=nrow(list3));

#o danh sach sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi
print("danh sach sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi");
#dung lenh filter loc danh sach sv so lan nop nhieu nhat
x1=size_list %>% filter(size == max_number_of_submission) ;
#dung lenh filter loc danh sach sv so lan nop nhieu nhi
x2=size_list2 %>% filter(size == max2_number_of_submission) ;
#dung lenh union de ket hop 2 cai nhieu nhat va nhieu nhi lai
union1=union(x1,x2);
#in ra danh sach thoa yeu cau
print(union1[ ,1],n=nrow(union1));


#p so sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi
print("so sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi");
#dung bien count1 de dem so sinh vien thoa yeu cau, neu trong danh sach gap sv thoa thi cong len 1
count1=0;
for (i in 1: nrow(size_list))
if ((size_list[i,2]==max_number_of_submission) | (size_list[i,2]==max2_number_of_submission))
{
    count1=count1+1;
}
print(count1);


#q pho diem so luong sv nop bai nhieu nhat hoac nhi
print("pho diem so luong sv nop bai nhieu nhat hoac nhi");
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


#r 1/3 danh sach sc nop bai nhieu nhat
print("1/3 danh sach sc nop bai nhieu nhat");
df <-size_list[order(size_list$size, decreasing= TRUE),];
m = round(1/3*nrow(df), 0);
#ham slice_max la cat tu tren xuong theo y minh muon, o day cat 1/3 so sv dau
one_third <- df %>% slice_max(size, n=m);
#in ra danh sach thoa yeu cau
print(one_third[ ,1], n=nrow(one_third));

#s 1/3 so luong sv nop bai nhieu nhat
print("so luong sv nop bai nhieu nhat");
print(round(1/3*nrow(df), 0));

#t pho diem 1/3 so luong sv nop bai nhieu nhat
print("so luong sv nop bai nhieu nhat");
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

#u pho diem voi k cho truoc
print("pho diem voi k cho truoc");
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