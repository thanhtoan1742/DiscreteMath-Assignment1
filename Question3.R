options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(xlsx);
library(e1071);

# read data
data <- read.xlsx("1.xlsx", sheetIndex = 1, encoding = "UTF-8", colIndex = 1:16);
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
print(min_number_of_submission);

# b in danh sach sinh vien co so lan nop it nhat
print("in danh sach sinh vien co so lan nop it nhat");
#loc danh sach sv so lan nop it nhat
list1=filter(size_list,size == min_number_of_submission);
print(list1[ ,1],n=nrow(list1));

#c in pho diem sinh vien co so lan nop it nhat
print("in pho diem sinh vien co so lan nop it nhat");
#loc pho diem sv so lan nop it nhat
least_nob_list  = data %>% filter(size == min_number_of_submission);
print(least_nob_list[, c(-2, -3, -4, -5)], width = Inf,n=nrow(least_nob_list));

#d so luong sinh vien co so lan nop nhieu nhat
print("so luong sinh vien co so lan nop nhieu nhat");
max_number_of_submission = max(size_list$size);
print(max_number_of_submission);

# e in danh sach sinh vien co so lan nop nhieu nhat
print("in danh sach sinh vien co so lan nop nhieu nhat");
#loc danh sach sv so lan nop it nhat
list2=filter(size_list,size == max_number_of_submission);
print(list2[ ,1],n=nrow(list2));

#f in pho diem sinh vien co so lan nop nhieu nhat
print("in pho diem sinh vien co so lan nop nhieu nhat");
#loc pho diem sv so lan nop nhieu nhat
most_nob_list  = data %>% filter(size == max_number_of_submission);
print(most_nob_list[, c(-2, -3, -4, -5)], width = Inf,n=nrow(most_nob_list));

#g so lan nop trung binh
print("so lan nop trung binh");
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
#loc pho diem sv so lan nop trung binh
mean_nob_list  = data %>% filter(size == round(mean(size_list$size),0));
print(mean_nob_list[, c(-2, -3, -4, -5)], width = Inf,n=nrow(mean_nob_list));

#j trung vi, cuc dai, cuc tieu
print("trung vi, cuc dai, cuc tieu");
median_number_of_submission=median(size_list$size);
#trung vi
print(median_number_of_submission);
#cuc dai
print(max_number_of_submission);
#cuc tieu
print (min_number_of_submission);

#k phuong sai, do lech chuan
print("phuong sai, do lech chuan");
#phuong sai
print(var(size_list$size));
#do lech chuan
print(sd(size_list$size));


#l do meo lech, do nhon
print("do meo lech, do nhon");
#do meo lech
print(skewness(size_list$size));
#do nhon
print(kurtosis(size_list$size));

#m tu phan vi
print("tu phan vi");
quantile(size_list$size);


#n danh sach sv nop bai nhieu nhi
print("danh sach sv nop bai nhieu nhi");
#lay data2 gom nhung thang it hon so lan nop bai nhieu nhat
data2 = data %>% filter(total_score >= 0, size < max_number_of_submission) %>% group_by(stdid);
size_list2 = summarise(data2, "size" = n());
data2 = mutate(data2, "size" = n());
#loc danh sach nhung thang nop bai nhieu nhat trong data2, cung la nop nhieu nhi trong data
max2_number_of_submission = max(size_list2$size);
list3= filter(size_list2,size == max2_number_of_submission);
print(list3[ ,1], n=nrow(list3));

#o danh sach sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi
print("danh sach sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi");

x1=size_list %>% filter(size == max_number_of_submission) ;
x2=size_list2 %>% filter(size == max2_number_of_submission) ;
#ket hop 2 cai nhieu nhat va nhieu nhi lai
union1=union(x1,x2);
print(union1[ ,1],n=nrow(union1));


#p so sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi
print("so sinh vien co so lan nop bai nhieu nhat hoac nhieu nhi");
count1=0;
for (i in 1: nrow(size_list))
if ((size_list[i,2]==max_number_of_submission) | (size_list[i,2]==max2_number_of_submission))
{
    count1=count1+1;
}
print(count1);


#q pho diem so luong sv nop bai nhieu nhat hoac nhi
print("pho diem so luong sv nop bai nhieu nhat hoac nhi");
most_nob_list  = data %>% filter(size == max_number_of_submission);
most2_nob_list  = data %>% filter(size == max2_number_of_submission);
#ket hop 2 cai nhieu nhat va nhieu nhi lai
union2=union(most_nob_list,most2_nob_list);
print(union2[, c(-2, -3, -4, -5)], width = Inf, n=nrow(union2));

#r 1/3 danh sach sc nop bai nhieu nhat
print("1/3 danh sach sc nop bai nhieu nhat");
df <-size_list[order(size_list$size, decreasing= TRUE),];
m = round(1/3*nrow(df), 0);
#ham slice_max la cat tu tren xuong theo y minh muon, o day cat 1/3 so sv dau
one_third <- df %>% slice_max(size, n=m);
print(one_third[ ,1], n=nrow(one_third));

#s 1/3 so luong sv nop bai nhieu nhat
print("so luong sv nop bai nhieu nhat");
print(round(1/3*nrow(df), 0));

#t pho diem 1/3 so luong sv nop bai nhieu nhat
print("so luong sv nop bai nhieu nhat");
one_third2 <- df %>% slice_max(size, n=m);
one_third2 <- data %>% filter(stdid %in% one_third2$stdid);
print(one_third2[, c(-2, -3, -4, -5)], width = Inf,n=nrow(one_third2));

#u pho diem voi k cho truoc
print("pho diem voi k cho truoc");
k = 3;
k_score <- size_list %>% group_by(size) %>% summarise() %>% slice_max(size, n = k);
k_group <- size_list %>% filter(size %in% k_score$size);
k_group <- data %>% filter(stdid %in% k_group$stdid);
print(k_group[, c(-2, -3, -4, -5)], width = Inf,n=nrow(k_group));
