options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(readxl);
library(xlsx);

args = commandArgs(trailingOnly = TRUE);

file_number = 1;
if (length(args) > 0)
  file_number = as.numeric(args[1]);
tid = 6;
if (length(args) > 1)
  tid = as.numeric(args[2]);
file_in = paste("Data\\", as.character(file_number), ".xlsx", sep = "");
file_out = paste("Result\\", as.character(file_number), ".tsv", sep = "");

to_number = function(a) {
    res = as.numeric(sub(",", ".", a, fixed = TRUE));
    return(res);
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


#Read the Excel file
data <- read_excel(file_in, sheet = 1);
data <- head(data, -1);
namesList <-  c("Maso", "Tinhtrang", "BatDau", "HoanThanh", "Thoigian", "Diem/10,00", "Q. 1 /1,00", "Q. 2 /1,00", "Q. 3 /1,00", "Q. 4 /1,00", "Q. 5 /1,00", "Q. 6 /1,00", "Q. 7 /1,00", "Q. 8 /1,00", "Q. 9 /1,00", "Q. 10 /1,00");
names(data) <- namesList;
row.names(data) <- NULL;
# data = data.frame();
# data = read.xlsx("Data/2.xlsx", sheetIndex = 1);
# namesList = c("Maso", "Tinhtrang", "BatDau", "HoanThanh", "Thoigian", "Diem/10,00", "Q. 1 /1,00", "Q. 2 /1,00", "Q. 3 /1,00", "Q. 4 /1,00", "Q. 5 /1,00", "Q. 6 /1,00", "Q. 7 /1,00", "Q. 8 /1,00", "Q. 9 /1,00", "Q. 10 /1,00");
# Encoding(namesList) = "UTF-8";
# names(data) = namesList;
# row.names(data) = NULL;
# Encoding(data[, 2]) = "UTF-8";
# Encoding(data[, 5]) = "UTF-8";


#Refine data from Excel file
data$'BatDau' <- strptime(data$'BatDau', format = "%d %B %Y  %I:%M %p")
data$'HoanThanh' <- strptime(data$'HoanThanh', format = "%d %B %Y  %I:%M %p")
data$'Diem/10,00' <- suppressWarnings(as.numeric(sub(",", ".",data$'Diem/10,00', fixed = TRUE)))
data$'Maso' <- as.integer(data$Maso);
data <- data %>% filter(data$HoanThanh >= min(data$HoanThanh,na.rm = TRUE));

tgdata <- data %>% group_by(Maso) %>% transmute(Cachnhau = max(HoanThanh)-min(HoanThanh)) %>% ungroup(); 
landata <- data %>% group_by(Maso) %>% transmute(n = n()) %>% ungroup();

write_data("Câu 4");
#a
write_data("a. ");
NDtgdata <- tgdata[!duplicated(tgdata$'Maso'),]
adata <- NDtgdata
rm(NDtgdata)
write_data(adata);


#b
write_data("b. ");
rangetime <- as.vector(adata %>% count(Cachnhau));
rangetime <- rangetime%>% filter(rangetime$Cachnhau >= min(rangetime$Cachnhau,na.rm = TRUE));
write_data(rangetime);
y = max(rangetime$n);
# barplot(rangetime$n ,xlab = 'Thoi gian lam viec cua cac sinh vien',ylab = 'So sinh vien', ylim = c(0,max(rangetime$n)+100),names.arg = rangetime$Cachnhau);

  
#c
write_data("c. ");
tsdata = data.frame();
tsdata <- tgdata %>% mutate(f = tgdata$Cachnhau/landata$n);
NDtsdata <- tsdata%>% mutate('Diem' = data$'Diem/10,00');
NDtsdata <- NDtsdata[!duplicated(tgdata$'Maso'),]
NDtsdata <- NDtsdata[,-2];
NDtsdata <- NDtsdata %>% filter(NDtsdata$f >= min(NDtsdata$f,na.rm = TRUE));
rm(tsdata)
write_data(NDtsdata);

#d
write_data("d. ");
lowtsdata <- NDtsdata %>% filter(NDtsdata$f == min(NDtsdata$f,na.rm = TRUE));
ltsddata <- lowtsdata[,-2];
dtsdata <- ltsddata[,-2];
rm(lowtsdata)
write_data(dtsdata);

#e
write_data("e. ");
jpeg("Screenshot\\q4e-file2.jpeg");
lowtime <- ltsddata %>% count(Diem);
lowtime <- lowtime %>% filter(lowtime$Diem >= min(NDtsdata$f,na.rm = TRUE));
write_data(lowtime);
# barplot(lowtime$n,xlab = 'Diem cua cac sinh vien nop it nhat',ylab = 'So sinh vien', ylim = c(0,max(lowtime$n)+100),names.arg = lowtime$Diem);
dev.off();


#f
write_data("f. ");
hightsdata <- NDtsdata %>% filter(NDtsdata$f == max(NDtsdata$f,na.rm = TRUE));
write_data(nrow(hightsdata));

#g
write_data("g. ");
htsdata <- hightsdata[,-2];
gtsdata <- htsdata[,-2];
write_data(gtsdata);

#h
write_data("h. ");
hightime <- htsdata %>% count(Diem);
# barplot(hightime$n,xlab = 'Diem cua cac sinh vien nop it nhat',ylab = 'So sinh vien', ylim = c(0,max(hightime$n)),names.arg = hightime$Diem);
write_data(hightime %>% select(Diem, n));

#i
write_data("i. ");
nohightsdata <- NDtsdata %>% filter(NDtsdata$f != max(NDtsdata$f,na.rm = TRUE));
a = max(nohightsdata$f);
secondhightsdata <- nohightsdata %>% filter(nohightsdata$f == max(nohightsdata$f,na.rm = TRUE));
rm(nohightsdata)
write_data(secondhightsdata);

#j
write_data("j. ");
fshightsdata <- NDtsdata %>% filter(NDtsdata$f >= a);
write_data(fshightsdata);

#k
write_data("k. ");
kdata <- as.data.frame(data[,1]);
colnames(kdata)[1] <- 'Maso'
kdata <- kdata %>% mutate('Hoanthanh' = data$HoanThanh);
kdata <- kdata %>% mutate('Lan' = landata$n);
#Split data
ykdata <- kdata %>% filter(kdata$Lan > 1);
nkdata <- kdata %>% filter(kdata$Lan == 1);
#Deal with 1 submit
nkdata <- as.data.frame(nkdata[,1])
colnames(nkdata)[1] <- 'Maso';
nkdata <- nkdata %>% mutate(ThoigianTB = 0);
#Deal with 2 submit
firstsubmit <- ykdata %>% group_by(Maso) %>% transmute(MaxThoigian = max(Hoanthanh)) %>% ungroup();
timedata <- firstsubmit %>% mutate('Thoigian' = firstsubmit$MaxThoigian-ykdata$Hoanthanh);
timedata <- timedata %>% filter(Thoigian > 0);
timedata <- timedata[,-2]
timedata <- timedata %>% group_by(Maso) %>% transmute(ThoigianTB = min(Thoigian)/2) %>% ungroup();
timedata <- as.data.frame(timedata[!duplicated(timedata$'Maso'),])
kdata <- rbind(timedata,nkdata)
write_data(kdata)

#l
write_data("l. ");
freq <- as_tibble(NDtsdata %>% count(f));
freq <- freq%>% mutate('Tansuat' = freq$n/nrow(NDtsdata));
colnames(freq)[colnames(freq) == 'n'] <- 'TanSo';
cumfreq <- 1:nrow(freq)
cumfreq[1] <- freq$Tansuat[1];
for(i in 2:nrow(freq)){
  cumfreq[i] <- cumfreq[i-1]+freq$Tansuat[i];
}
freq$TanSuatTichLuy <- cumfreq;
write_data(freq);

#m
write_data("m. ");
# barplot(freq$TanSo,xlab = 'Trung binh thoi gian cua sinh vien',ylab = 'Tan so', ylim = c(0,max(freq$TanSo)+100),names.arg = freq$TrungBinh);
# write_data(freq %>% select(TrungBinh, TanSo));
#n
write_data("n. ");
pie(freq$Tansuat,freq$f,col = rainbow(nrow(freq)),radius = 1,main='Tan suat')

#o
write_data("o. ");
# barplot(freq$TanSuatTichLuy,xlab = 'Trung binh thoi gian cua sinh vien',ylab = 'Tan suat tich luy', ylim = c(0,max(freq$TanSuatTichLuy)),names.arg = freq$TrungBinh);
# write_data(freq %>% select(TrungBinh, TanSuatTichLuy));

#p
write_data("p. ");
tgtsdata <- NDtsdata %>% mutate('Cachnhau' = adata$'Cachnhau');
tgtsdata <- tgtsdata[,-3];
tgtsdata <- tgtsdata%>% filter(tgtsdata$f >= min(tgtsdata$f,na.rm = TRUE));
tgtsdata$f <- as.numeric(tgtsdata$f)
tgtsdata$Cachnhau <- as.numeric(tgtsdata$Cachnhau)
write_data(tgtsdata[,2:3] %>% summarise_all(median))
write_data(tgtsdata[,2:3] %>% summarise_all(max));
write_data(tgtsdata[,2:3] %>% summarise_all(min));


#q
write_data("q. ");
#Do bien thien:
max(tgtsdata$Cachnhau)-min(tgtsdata$Cachnhau);
max(tgtsdata$f)-min(tgtsdata$f);
#Phuong sai va do lech chuan
write_data(tgtsdata[,2:3] %>% summarise_all(var));
write_data(tgtsdata[,2:3] %>% summarise_all(sd));

#r
write_data("r. ");
write_data(tgtsdata[,2:3] %>% summarise_all(skewness));
write_data(tgtsdata[,2:3] %>% summarise_all(kurtosis));

#s
write_data("s. ");
#quantile cho Thoi gian
write_data(quantile(tgtsdata$Cachnhau, c(0.25, 0.75), type = 1))
#quantile cho Tan suat
write_data(quantile(tgtsdata$f, c(0.25 ,0.75), type = 1))


detach("package:dplyr", unload = TRUE) ;
detach("package:xlsx", unload = TRUE) ;
detach("package:ggplot2", unload = TRUE) ;
detach("package:e1071", unload = TRUE) ;