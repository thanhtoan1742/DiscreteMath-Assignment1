options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(xlsx);

MD = 7241;
filename = "data\\2.xlsx";

to_number = function(a) {
    res = as.numeric(sub(",", ".", a, fixed = TRUE));
    return(res);
}

average = function(a) {
    res = 0.0;
    for (x in a)
        res = res + to_number(x);
    res = res / length(a);
    return(res);
}

# READ DATA
data <- read.xlsx(filename, sheetIndex = 1, encoding = "UTF-8", colIndex = 1:16);
data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
names(data) <- namesList;
row.names(data) <- NULL;
data <- as_tibble(data) %>% filter(total_score >= 0);
data$time_begin <- strptime(data$time_begin, format = "%d %B %Y  %I:%M %p");
data$total_score <- to_number(data$total_score);

time_begin_distrubution <- data %>% group_by(stdid) %>% summarise("time_begin" = min(as.POSIXct(time_begin)))%>% arrange(desc(time_begin));
time_begin_distrubution <- time_begin_distrubution %>% group_by(time_begin) %>% summarise("count" = n());
for (i in 2:nrow(time_begin_distrubution))
    time_begin_distrubution[i, ]$count = time_begin_distrubution[i, ]$count + time_begin_distrubution[i - 1, ]$count;

print(time_begin_distrubution);
ggplot(data = time_begin_distrubution) +
    geom_line(mapp = aes(x = time_begin, y = count));

hardwork_time_begin = time_begin_distrubution[nrow(time_begin_distrubution) %/% 3, ]$time_begin;
print(min(time_begin_distrubution$time_begin));
print(hardwork_time_begin);
print(max(time_begin_distrubution$time_begin));