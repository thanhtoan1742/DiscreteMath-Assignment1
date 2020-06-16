options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(xlsx);

MD = 7241;
filename = "data\\1.xlsx";

to_number = function(a) {
    res = as.numeric(sub(",", ".", a, fixed = TRUE));
    return(res);
}

# QUESTION 5
get_max_nob = function(data) {
    return(max(data$nob));
}

average = function(a) {
    res = 0.0;
    for (x in a)
        res = res + a;
    res = res / length(a);
    return(res);
}

Question5 = function(data) {
    by_stdid <- data %>% filter(total_score >= 0) %>% arrange(desc(stdid));
    by_stdid <- by_stdid %>% mutate("sub_no" = 0);
    # nob : number of submissions
    max_nob <- by_stdid %>% group_by(stdid) %>% summarise("nob" = n()) %>% get_max_nob();
    max_nob <- max(max_nob, 6);
    score <- matrix(nrow = n_distinct(by_stdid$stdid), ncol = max_nob);

    i = 1;
    u = 0;
    while (i <= nrow(by_stdid)) {
        j = i;
        u = u + 1;
        v = 0;
        while ((j <= nrow(by_stdid)) & (by_stdid[i, ]$stdid == by_stdid[j, ]$stdid)) {
            v = v + 1;
            if (v == 1) {
                score[u, v] = to_number(by_stdid[j, ]$total_score);
            } else
                score[u, v] = max(to_number(by_stdid[j, ]$total_score), to_number(score[u, v - 1]));
            by_stdid[j, ]$sub_no = score[u, v];
            j = j + 1;
        }

        while (v < max_nob) {
            v = v + 1;
            score[u, v] = score[u, v - 1];
        }
        i = j;
    }

    score <- as_tibble(score);
    score <- score %>% mutate("stdid" = "")
    score$stdid <- unique(by_stdid$stdid);
    names(score) <- c(paste("sub_no_", as.character(1:max_nob), sep = ""), "stdid");
    row.names(score) <- NULL;
    
    score_frequency <- score %>% group_by(sub_no_6) %>% summarise("freq" = n());
    print(score_frequency);
    ggplot(data = score_frequency) +   
        geom_line(mapping = aes(x = sub_no_6, y = freq));
    ggsave("max_score_6.pdf");

    score_frequency <- score %>% group_by(sub_no_3) %>% summarise("freq" = n());
    print(score_frequency);
    ggplot(data = score_frequency) +   
        geom_line(mapping = aes(x = sub_no_3, y = freq));
    ggsave("max_score_3.pdf");

    score_frequency <- score %>% group_by(sub_no_1) %>% summarise("freq" = n());
    print(score_frequency);
    ggplot(data = score_frequency) +   
        geom_line(mapping = aes(x = sub_no_1, y = freq));
    ggsave("max_score_1.pdf");

    average_score <- score[, 1:max_nob] %>% summarise_at(vars(starts_with("sub_no")), mean);
    average_score <- tibble(no = 1:max_nob, ave = as.numeric(average_score[1, ]));

    print(average_score);
    ggplot(data = average_score) +   
        geom_line(mapping = aes(x = no, y = ave));
    ggsave("average_score.pdf");
 }

# READ DATA
data <- read.xlsx(filename, sheetIndex = 1, encoding = "UTF-8", colIndex = 1:16);
data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
names(data) <- namesList;
row.names(data) <- NULL;
data <- as_tibble(data);
arrange(data, desc(stdid));

Question5(data);
# # original data    
# data <- as.data.frame(data);
# namesList <- c("Mã số ID", "Tình trạng", "Đã bắt đầu vào lúc", "Đã hoàn thành", "Thời gian thực hiện", "Điểm/10,00", "Q. 1 /1,00", "Q. 2 /1,00", "Q. 3 /1,00", "Q. 4 /1,00", "Q. 5 /1,00", "Q. 6 /1,00", "Q. 7 /1,00", "Q. 8 /1,00", "Q. 9 /1,00", "Q. 10 /1,00");
# Encoding(namesList) <- "UTF-8";
# names(data) <- namesList;
# #write.xlsx(data, file = "0.xlsx", row.names = FALSE);