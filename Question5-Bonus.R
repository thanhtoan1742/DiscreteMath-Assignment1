options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(readxl);
library(xlsx);

MD = 7241;
filename = "data\\4.xlsx";

debug_log = function(data) {
    print("-------------------------------DEBUG-------------------------------")
    print(data);
    print("-------------------------------END DEBUG-------------------------------")
}

to_number = function(a) {
    res = as.numeric(sub(",", ".", a, fixed = TRUE));
    return(res);
}

print_score_detail = function(data) {
    write_tsv(data[, c(1, 6:16)], path = "output.tsv", col_names = FALSE);
}

hard_working_student = function(data) {
    hard_working <- data %>% group_by(stdid) %>% summarise("time_begin" = min(time_begin), "nob" = n());
    hard_working_threshold = nrow(hard_working) %/% 3;
    hard_working <- hard_working %>% slice_min(time_begin, n = hard_working_threshold);
    return(hard_working);
}

smart_student = function(data) {
    nob = 3;
    smart_score = 8;
    smart <- data %>% group_by(stdid) %>% slice_min(time_begin, n = nob) %>% summarise("total_score" = max(total_score));
    smart <- smart %>% filter(total_score >= smart_score);
    return(smart);
}

dealing_student = function(data) {
    dealing <- data %>% group_by(stdid) %>% summarise("time_begin" = min(time_begin));
    dealing_threshold = nrow(dealing) %/% 10;
    dealing <- dealing %>% slice_max(time_begin, n = dealing_threshold);
    return(dealing);
}

good_student = function(data) {
    good <- data %>% group_by(stdid) %>% summarise("total_score" = max(total_score));
    good_threshold = nrow(good) %/% 5;
    good <- good %>% slice_max(total_score, n = good_threshold);
    return(good);
}

# QUESTION 5
get_max_nob = function(data) {
    return(max(data$nob));
}

Question5 = function(data) {
    by_stdid <- data %>% arrange(desc(stdid));
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
                score[u, v] = by_stdid[j, ]$total_score;
            } else
                score[u, v] = max(by_stdid[j, ]$total_score, score[u, v - 1]);
            j = j + 1;
        }

        while (v < max_nob) {
            v = v + 1;
            score[u, v] = score[u, v - 1];
        }
        i = j;
    }

    score <- as_tibble(score) %>% mutate("stdid" = "");
    score$stdid <- unique(by_stdid$stdid);
    names(score) <- c(paste("sub_no_", as.character(1:max_nob), sep = ""), "stdid");
    row.names(score) <- NULL;

    # a
    score_frequency <- score %>% group_by(sub_no_6) %>% summarise("freq" = n());
    ggplot(data = score_frequency) + 
        geom_line(mapping = aes(x = sub_no_6, y = freq)) +
        labs(title = "Student's score after 6 submissions", x = "Score", y = "Number of student");
    ggsave("Screenshot\\Q5\\5a_file4.png");

    # b
    score_frequency <- score %>% group_by(sub_no_3) %>% summarise("freq" = n());
    ggplot(data = score_frequency) +   
        geom_line(mapping = aes(x = sub_no_3, y = freq)) +
        labs(title = "Student's score after 3 submissions", x = "Score", y = "Number of student");
    ggsave("Screenshot\\Q5\\5b_file4.png");

    # c
    average_score <- vector(mode = "numeric", length = max_nob);
    for (i in 1:max_nob)
        average_score[i] = score %>% pull(i) %>% mean();
    average_score <- tibble(no = 1:max_nob, ave = average_score);

    ggplot(data = average_score) +   
        geom_line(mapping = aes(x = no, y = ave)) +
        labs(title = "Student's average score after x submissions", x = "Number of submission", y = "Average score");
    ggsave("Screenshot\\Q5\\5c_file4.png");

    # d
    print("diem trung binh:")
    print(as.numeric(average_score[max_nob, 2]));
    # print(mean(by_stdid$total_score));
}

# QUESTION 10
hard_working_student_with_many_sub = function(data) {
    hard_working_nob = 4;
    hard_working <- hard_working_student(data) %>% filter(nob >= hard_working_nob);
    return(hard_working);
}

Question10 = function(data) {
    active_student <- union(hard_working_student_with_many_sub(data)$stdid, smart_student(data)$stdid);
    active_student <- data %>% filter(stdid %in% active_student) %>% group_by(stdid) %>% slice_max(total_score, n = 1);

    # b
    print("so luong sinh vien chu dong:");
    print(nrow(active_student));

    # c
    print_score_detail(active_student);
}

# QUESTION 11
Question11 = function(data) { 
}

# READ DATA
data <- read_excel(filename, sheet = 1);
data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10");
names(data) <- namesList;
row.names(data) <- NULL;
data <- as_tibble(data) %>% filter(total_score >= 0);
data$time_begin <- strptime(data$time_begin, format = "%d %B %Y  %I:%M %p");
data$total_score <- to_number(data$total_score);
data <- arrange(data, desc(time_begin));

Question5(data);
Question10(data);