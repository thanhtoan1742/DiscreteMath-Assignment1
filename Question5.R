options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(readxl);
library(xlsx);

file_number = 1;
tid = 6;
file_in = paste("Data\\", as.character(file_number), ".xlsx", sep = "");
file_out = paste("Result\\", as.character(file_number), ".tsv", sep = "");

debug_log = function(data) {
    print("-------------------------------DEBUG-------------------------------")
    print(data);
    print("-------------------------------END DEBUG-------------------------------")
}

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

# QUESTION 5
get_max_nob = function(data) {
    return(max(data$nob));
}

Question5 = function(data) {
    write_data("Câu 5");
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
    plot_file_name = paste("Screenshot\\q5a_file", as.character(file_number), ".png", sep = "");
    ggsave(plot_file_name);

    # b
    score_frequency <- score %>% group_by(sub_no_3) %>% summarise("freq" = n());
    ggplot(data = score_frequency) +   
        geom_line(mapping = aes(x = sub_no_3, y = freq)) +
        labs(title = "Student's score after 3 submissions", x = "Score", y = "Number of student");
    plot_file_name = paste("Screenshot\\q5b_file", as.character(file_number), ".png", sep = "");
    ggsave(plot_file_name);

    # c
    average_score <- vector(mode = "numeric", length = max_nob);
    for (i in 1:max_nob)
        average_score[i] = score %>% pull(i) %>% mean();
    average_score <- tibble(no = 1:max_nob, ave = average_score);

    ggplot(data = average_score) +   
        geom_line(mapping = aes(x = no, y = ave)) +
        labs(title = "Student's average score after x submissions", x = "Number of submission", y = "Average score");
    plot_file_name = paste("Screenshot\\q5c_file", as.character(file_number), ".png", sep = "");
    ggsave(plot_file_name);

    # d
    write_data("d. Điểm trung bình các sinh viên đạt được:")
    write_data(as.numeric(average_score[max_nob, 2]));
}

# READ DATA
data <- read_excel(file_in, sheet = 1);
data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10");
names(data) <- namesList;
row.names(data) <- NULL;
data <- as_tibble(data) %>% filter(total_score >= 0);
data$time_begin <- strptime(data$time_begin, format = "%d %B %Y  %I:%M %p");
data$total_score <- to_number(data$total_score);
data <- arrange(data, desc(time_begin));

Question5(data);