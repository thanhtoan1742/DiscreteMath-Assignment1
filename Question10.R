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

# QUESTION 10
hard_working_student_with_many_sub = function(data) {
    hard_working_nob = 4;
    hard_working <- hard_working_student(data) %>% filter(nob >= hard_working_nob);
    return(hard_working);
}

Question10 = function(data) {
    write_data("Câu 10");
    active_student <- union(hard_working_student_with_many_sub(data)$stdid, smart_student(data)$stdid);
    active_student <- data %>% filter(stdid %in% active_student) %>% group_by(stdid) %>% slice_max(total_score, n = 1);
    # b
    write_data("b. Số lượng sinh viên chủ động:");
    write_data(nrow(active_student));

    # c
    write_data("c. Phổ điểm của sinh viên biết cách học chủ động:");
    spectrum_active_student = active_student %>% group_by(total_score) %>% summarise("frequency" = n());
    write_data(spectrum_active_student);
    ggplot(data = spectrum_active_student) +   
        geom_line(mapping = aes(x = total_score, y = frequency)) +
        labs(title = "spectrum of scores", x = "Score", y = "Frequency");
    plot_file_name = paste("Screenshot\\q10c_file", as.character(file_number), ".png", sep = "");
    ggsave(plot_file_name);
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

Question10(data);