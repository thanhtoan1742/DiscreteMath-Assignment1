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
    
    score_frequency <- score %>% group_by(sub_no_6) %>% summarise("freq" = n());
    ggplot(data = score_frequency) +   
        geom_line(mapping = aes(x = sub_no_6, y = freq));
    ggsave("max_score_6.pdf");

    score_frequency <- score %>% group_by(sub_no_3) %>% summarise("freq" = n());
    ggplot(data = score_frequency) +   
        geom_line(mapping = aes(x = sub_no_3, y = freq));
    ggsave("max_score_3.pdf");

    average_score <- vector(mode = "numeric", length = max_nob);
    for (i in 1:max_nob)
        average_score[i] = score %>% pull(i) %>% mean();
    print(average_score);
    average_score <- tibble(no = 1:max_nob, ave = average_score);

    print(average_score);
    ggplot(data = average_score) +   
        geom_line(mapping = aes(x = no, y = ave));
    ggsave("average_score.pdf");

    print(average_score[max_nob, 2]);
    print(mean(by_stdid$total_score));
 }

# READ DATA
data <- read.xlsx(filename, sheetIndex = 1, encoding = "UTF-8", colIndex = 1:16);
data <- head(data, -1);
namesList <- c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
names(data) <- namesList;
row.names(data) <- NULL;
data <- as_tibble(data) %>% filter(total_score >= 0);
data$total_score <- to_number(data$total_score);
data$time_begin <- strptime(data$time_begin, format = "%d %B %Y  %I:%M %p");
arrange(data, desc(stdid));

Question5(data);
Question10(data);


### QUESTION 5
# the matrix score (it is acutally a tibble) save the maximum score after a number of submissions. 
# the formula is:
#     score[i, j] = the maximum score of student i after j submissions.
#     score[i, j] = max(score[i, j - 1], the score of the j-th submission).
#     if there is no j-th submission, the score of j-th submission is set to 0.

# i make 3 plot. They are:
#     -the scores of all students after sixth submission.
#     -the scores of all students after third submission.
#     -the average score after the maximun number of submissions.