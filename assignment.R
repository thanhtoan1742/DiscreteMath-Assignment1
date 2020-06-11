average_score = function(data, data_size) {
    sort_order = order(data$uid, data$tid);

    average = vector();
    student_problem_count = 0;
    max_score = data[sort_order[1], ]$score;
    j = 1;
    k = 0;
    for (i in 1:data_size) {
        if ((data[sort_order[i], ]$uid == data[sort_order[j], ]$uid) & (data[sort_order[i], ]$tid == data[sort_order[j], ]$tid)) {
            k = k + 1;
            max_score = max(max_score, data[sort_order[i], ]$score);

            if (k <= length(average)) {
                average[k] = average[k] + max_score;
            } else {
                if (k > 1) {
                    average[k] = average[k - 1];
                } else {
                    average[k] = 0;
                }
                average[k] = average[k] + max_score;
            }
        } else {
            student_problem_count = student_problem_count + 1;
            while (k < length(average)) {
                k = k + 1;
                average[k] = average[k] + max_score;
            }

            k = 0;
            max_score = data[sort_order[i], ]$score;
            j = i;
        }
    }
    student_problem_count = student_problem_count + 1;
    while (k < length(average)) {
        k = k + 1;
        average[k] = average[k] + max_score;
    }

    return(average / student_problem_count);
}

#Setup
MD = 12;
data = read.csv("data.csv");
data_size = nrow(data);

#Question 1
tidn = (MD %% 2) + 1;
startn = 1 + floor(MD / 6) * 30;
score = subset(data, tid == tidn)[startn:(startn + 399), 5];

#Question 2
number_of_student = length(score);

#Question 6
average = average_score(data, data_size);
print(average[min(6, length(average))]);
k = ((MD * 37 + 59) %% 5 + 1) * (tidn * 2 - 3) + 6;
print(average[min(k, length(average))]);
print(average);

png(file = "plot.png");
barplot(average);
dev.off();
