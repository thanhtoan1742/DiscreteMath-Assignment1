average_score_k_time = function(data, k) {
    uid_levels = levels(factor(data$uid));
    for (u in uid_levels)
        for (i in 1:2)
        {
            submission_set = subset(data, uid == u & tid == i);
            print(submission_set);
        }
}

MD = 12;
#MD = scan();
data = read.csv("data.csv");
data_size = nrow(data);

tidn = (MD %% 2) + 1;
startn = 1 + floor(MD / 6) * 30;
score = subset(data, tid == tidn)[startn:(startn + 399), 5];

number_of_student = length(score);
average_score_k_time(data, 2);
