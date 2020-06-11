average_score = function(data, data_size) {
  sort_order = order(data$uid, data$tid);

  average = vector();
  student_problem_count = 0;
  max_score = data[sort_order[1],]$score;
  j = 1;
  k = 0;
  for (i in 1:data_size) {
    if ((data[sort_order[i], ]$uid == data[sort_order[j], ]$uid) & (data[sort_order[i], ]$tid == data[sort_order[j], ]$tid)) {
      k = k + 1;
      max_score = max(max_score, data[sort_order[i],]$score);

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
      max_score = data[sort_order[i],]$score;
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

#Question 1
solve_1 = function(data) {
  print(nrow(data));
  return(nrow(data));
}

#Get data
get_data = function(tid) {
  data = read.xlsx("data\\CO1007_TV_HK192-Quiz 1.1-điểm.xlsx");
  print(data);
  return(data);
}

#Setup
make_tid = function(MD) {
  tid = vector();
  tid[1] = MD %% 24 + 1;
  t = MD / 10;
  for (i in 2:4) {
    tid[i] = (tid[1] + t %% 10) %% 24 + 1;
    t = t / 10;
  }
  return(tid);
}

MD = 1357;
tid = make_tid(MD);

for (i in 1:4) {
  data = get_data(tid[i]);
  solve_1(data);
}
