options(encoding = "UTF-8");
library(utf8);
library(tidyverse);
library(xlsx);
library(e1071);

sum_all = function(data_col,data_size) {
  all_score = 0
  for(i in 1:data_size)
  {
    all_score = all_score + (data_col[i])
  }
  return (all_score)
}

#Get data
DataChart = xlsx::read.xlsx("Data\\5.xlsx", sheetIndex = 1, stringsAsFactors = FALSE)
colnames(DataChart) = c("stdid", "stat", "time_begin", "time_end", "time_duration", "total_score", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")

#Question 1
"Number of submit"


#Question 2
# 2.a) Sum of all student score
for(i in 1:nrow(DataChart))
{
  if(DataChart$total_score[i] == "-")
  {
    DataChart$total_score[i] = "0,00"
  }
  if(DataChart$Q1[i] == "-")
  {
    DataChart$Q1[i] = "0,00"
  }
  if(DataChart$Q2[i] == "-")
  {
    DataChart$Q2[i] = "0,00"
  }
  if(DataChart$Q3[i] == "-")
  {
    DataChart$Q3[i] = "0,00"
  }
  if(DataChart$Q4[i] == "-")
  {
    DataChart$Q4[i] = "0,00"
  }
  if(DataChart$Q5[i] == "-")
  {
    DataChart$Q5[i] = "0,00"
  }
  if(DataChart$Q6[i] == "-")
  {
    DataChart$Q6[i] = "0,00"
  }
  if(DataChart$Q7[i] == "-")
  {
    DataChart$Q7[i] = "0,00"
  }
  if(DataChart$Q8[i] == "-")
  {
    DataChart$Q8[i] = "0,00"
  }
  if(DataChart$Q9[i] == "-")
  {
    DataChart$Q9[i] = "0,00"
  }
  if(DataChart$Q10[i] == "-")
  {
    DataChart$Q10[i] = "0,00"
  }
}

DataChart$total_score = gsub(",",".",DataChart$total_score);
DataChart$total_score = as.numeric(DataChart$total_score);
sum_all_total_score = sum_all(DataChart$total_score, nrow(DataChart));
print("sum score of all submit")
print(sum_all_total_score);

# 2.b) Finding lowest-score
lowest_score_b = min(DataChart$total_score);
print("lowest score");
print(lowest_score_b);

# 2.c) List of student with lowest-score
lowest_chart_c = subset(DataChart, total_score == lowest_score_b);
print("list student have a lowest score submit");
print(lowest_chart_c$stdid);

# 2.d) Spectral of submit time from students of lowest score
lowest_time_d = data.frame(
  "stdid" = lowest_chart_c$stdid,
  "times" = c(0)
);

for (i in 1:nrow(lowest_time_d))
{
  stdid_temp = lowest_time_d$stdid[i];
  for(j in 1:nrow(DataChart))
  {
    stdid_temp_2 = DataChart$stdid[j];
    if(!is.na(stdid_temp_2) && (stdid_temp_2 == stdid_temp))
    {
      lowest_time_d$times[i] = lowest_time_d$times[i] + 1;
    }
  }
}
print("Time submit of students having at least 1 lowest score submit");
print(lowest_time_d);

#2.e) Get the lowest student final score
DataChart = subset(DataChart, !(is.na(stdid)) );
stdid_list_e = DataChart;

stdid_list_e_stdid = stdid_list_e$stdid;
stdid_list_e_stdid = unique(stdid_list_e_stdid, incomparables = FALSE)

final_score_chart_e = data.frame(
  "stdid" = stdid_list_e_stdid,
  "final_score" = c(0)
)

print("Final Score Chart stdid");
#print(final_score_chart_e$stdid);

for(i in 1:nrow(final_score_chart_e))
{
  for(j in 1:nrow(DataChart))
  {
    if(DataChart$stdid[j] == final_score_chart_e$stdid[i])
    {
      if(DataChart$total_score[j] > final_score_chart_e$final_score[i])
      {
        final_score_chart_e$final_score[i] = DataChart$total_score[j];
      }
    }
  }
}
print("Final Score Chart stdid and final_score");
print(final_score_chart_e);

print("Lowest Final score")
print(min(final_score_chart_e$final_score));
lowest_final_score_e = min(final_score_chart_e$final_score)

#2.f) List of student with lowest final score
lowest_final_list = subset(final_score_chart_e, final_score == lowest_final_score_e);
print("List of student with lowest final score")
print(lowest_final_list$stdid);

#2.g) Spectral of submit time from students of lowest final score
lowest_final_list$times = c(0)
for (i in 1:nrow(lowest_final_list))
{
  stdid_temp = lowest_final_list$stdid[i];
  for(j in 1:nrow(DataChart))
  {
    stdid_temp_2 = DataChart$stdid[j];
    if(!is.na(stdid_temp_2) && (stdid_temp_2 == stdid_temp))
    {
      lowest_final_list$times[i] = lowest_final_list$times[i] + 1;
    }
  }
}
print("List of student with lowest final score and submit time")
print(lowest_final_list);

#2.h) Find Highest_score
highest_score_h = max(DataChart$total_score)
print("Highest Final Score");
print(highest_score_h);

#2.i) List of students with at least 1 highest score
highest_chart_i = subset(DataChart, total_score == highest_score_h)
print("stdid List of student with atleast one highest_score");
print(nrow(highest_chart_i$stdid));

#2.j) Spectral of submit time from students of final lowest score
highest_final_score_chart = data.frame(
  "stdid" = highest_chart_i$stdid,
  "times" = c(0)
)

for (i in 1:nrow(highest_final_score_chart))
{
  stdid_temp = highest_final_score_chart$stdid[i];
  for(j in 1:nrow(DataChart))
  {
    stdid_temp_2 = DataChart$stdid[j];
    if(!is.na(stdid_temp_2) && (stdid_temp_2 == stdid_temp))
    {
      highest_final_score_chart$times[i] = highest_final_score_chart$times[i] + 1;
    }
  }
}
print("Highest final score chart and submit times");
#print(highest_final_score_chart);
#2.k)

#2.l)

#2.m)

#2.n) Caculate average final_score of each student
average_final_score_all_student = sum(final_score_chart_e$final_score) / nrow(final_score_chart_e);
print("average_final_score_all_student")
print(average_final_score_all_student)
final_score_chart_e$average_score = c(0)
final_score_chart_e$times = c(0)
for (i in 1:nrow(final_score_chart_e))
{
  stdid_temp = final_score_chart_e$stdid[i];
  for(j in 1:nrow(DataChart))
  {
    stdid_temp_2 = DataChart$stdid[j];
    if(!is.na(stdid_temp_2) && (stdid_temp_2 == stdid_temp))
    {
      final_score_chart_e$average_score[i] = final_score_chart_e$average_score[i] * final_score_chart_e$times[i] + DataChart$total_score[j]
      final_score_chart_e$times[i] = final_score_chart_e$times[i] + 1
      final_score_chart_e$average_score[i] = final_score_chart_e$average_score[i] / final_score_chart_e$times[i]
    }
  }
}
print("Final score list with average score")
#print(final_score_chart_e);

#2.o) Number of student have a average final score
score_match_average_list = subset(final_score_chart_e, final_score == average_final_score_all_student)
print("Number of student have a average final score")
print(nrow(score_match_average_list))

#2.p) Median, Max, Min
Median_final_score = median(final_score_chart_e$final_score)
print("Median average score");
print(Median_final_score);

Max_final_score = max(final_score_chart_e$final_score)
print("Max average score");
print(Max_final_score);

Min_final_score = min(final_score_chart_e$final_score)
print("Min average score");
print(Min_final_score);

#2.q)Range, Variance, Standard Deviation
range_final = range(final_score_chart_e$final_score)
print("Range of average score");
print(range_final);
range_final_value = range_final[2] - range_final[1]

#Probability of Distribution Average score
probability_chart = data.frame(
  "score" = NULL,
  "times" = NULL
)
for(i in 1:nrow(final_score_chart_e))
{
  if(final_score_chart_e$final_score[i] %in% probability_chart$score)
  {
    index = which(probability_chart$score == final_score_chart_e$final_score[i])
    probability_chart$times[index] = probability_chart$times[index] + 1
  }
  else 
  {
    temp_chart = data.frame(
    "score" = c(final_score_chart_e$final_score[i]),
    "times" = 1
    )
    probability_chart = rbind(probability_chart,temp_chart)
  }
}

result_chart = probability_chart
probability_chart$times = probability_chart$times / nrow(final_score_chart_e)
probability_chart = probability_chart[order(-result_chart$score),]

print("probablility of distribution")
print(probability_chart);

variance_final_score = var(final_score_chart_e$final_score)
standard_deviation_final_score = sd(final_score_chart_e$final_score)
print("Variance average score")
print(variance_final_score)
print("Standard Deviation average score")
print(standard_deviation_final_score)

#2.r)Skewness and Kurtosis of Average Score
print("Skewness Average Score")
print(skewness(final_score_chart_e$final_score))
print("Kurtosis Average Score")
print(kurtosis(final_score_chart_e$final_score))

#2.s)Quantile 1st and 3rd
print("Quantile")
quantile(final_score_chart_e$final_score)

#2.t)
print("result_chart")
result_chart = result_chart[order(-result_chart$score), ,drop = FALSE]
print(result_chart$times[1] + result_chart$times[2])

#2.u)
final_score_chart_e_first = subset(final_score_chart_e, final_score == result_chart$score[1])
final_score_chart_e_second = subset(final_score_chart_e, final_score == result_chart$score[2])
final_score_chart_e_first_second = rbind(final_score_chart_e_first, final_score_chart_e_second)
final_score_chart_e_first_second = final_score_chart_e_first_second
#2.v)
print(result_chart)

#2.w)
result_chart$total_submit = c(0)
for(i in 1:nrow(result_chart))
{
  for(j in 1:nrow(final_score_chart_e))
  {
    if(result_chart$score[i] == final_score_chart_e$final_score[j])
    {
      result_chart$total_submit[i] = result_chart$total_submit[i] + final_score_chart_e$times[j]
    }
  }
}

print(sum(final_score_chart_e$times))
print(result_chart)
