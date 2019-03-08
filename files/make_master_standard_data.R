# File for making .csv of all subject's standard data
library(data.table)
library(dplyr)

EXPERIMENTS = c(3,5)

# 3 = experiment with 3 payoff conditions
# 5 = experiment with 5 payoff conditions

for (exp in EXPERIMENTS) {
  if (exp == 3) {
    data_dir = sprintf("%s/exp2/standard_data",getwd())
  } else {
    data_dir = sprintf("%s/exp1/standard_data",getwd())
  }
  files <- dir(data_dir, pattern = 'standard_test_data.csv', full.names = TRUE)
  tables <- lapply(files, fread)
  master_table = bind_rows(tables)
  write.csv(master_table,sprintf("%s/master_data/all_standard_test_data%s.csv",getwd(),exp))
}
