

list.files('../GettingCleaning/UCI HAR Dataset/train')

library(data.table)
library(dplyr)
library(tidyr)

activity_labels <- fread('../GettingCleaning/UCI HAR Dataset/activity_labels.txt')

features <- fread('../GettingCleaning/UCI HAR Dataset/features.txt')
features <- rename(features, var_id=V1, var_name=V2)

x_train <- fread('../GettingCleaning/UCI HAR Dataset/train/X_train.txt')
names(x_train) <- features[, var_name]

y_train <- fread('../GettingCleaning/UCI HAR Dataset/train/y_train.txt')

sub_train <- fread('../GettingCleaning/UCI HAR Dataset/train/subject_train.txt')
sub_train <- rename(sub_train, subject_id=V1)

x_train <- cbind(sub_train, x_train)
train <- cbind(x_train, y_train[activity_labels, .(label_name = i.V2),on=list(V1=V1) , nomatch = 0])

####
x_test <- fread('../GettingCleaning/UCI HAR Dataset/test/X_test.txt')
names(x_test) <- features[, var_name]

y_test <- fread('../GettingCleaning/UCI HAR Dataset/test/y_test.txt')

sub_test <- fread('../GettingCleaning/UCI HAR Dataset/test/subject_test.txt')
sub_test <- rename(sub_test, subject_id=V1)

x_test <- cbind(sub_test, x_test)
test <- cbind(x_test, y_test[activity_labels, .(label_name = i.V2),on=list(V1=V1) , nomatch = 0])

combined_data <- rbind(train, test)

cols <- c('subject_id', 
          names(combined_data)[
              (grepl("mean",names(combined_data)) & !grepl("meanFreq",names(combined_data)))|
                  (grepl("std",names(combined_data)))],
          'label_name')

final_data <- combined_data[,..cols]

final_agg_data <- final_data[,
                             lapply(.SD, mean),
                             by=.(subject_id, label_name)]

df_inter <- gather(final_agg_data, 'variable-stat-cordinate', average, -subject_id, -label_name, na.rm=TRUE) %>%
    separate('variable-stat-cordinate', c("measured_variable","stats","coordinate"), sep="-") 

setDT(df_inter)

df_inter[grepl('Mag',measured_variable), c("coordinate", "measured_variable"):=list('Mag', gsub('Mag', '', measured_variable)) ]

df_inter[grepl('BodyBody', measured_variable), measured_variable := gsub('BodyBody', 'Body', measured_variable)]

project_result <- df_inter %>% spread(coordinate, average)

setDT(project_result)

project_result <- project_result[stats=='mean()'][, stats:=NULL]

project_result






