################################################################################
## 함수명: my_NA_variable_clean
## 제작일: 2024-07-28
## 인  자: 데이터프레임, NA_cut_off - NA 비율 기준
## 반환값: NA 비율 기준에 따라 NA가 많은 열을 제거한 데이터프레임
## 참  고: 제거되는 열의 이름과 NA 개수를 출력
################################################################################
my_NA_variable_clean <- function(data, NA_cut_off) {
  # Calculate the threshold in terms of number of NAs
  NA_threshold <- nrow(data) * (NA_cut_off / 100)
  
  # Initialize vectors to store column names and NA counts
  columns_with_na <- c()
  na_counts <- c()
  
  # Identify columns to retain and store information about columns with NAs
  columns_to_retain <- sapply(names(data), function(column_name) {
    na_count <- sum(is.na(data[[column_name]]))
    if (na_count > 0) {
      columns_with_na <<- c(columns_with_na, column_name)
      na_counts <<- c(na_counts, na_count)
    }
    return(na_count <= NA_threshold)
  })
  
  # Subset the data to retain only the desired columns
  cleaned_data <- data[, columns_to_retain]
  
  # Print columns with NAs and their counts
  if (length(columns_with_na) > 0) {
    cat("Columns with NAs:\n")
    for (i in seq_along(columns_with_na)) {
      cat(columns_with_na[i], ": ", na_counts[i], " NAs\n", sep="")
    }
  } else {
    cat("No columns with NAs found.\n")
  }
  
  return(cleaned_data)
}


################################################################################
## 함수명: my_NA_row_clean 
## 제작일: 2024-07-17
## 인  자: 데이터프레임
## 반환값: NA가 포함된 열을 제거한 데이터프레임
################################################################################
my_NA_row_clean <- function(data) {
  # Identify columns with NAs and count the number of NAs in each column
  na_info <- sapply(data, function(column) {
    sum(is.na(column))
  })
  
  # Get column names with NAs
  columns_with_na <- names(na_info)[na_info > 0]
  
  # Print column names and number of NAs
  if (length(columns_with_na) > 0) {
    cat("Columns with NAs:\n")
    for (column in columns_with_na) {
      cat(column, ": ", na_info[column], " NAs\n", sep = "")
    }
  } else {
    cat("No columns with NAs found.\n")
  }
  
  # Remove rows with NAs
  cleaned_data <- na.omit(data)
  
  return(cleaned_data)
}



################################################################################
## my_functions_for_load_raw_data
################################################################################
my_read_csv_from_raw_data_folder <- function(csv_filename) {
  # 현재 작업 디렉토리 설정
  project_folder <<- getwd()
  raw_data_folder <<- file.path(project_folder, "raw_data")
  
  # 입력된 파일명에 해당하는 파일 경로 설정
  csv_path <- file.path(raw_data_folder, csv_filename)
  df <- read.csv(csv_path, header=T, dec=".")
  
  return(df)
}

################################################################################
## my_functions_deidentify_raw_data
################################################################################
## 데이터프레임형식 raw_data와 환자번호 컬럼명을 인자로 받아서 serial number를 부여하여 반환하는 함수 
my_deidentify_raw_data <- function(df, col_name) {
  # 열을 정렬하고 고유한 값들을 추출합니다.
  unique_values <- unique(df[[col_name]])
  sorted_values <- sort(unique_values)
  
  # 고유한 값들에 대해 순서를 부여합니다.
  value_to_index <- match(sorted_values, unique_values)
  
  # 해당 열을 고유한 값에 대응하는 숫자로 대체합니다.
  df[[col_name]] <- as.character(value_to_index[match(df[[col_name]], unique_values)])
  
  return(df)
}


################################################################################
## 함수명: my_histgram_with_outliers 
## 제작일: 2024-07-17
## 인  자: 데이터프레임과 컬럼명
## 반환값: outlier를 표시한 히스토그램
################################################################################
my_histgram_with_outliers <- function(df, column_name) {
  
  library(ggplot2)
  # 데이터 준비
  
  data <- df[[column_name]]
  
  # 평균과 표준편차 계산
  mean_value <- mean(data, na.rm = TRUE)
  sd_value <- sd(data, na.rm = TRUE)
  
  # 이상치 범위 계산
  lower_bound <- mean_value - 3 * sd_value
  upper_bound <- mean_value + 3 * sd_value
  
  # 데이터에 이상치 여부 표시
  df$outlier <- ifelse(data < lower_bound | data > upper_bound, "outlier", "normal")
  
  # 히스토그램 그리기
  p <- ggplot(df, aes_string(x = column_name, fill = "outlier")) +
    geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
    scale_fill_manual(name = "Legend", values = c("normal" = "blue", "outlier" = "red")) +
    geom_point(data = subset(df, outlier == "outlier"), aes_string(x = column_name, y = 0), 
               color = "red", size = 4, shape = 21) +
    geom_vline(aes(xintercept = mean_value, color = "Mean"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = lower_bound, color = "3 Std Dev Below"), linetype = "dotted", size = 1) +
    geom_vline(aes(xintercept = upper_bound, color = "3 Std Dev Above"), linetype = "dotted", size = 1) +
    scale_color_manual(name = "Statistics", values = c("Mean" = "green", "3 Std Dev Below" = "red", "3 Std Dev Above" = "red")) +
    labs(title = paste("Histogram of", column_name),
         x = column_name,
         y = "Frequency") +
    theme_minimal()
  
  # 이상치 값 출력
  # outliers <- df[data < lower_bound | data > upper_bound, column_name]
  # cat("Outliers (values that are more than 3 standard deviations from the mean):\n")
  # print(outliers)
  
  return (p)
}


my_histogram_log_transformation_with_outliers <- function(df, column_name) {

  library(ggplot2)
    # 원본 데이터 저장
  original_data <- df[[column_name]]
  
  # NA 값을 포함한 행을 삭제
  df <- df[complete.cases(df), ]
  
  # 데이터의 해당 컬럼을 로그 변환
  df[[column_name]] <- log(df[[column_name]])
  
  # 로그 변환된 데이터 준비
  data <- df[[column_name]]
  
  # 로그 변환된 값의 평균과 표준편차 계산
  mean_value <- mean(data, na.rm = TRUE)
  sd_value <- sd(data, na.rm = TRUE)
  
  # 이상치 범위 계산
  lower_bound <- mean_value - 3 * sd_value
  upper_bound <- mean_value + 3 * sd_value
  
  # 데이터에 이상치 여부 표시
  df$outlier <- ifelse(data < lower_bound | data > upper_bound, "outlier", "normal")
  
  # 히스토그램 그리기
  p <- ggplot(df, aes_string(x = column_name, fill = "outlier")) +
    geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7, position = "identity") +
    scale_fill_manual(name = "Legend", values = c("normal" = "blue", "outlier" = "red")) +
    geom_point(data = subset(df, outlier == "outlier"), aes_string(x = column_name, y = 0), 
               color = "red", size = 4, shape = 21) +
    geom_vline(aes(xintercept = mean_value, color = "Mean"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = lower_bound, color = "3 Std Dev Below"), linetype = "dotted", size = 1) +
    geom_vline(aes(xintercept = upper_bound, color = "3 Std Dev Above"), linetype = "dotted", size = 1) +
    scale_color_manual(name = "Statistics", values = c("Mean" = "green", "3 Std Dev Below" = "red", "3 Std Dev Above" = "red")) +
    labs(title = paste("Histogram of Log-transformed", column_name),
         x = paste("Log of", column_name),
         y = "Frequency") +
    theme_minimal()
  
  # 이상치 값 출력 (로그 변환 이전 값으로 변환)
  outlier_indices <- which(data < lower_bound | data > upper_bound)
  outliers <- original_data[outlier_indices]
  # cat("Outliers (values that are more than 3 standard deviations from the mean):\n")
  # print(outliers)
  
  print(p)
}



my_validation_AGE_SEX_with_graph<-function(dt) {
 
  # 5살 구간으로 AGE 열을 범주화합니다.
  dt$AGE_GROUP <- cut(dt$AGE, breaks = seq(0, 100, by = 5), right = FALSE)
  
  # AGE_GROUP 열의 분포를 Sex 열에 따라 막대 그래프로 시각화합니다.
  xgraph <- ggplot(dt, aes(x = AGE_GROUP, fill = as.factor(SEX))) +
    geom_bar(position = "dodge", color = "black") +
    labs(title = "AGE 분포 (5살 구간)", x = "AGE Group", y = "Count") +
    theme_minimal() +
    scale_x_discrete(drop=FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(xgraph)
}


my_validation_with_graph <- function(dt, col_name) {
  # 주어진 컬럼의 값들의 빈도수를 계산하여 데이터프레임으로 변환
  freq_table <- as.data.frame(table(dt[[col_name]]))
  colnames(freq_table) <- c("Value", "Frequency")
  
  # 빈도 그래프를 생성
  xgraph <- ggplot(freq_table, aes(x = Value, y = Frequency, fill = Value)) +
    geom_bar(stat = "identity", color = "black") +
    labs(title = paste(col_name, "값들의 빈도수"), x = col_name, y = "빈도수") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_discrete(name = col_name)
  
  return(xgraph)
}

# 박스플롯을 생성하는 함수 정의
my_create_box_plot <- function(dt, column_name) {

  # ggplot2를 사용하여 박스플롯 생성
  plot <- ggplot(dt, aes_string(x = "", y = column_name)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", column_name),
         y = column_name,
         x = "") +
    theme_minimal()
  
  return(plot)
}

my_plot_histograms <- function(data) {
  # 히스토그램 저장할 리스트 초기화
  histogram_list <- list()
  
  # 각 열에 대해 히스토그램 그리기
  for (col_name in names(data)) {
    # 숫자형 컬럼인지 확인
    if (is.numeric(data[[col_name]])) {
      p <- ggplot(data, aes_string(x = col_name)) +
        geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency") +
        theme_minimal()
      
      # 히스토그램을 리스트에 추가
      histogram_list[[col_name]] <- p
    }
  }
  
  # 히스토그램 리스트 반환
  return(histogram_list)
}

my_get_columns_with_missing_data <- function(data, threshold) {
  # 결측치 비율 계산
  missing_percentage <- sapply(data, function(col) {
    sum(is.na(col)) / length(col) * 100
  })
  
  # 기준치 이상인 열 이름 필터링
  columns_above_threshold <- names(missing_percentage[missing_percentage > threshold])
  
  # 기준치 이상인 열 이름 리스트 반환
  return(columns_above_threshold)
}

my_histogram <- function(dt, column_name) {
  
  # ggplot2를 사용하여 박스플롯 생성
  plot <- ggplot(dt, aes_string(x = "", y = column_name)) +
    geom_histogram() +
    labs(title = paste("Histogram of", column_name),
         y = column_name,
         x = "") +
    theme_minimal()
  
  return(plot)
}