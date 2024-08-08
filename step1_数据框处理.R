rm(list=ls())
#1.创建文件夹，读取数据，保存数据，加载包 ####
library(tidyr)
library(dplyr)
library(stringr)
library(rio)

if(!dir.exists("step1_数据处理")){
  dir.create("step1_数据处理")
}
getwd()
data<- import("大PANEL诊断入选marker调研-.xlsx",sheet=2)
current_dir <- getwd()
current_dir
file_path <- file.path(current_dir, "step1_数据处理")
setwd(file_path)
setwd("../")

#2.查看数据格式,并获得列名的indices####
library(data.table)
str(data)
colnames_vector <- colnames(data)
column_indices <- sapply(colnames_vector, function(x) which(colnames(data) == x))
names(column_indices) <- colnames_vector
print(column_indices)

##2.1我的方案 ####
colnames_vector <- colnames(data)
column_indices <- (1:ncol(data))
names(column_indices) <- colnames_vector
print(column_indices)

#3.查看数据框结构，将部分列由chr转换为num ####
##3.1确定需要转换的列的索引 ####
cols_to_convert <- c(5:32)
##3.2根据索引获得列名 ####
cols_to_convert <- colnames(data)[cols_to_convert]
##3.3这里使用lapply来应用转换，然后用as.data.frame来确保结果仍然是data.frame ####
converted_data <- as.data.frame(lapply(data[, cols_to_convert], 
                                       function(x) as.numeric(as.character(x))
                                       )
                                )
##3.4将转换后的列赋值回原始data.frame ####
data[, cols_to_convert] <- converted_data
str(data)


#4. 对诊断字段的处理 ####
df = data[,c("FlowDs", "PathDs")]

# 清洗数据，例如去除空白字符
df <- df %>%
  mutate(FlowDs = str_trim(FlowDs),
         PathDs = str_trim(PathDs))


##4.1 把顺序整理一下，把BCL，DLBCL，FL等诊断字段排前面，等下拆分后都位于第一列，方便提取 ####
# 假设df是你的数据框，FlowDs是你要替换文本的列名
# 使用str_replace_all同时替换两个条件
df <- df %>%
  mutate(FlowDs = str_replace_all(
    FlowDs,
    c("5N_10N_small_BCL" = "BCL_5N_10N_small", 
      "5N_10N_BCL" = "BCL_5N_10N",
      "CD5_small_BCL" = "BCL_CD5_small",
      "CD5_BCL" = "BCL_CD5",
      "Atypical_CLL_3" = "CLL_Atypical_3")
  )) %>%
  mutate(PathDs = str_replace_all(
    PathDs,
    c("EN_MZL" = "MZL_EN",
      "5N_10N_small_BCL" = "BCL_5N_10N_small",
      "small_BCL_unclassfiable" = "BCL_small_unclassifiable",
      "small_BCL_pred_MZL" = "MZL_pred",
      "CD5_small_BCL_?" =  "BCL_CD5_small_?")
  ))

data[,c("FlowDs","PathDs")]= df

##4.2 把整理过顺序的病理诊断字符串分割 str_split ####
A = str_split(df$FlowDs, pattern = "_", simplify = T)
B = str_split(df$PathDs, pattern = "_", simplify = T)
#A和B都提取第一行，合并成新的数据框
df2= cbind(A[,1], B[,1])
colnames(df2) = c("FlowDs", "PathDs")
df2 = as.data.frame(df2)


getwd()
save(data,df2,df,
     file = "step1.Rdata")
