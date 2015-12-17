
source("readdata.R") ; source("functions1.R") ; source("functions2.R") ; source("functions_plot.R")
# remove few features
features_selected <- features_all[-c(38,51,52,73,74),]

# run for CV complication
proc_data <- gen_proc_data(data_raw,features_selected,data_raw$cv_comp_new)
sel_list_vif <- vif_test(data_raw,outcome =data_raw$cv_comp_new,feature_list = features_selected )
temp1 <- run_gams_model_single_run(proc_data,sel_list_vif)

t <- temp1$predicted_values
t$category <- ifelse(t$predicted<=0.08,"low",ifelse(t$predicted>0.35,"high","moderate"))

proc_data_num <- gen_proc_data_PCA(data_raw,features_selected,data_raw$cv_comp_new)
proc_data_num$outcome <- proc_data$outcome
red_data <- proc_data_num[-which(t$category=="high"),]
temp2 <- run_SVM_model_single_run(red_data,sel_list_vif,cost = 20,gamma = 0.1)
