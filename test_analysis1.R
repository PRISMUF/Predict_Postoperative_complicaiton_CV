
source("readdata.R") ; source("functions1.R") ; source("functions2.R")

# remove few features
features_selected <- features_all[-c(38,51,52),]

# perform variance inflation factor (VIF) test
sel_list_vif <- vif_test(data_raw,outcome =data_raw$cv_comp_new,feature_list = features_selected )

# run for CV complication
proc_data <- gen_proc_data(data_raw,features_selected,data_raw$cv_comp_new)
temp_cv <- run_gams_model(proc_data,features_selected)
model <- temp_cv$model 
train_result <- temp_cv$trainResults_rawData
test_result  <- temp_cv$testResults_rawData

# run for CV complication with PCA
proc_data <- gen_proc_data_PCA(data_raw,features_selected,data_raw$cv_comp_new)
temp_cv_pca <- run_gams_model_PCA(clean_data_pca = proc_data,outcome = data_raw$cv_comp_new,variance_prop = 0.9)
model <- temp_cv_pca$model 
train_result <- temp_cv_pca$trainResults_rawData
test_result  <- temp_cv_pca$testResults_rawData

# run for CV complication with reduced data set
proc_data <- gen_proc_data(data_raw,sel_list_vif,data_raw$cv_comp_new)
temp_cv_red_feat <- run_gams_model(proc_data,sel_list_vif)
model <- temp_cv_red_feat$model 
train_result <- temp_cv_red_feat$trainResults_rawData
test_result  <- temp_cv_red_feat$testResults_rawData


# run for CV complication with PCA for 0.5 fraction variance
proc_data <- gen_proc_data_PCA(data_raw,features_selected,data_raw$cv_comp_new)
temp_cv_pca <- run_gams_model_PCA(clean_data_pca = proc_data,outcome = data_raw$cv_comp_new,variance_prop = 0.5)
model <- temp_cv_pca$model 
train_result <- temp_cv_pca$trainResults_rawData
test_result  <- temp_cv_pca$testResults_rawData