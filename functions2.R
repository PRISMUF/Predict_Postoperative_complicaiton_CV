# Set of user defined functions for model building and performance reporting
# Created by Paul Thottakkara : Last Updated on 2 December 2015 

# develop gams model and report performance
run_gams_model <- function(proc_data,feature_list){
  if("outcome"%in%colnames(proc_data)){print("outcome variable found")}else{
    print("outcome variable missing");
    return(list("model"=NULL,"testResults_rawData"=NULL,"trainResults_rawData"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  s <- "outcome~"
  for(i in 1:nrow(feature_list)){
    if(feature_list[i,2]=="num"){s <- paste0(s,"s(",feature_list[i,1],",k=5)+")}else{
      s <- paste0(s,feature_list[i,1],"+")
    }
  }
  s <- substr(s,1,nchar(s)-1) ; s <- as.formula(s)
  
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  print("Running GAMs iterations to report training and test performance")
  for(i in 1:10){
      print(i) ; 
      train_ind <- sample(1:nrow(data_model),size = floor(0.7*nrow(data_model)))
      test_ind <- setdiff(1:nrow(data_model),train_ind)
      model <- bam(formula = s,family = fm,data = data_model[train_ind,])
      temp <- perf_gam(model,traindata=data_model[train_ind,],testdata=data_model[test_ind,],thres_tune)
      train_result <- rbind(train_result,temp$train_result)
      test_result <- rbind(test_result,temp$test_result)
  }
  model = bam(formula = s,family = fm,data = data_model)
  print("Model building completed")
  pred <- predict(model,data_model,type="response") ; 
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"testResults_rawData"=test_result,"trainResults_rawData"=train_result,"predicted_values"=pred_val,"status"="PASS"))
}

# funciton to calculate the performance of gams model
perf_gam <- function(model,traindata,testdata,thres_tune=NULL){
  if(is.null(thres_tune)){
    prev <- 0.5
  }else{
    prev <- as.numeric(thres_tune)
  }
  ind <- sample(1:nrow(traindata),floor(0.42*nrow(traindata)))
  pred <- as.numeric(predict(model,newdata = traindata[ind,],type="response"))
  obser <- rep(0,length(ind)); obser[traindata$outcome[ind]==1] <- 1
  
  train_result <- measure_perform(pred,obser,prev)
  
  pred <- as.numeric(predict(model,newdata = testdata,type="response"))
  obser <- rep(0,nrow(testdata)) ; obser[testdata$outcome==1] <- 1
  test_result <- measure_perform(pred,obser,prev)
  
  return(list("train_result"=train_result,"test_result"=test_result))
}

# funcitons for performance measure
measure_perform <- function(pred,obser,prev){
  results <- data.frame(matrix(nrow=1,ncol=6));  colnames(results) <- c("Accuracy","AUC","PPV","NPV","FALSE_Negtives","HL")
  acc <- ifelse(pred>prev,1,0)
  tab <- table(acc,obser)
  
  results$Accuracy <- sum(tab[c(1,4)])/sum(tab)
  results$AUC <- roc.area(obser,pred)$A
  results$PPV <- sum(tab[c(4)])/sum(tab[c(2,4)])
  results$NPV <- sum(tab[c(1)])/sum(tab[c(1,3)])
  results$FALSE_Negtives <- sum(tab[c(3)])/sum(tab[c(3,4)]) 
  results$HL <- hosmerlem(y=obser,yhat=pred)$p
  return(results)
}
hosmerlem <- function (y, yhat, g = 10){
  id <- is.finite(y) & is.finite(yhat)
  y<-y[id]
  yhat<-yhat[id]
  n <- length(yhat);
  a = sort(yhat, decreasing = TRUE, index.return = TRUE);
  y1<-y[a$ix];
  p1<-seq(0,0,length = g);
  p2<-seq(0,0,length = g);
  for (i in 1:g)
  {
    p1[i] = mean(a$x[(as.integer((i-1)/g*n)+1):as.integer(i/g*n)]);
    p2[i]  = sum(y1[(as.integer((i-1)/g*n)+1):as.integer(i/g*n)]==1); 
  }
  s <- sum((p1*n/g - p2)^2/(n/g*p1*(1-p1)));
  #plot(p1, col = "blue"); par(new= TRUE); plot(p2, col = "red");
  list("p" = 1-pchisq(s,g-2), "xi^2" = s)     
}

# generate data for modelling
gen_proc_data <- function(raw_data,feature_list,outcome){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  # replace missing values in admission source
  raw_data$Admission_Source[raw_data$Admission_Source==""] <- "outpatient"
  raw_data <- raw_data[,which(colnames(raw_data)%in%feature_list[,1])]
  raw_data$outcome <- t
  
  if(sum(colnames(raw_data)%in%feature_list[,1])!=nrow(feature_list)){
    print("fields mismatch in feature list and raw data, following not found in raw data")
    print(as.character(feature_list[which(!feature_list[,1]%in%colnames(raw_data)),1]))
  }
  
  for(i in 1:nrow(feature_list)){
    ind <- which(colnames(raw_data)==feature_list[i,1])
    if(length(ind)==0){print(feature_list[i,1]);next}
    if(length(ind)>1){ind <- ind[1]}
    if(feature_list[i,2]=="num"){
      temp <- outlier_detect(raw_data[,ind])
      raw_data[,ind] <- temp$data ; next
    }
    if(feature_list[i,2]=="cat"&feature_list[i,1]=="pr1c"){
      t <- raw_data[,ind] ; t <- gsub("[^0-9]","",t)
      d<-TrainProcedureFeature(t, outcome, 100)
      raw_data[,ind] <- d$procedures ; next
    }
    if(feature_list[i,2]=="cat"&length(unique(raw_data[,ind]))>5){
      d<-TrainCategoricalFeature(raw_data[,ind], outcome, 100, 2)          
      raw_data[,ind] <- d$d	; next
    }else{
      raw_data[,ind] <- clean_categorical(raw_data[,ind])
    }
  }
  
  return(raw_data)
}
clean_categorical <- function(vect){
  ind <- which(is.na(vect)|vect==""|vect=="MISSING"|vect=="NA"|vect=="UNKNOWN"|vect=="-"|vect==" ")
  if(length(ind)>0){levels(vect) <- c(levels(vect),"MISSING");vect[ind]<-"MISSING"}
  vect <- as.factor(vect) ; absent_levels <- levels(vect)[which(!(levels(vect)%in%unique(vect)))]
  if(length(absent_levels)>0){vect <- droplevels(vect,absent_levels)}
  vect <- grouping_algo(vect)
  return(vect)
}
grouping_algo <- function(vect){
  thres <- 50
  tab <- table(vect) ; ind <- which(tab<thres)
  if(length(ind)>0){
    low_prop <- names(tab)[ind] ; levels(vect) <- c(levels(vect),"OTHERS")
    ind <- which(vect%in%low_prop) ; vect[ind] <- "OTHERS"
    ind <- which(table(vect)>0) ; sel_levels <- names(table(vect))[ind]
    vect <- droplevels(vect,sel_levels)
    if(sum(vect=="OTHERS")<thres){
      if(length(unique(vect))==2){return(vect)}
      tab <- sort(table(vect)) ; rep_level <- names(tab)[2]
      vect[which(vect==rep_level)] <- "OTHERS"
      vect <- droplevels(vect,rep_level)
    }
  }
  return(vect)
}
outlier_detect <- function(vect){
  median_dev <- mad(vect,na.rm = TRUE) ; x_bar <- mean(vect,na.rm = TRUE)
  ind_l <- which(vect<quantile(vect,probs = 0.01,na.rm = T)) ; ind_u <- which(vect>quantile(vect,probs = 0.99,na.rm = T))
  ind <- which(is.na(vect)) ; if(length(ind)>0){vect[ind]<-x_bar}
  zvalue <- abs(0.6745*(vect-x_bar)) ; zvalue <- zvalue/median_dev
  ind1 <- which(zvalue>5) ; out_ind <- NULL
  ind <- intersect(ind1,ind_l) ; new_vect <- vect
  if(length(ind)>0){new_vect[ind] <- runif(length(ind),quantile(vect,probs = 0.005,na.rm = T),quantile(vect,probs = 0.05,na.rm = T)) ; out_ind <- ind} 
  ind <- intersect(ind1,ind_u) 
  if(length(ind)>0){new_vect[ind] <- runif(length(ind),quantile(vect,probs = 0.95,na.rm = T),quantile(vect,probs = 0.995,na.rm = T)) ; out_ind <- c(out_ind,ind)} 
  #print(paste0(length(ind)," outliers replaced "))
  return(list("data"=new_vect,"outlier_index"=out_ind))
}

# clean and convert data to numerical values for pca
gen_proc_data_PCA <- function(raw_data,feature_list,outcome){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  # replace missing values in admission source
  raw_data$Admission_Source[raw_data$Admission_Source==""] <- "outpatient"
  raw_data <- raw_data[,which(colnames(raw_data)%in%feature_list[,1])]
  outcome <- t ; rm(t)
  
  if(sum(colnames(raw_data)%in%feature_list[,1])!=nrow(feature_list)){
    print("fields mismatch in feature list and raw data, following not found in raw data")
    print(as.character(feature_list[which(!feature_list[,1]%in%colnames(raw_data)),1]))
  }
  
  for(i in 1:nrow(feature_list)){
    ind <- which(colnames(raw_data)==feature_list[i,1])
    if(length(ind)==0){print(feature_list[i,1]);next}
    if(length(ind)>1){ind <- ind[1]}
    if(feature_list[i,2]=="num"){
      temp <- outlier_detect(raw_data[,ind])
      raw_data[,ind] <- as.numeric(temp$data) ; next
    }
    if(feature_list[i,2]=="cat"&feature_list[i,1]=="pr1c"){
      t <- raw_data[,ind] ; t <- gsub("[^0-9]","",t)
      d<-TrainProcedureFeature(t, outcome, 100) ; rm(t)
      raw_data[,ind] <- as.numeric(d$procedures) ; next
    }else{
      t <- clean_categorical(raw_data[,ind])
      d<-TrainCategoricalFeature(t, outcome, 100, 2)  ; rm(t)        
      raw_data[,ind] <- as.numeric(d$d)	;
    }
  }
  
  return(raw_data)
}

# generate pca from the data 
gen_data_withPC <- function(clean_data,variance_prop=NULL){
  status = "PASS"
  for(i in 1:ncol(clean_data)){
    if(sum(is.na(is.numeric(clean_data[,i])))>0){status="FAIL";print(paste0("Non-numeric entry in ",colnames(clean_data)[i]))}
  }
  if(status=="FAIL"){print("Operation terminated due to non-numeric entries for PCA") ; return(list("status"=status,"data"=NULL))}
  if(is.na(variance_prop)|variance_prop<0|variance_prop>1){variance_prop <- 0.9}
  pca_obj <- prcomp(clean_data,scale=TRUE,center = TRUE)
  pca_data <- pca_obj$x ; sm <- summary(pca_obj) ; cum_var <- sm$importance[3,]
  no_of_pca <- which(abs(variance_prop-cum_var)==min(abs(variance_prop-cum_var),na.rm = T))
  print(paste0("# PC's requried for capturing ",variance_prop," fraction of variance is ",no_of_pca))
  pca_data <- pca_data[,1:no_of_pca]
  return(list("status"=status,"data"=pca_data))
}

# develop gams model with and report performance
run_gams_model_PCA <- function(clean_data_pca,outcome,variance_prop=NULL){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  t <- gen_data_withPC(clean_data_pca,variance_prop)
  if(t$status=="FAIL"){
    print("Stopped due to error at PC's creation");
    return(list("model"=NULL,"testResults_rawData"=NULL,"trainResults_rawData"=NULL,"predicted_values"=NULL,"status"="FAIL"))
  }
  proc_data <- as.data.frame(t$data) ; 
  colnames(proc_data) <- paste0("PC",1:ncol(proc_data))
  s <- as.formula(paste0("outcome~",paste0("s(",colnames(proc_data),",k=5)",collapse = "+")))
  proc_data$outcome <- outcome ; rm(t)
  response <- rep(0,nrow(proc_data)) ; response[proc_data$outcome==1] <- 1
  fm <- binomial() ; fm$link <- "logit"
  test_result <- NULL ; train_result <- NULL ; thres_tune <- round(sum(proc_data$outcome==1)/nrow(proc_data),3)
  data_model <- proc_data;
  print("Running GAMs iterations to report training and test performance")
  for(i in 1:10){
    print(i) ; 
    train_ind <- sample(1:nrow(data_model),size = floor(0.7*nrow(data_model)))
    test_ind <- setdiff(1:nrow(data_model),train_ind)
    model <- bam(formula = s,family = fm,data = data_model[train_ind,])
    temp <- perf_gam(model,traindata=data_model[train_ind,],testdata=data_model[test_ind,],thres_tune)
    train_result <- rbind(train_result,temp$train_result)
    test_result <- rbind(test_result,temp$test_result)
  }
  model = bam(formula = s,family = fm,data = data_model)
  print("Model building completed")
  pred <- predict(model,data_model,type="response") ; 
  pred_val <- as.data.frame(cbind(response,pred)) ; colnames(pred_val) <- c("observed","predicted")
  pred_val$observed <- rep(0,nrow(pred_val)) ; pred_val$observed[which(response==1)] <- 1
  
  print("+++++++++++++++++  Completed +++++++++++++++++++")
  return(list("model"=model,"testResults_rawData"=test_result,"trainResults_rawData"=train_result,"predicted_values"=pred_val,"status"="PASS"))
}

# perform vif test
vif_test <- function(raw_data,outcome,feature_list){
  t <- rep(0,length(outcome)) ; t[outcome==1] <- 1 ; outcome <- t 
  # replace missing values in admission source
  raw_data$Admission_Source[raw_data$Admission_Source==""] <- "outpatient"
  raw_data <- raw_data[,which(colnames(raw_data)%in%feature_list[,1])]
  
  if(sum(colnames(raw_data)%in%feature_list[,1])!=nrow(feature_list)){
    print("fields mismatch in feature list and raw data, following not found in raw data")
    print(as.character(feature_list[which(!feature_list[,1]%in%colnames(raw_data)),1]))
    print("execution terminated")
    return(NULL)
  }
  
  for(i in 1:nrow(feature_list)){
    ind <- which(colnames(raw_data)==feature_list[i,1])
    if(length(ind)==0){print(feature_list[i,1]);next}
    if(length(ind)>1){ind <- ind[1]}
    if(feature_list[i,2]=="num"){
      temp <- outlier_detect(raw_data[,ind])
      raw_data[,ind] <- as.numeric(temp$data) ; next
    }
    if(feature_list[i,2]=="cat"&feature_list[i,1]=="pr1c"){
      t <- raw_data[,ind] ; t <- gsub("[^0-9]","",t)
      d<-TrainProcedureFeature(t, outcome, 100) ; rm(t)
      raw_data[,ind] <- as.numeric(d$procedures) ; next
    }else{
      t <- clean_categorical(raw_data[,ind])
      d<-TrainCategoricalFeature(t, outcome, 100, 2)  ; rm(t)        
      raw_data[,ind] <- as.numeric(d$d)	;
    }
  }
  
  x <- raw_data[,which(colnames(raw_data)%in%feature_list[,1])]
  y <- outcome
  t <- vif(y,x,mode="dense",trace = F,subsize = round(0.6*length(y),0))
  selected_feat <- feature_list[t$select,]
  return(selected_feat)
  
}
