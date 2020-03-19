setwd("E:/阴性对照/UKB饮食摄入-血压")
dt <- read.csv('饮食摄入-血压.csv',stringsAsFactors =F)
col <- read.csv("colname.csv",stringsAsFactors =F)
colnames(dt) <- c("case_id",col[,2])

dd1 <- read.csv("negative_control_0226.csv",stringsAsFactors =F)
dt$enthic <- dd1$X21000.0.0

all_x <- c("Cooked_vegetable_intake0","Salad_raw_vegetable_intake0","Fresh_fruit_intake0","Dried_fruit_intake0",
           "Oily_fish_intake0","Non-oily_fish_intake0","Processed_meat_intake0","Poultry_intake0",
           "Beef_intake0","Lamb/mutton_intake0","Pork_intake0","Cheese_intake0","Bread intake0",
           "Cereal intake0","Tea intake0","Water intake0","Average weekly spirits intake0",
           "Salt added to food0","Coffee intake0")


# SBP:DBP
Y_DBP0 <- apply(cbind(dt[,"DBP00"],dt[,"DBP01"]),1,function(x) mean(x,na.rm=T)) 
Y_DBP2 <- apply(cbind(dt[,"DBP20"],dt[,"DBP21"]),1,function(x) mean(x,na.rm=T)) 
Y_SBP0 <- apply(cbind(dt[,"SBP00"],dt[,"SBP01"]),1,function(x) mean(x,na.rm=T))
Y_SBP2 <- apply(cbind(dt[,"SBP20"],dt[,"SBP21"]),1,function(x) mean(x,na.rm=T))

dt_nc1 <- dt[which((Y_SBP0 < 140 & Y_SBP0 > 90) & (Y_DBP0 < 90 & Y_DBP0 > 60)),] 
Y_SBP2 <- Y_SBP2[which((Y_SBP0 < 140 & Y_SBP0 > 90) & (Y_DBP0 < 90 & Y_DBP0 > 60))]
Y_DBP2 <- Y_DBP2[which((Y_SBP0 < 140 & Y_SBP0 > 90) & (Y_DBP0 < 90 & Y_DBP0 > 60))]


dt_nc <- dt_nc1[(dt_nc1$enthic %in% c(1,1001,1002,1003)),] #white people 235778
Y_SBP2 <- Y_SBP2[dt_nc1$enthic  %in% c(1,1001,1002,1003)] 
Y_DBP2 <- Y_DBP2[dt_nc1$enthic  %in% c(1,1001,1002,1003)] 

basic <- NULL
for(i in 1:length(all_x)){
  basic <- rbind(basic,c(median(dt_nc[,all_x[i]],na.rm=T),quantile(dt_nc[,all_x[i]],c(1/4,3/4),na.rm=T)))
}
rownames(basic) <- NULL
basic <- cbind(basic,basic[,3]-basic[,2])
basic <- cbind(basic,paste0(basic[,1],"(",basic[,4],")"))
colnames(basic) <- c("median","Q_1/4","Q_3/4","Q","co")
rownames(basic) <- all_x
write.csv(basic,"basic_BP0227.csv")

mean(Y_SBP2,na.rm=T) # 131.0837
sd(Y_SBP2,na.rm=T) # 131.0837
table(dt_nc$sex)
# 0      1 
# 141375  94403 
# 0.5996107
mean(dt_nc$year_of_birth,na.rm=T) #1953.192
sd(dt_nc$year_of_birth,na.rm=T) # 8.128649

result_all <- NULL
err <- NULL
for(p in 1:length(all_x)){
  
  dt_nc1 <- dt_nc[,all_x[-p]]
  X_coffee1 <- dt_nc[,all_x[p]]
  dt_cov <- dt_nc[,2:4]
  dt_cov <- apply(dt_cov,2,function(x) scale(x,center = TRUE,scale = TRUE))
  

  beta_XW <- NULL
  se_XW <- NULL
  beta_YW <- NULL
  se_YW <- NULL
  beta_YW.X <- NULL
  se_YW.X <- NULL
  
  for(i in 1:ncol(dt_nc1)){
    
    dt_once <- cbind(X_coffee1,dt_cov,dt_nc1[,i],Y_SBP2)
    dt_once1 <- na.omit(dt_once)
    
    fit1 <- lm(scale(dt_once1[,1],center = TRUE,scale = TRUE)~dt_once1[,5])
    if(is.na(coef(fit1)[2])){
      err <- rbind(err,c(all_x[p],colnames(dt_nc1)[i]))
      next
    }else{
      beta_XW <- c(beta_XW,coef(fit1)[2])
      se_XW <- c(se_XW,summary(fit1)$coef[2,2])
    }
    
    
    fit2 <- lm(scale(dt_once1[,6],center = TRUE,scale = TRUE)~dt_once1[,5]+as.matrix(dt_once1[,2:4]))

    if(is.na(coef(fit2)[2])){
      err <- rbind(err,c(all_x[p],colnames(dt_nc1)[i]))
      beta_XW <- beta_XW[-nrow(beta_XW),]
      se_XW <- se_XW[-nrow(se_XW),]
      next
    }else{
      beta_YW <- c(beta_YW,coef(fit2)[2])
      se_YW <- c(se_YW,summary(fit2)$coef[2,2])
    }
    
    
    fit3 <- lm(scale(dt_once1[,6],center = TRUE,scale = TRUE)~dt_once1[,5]+dt_once1[,1])
    if (is.na(coef(fit3)[2])) {
      err <- rbind(err,c(all_x[p],colnames(dt_nc1)[i]))
      beta_XW <- beta_XW[-nrow(beta_XW),]
      se_XW <- se_XW[-nrow(se_XW),]
      beta_YW <- beta_YW[-nrow(beta_YW),]
      se_YW <- se_YW[-nrow(se_YW),]
      next
    }
    else {
      beta_YW.X <- c(beta_YW.X,coef(fit3)[2])
      se_YW.X <- c(se_YW.X,summary(fit3)$coef[2,2])
    }
    # cat(i,'\n')
  }
  
  ############################# rho ############################
  if(length(err[err[,1]==all_x[p],2])!=0) dt_nc1 <- dt_nc1[,-which(colnames(dt_nc1)==err[err[,1]==all_x[p],2])]
  
  rho <- matrix(rep(1,ncol(dt_nc1)*ncol(dt_nc1)),ncol(dt_nc1),ncol(dt_nc1))
  for(i in 1:(ncol(dt_nc1)-1)){
    for(j in (i+1):ncol(dt_nc1)){
      rr_once <- cbind(dt_nc1[,i],dt_nc1[,j])
      rr_once <- na.omit(rr_once)
      rho[i,j] <- rho[j,i] <- cor(rr_once[,1],rr_once[,2])
    }
  }
  
  ####################################  method1 ##################################
  
  Omega = se_YW %o% se_YW*rho
  det(Omega)
  X0 <- cbind(beta_XW,beta_YW.X) 
  
  beta1 = solve(t(X0) %*% solve(Omega) %*% X0) %*% t(X0) %*%  solve(Omega ) %*% beta_YW
  se_beta1 = sqrt(diag(solve(t(X0) %*% solve(Omega) %*% X0)))
  beta_ivw <- beta1[1]
  se_ivw <- se_beta1[1]
  low_ivw <- beta_ivw - qt(0.975, df=length(beta_XW)-2) * se_ivw
  up_ivw <- beta_ivw + qt(0.975, df=length(beta_XW)-2) * se_ivw
  result_ivw <- c(beta_ivw,se_ivw,low_ivw,up_ivw)
  
  
  ####################################  method2 ##################################
  
  X2 <- cbind(rep(1,length(beta_XW)),beta_XW,beta_YW.X) 
  beta2 = solve(t(X2) %*% solve(Omega) %*% X2) %*% t(X2) %*%  solve(Omega) %*% beta_YW
  se_beta2 = sqrt(diag(solve(t(X2) %*% solve(Omega) %*% X2)))
  beta_egger <- beta2[2]
  se_egger <- se_beta2[2]
  low_egger <- beta_egger - qt(0.975, df=length(beta_XW)-3) * se_egger
  up_egger <- beta_egger + qt(0.975, df=length(beta_XW)-3) * se_egger
  result_egger <- c(beta_egger,se_egger,low_egger,up_egger)
  
  assump1 <- cor.test(beta_YW.X,beta_XW)
  
  rre <- c(result_ivw,result_egger,det(rho),assump1$estimate,assump1$p.value)
  result_all <- rbind(result_all,rre)
  
  cat(p,'\n')
}

colnames(result_all) <- c("beta_ivw","se_ivw","low_ivw","up_ivw",
                          "beta_egger","se_egger","low_egger","up_egger",
                          "det_rho","assump1_cor","assump1_cor_pval")
rownames(result_all) <- all_x

################################ association analysis ###############################################

YEAR <- 2006 - dt_nc$year_of_birth
result_SBP_asso_noadj <- NULL
result_DBP_asso_noadj <- NULL
result_SBP_asso_adj <- NULL
result_DBP_asso_adj <- NULL
for(i in 1:length(all_x)){
  fit1 <- lm(Y_SBP2 ~ dt_nc[,all_x[i]])
  result_SBP_asso_noadj <- rbind(result_SBP_asso_noadj,c(fit1$coef[2],summary(fit1)$coef[2,2],summary(fit1)$coef[2,4]))
  fit2 <- lm(Y_DBP2 ~ dt_nc[,all_x[i]])
  result_DBP_asso_noadj <- rbind(result_DBP_asso_noadj,c(fit2$coef[2],summary(fit2)$coef[2,2],summary(fit2)$coef[2,4]))
  
  fit1 <- lm(Y_SBP2 ~ dt_nc[,all_x[i]] + dt_nc$sex + YEAR + dt_nc$UKB_CENTRE0)
  result_SBP_asso_adj <- rbind(result_SBP_asso_adj,c(fit1$coef[2],summary(fit1)$coef[2,2],summary(fit1)$coef[2,4]))
  fit2 <- lm(Y_DBP2 ~ dt_nc[,all_x[i]] + dt_nc$sex + YEAR + dt_nc$UKB_CENTRE0)
  result_DBP_asso_adj <- rbind(result_DBP_asso_adj,c(fit2$coef[2],summary(fit2)$coef[2,2],summary(fit2)$coef[2,4]))
}


result_all <- cbind(result_all,result_DBP_asso_noadj,result_DBP_asso_adj)
colnames(result_all) <- c("beta_ivw","se_ivw","low_ivw","up_ivw",
                          "beta_egger","se_egger","low_egger","up_egger",
                          "det_rho","assump1_cor","assump1_cor_pval",
                          
                          "beta_asso_noadj","se_asso_noadj","pval_asso_noadj",
                          "beta_asso_adj","se_asso_adj","pval_asso_adj")
write.csv(result_all,"result_all_DBP_0229.csv")

result_all <- cbind(result_all,result_SBP_asso_noadj,result_SBP_asso_adj)
colnames(result_all) <- c("beta_ivw","se_ivw","low_ivw","up_ivw",
                          "beta_egger","se_egger","low_egger","up_egger",
                          "det_rho","assump1_cor","assump1_cor_pval",
                          
                          "beta_asso_noadj","se_asso_noadj","pval_asso_noadj",
                          "beta_asso_adj","se_asso_adj","pval_asso_adj")


write.csv(result_all,"result_all_SBP_0229.csv")


rre <- NULL
for(k in 1:(length(all_x)-1)){
  for(l in (k+1):length(all_x)){
    ccr <- dt[,all_x[c(k,l)]]
    ccr <- na.omit(ccr)
    rre <- c(rre,cor(ccr[,1],ccr[,2]))
  }
}

median(abs(rre))
# 0.04052952
quantile(abs(rre),c(1/4,3/4),na.rm=T)
# 25%        75% 
#   0.01861357 0.08434878 










