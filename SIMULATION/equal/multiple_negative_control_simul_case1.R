

# install.packages("matrixNormal")
# N:sample size
# g:number of IVs
# G-X:0.2
# X-M:b3
# M-Y:b4
# X-Y:b2
library(meta)
library(matrixNormal)

DataGenerator <- function(g,b,d,c,sigma1,sigma2,cor1,cor2){
  
  a <- rep(0.5,g)
  M <- matrix(c(b*a,d*a*(1-b^2)/(1-(b*a)^2)),ncol=2)
  U <- matrix(rep(sigma1,g*g),nrow=g)
  U[upper.tri(U)] <- cor1
  U[lower.tri(U)] <- cor1
  V <- matrix(c(sigma2,cor2,cor2,sigma2),nrow = 2)
  beta_all <- rmatnorm(M,U,V)
  
  beta_XG <- beta_all[,1]
  beta_WY_X <- beta_all[,2]
  
  b0 <- 0
  beta_YG <- b0 + c*beta_XG + (1-(b*a)^2)/(1-b^2) * beta_WY_X + rnorm(g,0,1)
  
  datasimul <- list(data.frame(betaYG=beta_YG, betaWY_X=beta_WY_X, betaXG=beta_XG),
                    b0=mean(b0),
                    b2=mean((1-(b*a)^2)/(1-b^2)))
  return(datasimul)
}


main <- function(NN,g,b,d,c,sigma1,sigma2,cor1,cor2){

  result_ivw <- NULL
  result_egger <- NULL
  result_pav <- NULL
  result_old <- NULL
  intercept <- NULL
  b2 <- NULL
  
	for(i in 1:NN){

		fdata0 <- DataGenerator(g,b,d,c,sigma1,sigma2,cor1,cor2)
		fdata <- fdata0[[1]]
		intercept <- c(intercept,fdata0[[2]])
		b2 <- c(b2,fdata0[[3]])

	  rho <- matrix(rep(1,g*g),nrow=g)
	  rho[upper.tri(rho)] <- cor1
	  rho[lower.tri(rho)] <- cor1
	  
	  sebetaYG <- rep(1,g)
		
		####################################  method1 ##################################
		
		Omega = sebetaYG %o% sebetaYG*rho
		X0 <- cbind(fdata$betaXG,fdata$betaWY_X) 
		beta1 = solve(t(X0) %*% solve(Omega) %*% X0) %*% t(X0) %*%  solve(Omega) %*% fdata$betaYG
		se_beta1 = sqrt(diag(solve(t(X0) %*% solve(Omega) %*% X0)))
    beta_ivw <- beta1[1]
    se_ivw <- se_beta1[1]
    beta_ivw2 <- beta1[2]
    se_ivw2 <- se_beta1[2]
    low_ivw <- beta_ivw - qt(0.975, df=length(fdata$betaXG)-2) * se_ivw
    up_ivw <- beta_ivw + qt(0.975, df=length(fdata$betaXG)-2) * se_ivw
    result_ivw <- rbind(result_ivw,c(beta_ivw,se_ivw,low_ivw,up_ivw,beta_ivw2 ,se_ivw2 ))
    
    
    ####################################  method2 ##################################
		
    X2 <- cbind(rep(1,g),fdata$betaXG,fdata$betaWY_X) 
    beta2 = solve(t(X2) %*% solve(Omega) %*% X2) %*% t(X2) %*%  solve(Omega) %*% fdata$betaYG
    se_beta2 = sqrt(diag(solve(t(X2) %*% solve(Omega) %*% X2)))
    beta_egger <- beta2[2]
    se_egger <- se_beta2[2]
    beta_egger2 <- beta2[3]
    se_egger2 <- se_beta2[3]
    low_egger <- beta_egger - qt(0.975, df=length(fdata$betaXG)-3) * se_egger
    up_egger <- beta_egger + qt(0.975, df=length(fdata$betaXG)-3) * se_egger
    result_egger <- rbind(result_egger,c(beta_egger,se_egger,low_egger,up_egger,beta_egger2 ,se_egger2 ))
    
    
	}
  
  result_beta <- c(mean(result_ivw[,1]),mean(result_egger[,1]))
  result_beta2 <- c(mean(result_ivw[,5]),mean(result_egger[,5]))
  result_bias <- result_beta-rep(c,2)
  result_bias2 <- result_beta2-rep(mean(b2),2)
  result_se <- c(mean(result_ivw[,2]),mean(result_egger[,2]))
  result_se2 <- c(mean(result_ivw[,6]),mean(result_egger[,6]))
  result_mse <- result_bias^2+result_se^2
  
  result_CR <- c(mean(ifelse(result_beta[1]<result_ivw[,4] & result_beta[1]>result_ivw[,3],1,0)),
                 mean(ifelse(result_beta[2]<result_egger[,4] & result_beta[2]>result_egger[,3],1,0)))
  
  result_POWER <- c(mean(ifelse(0<result_ivw[,4] & 0>result_ivw[,3],0,1)),
                    mean(ifelse(0<result_egger[,4] & 0>result_egger[,3],0,1)))
  
  result_CIW <- c(mean(abs(result_ivw[,4]-result_ivw[,3])),
                  mean(abs(result_egger[,4]-result_egger[,3])))
  
	return(c(c,mean(intercept),result_beta,result_bias,result_se,result_mse,result_CR,result_POWER,result_CIW,
	         result_beta2,result_bias2,result_se2))
}

rere <- main(NN=1000,g=50,b=0.5,d=0.5,c=0,sigma1=4,sigma2=0.01,cor1=0.2,cor2=0)
names(rere) <- c("c","b0","beta_ivw","beta_egger","bias_ivw","bias_egger",
                 "se_ivw","se_egger", "mse_ivw","mse_egger",
                 "CR_ivw","CR_egger","POWER_ivw","POWER_egger",
                 "CIW_ivw","CIW_egger","beta_ivw2","beta_egger2",
                 "bias_ivw2","bias_egger2", "se_ivw2","se_egger2")
rere


##################################################################


cor1 <- seq(0,0.9,0.1)
result1 <- NULL
for(i in 1:length(cor1)){
  result <- main(NN=1000,g=20,b=0.5,d=0.5,c=0,sigma1=4,sigma2=1,cor1=cor1[i],cor2=0)
  result1 <- rbind(result1,result)
}
colnames(result1) <- c("c","b0","beta_ivw","beta_egger","bias_ivw","bias_egger",
                       "se_ivw","se_egger", "mse_ivw","mse_egger",
                       "CR_ivw","CR_egger","POWER_ivw","POWER_egger",
                       "CIW_ivw","CIW_egger","beta_ivw2","beta_egger2",
                       "bias_ivw2","bias_egger2", "se_ivw2","se_egger2")
result1 <- cbind(cor1,result1)
write.csv(result1,"result_cor1_case1_nocausal_20200204.csv")

###################################################################

cor2 <- seq(0,0.9,0.1)
result1 <- NULL
for(i in 1:length(cor2)){
  result <- main(NN=1000,g=20,b=0.5,d=0.5,c=0,sigma1=4,sigma2=1,cor1=0.15,cor2=cor2[i])
  result1 <- rbind(result1,result)
}
colnames(result1) <- c("c","b0","beta_ivw","beta_egger","bias_ivw","bias_egger",
                       "se_ivw","se_egger", "mse_ivw","mse_egger",
                       "CR_ivw","CR_egger","POWER_ivw","POWER_egger",
                       "CIW_ivw","CIW_egger","beta_ivw2","beta_egger2",
                       "bias_ivw2","bias_egger2", "se_ivw2","se_egger2")
result1 <- cbind(cor2,result1)
write.csv(result1,"result_cor2_case1_nocausal_20200204.csv")


##################################################################

g <- seq(5,50,5)
result1 <- NULL
for(i in 1:length(g)){
  result <- main(NN=1000,g=g[i],b=0.5,d=0.5,c=0,sigma1=4,sigma2=1,cor1=0.15,cor2=0)
  result1 <- rbind(result1,result)
}
colnames(result1) <- c("c","b0","beta_ivw","beta_egger","bias_ivw","bias_egger",
                       "se_ivw","se_egger", "mse_ivw","mse_egger",
                       "CR_ivw","CR_egger","POWER_ivw","POWER_egger",
                       "CIW_ivw","CIW_egger","beta_ivw2","beta_egger2",
                       "bias_ivw2","bias_egger2", "se_ivw2","se_egger2")
result1 <- cbind(g,result1)
write.csv(result1,"result_g_case1_nocausal_20200204.csv")


################################# casual effect 2#################################

cor1 <- seq(0,0.9,0.1)
result1 <- NULL
for(i in 1:length(cor1)){
  result <- main(NN=1000,g=20,b=0.5,d=0.5,c=2,sigma1=4,sigma2=1,cor1=cor1[i],cor2=0)
  result1 <- rbind(result1,result)
}
colnames(result1) <- c("c","b0","beta_ivw","beta_egger","bias_ivw","bias_egger",
                       "se_ivw","se_egger", "mse_ivw","mse_egger",
                       "CR_ivw","CR_egger","POWER_ivw","POWER_egger",
                       "CIW_ivw","CIW_egger","beta_ivw2","beta_egger2",
                       "bias_ivw2","bias_egger2", "se_ivw2","se_egger2")
result1 <- cbind(cor1,result1)
write.csv(result1,"result_cor1_case1_20200204.csv")

###################################################################

cor2 <- seq(0,0.9,0.1)
result1 <- NULL
for(i in 1:length(cor2)){
  result <- main(NN=1000,g=20,b=0.5,d=0.5,c=2,sigma1=4,sigma2=1,cor1=0.15,cor2=cor2[i])
  result1 <- rbind(result1,result)
}
colnames(result1) <- c("c","b0","beta_ivw","beta_egger","bias_ivw","bias_egger",
                       "se_ivw","se_egger", "mse_ivw","mse_egger",
                       "CR_ivw","CR_egger","POWER_ivw","POWER_egger",
                       "CIW_ivw","CIW_egger","beta_ivw2","beta_egger2",
                       "bias_ivw2","bias_egger2", "se_ivw2","se_egger2")
result1 <- cbind(cor2,result1)
write.csv(result1,"result_cor2_case1_20200204.csv")


##################################################################

g <- seq(5,50,5)
result1 <- NULL
for(i in 1:length(g)){
  result <- main(NN=1000,g=g[i],b=0.5,d=0.5,c=2,sigma1=4,sigma2=1,cor1=0.15,cor2=0)
  result1 <- rbind(result1,result)
}
colnames(result1) <- c("c","b0","beta_ivw","beta_egger","bias_ivw","bias_egger",
                       "se_ivw","se_egger", "mse_ivw","mse_egger",
                       "CR_ivw","CR_egger","POWER_ivw","POWER_egger",
                       "CIW_ivw","CIW_egger","beta_ivw2","beta_egger2",
                       "bias_ivw2","bias_egger2", "se_ivw2","se_egger2")
result1 <- cbind(g,result1)
write.csv(result1,"result_g_case1_20200204.csv")



