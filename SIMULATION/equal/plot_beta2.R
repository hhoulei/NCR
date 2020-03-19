
setwd("E:/阴性对照/Simulation/20200206/equal")
result1 <- read.csv("result_g_case1_nocausal_20200204.csv",stringsAsFactors =F)
result1 <- result1[,-1]

result2 <- read.csv("result_g_case2_nocausal_20200204.csv",stringsAsFactors =F)
result2 <- result2[,-1]

result3 <- read.csv("result_g_case3_nocausal_20200204.csv",stringsAsFactors =F)
result3 <- result3[,-1]

################################## g ###############
pdf("Additional file 2.pdf",width =8,height = 9)
par(mfrow=c(2,2))
#bias
plot(result1$g,result1$bias_ivw2,type="o",col="tomato",lty=1,pch=16, 
     xlab="the number of NCs", ylab="Bias",ylim=c(-0.3,0.3))
abline(h=0,lty=2,col = "gray")
lines(result2$g,result2$bias_ivw2,type="o",pch=16,lty=1, col="dodgerblue")
lines(result3$g,result3$bias_ivw2,type="o",pch=16,lty=1, col="limegreen")

lines(result1$g,result1$bias_egger2,type="o",pch=17,lty=2, col="tomato")
lines(result2$g,result2$bias_egger2,type="o",pch=17,lty=2, col="dodgerblue")
lines(result3$g,result3$bias_egger2,type="o",pch=17,lty=2, col="limegreen")

legend("topright", c("case(a) NCR-IVW","case(b) NCR-IVW","case(c) NCR-IVW",
                     "case(a) NCR-Egger","case(b) NCR-Egger","case(c) NCR-Egger"),
       lty=c(1,1,1,2,2,2), pch=c(16,16,16,17,17,17), 
       col=rep(c("tomato","dodgerblue","limegreen"),2),ncol=2,cex=0.8)

dev.off()

###################################################################################3##


result1 <- read.csv("result_g_case1_20200204.csv",stringsAsFactors =F)
result1 <- result1[,-1]

result2 <- read.csv("result_g_case2_20200204.csv",stringsAsFactors =F)
result2 <- result2[,-1]

result3 <- read.csv("result_g_case3_20200204.csv",stringsAsFactors =F)
result3 <- result3[,-1]

################################## g ###############
pdf("equal_g_beta2.pdf",width =4.5,height = 6)
#bias
plot(result1$g,result1$bias_ivw2,type="o",col="tomato",lty=1,pch=16, 
     xlab="the number of NCs", ylab="Bias",ylim=c(-0.3,0.3))
abline(h=0,lty=2,col = "gray")
lines(result2$g,result2$bias_ivw2,type="o",pch=16,lty=1, col="dodgerblue")
lines(result3$g,result3$bias_ivw2,type="o",pch=16,lty=1, col="limegreen")

lines(result1$g,result1$bias_egger2,type="o",pch=17,lty=2, col="tomato")
lines(result2$g,result2$bias_egger2,type="o",pch=17,lty=2, col="dodgerblue")
lines(result3$g,result3$bias_egger2,type="o",pch=17,lty=2, col="limegreen")

legend("topright", c("case(a) NCR-IVW","case(b) NCR-IVW","case(c) NCR-IVW",
                     "case(a) NCR-Egger","case(b) NCR-Egger","case(c) NCR-Egger"),
       lty=c(1,1,1,2,2,2), pch=c(16,16,16,17,17,17), 
       col=rep(c("tomato","dodgerblue","limegreen"),2),ncol=2,cex=0.8)

dev.off()

