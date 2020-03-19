
setwd("E:/阴性对照/Simulation/20200206/equal")
result1 <- read.csv("result_cor1_case1_nocausal_20200204.csv",stringsAsFactors =F)
result1 <- result1[,-1]

result2 <- read.csv("result_cor1_case2_nocausal_20200204.csv",stringsAsFactors =F)
result2 <- result2[,-1]

result3 <- read.csv("result_cor1_case3_nocausal_20200204.csv",stringsAsFactors =F)
result3 <- result3[,-1]

################################## g ###############
pdf("Figure 5.pdf",width =9,height = 16)
par(mfrow=c(4,3))
#bias
plot(result1$cor1,result1$bias_ivw,type="o",col="tomato",lty=1,pch=16, 
     xlab="correlation among NCs", ylab="Bias",ylim=c(-0.1,0.3))
abline(h=0,lty=2,col = "gray")
lines(result2$cor1,result2$bias_ivw,type="o",pch=16,lty=1, col="dodgerblue")
lines(result3$cor1,result3$bias_ivw,type="o",pch=16,lty=1, col="limegreen")

lines(result1$cor1,result1$bias_egger,type="o",pch=17,lty=2, col="tomato")
lines(result2$cor1,result2$bias_egger,type="o",pch=17,lty=2, col="dodgerblue")
lines(result3$cor1,result3$bias_egger,type="o",pch=17,lty=2, col="limegreen")

legend("topright", c("case(a) NCR-IVW","case(b) NCR-IVW","case(c) NCR-IVW",
                     "case(a) NCR-Egger","case(b) NCR-Egger","case(c) NCR-Egger"),
       lty=c(1,1,1,2,2,2), pch=c(16,16,16,17,17,17), 
       col=rep(c("tomato","dodgerblue","limegreen"),2),ncol=2,cex=0.8)

##se
plot(result1$cor1,result1$se_ivw,type="o",col="tomato",lty=1,pch=16, 
     xlab="correlation among NCs", ylab="SE",ylim=c(-0.3,1))
abline(h=0,lty=2,col = "gray")
lines(result2$cor1,result2$se_ivw,type="o",pch=16,lty=1, col="dodgerblue")
lines(result3$cor1,result3$se_ivw,type="o",pch=16,lty=1, col="limegreen")

lines(result1$cor1,result1$se_egger,type="o",pch=17,lty=2, col="tomato")
lines(result2$cor1,result2$se_egger,type="o",pch=17,lty=2, col="dodgerblue")
lines(result3$cor1,result3$se_egger,type="o",pch=17,lty=2, col="limegreen")

legend("topright", c("case(a) NCR-IVW","case(b) NCR-IVW","case(c) NCR-IVW",
                     "case(a) NCR-Egger","case(b) NCR-Egger","case(c) NCR-Egger"),
       lty=c(1,1,1,2,2,2), pch=c(16,16,16,17,17,17), 
       col=rep(c("tomato","dodgerblue","limegreen"),2),ncol=2,cex=0.8)

#type I error
plot(result1$cor1,result1$POWER_ivw,type="o",col="tomato",lty=1,pch=16, 
     xlab="correlation among NCs", ylab="Type I error rate",ylim=c(-0.05,1))
abline(h=0.05,lty=2,col = "gray")
lines(result2$cor1,result2$POWER_ivw,type="o",pch=16,lty=1, col="dodgerblue")
lines(result3$cor1,result3$POWER_ivw,type="o",pch=16,lty=1, col="limegreen")

lines(result1$cor1,result1$POWER_egger,type="o",pch=17,lty=2, col="tomato")
lines(result2$cor1,result2$POWER_egger,type="o",pch=17,lty=2, col="dodgerblue")
lines(result3$cor1,result3$POWER_egger,type="o",pch=17,lty=2, col="limegreen")

legend("topright", c("case(a) NCR-IVW","case(b) NCR-IVW","case(c) NCR-IVW",
                     "case(a) NCR-Egger","case(b) NCR-Egger","case(c) NCR-Egger"),
       lty=c(1,1,1,2,2,2), pch=c(16,16,16,17,17,17), 
       col=rep(c("tomato","dodgerblue","limegreen"),2),ncol=2,cex=0.8)

dev.off()

###################################################################################3##

result1 <- read.csv("result_cor1_case1_20200204.csv",stringsAsFactors =F)
result1 <- result1[,-1]

result2 <- read.csv("result_cor1_case2_20200204.csv",stringsAsFactors =F)
result2 <- result2[,-1]

result3 <- read.csv("result_cor1_case3_20200204.csv",stringsAsFactors =F)
result3 <- result3[,-1]

################################## g ###############
pdf("equal_cor1.pdf",width =9,height = 4)
par(mfrow=c(1,3))
#bias
plot(result1$cor1,result1$bias_ivw,type="o",col="tomato",lty=1,pch=16, 
     xlab="correlation among NCs", ylab="Bias",ylim=c(-0.1,0.3))
abline(h=0,lty=2,col = "gray")
lines(result2$cor1,result2$bias_ivw,type="o",pch=16,lty=1, col="dodgerblue")
lines(result3$cor1,result3$bias_ivw,type="o",pch=16,lty=1, col="limegreen")

lines(result1$cor1,result1$bias_egger,type="o",pch=17,lty=2, col="tomato")
lines(result2$cor1,result2$bias_egger,type="o",pch=17,lty=2, col="dodgerblue")
lines(result3$cor1,result3$bias_egger,type="o",pch=17,lty=2, col="limegreen")

legend("topright", c("case(a) NCR-IVW","case(b) NCR-IVW","case(c) NCR-IVW",
                     "case(a) NCR-Egger","case(b) NCR-Egger","case(c) NCR-Egger"),
       lty=c(1,1,1,2,2,2), pch=c(16,16,16,17,17,17), 
       col=rep(c("tomato","dodgerblue","limegreen"),2),ncol=2,cex=0.8)

##se
plot(result1$cor1,result1$se_ivw,type="o",col="tomato",lty=1,pch=16, 
     xlab="correlation among NCs", ylab="SE",ylim=c(-0.3,1))
abline(h=0,lty=2,col = "gray")
lines(result2$cor1,result2$se_ivw,type="o",pch=16,lty=1, col="dodgerblue")
lines(result3$cor1,result3$se_ivw,type="o",pch=16,lty=1, col="limegreen")

lines(result1$cor1,result1$se_egger,type="o",pch=17,lty=2, col="tomato")
lines(result2$cor1,result2$se_egger,type="o",pch=17,lty=2, col="dodgerblue")
lines(result3$cor1,result3$se_egger,type="o",pch=17,lty=2, col="limegreen")

legend("topright", c("case(a) NCR-IVW","case(b) NCR-IVW","case(c) NCR-IVW",
                     "case(a) NCR-Egger","case(b) NCR-Egger","case(c) NCR-Egger"),
       lty=c(1,1,1,2,2,2), pch=c(16,16,16,17,17,17), 
       col=rep(c("tomato","dodgerblue","limegreen"),2),ncol=2,cex=0.8)

#type I error
plot(result1$cor1,result1$POWER_ivw,type="o",col="tomato",lty=1,pch=16, 
     xlab="correlation among NCs", ylab="Power",ylim=c(0,1.3))
abline(h=0.05,lty=2,col = "gray")
lines(result2$cor1,result2$POWER_ivw,type="o",pch=16,lty=1, col="dodgerblue")
lines(result3$cor1,result3$POWER_ivw,type="o",pch=16,lty=1, col="limegreen")

lines(result1$cor1,result1$POWER_egger,type="o",pch=17,lty=2, col="tomato")
lines(result2$cor1,result2$POWER_egger,type="o",pch=17,lty=2, col="dodgerblue")
lines(result3$cor1,result3$POWER_egger,type="o",pch=17,lty=2, col="limegreen")

legend("topright", c("case(a) NCR-IVW","case(b) NCR-IVW","case(c) NCR-IVW",
                     "case(a) NCR-Egger","case(b) NCR-Egger","case(c) NCR-Egger"),
       lty=c(1,1,1,2,2,2), pch=c(16,16,16,17,17,17), 
       col=rep(c("tomato","dodgerblue","limegreen"),2),ncol=2,cex=0.8)

dev.off()
