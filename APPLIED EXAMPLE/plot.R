setwd("E:/阴性对照/UKB饮食摄入-血压")
library(forestplot)

dt1 <- read.csv("result_all_SBP_0229.csv",stringsAsFactors = F)
dt2 <- read.csv("result_all_DBP_0229.csv",stringsAsFactors = F)
dt3 <- read.csv("result_all_CRP_0229.csv",stringsAsFactors = F)

all_x <- c("Cooked vegetable intake","Salad raw vegetable intake","Fresh fruit intake","Dried fruit intake",
           "Oily fish intake","Non-oily fish intake","Processed meat intake","Poultry intake",
           "Beef intake","Lamb/mutton intake","Pork intake","Cheese intake","Bread intake",
           "Cereal intake","Tea intake","Water intake","Average weekly spirits intake",
           "Salt added to food","Coffee intake")

beta <- cbind(dt1$beta_asso_noadj[12:19],dt1$beta_asso_adj[12:19],dt1$beta_ivw[12:19],dt1$beta_egger[12:19])      
beta <- cbind(NA,beta)
beta <- c(t(beta))
beta_1 <- c("Beta",NA,round(beta,3))
beta <- c(NA,NA,beta)
low <-  cbind(dt1$beta_asso_noadj[12:19]-1.96*dt1$se_asso_noadj[12:19],
              dt1$beta_asso_adj[12:19]-1.96*dt1$se_asso_adj[12:19],
              dt1$low_ivw[12:19],dt1$low_egger[12:19])        
low <- cbind(NA,low)
low <- c(t(low))
low <- c(NA,NA,low)
up <- cbind(dt1$beta_asso_noadj[12:19]+1.96*dt1$se_asso_noadj[12:19],
            dt1$beta_asso_adj[12:19]+1.96*dt1$se_asso_adj[12:19],
            dt1$up_ivw[12:19],dt1$up_egger[12:19])        
up <- cbind(NA,up)
up <- c(t(up))
up <- c(NA,NA,up)

beta2 <- cbind(dt2$beta_asso_noadj[12:19],dt2$beta_asso_adj[12:19],dt2$beta_ivw[12:19],dt2$beta_egger[12:19])      
beta2 <- cbind(NA,beta2)
beta2 <- c(t(beta2))
beta2_1 <- c("Beta",NA,round(beta2,3))
beta2 <- c(NA,NA,beta2)
low2 <-  cbind(dt2$beta_asso_noadj[12:19]-1.96*dt2$se_asso_noadj[12:19],
              dt2$beta_asso_adj[12:19]-1.96*dt2$se_asso_adj[12:19],
              dt2$low_ivw[12:19],dt2$low_egger[12:19])        
low2 <- cbind(NA,low2)
low2 <- c(t(low2))
low2 <- c(NA,NA,low2)
up2 <- cbind(dt2$beta_asso_noadj[12:19]+1.96*dt2$se_asso_noadj[12:19],
            dt2$beta_asso_adj[12:19]+1.96*dt2$se_asso_adj[12:19],
            dt2$up_ivw[12:19],dt2$up_egger[12:19])        
up2 <- cbind(NA,up2)
up2 <- c(t(up2))
up2 <- c(NA,NA,up2)

beta3 <- cbind(dt3$beta_asso_noadj[12:19],dt3$beta_asso_adj[12:19],dt3$beta_ivw[12:19],dt3$beta_egger[12:19])      
beta3 <- cbind(NA,beta3)
beta3 <- c(t(beta3))
beta3_1 <- c("Beta",NA,round(beta3,3))
beta3 <- c(NA,NA,beta3)
low3 <-  cbind(dt3$beta_asso_noadj[12:19]-1.96*dt3$se_asso_noadj[12:19],
              dt3$beta_asso_adj[12:19]-1.96*dt3$se_asso_adj[12:19],
              dt3$low_ivw[12:19],dt3$low_egger[12:19])        
low3 <- cbind(NA,low3)
low3 <- c(t(low3))
low3 <- c(NA,NA,low3)
up3 <- cbind(dt3$beta_asso_noadj[12:19]+1.96*dt3$se_asso_noadj[12:19],
            dt3$beta_asso_adj[12:19]+1.96*dt3$se_asso_adj[12:19],
            dt3$up_ivw[12:19],dt3$up_egger[12:19])        
up3 <- cbind(NA,up3)
up3 <- c(t(up3))
up3 <- c(NA,NA,up3)


col <- c("dodgerblue","limegreen","tomato","deeppink")

subtype <- c(t(matrix(c(all_x[12:19],rep(c("  lm","  lm_adj","  NCR-IVW","  NCR-Egger"),each=8)),ncol=5)))
subtype <- c('Dietary intake',NA,subtype)
#subtype <- c(t(matrix(rep(c("  lm","  lm_adj","  NCR-IVW","  NCR-Egger"),each=11)),ncol=5))

pci <- rep('*',length(low))
pci[low<0 & up>0] <- ' '
pci[is.na(low)] <- NA
CI <- paste0('[',round(low,3),',',round(up,3),']',pci)
CI[is.na(low)] <- NA
CI[1] <- "95% CI"

pci2 <- rep('*',length(low2))
pci2[low2<0 & up2>0] <- ' '
pci2[is.na(low2)] <- NA
CI2 <- paste0('[',round(low2,3),',',round(up2,3),']',pci2)
CI2[is.na(low2)] <- NA
CI2[1] <- "95% CI"

pci3 <- rep('*',length(low3))
pci3[low3<0 & up3>0] <- ' '
pci3[is.na(low3)] <- NA
CI3 <- paste0('[',round(low3,3),',',round(up3,3),']',pci3)
CI3[is.na(low3)] <- NA
CI3[1] <- "95% CI"

pdf("example_11_0229_4.pdf",height = 9,width = 16) 
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3,widths=c(1.5,1,1))))
pushViewport(viewport(layout.pos.col = 1))
forestplot(labeltext=cbind(subtype,beta_1,CI),
           mean=beta,
           lower=low, upper=up,
           xlab="SBP",
           xticks=c(-0.5,0,0.5),
           is.summary = is.na(beta),
           fn.ci_nrom='fpDrawDiamondCI',
           hrzl_lines=list("3" = gpar(lwd=1, col="gray"),
                           "6" = gpar(lwd=70, lineend="butt", columns=c(2:4), col="#99999920"),
                           "16" = gpar(lwd=70, lineend="butt", columns=c(2:4), col="#99999920"),
                           "26" = gpar(lwd=70, lineend="butt", columns=c(2:4), col="#99999920"),
                           "36" = gpar(lwd=70, lineend="butt", columns=c(2:4), col="#99999920")),
                           # "46" = gpar(lwd=70, lineend="butt", columns=c(2:4), col="#99999920"),
                           # "56" = gpar(lwd=70, lineend="butt", columns=c(2:4), col="#99999920")),
           txt_gp=fpTxtGp(label=gpar(cex=0.8),ticks=gpar(cex=1),xlab=gpar(cex = 1), title=gpar(cex = 1)),
           col=fpColors(box='steelblue', lines="black", zero = "gray50"),
           #lines=rep(brewer.pal(4,'Set3'),11)
           zero=0, cex=0.5, lineheight = "auto", boxsize=0.4, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4,graph.pos = 2,new_page=F)
popViewport()
pushViewport(viewport(layout.pos.col = 2))
forestplot(labeltext=cbind(beta2_1,CI2),
           mean=beta2,
           lower=low2, upper=up2,
           xlab="DBP",
           xticks=c(-0.5,0,0.5),
           is.summary = is.na(beta),
           fn.ci_nrom='fpDrawDiamondCI',
           hrzl_lines=list("3" = gpar(lwd=1, col="gray"),
                          "6" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920"),
                          "16" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920"),
                          "26" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920"),
                          "36" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920")),
                          #"46" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920"),
                         # "56" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920")),
           txt_gp=fpTxtGp(label=gpar(cex=0.8),ticks=gpar(cex=1),xlab=gpar(cex = 1), title=gpar(cex = 1)),
           col=fpColors(box='steelblue', lines="black", zero = "gray50"),
           #lines=rep(brewer.pal(4,'Set3'),11)
           zero=0, cex=0.5, lineheight = "auto", boxsize=0.4, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4,graph.pos = 1,new_page=F)
popViewport()
pushViewport(viewport(layout.pos.col = 3))
forestplot(labeltext=cbind(beta3_1,CI3),
           mean=beta3,
           lower=low3, upper=up3,
           xlab="CRP",
           xticks=c(-0.5,0,0.5),
           is.summary = is.na(beta),
           fn.ci_nrom='fpDrawDiamondCI',
           hrzl_lines=list("3" = gpar(lwd=1, col="gray"),
                          "6" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920"),
                          "16" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920"),
                          "26" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920"),
                          "36" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920")),
                          # "46" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920"),
                          # "56" = gpar(lwd=70, lineend="butt", columns=c(1:3), col="#99999920")),
           txt_gp=fpTxtGp(label=gpar(cex=0.8),ticks=gpar(cex=1),xlab=gpar(cex = 1), title=gpar(cex = 1)),
           col=fpColors(box='steelblue', lines="black", zero = "gray50"),
           #lines=rep(brewer.pal(4,'Set3'),11)
           zero=0, cex=0.5, lineheight = "auto", boxsize=0.4, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4,graph.pos = 1,new_page=F)
popViewport()
dev.off()