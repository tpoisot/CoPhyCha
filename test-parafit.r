library(ape)

data(gopher.D)
data(lice.D)
data(HP.links)

res <- parafit(gopher.D, lice.D, HP.links, nperm=99, test.links=TRUE)

added_links = round(10^seq(from=0,to=2,length=20),0)
matrix_replicates = 100

OUT = NULL

for(cl in added_links){
	for(repl in c(1:matrix_replicates)){
		original_hp = HP.links
		## Add the number of additional links
		ToAdd = cl
		while(ToAdd > 0){
			x = sample(c(1:nrow(HP.links)),1)
			y = sample(c(1:ncol(HP.links)),1)
			if(HP.links[x,y]==0){
				original_hp[x,y] = 1
				ToAdd = ToAdd - 1
			}
		}
		## Do the ParaFit
		temp_res = parafit(gopher.D, lice.D, original_hp, nperm=99)
		temp_PFG = temp_res$ParaFitGlobal
		temp_PV = temp_res$p.global
		OUT = rbind(OUT,c(cl,repl,temp_PFG,temp_PV))
	}
}

OUT = as.data.frame(OUT)
colnames(OUT) = c('l','r','PFG','PV')

Means = aggregate(.~l, data=OUT, FUN=mean)
Sds = aggregate(.~l, data=OUT, FUN=sd)

png(file='parafit-test.png',width=900)

par(mfcol=c(1,2),las=2)

plot(PFG~l,data=Means,pch=NA,log='x',xlab='Number of interactions added',ylab='Parafit global statistic')
abline(h=res$ParaFitGlobal,lwd=2)
arrows(x0=Means$l,y1=Means$PFG-Sds$PFG/2,y0=Means$PFG+Sds$PFG/2,code=3,angle=90,length=0.03)
points(PFG~l,data=Means,pch=23,type='p',bg='lightgrey',cex=1.2)


plot(PV~l,data=Means,pch=NA,log='x',xlab='Number of interactions added',ylab='Parafit p-value')
abline(h=res$p.global,lwd=2)
abline(h=0.05,lty=2)
arrows(x0=Means$l,y1=Means$PV-Sds$PV/2,y0=Means$PV+Sds$PV/2,code=3,angle=90,length=0.03)
points(PV~l,data=Means,pch=23,type='p',bg='lightgrey',cex=1.2)

dev.off()