bi = read.table('bibliometry.txt',h=T)

png(file='bibliometry.png',width=900)

par(mfcol=c(1,2),xaxs='i',yaxs='i')

plot(COSPE~YEAR,bi,type='l',lwd=3,ylim=c(0,50),xlab='Year',ylab='Number of articles')
lines(COEVO~YEAR,bi,lwd=3,lty=3)
legend('topleft',legend=c('Cospeciation','Coevolution'),bty='n',lwd=3,lty=c(1,3))

plot(RATIO~YEAR,bi,type='l',lwd=3,ylim=c(0,1),xlab='Year',ylab='Ratio')

dev.off()