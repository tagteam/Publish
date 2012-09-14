ORplot <- function(x,...){
  
  par(mar=c(6,15,6,4),xpd=FALSE)
  plot(0,0,type="n",ylim=c(1,6),xlim=c(-5,26),ylab="",xlab="Odds ratio (log scale)",axes=FALSE)
  grid(col=1)
  axis(1,at=c(-4,-2,0,2,4))
  abline(v=11)
  
}
