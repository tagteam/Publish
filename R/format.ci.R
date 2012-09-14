format.ci <- function(lower,upper,style=1,coll=";",degenerated="--",digits=3,sep=""){
  lower <- round(lower,digits)
  upper <- round(upper,digits)
  out <- switch(as.character(style),
                "1"=c("lower"=lower,"upper"=upper),
                "2"=paste("[",lower,coll,upper,"]",sep=sep),
                "3"=paste("(",lower,coll,upper,")",sep=sep),
                "4"=paste(lower,"-",upper,sep=sep),
                "5"=paste("CI-95%: ", lower,coll,upper,sep=sep))
  if (style>1 && is.character(degenerated)) out[lower==upper] <- degenerated
  out
}
