##' @export 
publish.summary.prodlim <- function(object,
                                    conf.int = 0.95,
                                    digits = 1,
                                    print=TRUE,
                                    latex=FALSE,
                                    ...){
  otab <- object$table
  if (class(otab)=="list"){
    onames <- names(otab)
    nix <- lapply(1:length(otab),function(i){
      ## publish(onames[i])
      if (latex==TRUE)
        publish(onames[i])
      cat("\n\n")
      publish(otab[[i]],digits=digits,rownames=FALSE,latex=latex,...)
    })
  }
  else{
    publish(otab,digits=digits,rownames=FALSE,latex=latex,...)
  }
}
