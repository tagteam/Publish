##' @export
publish.list <- function(object,
                         title,
                         level=0,
                         hrule=0,
                         title.level=1,
                         title.hrule=1,
                         ...){
  if (!missing(title)) publish(title,level=title.level,hrule=title.hrule)
  xnames <- names(object)
  nix <- lapply(1:length(object),function(i){
    if (!is.null(xnames)){      
      publish(xnames[i],level=level,hrule=hrule)
    }
    else cat("\n\n")
    inX <- object[[i]]
    publish(inX,level=min(level+1,3),...)
  })
}
