##' Parse interaction terms for regression tables
##'
##' Prepare a list of contrasts which combines regression coefficients
##' to describe statistical interactions.
##' @title Parse interaction terms
##' @param terms Terms of a formula
##' @param xlevels Factor levels corresponding to the variables in
##'     \code{terms}
##' @param units named list with unit labels. names should match variable names in formula.
##' @param format.factor For categorical variables. A string which specifies the format for factor labels.
##' The string should contain the keywords \code{"var"} and \code{"level"} which will be
##' replaced by the name of the variable and the current level, respectively.
##' Default is \code{"var(level)"}.
##' @param format.contrast For categorical variables. A string which specifies the format for constrast statements.
##' The string should contain the keywords \code{"var"}, \code{"level"} and \code{"ref"} which will be
##' replaced by the name of the variable, the current level and the reference level, respectively.
##' @param format.scale For continuous variables. For categorical variables. A string which specifies the format for factor labels.
##' The string should contain the keywords \code{"var"} and \code{"level"} which will be
##' replaced by the name of the variable and the current level, respectively.
##' Default is \code{"var(level)"}.
##' @param format.scale.unit For continuous variables which have a unit. A string which specifies the format for factor labels.
##' The string should contain the keywords \code{"var"} and \code{"unit"} which will be
##' replaced by the name of the variable and the unit, respectively.
##' Default is \code{"var(unit)"}.
##' @param sep a character string to separate the terms. Default is \code{": "}.
##' @param ... Not yet used
##' @return List of contrasts which can be passed to
##'     \code{lava::estimate}.
##' @seealso lava::estimate
##' @examples
##' 
##' tt <- terms(formula(SBP~age+sex*BMI))
##' xlev <- list(sex=c("male","female"),BMI=c("normal","overweight","obese"))
##' parseInteractionTerms(terms=tt,xlevels=xlev)
##' parseInteractionTerms(terms=tt,xlevels=xlev,format.factor="var level")
##' parseInteractionTerms(terms=tt,xlevels=xlev,format.contrast="var(level:ref)")
##'
##' tt2 <- terms(formula(SBP~age*factor(sex)+BMI))
##' xlev2 <- list("factor(sex)"=c("male","female"))
##' parseInteractionTerms(terms=tt2,xlevels=xlev2)
##' parseInteractionTerms(terms=tt2,xlevels=xlev2,units=list(age="yrs"))
##'
##'
##' data(Diabetes)
##' fit <- glm(bp.2s~age*factor(gender)+BMI,data=Diabetes)
##' parseInteractionTerms(terms=terms(fit$formula),xlevels=fit$xlevels,
##'                       format.scale="var -- level:ref",units=list("age"='years'))
##' parseInteractionTerms(terms=terms(fit$formula),xlevels=fit$xlevels,
##'                       format.scale.unit="var -- level:ref",units=list("age"='years'))
##' it <- parseInteractionTerms(terms=terms(fit$formula),xlevels=fit$xlevels)
##' ivars <- unlist(lapply(it,function(x)attr(x,"variables")))
##' lava::estimate(fit,function(p)lapply(unlist(it),eval,envir=sys.parent(-1)))
##' 
##' 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
parseInteractionTerms <- function(terms,
                                  xlevels,
                                  units,
                                  format.factor,
                                  format.contrast,
                                  format.scale,
                                  format.scale.unit,
                                  sep=": ",
                                  ...){
    if(any(attr(terms,"order")>2))
        stop("Interaction terms with order greater than 2 are not supported.")
    ilabs <- attr(terms,"term.labels")[attr(terms,"order")==2]
    inter.list <- strsplit(ilabs,":")
    intervars <- unique(unlist(inter.list))
    if (missing(units)) units <- NULL
    if (length(inter.list)>0){
        if (missing(format.factor))
            format.factor <- "var(level)"
        if (missing(format.scale.unit))
            format.scale.unit <- "var(unit)"
        if (missing(format.scale))
            format.scale <- "var"
        if (missing(format.contrast))
            format.contrast <- "var(level vs ref)"
        format.factor <- sub("var","%s",format.factor)
        format.factor <- sub("level","%s",format.factor)
        format.contrast <- sub("level","%s",format.contrast)
        format.contrast <- sub("ref","%s",format.contrast)
        format.contrast <- sub("var","%s",format.contrast)
        format.scale <- sub("var","%s",format.scale)
        format.scale.unit <- sub("var","%s",format.scale.unit)
        format.scale.unit <- sub("unit","%s",format.scale.unit)
        iterms <- lapply(inter.list,function(vv){
            v1 <- vv[1]
            ref1 <- xlevels[[v1]][[1]]
            v2 <- vv[2]
            ref2 <- xlevels[[v2]][[1]]
            if (is.null(ref1)){
                if (is.null(ref2)){
                    stop(paste("Can only handle interactions when at least one variable is a factor.\nBut argument xlevels contains no entry for either",
                               v1,
                               "or",
                               v2))
                } else{
                    ## v1 is continuous, v2 is a factor
                    ## model includes coef for one-unit change of v1 at ref2
                    ## need to ask for coef for one-unit change of v1 at other levs
                    levs2 <- xlevels[[v2]]
                    u1 <- units[[v1]]
                    if (is.null(u1)) {
                        labs <- sapply(levs2,function(l){
                            paste(sprintf(format.scale,v1),sprintf(format.factor,v2,l),sep=sep)
                        })
                    }else{
                        labs <- sapply(levs2,function(l){
                            paste(sprintf(format.scale.unit,v1,u1),sprintf(format.factor,v2,l),sep=sep)
                        })
                    }
                    ## collect the corresponding coefficients
                    contrast <- lapply(1:length(levs2),function(l){
                        if (l==1)
                            x <- bquote(p[.(v1)])
                        else
                            bquote(p[.(v1)]+p[.(paste(v1,":",paste(v2,levs2[[l]],sep=""),sep=""))])
                    })
                    names(contrast) <- labs
                    attr(contrast,"variables") <- c(v1,v2)
                    return(contrast)
                }
            }else{
                if (is.null(ref2)){
                    ## v2 is continuous, v1 is a factor
                    ## model includes coef for one-unit change of v2 at ref1
                    ## need to ask for coef for one-unit change of v2 at other levs
                    levs1 <- xlevels[[v1]]
                    u2 <- units[[v2]]
                    if (is.null(u2)) {
                        labs <- sapply(levs1,function(l){
                            paste(sprintf(format.scale,v2),sprintf(format.factor,v1,l),sep=sep)
                        })
                    }else{
                        labs <- sapply(levs1,function(l){
                            paste(sprintf(format.scale.unit,v2,u2),sprintf(format.factor,v1,l),sep=sep)
                        })
                    }
                    ## collect the corresponding coefficients
                    contrast <- lapply(1:length(levs1),function(l){
                        if (l==1)
                            bquote(p[.(v2)])
                        else
                            bquote(p[.(v2)]+p[.(paste(paste(v1,levs1[[l]],sep=""),":",v2,sep=""))])
                    })
                    names(contrast) <- labs
                    attr(contrast,"variables") <- c(v1,v2)
                    return(contrast)
                } else{
                    ## both are factors
                    levs1 <- xlevels[[v1]]
                    levs2 <- xlevels[[v2]]
                    labs1 <- paste(rep(sprintf(format.factor,v1,levs1),rep(length(levs2)-1,length(levs1))),
                                   sprintf(format.contrast,v2,levs2[-1],levs2[1]),sep=sep)
                    contrast1 <- unlist(lapply(1:length(levs1),function(l1){
                        if (l1==1)                            
                            lapply(2:(length(levs2)),function(l2){bquote(p[.(paste(v2,levs2[l2],sep=""))])})
                        else
                            lapply(2:(length(levs2)),function(l2){
                                bquote(p[.(paste(v2,levs2[l2],sep=""))]+p[.(paste(paste(v1,levs1[l1],sep=""),":",paste(v2,levs2[l2],sep=""),sep=""))])
                            })
                    }))
                    names(contrast1) <- labs1
                    labs2 <- paste(rep(sprintf(format.factor,v2,levs2),rep(length(levs1)-1,length(levs2))),
                                   sprintf(format.contrast,v1,levs1[-1],levs1[1]),sep=sep)
                    contrast2 <- unlist(lapply(1:length(levs2),function(l2){
                        if (l2==1)                            
                            lapply(2:(length(levs1)),function(l1){bquote(p[.(paste(v1,levs1[l1],sep=""))])})
                        else
                            lapply(2:(length(levs1)),function(l1){
                                ## need to reverse order in name of interaction term
                                bquote(p[.(paste(v1,levs1[l1],sep=""))]+p[.(paste(paste(v1,levs1[l1],sep=""),":",paste(v2,levs2[l2],sep=""),sep=""))])
                            })
                    }))
                    names(contrast2) <- labs2
                    contrast <- c(contrast1,contrast2)
                    attr(contrast,"variables") <- c(v1,v2)
                    return(contrast)
                }
            }
        })
        names(iterms) <- ilabs
        iterms
    }
}
