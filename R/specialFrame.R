##' Extract data and design matrix including specials from call
##'
##' Obtain a list with the data used for event history regression analysis. This
##' function cannot be used directly on the user level but inside a function
##' to prepare data for survival analysis. 
##' @title Special frame
##' @param formula Formula whose left hand side specifies the event
##' history, i.e., either via Surv() or Hist().
##' @param data Data frame in which the formula is interpreted
##' @param unspecialsDesign Passed as is to
##' \code{\link{model.design}}.
##' @param specials Character vector of special function names.
##' Usually the body of the special functions is function(x)x but
##' e.g., \code{\link{strata}} from the survival package does treat
##' the values
##' @param specialsFactor Passed as is to \code{\link{model.design}}.
##' @param specialsDesign Passed as is to \code{\link{model.design}}
##' @param stripSpecialNames Passed as is to
##' \code{\link{model.design}}
##' @param dropIntercept Passed as is to \code{\link{model.design}}
##' @param na.action Passed as is to \code{\link{model.frame}}
##' na.action.
##' @return A list which contains
##' - the response
##' - the design matrix (see \code{\link{model.design}})
##' - one entry for each special (see \code{\link{model.design}})
##' @seealso model.frame model.design Hist
##' @examples
##' 
##' ## Here are some data with an event time and no competing risks
##' ## and two covariates X1 and X2.
##' ## Suppose we want to declare that variable X1 is treated differently
##' ## than variable X2. For example, X1 could be a cluster variable, or
##' ## X1 should have a proportional effect on the outcome.
##' dsurv <- data.frame(y=1:7,
##'                     X2=c(2.24,3.22,9.59,4.4,3.54,6.81,5.05),
##'                     X3=c(1,1,1,1,0,0,1),
##'                     X4=c(44.69,37.41,68.54,38.85,35.9,27.02,41.84),
##'                     X1=factor(c("a","b","a","c","c","a","b"),
##'                         levels=c("c","a","b")))
##' ## define special functions prop and cluster
##' prop <- function(x)x
##' cluster <- function(x)x
##' ## We pass a formula and the data
##' e <- specialFrame(status~prop(X1)+X2+cluster(X3)+X4,
##'                         data=dsurv,
##'                         specials=c("prop","cluster"))
##' ## The first element is the response
##' e$response
##' ## The other elements are the design, i.e., model.matrix for the non-special covariates
##' e$design
##' ## and a data.frame for the special covariates
##' e$prop
##' ## The special covariates can be returned as a model.matrix 
##' e2 <- specialFrame(status~prop(X1)+X2+cluster(X3)+X4,
##'                          data=dsurv,
##'                          specials=c("prop","cluster"),
##'                          specialsDesign=TRUE)
##' e2$prop
##' ## and the non-special covariates can be returned as a data.frame
##' e3 <- specialFrame(status~prop(X1)+X2+cluster(X3)+X4,
##'                          data=dsurv,
##'                          specials=c("prop","cluster"),
##'                          specialsDesign=TRUE,
##'                          unspecialsDesign=FALSE)
##' e3$design
##' 
##'  
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
specialFrame <- function(formula,
                         data,
                         unspecialsDesign=TRUE,
                         specials,
                         specialsFactor=TRUE,
                         specialsDesign=FALSE,
                         stripSpecialNames=TRUE,
                         dropIntercept=TRUE,
                         na.action=options()$na.action){
    # {{{call model.frame
    ## data argument is used to resolve '.' see help(terms.formula)
    Terms <- terms(x=formula, specials=specials, data = data)
    m <- model.frame(formula=Terms,data=data,subset=NULL,na.action=na.action)
    if (NROW(m) == 0) stop("No (non-missing) observations")
    # }}}
    # {{{ extract response
    response <- model.extract(m, "response")
    # }}}
    # {{{ design
    design <- prodlim::model.design(data=m,
                                    maxOrder=1,
                                    dropIntercept=dropIntercept,
                                    unspecialsDesign=unspecialsDesign,
                                    specialsFactor=specialsFactor,
                                    specialsDesign=specialsDesign,
                                    stripSpecialNames=stripSpecialNames)
    # }}}
    out <- c(list(response=response),
             design[sapply(design,length)>0])
    attr(out,"Terms") <- Terms
    attr(out,"na.action") <- attr(m,"na.action")
    class(out) <- "specialFrame"
    out
}
##' @S3method as.data.frame specialFrame
##' @method as.data.frame specialFrame
as.data.frame.specialFrame <- function(x,...){
    Y <- data.frame(unclass(x$response))
    X <- do.call("cbind",x[-1])
    cbind(Y,X)
}
