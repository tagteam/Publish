##' Extract data and design matrix including specials from call
##'
##' Obtain a list with the data used for event history regression analysis. This
##' function cannot be used directly on the user level but inside a function
##' to prepare data for survival analysis. 
##' @title Special frame
##' @param formula Formula whose left hand side specifies the event
##' history, i.e., either via Surv() or Hist().
##' @param data Data frame in which the formula is interpreted
##' @param unspecials.design Passed as is to
##' \code{\link{model.design}}.
##' @param specials Character vector of special function names.
##' Usually the body of the special functions is function(x)x but
##' e.g., \code{\link{strata}} from the survival package does treat
##' the values
##' @param specials.factor Passed as is to \code{\link{model.design}}.
##' @param specials.design Passed as is to \code{\link{model.design}}
##' @param strip.specials Passed as \code{specials} to
##' \code{\link{strip.terms}}
##' @param  strip.arguments Passed as \code{arguments} to
##' \code{\link{strip.terms}}
##' @param  strip.alias Passed as \code{alias.names} to
##' \code{\link{strip.terms}}
##' @param  strip.unspecials Passed as \code{unspecials} to
##' \code{\link{strip.terms}}
##' @param drop.intercept Passed as is to \code{\link{model.design}}
##' @param response If FALSE do not get response data.
##' @param na.action Decide what to do with missing values. 
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
##' d <- data.frame(y=1:7,
##'                 X2=c(2.24,3.22,9.59,4.4,3.54,6.81,5.05),
##'                 X3=c(1,1,1,1,0,0,1),
##'                 X4=c(44.69,37.41,68.54,38.85,35.9,27.02,41.84),
##'                 X1=factor(c("a","b","a","c","c","a","b"),
##'                     levels=c("c","a","b")))
##' ## define special functions prop and cluster
##' prop <- function(x)x
##' cluster <- function(x)x
##' ## We pass a formula and the data
##' e <- specialFrame(y~prop(X1)+X2+cluster(X3)+X4,
##'                   data=d,
##'                   specials=c("prop","cluster"))
##' ## The first element is the response
##' e$response
##' ## The other elements are the design, i.e., model.matrix for the non-special covariates
##' e$design
##' ## and a data.frame for the special covariates
##' e$prop
##' ## The special covariates can be returned as a model.matrix 
##' e2 <- specialFrame(y~prop(X1)+X2+cluster(X3)+X4,
##'                    data=d,
##'                    specials=c("prop","cluster"),
##'                    specials.design=TRUE)
##' e2$prop
##' ## and the non-special covariates can be returned as a data.frame
##' e3 <- specialFrame(y~prop(X1)+X2+cluster(X3)+X4,
##'                    data=d,
##'                    specials=c("prop","cluster"),
##'                    specials.design=TRUE,
##'                    unspecials.design=FALSE)
##' e3$design
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
specialFrame <- function(formula,
                         data,
                         unspecials.design=TRUE,
                         specials,
                         specials.factor=TRUE,
                         specials.design=FALSE,
                         strip.specials=TRUE,
                         strip.arguments=NULL,
                         strip.alias=NULL,
                         strip.unspecials=NULL,
                         drop.intercept=TRUE,
                         response=TRUE,
                         na.action=options()$na.action){
    # {{{ get all variables and remove missing values
    ## get_all_vars fails when data.frame contains labelled variables (Hmisc)
    ## if (na.action %in% c("na.omit","na.fail","na.exclude") || is.function(na.action))
    ## mm <- do.call(na.action,list(object=get_all_vars(formula,data)))
    ## else
    ## mm <- get_all_vars(formula,data)
    # }}}
    # {{{call model.frame
    ## data argument is used to resolve '.' see help(terms.formula)
    if (!is.null(strip.specials)){
        ## eval without the data to avoid evaluating special specials
        # Terms <- terms(x=formula, specials=unique(c(specials,unlist( strip.alias))))
        Terms <- terms(x=formula, specials=specials)
        Terms <- prodlim::strip.terms(Terms,
                                      specials=strip.specials,
                                      arguments= strip.arguments,
                                      alias.names= strip.alias,
                                      unspecials= strip.unspecials)
    }else{
        ## data argument is used to resolve '.' see help(terms.formula)
        Terms <- terms(x=formula, specials=specials, data = data)
    }
    ## mm <- na.omit(get_all_vars(formula(Terms),data))
    mm <- do.call(na.action,list(get_all_vars(formula(Terms),data)))
    #mm <- model.frame(formula=formula(Terms),data=data,na.action=na.action)
    if (NROW(mm) == 0) stop("No (non-missing) observations")

    # {{{ extract response
    if (response==TRUE && attr(Terms,"response")!=0){
        response <- model.frame(update(formula,".~1"), data=mm,na.action="na.pass")
    }else response <- NULL
    # }}}
    # {{{ design
    design <- prodlim::model.design(Terms,
                                    data=mm,
                                    maxOrder=1,
                                    dropIntercept=drop.intercept,
                                    unspecialsDesign=unspecials.design,
                                    specialsFactor=specials.factor,
                                    specialsDesign=specials.design)
    # }}}
    out <- c(list(response=response),
             design[sapply(design,length)>0])
    attr(out,"Terms") <- Terms
    attr(out,"na.action") <- attr(mm,"na.action")
    class(out) <- "specialFrame"
    out
}
##' @export
as.data.frame.specialFrame <- function(x,...){
    Y <- data.frame(unclass(x$response))
    X <- do.call("cbind",x[-1])
    cbind(Y,X)
}
