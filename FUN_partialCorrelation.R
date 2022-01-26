### FUN_partialCorrelation.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jan 26 2022 (09:26) 
## Version: 
## Last-Updated: jan 26 2022 (11:34) 
##           By: Brice Ozenne
##     Update #: 33
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

##' @examples
##' n <- 1e2
##' set.seed(10)
##' df <- data.frame(Y = rnorm(n),
##'                  X = rnorm(n),
##'                  K = as.character(rbinom(n, size = 3, prob = 0.5)))
##' 
##' ## 1 covariate
##' e1.lm <- lm(Y~X, data = df)
##' partialCorrelation(e1.lm, var = "X")
##' partialCorrelation(e1.lm, var = "X", fisher.transform = TRUE)
##' cor.test(df$Y,df$X) ## same p-value and CI
##'
##' ## 2 covariates
##' e2.lm <- lm(Y~X+K, data = df)
##' partialCorrelation(e2.lm, var = "X")

partialCorrelation <- function (object, var, fisher.transform, cluster) UseMethod("partialCorrelation")

## * partialCorrelation.lm
partialCorrelation.lm <- function (object, var, fisher.transform = FALSE, cluster = NULL){
    if (length(var) != 1) {
        stop("Argument 'var' must have length 1 \n")
    }
    object.formula <- formula(object)
    if (attr(terms(object.formula), "response") != 1) {
        stop("Note implemented for \"lm\" object with multiple response variables \n")
    }
    name.var <- all.vars(object.formula)
    Y <- name.var[1]
    if (any(var %in% name.var == FALSE)) {
        stop("Argument 'var' must correspond to variables in the model \n")
    }
    if (var %in% names(object$xlevels)) {
        stop("Argument 'var' must correspond to a numeric variable \n")
    }
    X <- setdiff(name.var, c(Y, var))
    if (length(X) > 0) {
        object1.formula <- paste0(Y, "~", paste(X, collapse = "+"))
        object2.formula <- paste0(var, "~", paste(X, collapse = "+"))
    }
    else {
        object1.formula <- paste0(Y, "~1")
        object2.formula <- paste0(var, "~1")
    }
    object1 <- update(object, formula = as.formula(object1.formula))
    object2 <- update(object, formula = as.formula(object2.formula))
    n.obs <- nobs(object) + length(object$na.action)
    df.res <- data.frame(matrix(NA, nrow = n.obs, ncol = 2, dimnames = list(NULL, 
        c("res1", "res2"))))
    df.res[setdiff(1:n.obs, object1$na.action), "res1"] <- scale(residuals(object1))
    df.res[setdiff(1:n.obs, object2$na.action), "res2"] <- scale(residuals(object2))
    if (!is.null(cluster)) {
        df.res[, cluster] <- eval(object$call$data)[[cluster]]
    }
    e.lmres <- lm(res1 ~ res2, data = df.res)
    out <- cbind(as.data.frame(summary(e.lmres)$coef["res2",,drop=FALSE]),df = df.residual(e.lmres), confint.default(e.lmres)["res2",,drop=FALSE])
    names(out) <- c("estimate","se","statistic","p.value","df","lower","upper")
    rownames(out) <- NULL

    e.lmres.iid <- iid(e.lmres, cluster = cluster)[,"res2"]
    attr(out, "iid") <- setNames(sqrt(vcov(e.lmres)["res2","res2"]) * e.lmres.iid / sqrt(sum(e.lmres.iid^2)),
                                 names(e.lmres.iid))
    if (fisher.transform) {
        out.trans <- out
        out.trans$std.error <- 1/sqrt(out[,"df"]-1) ## i.e. 1/sqrt(n-3)
        out[, "lower"] <- tanh(atanh(out.trans$estimate) + qnorm(0.025) * out.trans$std.error)
        out[, "upper"] <- tanh(atanh(out.trans$estimate) + qnorm(0.975) * out.trans$std.error)
        out[, "p.value"] <- 2 * (1 - pt(sqrt(out[,"df"]) * abs(out.trans$estimate)/sqrt(1 - out.trans$estimate^2), df = out[,"df"]))
    }
    return(out)
}

## * partialCorrelation.mmm
partialCorrelation.mmm <- function (object, var, cluster = NULL){
    ls.out <- lapply(object, function(iM) {
        iVar <- grep(var, all.vars(formula(iM)), value = TRUE)
        if (length(iVar) > 0) {
            return(partialCorrelation(iM, var = iVar, fisher.transform = FALSE, cluster = cluster))
        }
        else {
            return(NULL)
        }
    })
    test.null <- sapply(ls.out, length)
    if (all(test.null == 0)) {
        return(NULL)
    }
    ls.out <- ls.out[test.null > 0]
    dt.out <- do.call(rbind, ls.out)
    n.model <- length(ls.out)
    name.model <- names(ls.out)
    if (!is.null(cluster)) {
        unique.cluster <- unique(unlist(lapply(ls.out, function(iM) {
            names(attr(iM, "iid"))
        })))
        n.cluster <- length(unique.cluster)
        iid.out <- matrix(NA, nrow = n.cluster, ncol = n.model, 
            dimnames = list(unique.cluster, name.model))
        for (iM in 1:n.model) {
            iid.out[names(attr(ls.out[[iM]], "iid")), iM] <- as.double(attr(ls.out[[iM]], 
                "iid"))
        }
    } else {
        iid.out <- do.call(cbind, lapply(ls.out, function(iM) {
            attr(iM, "iid")
        }))
    }
    vec.sd <- apply(iid.out, 2, function(iCol) {
        sqrt(sum(iCol^2, na.rm = TRUE))
    })
    M.cor <- cor(iid.out, use = "pairwise")
    vcov.out <- M.cor * tcrossprod(vec.sd)
    linfct <- matrix(0, nrow = n.model, n.model, dimnames = list(name.model, 
        name.model))
    diag(linfct) <- 1
    out <- list(model = NULL, linfct = linfct, rhs = rep(0, n.model), 
        coef = dt.out$estimate, vcov = vcov.out, df = if (any(!is.na(dt.out$df))) {
            median(dt.out$df)
        } else {
            0
        }, alternative = "two.sided", type = NULL, robust = FALSE)
    class(out) <- c("glht2", "glht")
    return(out)
}
##----------------------------------------------------------------------
### FUN_partialCorrelation.R ends here
