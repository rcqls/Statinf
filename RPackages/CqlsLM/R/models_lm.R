model_lm <- function(form, data) {
    data_vars <- all.vars(form)
    form_vars <- as.character(attr(terms(form), "var"))[-1]
    form_log <- all(paste0("log(", data_vars, ")") == form_vars)
    data_vars_ext <- c()
    if (is.data.frame(data)) {
        data_vars_ext <- setdiff(names(data), data_vars)
    }
    mlm <- model_lm_by_extnames(yname = data_vars[1], extnames = c(data_vars[1], data_vars_ext), log = form_log, data = data)
    if(form != mlm$formula) {
        print(list(form=form,mlm=mlm$formula))
        stop("Bad formula for model_lm!")
    }
    mlm
}

model_lm_by_extnames <- function(yname, extnames, log, data) {
    mlm <- list(
        yname = yname,
        xnames = setdiff(names(data), extnames),
        extnames = extnames,
        log = log,
        data = data
    )
    class(mlm) <- "model_lm"
    mlm$formula <- formula(mlm)
    mlm
}

formula.model_lm <- function(mlm, text = FALSE) {
    yexpr <- mlm$yname
    xexprs <- mlm$xnames
    if(mlm$log) {
        yexpr <- paste0("log(",yexpr,")")
        xexprs <- paste0("log(",xexprs,")",collapse="+")
    }
    form <- paste0(yexpr, "~", xexprs)
    if (!text)  form <- formula(form)
    form
}

summary.model_lm <- function(mlm) {
    summary(lm(formula(mlm),data=mlm$data))
}

model_lm_next <- function(mlm, alpha = 0.05, verbose = FALSE) {
    sumlm <- coef(summary(lm(formula(mlm),data=mlm$data)))[-1,4]
    i_max <- which.max(sumlm)
    pval_max <- sumlm[i_max]
    if(pval_max > alpha) {
        if(verbose) print(paste0(mlm$xnames[i_max]," substracted with pvalue ",pval_max))
        model_lm_by_extnames(yname=mlm$yname,extnames=c(mlm$extnames, mlm$xnames[i_max]),data=mlm$data,log=mlm$log)
    } else {
        if(verbose) print("No variable to substract!")
        NULL
    }
}

models_lm <- function(mlm, alpha = 0.05, verbose = FALSE) {
    mlms <- new.env()
    mlms$first <- mlm
    mlms$alpha <- alpha
    mlms$data <- mlm$data
    mlms$formula <- list(formula(mlms$first))
    class(mlms) <- "models_lm"
    mlms$last <- mlms$first
    models_lm_all(mlms, verbose=verbose)
    mlms
}

models_lm_next <- function(mlms, verbose = verbose) {
    last <- model_lm_next(mlms$last, mlms$alpha, verbose = verbose)
    if(!is.null(last)) {
        mlms$last <- last
        mlms$formula <- c(mlms$formula, formula(mlms$last))
    }
    last
}

models_lm_all <- function(mlms, verbose = verbose) {
    repeat {
        last <- models_lm_next(mlms, verbose = verbose)
        if(is.null(last)) break
    }
}

formula.models_lm <- function(mlms) mlms$formula

print.models_lm <- function(mlms) {
    print(formula(mlms))
}

summary.models_lm <- function(mlms) {
    lapply(formula(mlms), function(f) summary(lm(f, data = mlms$data)))
}