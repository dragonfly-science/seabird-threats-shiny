logit <- function(x) log(x/(1 - x))
ilogit <- function(x) exp(x)/(1+exp(x))

## Create complete Leslie matrix
les_mat <- function(afr, sa, si, pb, cs, bs) {
    ## if (!is.finite(afr))  print(afr)
    m <- diag(0, afr, afr)
    for (i in 1:(afr-1)) {
	m[i,i] <- 0
	m[i+1,i] <- si
    }
    m[1,ncol(m)] <- pb * cs * bs / 2
    m[nrow(m), ncol(m)] <- sa
    return(m)
}

eigens <- function(s) {
    if (length(s)) {
        nsamples <- length(s[[1]])
        z <- sapply(1:nsamples, function(i) {
            mat <- les_mat( s$afr[i], s$sa[i], s$si[i], s$pb[i], 1, s$bs[i])
            eig <- eigen.analysis(mat)
            eig$sensitivities <- eig$elasticities <- eig$repro.value <- NULL
            return(eig)
        }, simplify=F)
    }
}

p_dist_from_ci <- function(ci, n) {
    lcl <- ci[1]
    ucl <- ci[2]
    logitlcl <- logit(lcl)
    logitucl <- logit(ucl)
    sdlogit <- (logitucl-logitlcl) / (2*1.96)
    meanlogit <- mean(c(logitlcl,logitucl))
    z <- rnorm(n, meanlogit, sdlogit)
    s <- ilogit(z)
    return(s)
}

a_dist_from_ci <- function(ci, n) {
    z <- round(runif(n, ci[1], ci[2]))
    return(z)
}


samples_from_p_ci <- function(lcl, ucl, n) {
    lcl <- min(lcl, 0.999)
    ucl <- min(ucl, 0.999)
    z1 <- logit(lcl)
    z2 <- logit(ucl)
    mean <- mean(c(z1, z2))
    sd <- (z2 - mean) / qnorm(0.975)
    z <- ilogit(rnorm(n, mean, sd))
    return(z)
}

samples_from_n_ci <- function(lcl, ucl, n) {
    z1 <- log(lcl)
    z2 <- log(ucl)
    mean <- mean(c(z1, z2))
    sd <- (z2 - mean) / qnorm(0.975)
    return(as.integer(round(exp(rnorm(n, mean, sd)))))
}


calc_dem <- function(s, eig) {
    if (!is.null(s) & !is.null(eig)) {
        agedist <- sapply(eig, '[[', 'stable.stage', simplify=F)
        ## aged=agedist[[1]]
        pd <- do.call('rbind', sapply(1:length(agedist), function(i) {
            aged <- agedist[[i]]
            a <- s$afr[i]
            pad <- aged[a]
            nad <- s$nbp[i] * 2 / (s$pb[i])
            ntot <- nad / pad
            ni <- ntot * (1-pad)
            nnow <- ntot * aged
            mat <- les_mat( s$afr[i], s$sa[i], s$si[i], s$pb[i], 1, s$bs[i])
            dead_ad <- (1 - s$sa[i]) * nad
            dead_juv <- (1- s$si[i]) * ni
            data.frame(ni = ni, nad = nad, ntot = ntot, grate = eig[[i]]$lambda1,
                       dead_ad = dead_ad, dead_juv = dead_juv)
        }, simplify=F))
        pd <- cbind(do.call('cbind', s), pd)
        return(pd)
    }
}


alpha <- function (colour, alpha) {
    alpha[is.na(alpha)] <- 0
    col <- col2rgb(colour, TRUE)/255
    if (length(colour) != length(alpha)) {
        if (length(colour) > 1 && length(alpha) > 1) {
            stop("Only one of colour and alpha can be vectorised")
        }
        if (length(colour) > 1) {
            alpha <- rep(alpha, length.out = length(colour))
        }
        else if (length(alpha) > 1) {
            col <- col[, rep(1, length(alpha)), drop = FALSE]
        }
    }
    col[4, ] <- ifelse(col[4, ] == 1, alpha, col[4, ])
    new_col <- rgb(col[1, ], col[2, ], col[3, ], col[4, ])
    new_col[is.na(colour)] <- NA
    return(new_col)
}

