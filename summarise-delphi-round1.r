datdir <- '~/share/doc/seabird-threats/data/survey/first_round'
datfiles <- dir(datdir, '^post')

allsamples <- sapply(datfiles, function(f) {
    cat(f, '\n')
    read.csv(sprintf('%s/%s', datdir, f), as.is=T, header=F)
}, simplify=F)

pars <- sub('^post-(.*)\\.csv$', '\\1', datfiles)
names(allsamples) <- pars

save(allsamples, file = sprintf('%s/all-post-samples.rdata', datdir))


par=names(allsamples)[1]
postsumm <- sapply(names(allsamples), function(par) {
    cat(par,'\n')
    d <- allsamples[[par]]
    samples <- d[, 3:ncol(d)]
    rownames(samples) <- d[,2]
    summ <- do.call('rbind', sapply(rownames(samples), function(sp) {
        x <- unlist(samples[sp,])
        data.frame(sp   = sp,
                   par  = par,
                   mean = mean(x),
                   median = median(x),
                   lcl  = quantile(x, 0.025, names=F),
                   ucl  = quantile(x, 0.975, names=F))
    }, simplify=F))
    return(summ)
}, simplify=F)
postsumm <- do.call('rbind', postsumm)

write.csv(postsumm, sprintf('%s/delphi-round1-derived-parameters.csv', datdir), row.names=F)
