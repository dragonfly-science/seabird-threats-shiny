options(scipen=10)

args <- commandArgs(trailingOnly = T)
if (!length(args))
    app <- 'app'  else app <- args[1]

#################################
## Read demographic parameters ##
#################################

r1summ <- read.csv('delphi-round2-derived-parameters.csv', as.is=T)

################################################
## Update to get the juvenile cohort survival ##
################################################

js <- subset(r1summ, par == 'juvenile-survival')
jcs <- js
jcs$par <- 'juvenile-coh-survival'

stats <- c('mean', 'median', 'lcl', 'ucl')
jcs[, stats] <- NA

## Diomedea albatrosses
jcs[jcs$sp == "Antipodean wandering albatross", stats] <- c(0.28, 0.28, 0.24, 0.34)*100
jcs[jcs$sp == "Gibson's wandering albatross", stats] <- c(0.28, 0.28, 0.24, 0.34)*100
jcs[jcs$sp == "Southern royal albatross", stats] <- c(0.35, 0.35, 0.32, 0.39)*100
jcs[jcs$sp == "Northern royal albatross", stats] <- c(0.38, 0.38, 0.26, 0.46)*100
## Others (Mollymawks)
for (i in which(is.na(jcs$mean))){
    jcs[i, stats] <- c(0.3, 0.3, 0.15, 0.4)*100
}

r1summ <- rbind(r1summ, jcs)

spp <- unique(r1summ$sp)

################################################
## Clutch size
################################################

cs <- data.table(sp = spp, par = 'clutch-size', mean=1, median=1, lcl=1, ucl=1)
r1summ <- rbind(r1summ, cs)

pars <- unique(r1summ$par)

#################
## Threat list ##
#################

threats <- unique(read.csv('threats.csv', as.is=T))
rownames(threats) <- NULL
threats <- threats[order(threats$threat_id),]
threats <- ifelse(threats$Subcategory != '',
                 sprintf('%s - %s', threats$Threat.class, threats$Subcategory),
                 threats$Threat.class)


save(spp, pars, threats, r1summ, file=sprintf('%s/shinydata.rdata', app))



