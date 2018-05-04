## devtools::install_github("trestletech/shinyTable")
## devtools::install_github('rstudio/DT')

library(data.table)
library(shiny)
library(RColorBrewer)
library(popbio)
library(shinyTable)
library(DT)
library(compiler)
loadcmp('functions.Rc')
## source('functions.r')

load('shinydata.rdata', verb=T)


pars <- c('Juvenile cohort survival' = 'juvenile-coh-survival',
         'Adult annual survival'     = 'adult-survival',
         'Breeding probability'     = 'breeding-probability',
         'Breeding success'         = 'breeding-success',
         'Age at first breeding'    = 'first-breeding-age',
         'Breeding pairs'           = 'breeding-pairs',
         'Clutch size'              = 'clutch-size'
         )

exist_pot <- c('Existing threat', 'Potential threat')

pars_short <- c('juvenile-survival'    = 'si',
               'juvenile-coh-survival' = 'sic',
               'adult-survival'       = 'sa',
               'breeding-probability' = 'pb',
               'breeding-success'     = 'bs',
               'first-breeding-age'   = 'afr',
               'breeding-pairs'       = 'nbp',
               'clutch-size'          = 'cs'
               )

rel_pars <- c('juvenile-coh-survival' = 'ni',
             'adult-survival'    = 'nad')

transfun <- list('juvenile-survival'    = list(tf = logit, itf = ilogit),
                'juvenile-coh-survival' = list(tf = logit, itf = ilogit),
                'adult-survival'       = list(tf = logit, itf = ilogit),
                'breeding-success'     = list(tf = logit, itf = ilogit),
                'breeding-probability' = list(tf = logit, itf = ilogit),
                'first-breeding-age'   = list(tf = log, itf = exp),
                'clutch-size'          = list(tf = I, itf = I))

scales <- list('juvenile-survival'    = 100,
              'juvenile-coh-survival'= 100,
              'adult-survival'       = 100,
              'breeding-success'     = 100,
              'breeding-probability' = 100,
              'first-breeding-age'   = 1,
              'breeding-pairs'       = 1,
              'clutch-size'          = 1)

labs <- c(si       = 'Immature annual survival (%)',
         sic      = 'Immature cohort survival (%)',
         sa       = 'Adult annual survival (%)',
         afr      = 'First breeding age',
         bs       = 'Breeding success (%)',
         pb       = 'Breeding probability (%)',
         nbp      = 'Breeding pairs',
         ni       = 'No. immatures',
         nad      = 'No. adults',
         ntot     = 'Total individuals',
         grate    = 'Growth rate (%)',
         dead_ad  = 'Adult fatalities',
         dead_juv = 'Immature fatalities',
         new_ad   = 'New adults',
         new_juv  = 'New immatures',
         cs       = 'Clutch size (eggs)')
         
  
nulldf <- data.frame('species'            = character(0),
                    'Threat'             = character(0),
                    'Impacted.parameter' = character(0),
                    'Threat.status'  = character(0),
                    'Individuals'        = numeric(0),
                    'Change'             = numeric(0), stringsAsFactors = F)


## ###################
## ## For debugging ##
## ###################
## input <- list(spp     = spp[1],
##              afr_lcl = 5     , afr_ucl = 8,
##              sic_lcl = 30    , sic_ucl = 50,
##              sa_lcl  = 92    , sa_ucl  = 96,
##              bs_lcl  = 60    , bs_ucl  = 75,
##              pb_lcl  = 60    , pb_ucl  = 75,
##              nbp_lcl = 10000 , nbp_ucl = 15000,
##              cs = 1,
##              threat = "Fishing - Direct NZ commercial - Trawl", impacted_par = 'adult-survival', exist_pot = "Existing threat",
##              impact_ind_n = NA, impact_ind_p = 0.1, nsamples = 1000)


##########################
### Define server logic ##
##########################
shinyServer(function(input, output, session) {

    rv <- reactiveValues(cachedTbl = nulldf, start = TRUE, gli = 0, oriDem = 0, oriThreat = 0,
                        fixed.x.axis = F, xrng = NULL)

    ## UI elements
    output$spp <- renderUI({
        selectInput("spp", label = 'Reset parameters',
                    choices = spp,
                    selected = spp[1], selectize = F)
    })


#############################
### Demographic parameters ##
#############################

    output$afr_lcl <- renderUI({
        numericInput("afr_lcl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='first-breeding-age')$lcl)),
                     step = 0.5)})
    output$afr_ucl <- renderUI({
        numericInput("afr_ucl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='first-breeding-age')$ucl)),
                     step = 0.5)})

    output$sic_lcl <- renderUI({
        numericInput("sic_lcl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='juvenile-coh-survival')$lcl)),
                     step = 0.5)})
    output$sic_ucl <- renderUI({
        numericInput("sic_ucl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='juvenile-coh-survival')$ucl)),
                     step = 0.5)})

    output$sa_lcl <- renderUI({
        numericInput("sa_lcl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='adult-survival')$lcl)),
                     step = 0.5)})
    output$sa_ucl <- renderUI({
        numericInput("sa_ucl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='adult-survival')$ucl)),
                     step = 0.5)})

    output$bs_lcl <- renderUI({
        numericInput("bs_lcl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='breeding-success')$lcl)),
                     step = 0.5)})
    output$bs_ucl <- renderUI({
        numericInput("bs_ucl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='breeding-success')$ucl)),
                     step = 0.5)})

    output$pb_lcl <- renderUI({
        numericInput("pb_lcl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='breeding-probability')$lcl)),
                     step = 0.5)})
    output$pb_ucl <- renderUI({
        numericInput("pb_ucl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='breeding-probability')$ucl)),
                     step = 0.5)})

    output$nbp_lcl <- renderUI({
        numericInput("nbp_lcl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='breeding-pairs')$lcl)),
                     step = 10)})
    output$nbp_ucl <- renderUI({
        numericInput("nbp_ucl", 
                     label = NULL,
                     value = ifelse(is.null(r1summ), NULL,
                                    round(subset(r1summ, sp==input$spp & par=='breeding-pairs')$ucl)),
                     step = 10)})

    output$cs <- renderUI({
        numericInput("cs", 
                     label = NULL,
                     value = 1,
                     step = 1)})

    
    all_dem_good <- reactive({
        go <- F
        if (!is.null(input$afr_lcl) & !is.null(input$afr_ucl) &
            !is.null(input$sic_lcl) & !is.null(input$sic_ucl) &
            !is.null(input$sa_lcl) & !is.null(input$sa_ucl) &
            !is.null(input$bs_lcl) & !is.null(input$bs_ucl) &
            !is.null(input$pb_lcl) & !is.null(input$pb_ucl) &
            !is.null(input$nbp_lcl) & !is.null(input$nbp_ucl) & !is.null(input$cs)) {
            if (is.finite(input$afr_lcl) & is.finite(input$afr_ucl) &
                is.finite(input$sic_lcl) & is.finite(input$sic_ucl) &
                is.finite(input$sa_lcl) & is.finite(input$sa_ucl) &
                is.finite(input$bs_lcl) & is.finite(input$bs_ucl) &
                is.finite(input$pb_lcl) & is.finite(input$pb_ucl) &
                is.finite(input$nbp_lcl) & is.finite(input$nbp_ucl) & is.finite(input$cs))
                go <- T
        } 
        return(go)
    })

    samples <- reactive({
        if (all_dem_good()) {
            s <- list(afr = samples_from_n_ci(input$afr_lcl, input$afr_ucl, input$nsamples),
                     sic  = samples_from_p_ci(input$sic_lcl/100, input$sic_ucl/100, input$nsamples),
                     sa   = samples_from_p_ci(input$sa_lcl/100, input$sa_ucl/100, input$nsamples),
                     bs   = samples_from_p_ci(input$bs_lcl/100, input$bs_ucl/100, input$nsamples),
                     pb   = samples_from_p_ci(input$pb_lcl/100, input$pb_ucl/100, input$nsamples),
                     nbp  = samples_from_n_ci(input$nbp_lcl, input$nbp_ucl, input$nsamples),
                     cs   = rep(input$cs, input$nsamples))
            meanafr <- mean(s$afr)
            s$si <- s$sic^(1/(meanafr - 1))
            return(s)
        }
    })


#####################
### Threat effects ##
#####################

    ## UI elements
    output$threat <- renderUI({
        selectInput("threat", 
                    label = "Threat",
                    choices = threats,
                    selected = threats[1], width='100%')})
    output$exist_pot <- renderUI({
        selectInput("exist_pot", 
                    label = "Type",
                    choices = exist_pot,
                    selected = exist_pot[1])})
    output$impacted_par <- renderUI({
        selectInput("impacted_par", 
                    label = "Parameter",
                    choices = pars[1:5],
                    selected = pars[1])})
    output$impact_ind_n <- renderUI({
        numericInput("impact_ind_n",
                     label = "Impact (individuals)",
                     value = NULL)
    })
    output$impact_ind_p <- renderUI({
        numericInput("impact_ind_p",
                     label = "Impact (parameter units)",
                     value = NULL)
    })
    output$addthreat <- renderUI({
        actionButton("addthreat", label = "Update")
    })


    all_threat_good <- reactive({
        go <- F
        if (!is.null(input$threat) & !is.null(input$impacted_par) & !is.null(input$exist_pot) &
            !is.null(input$impact_ind_n) & !is.null(input$impact_ind_p)) {
            if (is.finite(input$threat) & is.finite(input$impacted_par) & is.finite(input$exist_pot))
                go <- T
        } 
        return(go)
    })

    threat_effect_type <- reactive({
        if (!is.null(input$threat) & !is.null(input$impacted_par) & !is.null(input$exist_pot)) {
            if (!is.finite(input$impact_ind_n) & is.finite(input$impact_ind_p)) {
                return('p')
            } else if (is.finite(input$impact_ind_n) & !is.finite(input$impact_ind_p)) {
                return('n')
            } else return(NULL)
        }
    })

    observeEvent(input$addthreat, {
        if (is.null(rv$cachedTbl)) {
            tab <- nulldf
        } else {
            tab <- rv$cachedTbl
        }
        if (!is.null(input$threat) & !is.null(input$impacted_par) & !is.null(input$exist_pot)){

            if (input$impacted_par == "juvenile-survival" |
                input$impacted_par == "adult-survival")
                validate( need( (is.na(input$impact_ind_n) & !is.na(input$impact_ind_p)) |
                                (!is.na(input$impact_ind_n) & is.na(input$impact_ind_p)),
                               'Either Change or Individuals'))

            if (input$impacted_par == "first-breeding-age" |
                input$impacted_par == "breeding-probability" |
                input$impacted_par == "breeding-success")
                validate( need( is.na(input$impact_ind_n), 'Input as individuals not treated'))
            
            ## print('Adding threat')
            c <- tab$'Threat' == input$threat  &  tab$'Impacted.parameter' == input$impacted_par
            if (any(c)) {
                if ((threat_effect_type() == 'p' & (!is.finite(input$impact_ind_p) | input$impact_ind_p == 0)) |
                    (threat_effect_type() == 'n' & (!is.finite(input$impact_ind_n) | input$impact_ind_n == 0))) {
                    tab <- tab[-which(c), ]
                } else {
                    tab$'Threat.status'[c] <- input$exist_pot
                    tab$'Individuals'[c]   <- input$impact_ind_n
                    tab$'Change'[c]        <- input$impact_ind_p
                }
            } else {
                tab <- rbind(tab,
                            data.frame('species'            = input$spp,
                                       'Threat'             = input$threat,
                                       'Impacted.parameter' = input$impacted_par,
                                       'Threat.status'      = input$exist_pot,
                                       'Individuals'        = input$impact_ind_n,
                                       'Change'             = input$impact_ind_p,
                                       stringsAsFactors     = F))
            }
            pd <- popdist()
            ## Complete table
            missing_changes <- which(is.na(tab$Change) &
                                    tab[['Impacted.parameter']] %in% c('adult-survival', 'juvenile-coh-survival'))
            for (i in missing_changes) {
                par <- tab[i, 'Impacted.parameter']
                param <- pars_short[[par]]
                param_n <- rel_pars[[par]]
                tab[i, 'Change'] <- scales[[par]] * tab[i, 'Individuals'] / mean(pd[[param_n]])
            }
            missing_indivs <- which(is.na(tab$Individuals)  &
                                   tab[['Impacted.parameter']] %in% c('adult-survival', 'juvenile-coh-survival'))
            for (i in missing_indivs) {
                par <- tab[i, 'Impacted.parameter']
                param <- pars_short[[par]]
                param_n <- rel_pars[[par]]
                tab[i, 'Individuals'] <- (tab[i, 'Change'] / scales[[par]]) * mean(pd[[param_n]])
            }
            
            rv$cachedTbl <<- tab

        }
    })

    upd_samples <- reactive({
        print('Updating samples')
        newsamples <- NULL
        if (!is.null( rv$cachedTbl ) & nrow(rv$cachedTbl) > 0) {
            tab <- rv$cachedTbl
            pd <- popdist()
            s <- samples()
            if (!is.null(pd) & !is.null(tab)) {
                newsamples <- sapply(as.vector(pars), function(p) {
                    
                    pshort <- pars_short[[p]]
                    tf <- transfun[[p]]$tf
                    itf <- transfun[[p]]$itf
                    t <- tab[tab$Impacted.parameter == p, ]
                    if (nrow(t)) {
                        totp <- sum(ifelse(t[['Threat.status']] == 'Existing threat', 1, -1) *
                                   t[['Change']] / scales[[p]])
                        delta <- tf(mean(pd[[pshort]]) + totp) - tf(mean(pd[[pshort]]))
                        return( itf(tf(pd[[pshort]]) + delta) )
                    } else return( pd[[pshort]] )
                }, simplify = F)
                names(newsamples) <- as.vector(pars_short[names(newsamples)])
                ## Juvenile survival from cohort survival
                newsamples$si <- newsamples$sic^(1/(mean(newsamples$afr) - 1))
                newsamples <- newsamples[names(s)]
            }
        }
        return(newsamples)
        print('Finished updating samples')
    })


#################
### Harvesting ##
#################

    ## harvest_dem <- function() {
    ##     if (!is.null(input$afr_lcl) & !is.null(input$afr_ucl) &
    ##          !is.null(input$sic_lcl) & !is.null(input$sic_ucl) &
    ##          !is.null(input$sa_lcl) & !is.null(input$sa_ucl) &
    ##          !is.null(input$bs_lcl) & !is.null(input$bs_ucl) &
    ##          !is.null(input$pb_lcl) & !is.null(input$pb_ucl) &
    ##           !is.null(input$nbp_lcl) & !is.null(input$nbp_ucl) & !is.null(input$spp)) {

    ##         civars <- c('afr', 'sic', 'sa', 'bs', 'pb', 'nbp')
    ##         d <- data.table(species = input$spp,
    ##                        dem_par = civars,
    ##                        lcl = sapply(civars, function(x) input[[paste0(x, '_lcl')]]),
    ##                        ucl = sapply(civars, function(x) input[[paste0(x, '_ucl')]]))
    ##         return(d)
    ##     }
    ## }

    harvest_threats <- function() {
        tbl <- rv$cachedTbl
        if (!is.null(tbl) & nrow(tbl) >0 & !is.null(input$spp)) {
            tbl$species <- input$spp
            tbl <- tbl[, c('species', 'Threat', 'Impacted.parameter', 'Threat.status', 'Change', 'Individuals')]
        }
        return(tbl)
    }


#################
### UI Tables  ##
#################

    output$threat_table <- DT::renderDataTable({
        if (!is.null(rv$cachedTbl)) {
            tbl <- rv$cachedTbl[, c('Threat', 'Impacted.parameter', 'Threat.status',
                                   'Change', 'Individuals')]
            tbl$Individuals <- round(tbl$Individuals)
            tbl$Change <- round(tbl$Change, 2)
            DT::datatable(tbl,
                          escape = F,
                          options = list(autoWidth = FALSE, paging = FALSE,
                                         searching = FALSE, info   = FALSE,
                                         ordering  = FALSE),
                          class = 'stripe order-column nowrap hover compact',
                          rownames = F)
        }
    })



#################
### Demography ##
#################

    eigen <- reactive({
        s <- samples()
        eig <- NULL
        if (length(s))
            eig <- eigens(s)
        return(eig)
    })

    popdist <- reactive({
        eig <- eigen()
        if (!is.null(eig)) {
            s <- samples()
            pd <- calc_dem(s, eig)
            return(pd)
        }
    })


    summary_fun <- function(spd) {
        ## if (!is.data.table(spd)) setDT(spd)
        if (!is.null(spd)) {
            summ <- spd[, lapply(.SD, function(x) c(mean = mean(x),
                                                   lcl = quantile(x, 0.025, names=F),
                                                   ucl = quantile(x, 0.975, names=F)))]
            summ <- cbind(stat = c('mean', 'lcl', 'ucl'), summ)
            return(summ)
        }
    }

    prepare_all_dem_summary <- reactive({
        dem1 <- summary_fun(popdist())
        dem2 <- summary_fun(upd_popdist())
        dem <- NULL
        if (!is.null(dem1) & is.null(dem2))
            dem2 <- copy(dem1)
        if (!is.null(dem1) & !is.null(dem2)) {
            dem1[, stat := sprintf('%s_curr', stat)]
            dem2[, stat := sprintf('%s_threat', stat)]
            dem <- rbind(dem1, dem2)
            dem <- dem[, .(stat,afr,sic,si,sa,bs,pb,nbp,cs,ni,nad,ntot,grate,dead_ad,dead_juv)]
        }
        return(dem)
    })

    output$all_dem_summary <- DT::renderDataTable({
        dem <- copy(prepare_all_dem_summary())
        if (!is.null(dem)) {
            for (v in c('sic', 'si', 'sa', 'bs', 'pb'))
                dem[, eval(v) := round(100 * get(v), 1)]
            dem[, grate := round((grate - 1) * 100, 2)]
            for (v in setdiff(names(dem), c('stat', 'sic', 'si', 'sa', 'bs', 'pb', 'grate')))
                dem[, eval(v) := round(get(v))]
            colnms <- dem[, stat]
            rownms <- names(dem)
            dem <- transpose(dem[, -1])
            setnames(dem, colnms)
            dem <- cbind(par = rownms[-1], dem)
            dem[, lcl_ucl_curr := sprintf('%s–%s', lcl_curr, ucl_curr)]
            dem[, lcl_ucl_threat := sprintf('%s–%s', lcl_threat, ucl_threat)]
            dem[, c('lcl_curr', 'ucl_curr', 'lcl_threat', 'ucl_threat') := NULL]
            dem[, val_curr := sprintf('%s  (%s)', mean_curr, lcl_ucl_curr)]
            dem[, val_threat := sprintf('%s  (%s)', mean_threat, lcl_ucl_threat)]
            dem[, par := labs[par]]
            dem <- dem[, .(par, val_curr, val_threat)]
            setnames(dem, c('Parameter', 'Base', 'Scenario'))
            DT::datatable(dem, escape = F,
                          options = list(autoWidth = FALSE, paging = FALSE,
                                         searching = FALSE, info   = FALSE,
                                         ordering  = FALSE),
                          class = 'stripe order-column nowrap hover compact',
                          rownames = F)
        }
    })



########################
### Update demography ##
########################


    upd_eigen <- reactive({
        s2 <- upd_samples()
        eig2 <- NULL
        if (!is.null(s2)) {
            eig2 <- eigens(s2)
        } 
        return(eig2)
    })

    upd_popdist <- reactive({
        s2 <- upd_samples()
        eig2 <- upd_eigen()
        pd2 <- calc_dem(s2, eig2)
        return(pd2)
    })


###########################
### Plot of growth rates ##
###########################
    observeEvent(input$fix_x_axis, {
        if (!is.null(input$fix_x_axis)) {
            if (input$fix_x_axis == T) {
                rv$fixed.x.axis <- T
                pd <- isolate(popdist())
                pd2 <- isolate(upd_popdist())
                rv$xrng <- range(100 * (c(pd$grate, pd2$grate) - 1))
            } else {
                rv$fixed.x.axis <- F
                rv$xrng <- NULL
            }
        }
    })
    
    output$plot_lambdas <- renderPlot({
        pd <- popdist()
        if (!is.null(pd)) {
            pd2 <- upd_popdist()
            d <- 100 * (pd$grate - 1)
            dens <- density(d, from = min(d), to = max(d))
            x <- dens$x
            lcl <- rep(0, length(x))
            ucl <- dens$y
            
            if ( !is.null(pd2) )  {
                d2 <- 100 * (pd2$grate - 1)
                dens2 <- density(d2, from = min(d2), to = max(d2))
                x2 <- dens2$x
                lcl2 <- rep(0, length(x2))
                ucl2 <- dens2$y

                maxy <- max(c(dens$y, dens2$y))
                maxx <- max(c(dens$x, dens2$x))
                if (rv$fixed.x.axis == T & !is.null(rv$xrng)) {
                    xlims <- rv$xrng
                } else {
                    xlims <- range(c(dens$x, dens2$x))
                }
                par(mar=c(4, 0.5, 0.5, 0.5), mgp=c(2,.5,0), col.axis='#00000088', col.lab='#00000088')
                plot(NA,
                     xlim = xlims,
                     ylim = c(0, maxy*1.11), xlab='Annual growth rate (%)', main='',
                     xaxs = 'i', yaxs = 'i', bty='n', ylab='', las = 1, yaxt='n', xaxt='n')
                axis(1, at=pretty(c(x, x2), 10), col='#00000088')

                polygon(c(x, rev(x), x[1]), c(ucl, rev(lcl), ucl[1]), border = NA, col = '#00000011')
                lines(x, ucl, col = '#00000033')
                abline(v = mean(d), col='#00000099', lty=3)

                polygon(c(x2, rev(x2), x2[1]), c(ucl2, rev(lcl2), ucl2[1]), border = NA, col = '#BB000033')
                lines(x2, ucl2, col = '#BB000088')
                abline(v = mean(d2), col='red')

            } else {
                if (rv$fixed.x.axis == T & !is.null(rv$xrng)) {
                    xlims <- rv$xrng
                } else {
                    xlims <- range(dens$x)
                }
                par(mar=c(4, 0.5, 0.5, 0.5), mgp=c(2,.5,0), col.axis='#00000088', col.lab='#00000088')
                plot(NA,
                     xlim = xlims,
                     ylim = c(0, max(dens$y)*1.02), xlab='Annual growth rate (%)', main='',
                     xaxs = 'i', yaxs = 'i', bty='n', ylab='', las = 1, yaxt='n', xaxt='n')
                axis(1, at=pretty(x, 10), col='#00000088')
                
                polygon(c(x, rev(x), x[1]), c(ucl, rev(lcl), ucl[1]), border = NA, col = '#00000033')
                lines(x, ucl, col = '#00000088')
                abline(v = mean(d), col="#6A3D9A", lty=1)
                
            }
        }
    }) #, height = 250, width = 300)



#############
### Saving ##
#############

    ## Allow user to download their data
    output$downloadDemBtn <- downloadHandler(
        filename = function() {
            sprintf('seabird-demography_%s.csv',
                    format(Sys.time(), '%Y-%m-%d_%H:%M:%S'))
        },
        content = function(file) {
            demf <- prepare_all_dem_summary()
            c <- names(demf) %in% c('sic', 'si', 'sa', 'bs', 'pb')
            demf[, c] <- round(100 * demf[,c], 3)
            demf$grate <- round((demf$grate - 1) * 100, 3)
            c <- !(names(demf) %in% c('sic', 'si', 'sa', 'bs', 'pb', 'grate'))
            demf[, c] <- round(demf[, c])
            demf <- as.data.frame(t(demf))
            demf <- cbind(par=labs[rownames(demf)], demf)
            colnames(demf) <- c('Parameter',
                               'Base-mean', 'Base-2.5-percentile', 'Base-97.5-percentile',
                               'Scenario-mean', 'Scenario-2.5-percentile', 'Scenario-97.5-percentile')
            write.csv(demf, file, row.names = FALSE)
        }
    )
    output$downloadThreatBtn <- downloadHandler(
        filename = function() {
            sprintf('seabird-threats_%s.csv',
                    format(Sys.time(), '%Y-%m-%d_%H:%M:%S'))
        },
        content = function(file) {
            write.csv(harvest_threats(), file, row.names = FALSE)
        }
    )

})

