## ui.R
library(shiny)
addResourcePath('data', 'data')

shinyUI(fluidPage(
    tags$head(
        tags$style(".rightAlign{float:right;}"),
        tags$title("Demography"),
        tags$link(rel="stylesheet",
                  href="//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css")
    ),
    tags$link(
        rel = "stylesheet", 
        href="https://fonts.googleapis.com/css?family=Bitter|Open+Sans"
    ),
    theme = "bootstrap.css",
    tagList(
        div(
            class='banner', 
            h2("Demography of seabirds"), 
            p("Explore the life-history of seabirds, 
                and the impact of threats on their populations"
              )
        )
    ),
    fluidRow(
        class="step",
        column(3, 
               tags$h2(tags$span(class="badge", 1), "Base case"),
               tags$p("Set the life-history parameters, by specifying the 95% quantile interval of the distribution of each parameter. As a starting position, you may select a species from the drop-down list."),
               tags$div( uiOutput('spp', inline=T))
               ),
        column(4, 
               div(class = 'row2',
                   column(4, h6(' ')),
                   column(4, h6('Lower')),
                   column(4, h6('Upper'))),
               div(class = 'row2',
                   column(4, h6('Age at first breeding (y)')),
                   column(4, uiOutput('afr_lcl')),
                   column(4, uiOutput('afr_ucl'))),
               div(class = 'row2',
                   column(4, h6('Immature cohort survival (%)')),
                   column(4, uiOutput('sic_lcl')),
                   column(4, uiOutput('sic_ucl'))),
               div(class = 'row2',
                   column(4, h6('Adult annual survival (%)')),
                   column(4, uiOutput('sa_lcl')),
                   column(4, uiOutput('sa_ucl'))),
               div(class = 'row2',
                   column(4, h6('Clutch size (eggs)')),
                   column(4, uiOutput('cs')))
               ),
        column(4, 
               div(class = 'row2',
                   column(4, h6(' ')),
                   column(4, h6('Lower')),
                   column(4, h6('Upper'))),
               div(class = 'row2',
                   column(4, h6('Breeding success (%)')),
                   column(4, uiOutput('bs_lcl')),
                   column(4, uiOutput('bs_ucl'))),
               div(class = 'row2',
                   column(4, h6('Proportion of adults breeding (%)')),
                   column(4, uiOutput('pb_lcl')),
                   column(4, uiOutput('pb_ucl'))),
               div(class = 'row2',
                   column(4, h6('Annual breeding pairs')),
                   column(4, uiOutput('nbp_lcl')),
                   column(4, uiOutput('nbp_ucl')))
               )
    ),
    hr(),
    fluidRow(
        class="step",
        column(3, 
               tags$h2(tags$span(class="badge", 2), "Parameters"),
               tags$p("From the life-history parameters, the annual growth rate, annual mortalities, and other characteristics of the population are calculated. The calculations use a simple, deterministic matrix population model. The uncertainty is estimated by drawing samples from the distributions of the life-history parameters. The mean and 95% quantile interval are shown for each derived parameter."),
               fluidRow(
                   column(3, h6('No. samples')),
                   column(9, numericInput("nsamples", 
                            label = NULL,
                            value = 1000, min = 100, max = 1e6, step = 100, width = '50%')))
               ),
        column(3, 
               align = 'top',
               plotOutput("plot_lambdas", width='80%', height='250px'),
               tags$p("Distribution of the population growth rate. The vertical line indicates the mean value. Grey indicates
                the base case, and red indicates the threat scenario.")
               ),
        column(6, 
               align = 'top',
               DT::dataTableOutput(outputId="all_dem_summary"), br()
               ),
        br(), br()
    ),
    hr(),
    fluidRow(
        class="step",
        column(3, 
               tags$h2(tags$span(class="badge", 3), "Scenario"),
               tags$p("Having settled on the base case, add or remove threats to create scenarios. The impact of existing threats is removed from the base case, while potential threats are added to it.
            The impact of the threat may be 
                specified as a change in the corresponding parameter, or as a number of individuals.")
               ),
        column(8,
               tags$div(
                   column(3, align = 'bottom', uiOutput("threat")),
                   column(2, align = 'bottom', uiOutput("impacted_par")),
                   column(2, align = 'bottom', uiOutput("exist_pot")),
                   column(2, align = 'bottom', div(class='impact', uiOutput("impact_ind_p"))),
                   column(2, align = 'bottom', div(class='impact', uiOutput("impact_ind_n"))),
                   column(1, align = 'bottom', uiOutput("addthreat"))
               ),
               tags$div(
                   DT::dataTableOutput(outputId="threat_table", width='100%')
               )
               )
    ),
    hr(),
    fluidRow(
        class="step",
        column(3, 
               tags$h2(tags$span(class="badge", 4), "Save"),
               tags$p("Download the life-history parameters or the threats as CSV files, suitable for loading into spreadsheets or other software.")
               ),
        column(3,
               downloadButton("downloadDemBtn", "Save life-history parameters")
               ),
        column(3,
               downloadButton("downloadThreatBtn", "Save threats")
               )
    ),
    br(),
    wellPanel(
        fluidRow(column(2, HTML('')),
                column(3,
                        HTML('<a href="http://www.doc.govt.nz"><img src="data/Department_of_Conservation_New_Zealand_logo.svg.png" width="200px"></a>')
                        ),
                 column(3,
                        HTML('<a href="http://www.dragonfly.co.nz"><img src="data/logo.png" width="150px"></a>')
                        ),
                 column(3,
                        HTML('<a href="https://creativecommons.org/licenses/by/4.0/"><img src="data/cc.png" /><img src="data/by.png" /></a>')
                        )
                 ),
        fluidRow(column(12,
                        HTML('<p> 
                                Made for the <a href = "http://www.doc.govt.nz"> Department of Conservation </a>
                                by <a href = "http://www.dragonfly.co.nz">Dragonfly Data Science</a>.
                                The text content and code of this website is copyright Department of Conservation, and is
                                <a href="https://github.com/dragonfly-science/seabird-threats-shiny/blob/master/LICENSE">licensed for reuse</a>.
                                The photograph of albatross used in the header is copyright Kristina Hoeppner, and is
                                <a href= "https://www.flickr.com/photos/4nitsirk/16097480266">licensed for reuse</a>.
                                The source code is available on <a href="https://github.com/dragonfly-science/seabird-threats-shiny">GitHub</a>. 
                                If you have any problems or suggestions, you are welcome to <a href="https://github.com/dragonfly-science/seabird-threats-shiny/issues">raise an issue</a>.
                            </p>'
                          )
                    )
                )
            )

))
