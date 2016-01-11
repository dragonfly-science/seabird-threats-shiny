# Exploring seabird demographics

A self-contained shiny app, used for the New Zealand Department of Conservation 
seabird threat framework.

This is an interactive application to simulate the population dynamics
of albatross species, given a set of basic demographic parameters and
a list of population impacts or threats.


## Package dependencies

* **shiny** (on CRAN)
* **RColorBrewer** (on CRAN)
* **popbio** (on CRAN)
* **devtools** (on CRAN)
* **shinyTable** (install with `devtools::install_github("trestletech/shinyTable")`)
* **DT** (install with `devtools::install_github("rstudio/DT")`)


## Data

### Demographic parameters

All demographic parameters are contained in the file
`delphi-round2-derived-parameters.csv`, and were obtained from a
two-round Delphi process.

Additionally, adjustments to the survivorship of juveniles 
were added to the script `prepare-data.r`.

The demographic data can be updated either in the file
`delphi-round2-derived-parameters.csv`, or manually coded in
`prepare-data.r`.  After updating, run `Rscript prepare-data.r`.


### Threats

The list of potential threats is contained in the file `threats.csv`,
and can also be changed.
As with demographic parameters, update the data file used in the app
with `Rscript prepare-data.r`.
	

## App deployment

The app can be run locally by running the command
`Rscript -e "shiny::runApp('app', port=4321)"`. You will then be able to visit the app in your browser, at 
the URL [http://127.0.0.1:4321](http://127.0.0.1:4321).

To deplot the app publicly at shinyapps.io, follow the instructions 
on the [Rstudio website](http://shiny.rstudio.com/articles/shinyapps.html).


## Using makefile

If [GNU Make](https://www.gnu.org/software/make/) (or similar) is
installed, these convenience commands are available:

* `make data`: 
  Prepares the data; equivalent to running `Rscript prepare-data.r`
  
* `make shiny`:
  Executes the app locally; equivalent to running `Rscript -e "shiny::runApp('app', port=4321)"`
