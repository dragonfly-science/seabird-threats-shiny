# seabird-threats-shiny
Self-contained shiny app, used for the seabird threat framework.

This is an interactive application to simulate the population dynamics
of albatross species, given a set of basic demographic parameters and
optionally a list of population impacts/threats.


## Package dependencies

* **shiny** (on CRAN)
* **RColorBrewer** (on CRAN)
* **popbio** (on CRAN)
* **devtools** (on CRAN)
* **shinyTable** (install with `devtools::install_github("trestletech/shinyTable")`)
* **DT** (install with `devtools::install_github("rstudio/DT")`)


## Data

### Demographic parameters

All demographic parameters are contained in
`delphi-round2-derived-parameters.csv`, and were obtained from a
two-round Delphi process.

Additionally, corrections to the survivorship of juveniles to their
first breeding were added to the script `prepare-data.r`.

The demographic data can be updated either in the file
`delphi-round2-derived-parameters.csv`, or manually coded in
`prepare-data.r`.  After update, run `Rscript prepare-data.r` to
update the data file used by the app.


### Threats

The list of potential threats is contained in the file `threats.csv`,
and can also be changed.
As with demographic parameters, update the data file used in the app
with `Rscript prepare-data.r`.
	

## App deployment on www.shinyapps.io

See the web page at <http://shiny.rstudio.com/articles/shinyapps.html>


## Using makefile

If [GNU Make](https://www.gnu.org/software/make/) (or equivalent) is
installed, these commands are possible on the command line for
convenience:

* `make data`: 
  Prepares the data; same as running `Rscript prepare-data.r`
  
* `make shiny`:
  Executes the app locally, made available at the address
  <http://127.0.0.1:4321>; same as running `Rscript -e "shiny::runApp('app', port=4321)"`
