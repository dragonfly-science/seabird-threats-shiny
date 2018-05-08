HOST ?= 192.168.0.136

all:

.PHONY: data data2 shiny shiny2 deploy deploy2

# .PHONY: data shiny
data:
	Rscript prepare-data.r app  &&  cd app && Rscript -e "compiler::cmpfile('functions.r')"
shiny:
	Rscript -e "shiny::runApp('app', port=4321, host = '$(HOST)')"
deploy:
	Rscript -e "rsconnect::deployApp('app', appName='seabird-threats', appTitle='Simulations of seabird threats', account='dragonfly-science')"

blank:
	Rscript -e "shiny::runApp('app_blank', port=4321, host = '$(HOST)')"
deployblank:
	Rscript -e "rsconnect::deployApp('app_blank', appName='seabird-threats-explore', appTitle='Simulations of seabird threats', account='dragonfly-science')"

clean:
	rm shiny/figs/*
