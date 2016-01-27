all:

.PHONY: data data2 shiny shiny2 deploy deploy2

# .PHONY: data shiny
data:
	Rscript prepare-data.r app  &&  cd app && Rscript -e "compiler::cmpfile('functions.r')"
shiny:
	Rscript -e "shiny::runApp('app', port=4321)"
deploy:
	Rscript -e "shinyapps::deployApp('app', 'seabird-threats', 'dragonfly-science')"


clean:
	rm shiny/figs/*
