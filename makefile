all:

# .PHONY: data shiny
data:
	Rscript prepare-data.r

.PHONY: shiny
shiny:
	Rscript -e "shiny::runApp('app', port=4321)"
deploy:
	Rscript -e "shinyapps::deployApp('app', 'seabird-threats', 'dragonfly-science')"


clean:
	rm shiny/figs/*
