build:
	R -e 'devtools::document()'
	R CMD build .
	R CMD check --as-cran RelimpPCR_${VERSION}.tar.gz

clean:
	rm -rf RelimpPCR.Rcheck
	rm -rf man
	rm RelimpPCR_*.tar.gz
