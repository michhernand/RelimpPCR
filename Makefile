build:
	cd ./RelimpPCR && Rscript -e "Rcpp::compileAttributes()"
	R CMD build ./RelimpPCR
	R CMD INSTALL RelimpPCR_1.0.tar.gz

clean:
	rm -f RelimpPCR_*.tar.gz
	Rscript -e "devtools::clean_dll()"
	cd ./RelimpPCR && Rscript -e "remove.packages('RelimpPCR')"

test:
	Rscript -e "RelimpPCR::train_test_split_r(mtcars, as.vector(mtcars[,3]))"
