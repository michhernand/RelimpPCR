build:
	cd ./RelimpPCR && Rscript -e "Rcpp::compileAttributes()"
	R CMD build ./RelimpPCR
	R CMD INSTALL RelimpPCR_1.0.tar.gz

remove:
	cd ./RelimpPCR && Rscript -e "remove.packages('RelimpPCR')"

test:
	Rscript -e "RelimpPCR::train_test_split(mtcars, as.vector(mtcars[,3]))"
