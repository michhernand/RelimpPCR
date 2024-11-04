CXX = g++
CXXFLAGS = -Wall -O2 -fPIC

R_HOME := $(shell R RHOME)
RCPP_INCLUDE := $(shell Rscript -e "Rcpp:::CxxFlags()")
R_INCLUDES := $(shell $(R_HOME)/bin/R CMD config --cppflags)

SRC_DIR = RelimpPCR/src
BUILD_DIR = RelimpPCR/build
LIB_DIR = RelimpPCR/lib

EXTERNAL_INCLUDES = -I/usr/local/include
EXTERNAL_LIBS = -L/usr/local/lib

ALL_INCLUDES = $(EXTERNAL_INCLUDES) $(RCPP_INCLUDE) $(R_INCLUDES)
ALL_LIBS = $(EXTERNAL_LIBS)

build:
	cd ./RelimpPCR && Rscript -e "Rcpp::compileAttributes()"
	R CMD build ./RelimpPCR
	R CMD INSTALL RelimpPCR_1.0.tar.gz

clean:
	rm -f RelimpPCR_*.tar.gz
	Rscript -e "devtools::clean_dll()"
	cd ./RelimpPCR && Rscript -e "remove.packages('RelimpPCR')"

test:
	Rscript -e "RelimpPCR::train_test_split_r(as.matrix(mtcars), as.vector(mtcars[,3]), 0.7)"
	Rscript -e "RelimpPCR::split_and_normalize_r(as.matrix(mtcars), as.vector(mtcars[,3]), 0.7)"
	# Rscript -e "RelimpPCR::RelimpPCR(as.matrix(mtcars), as.vector(mtcars[,3]), 0.7)"
	Rscript -e "RelimpPCR::backwards_step_princomp_r(as.matrix(mtcars))"
