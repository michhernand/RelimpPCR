CXX = g++
CXXFLAGS = -Wall -O2 -fPIC -std=c++20
LDFLAGS = -shared

R_HOME := $(shell R RHOME)
RCPP_INCLUDE := $(shell Rscript -e "Rcpp:::CxxFlags()")
R_INCLUDES := $(shell $(R_HOME)/bin/R CMD config --cppflags)

SRC_DIR = src
BUILD_DIR = build
LIB_DIR = lib

EXTERNAL_INCLUDES = -I/usr/local/include
EXTERNAL_LIBS = -L/usr/local/lib

ALL_INCLUDES = $(EXTERNAL_INCLUDES) $(RCPP_INCLUDE) $(R_INCLUDES)
ALL_LIBS = $(EXTERNAL_LIBS)

build:
	Rscript -e "Rcpp::compileAttributes()"
	R CMD build .
	R CMD INSTALL RelimpPCR_1.0.tar.gz

clean:
	rm -f RelimpPCR_*.tar.gz
	Rscript -e "devtools::clean_dll()"
	Rscript -e "remove.packages('RelimpPCR')"

test:
	Rscript -e "RelimpPCR::train_test_split_r(as.matrix(mtcars), as.vector(mtcars[,3]), 0.7)"
	Rscript -e "RelimpPCR::split_and_normalize_r(as.matrix(mtcars), as.vector(mtcars[,3]), 0.7)"
	# Rscript -e "RelimpPCR::RelimpPCR(as.matrix(mtcars), as.vector(mtcars[,3]), 0.7)"
	Rscript -e "RelimpPCR::backwards_step_princomp_r(as.matrix(mtcars))"
