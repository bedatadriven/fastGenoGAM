
# Cross-section of the fastGenoGAM routine
# Based on commit f000c98

library(methods)
library(datasets)
library(utils)
library(stats)

print(search())

setwd("/home/alex/dev/genogam/fastGenoGAM/benchmarks/lbfgs")

#expected <- readRDS("expected.rds")
#
#str(expected)
print(R.version.string)
print(system.time(library(fastGenoGAM)))


control <-  readRDS("lbfgs_20180116134436_control.rds")
ellipses <- readRDS("lbfgs_20180116134436_ellipses.rds")
f <- readRDS("lbfgs_20180116134436_f.rds")
fact <- readRDS("lbfgs_20180116134436_fact.rds")
gr <- readRDS("lbfgs_20180116134436_gr.rds")
H0 <- readRDS("lbfgs_20180116134436_H0.rds")
x0 <- readRDS("lbfgs_20180116134436_x0.rds")



call.lbfgs <- function(x0, H0, gr, control, fact, ...) {
  fastGenoGAM:::.lbfgs(x0, H0, f, gr, control = control, fact = fact, ...)
}

while(TRUE) {
    print(system.time({

    res <- call.lbfgs(x0, H0, gr, control, fact, X = ellipses[["X"]], y = ellipses[["y"]],
                      offset = ellipses[["offset"]], theta = ellipses[["theta"]], lambda = ellipses[["lambda"]], S = ellipses[["S"]])


    cat("Result:\n")
    str(res)

                       }))

}

stopifnot(res$converged)
stopifnot(dim(res$par) == c(600, 1))
