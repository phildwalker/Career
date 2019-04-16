# Start Fresh Environment ------------

rm(list = ls())
cat("\014")
ifelse(is.null(dev.list()["RStudioGD"]), 
       print("No charts"), 
       dev.off(dev.list()["RStudioGD"])
)

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
