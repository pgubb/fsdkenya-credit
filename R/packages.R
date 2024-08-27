##################
# R Packages
##################

remove(list = ls(all.names = TRUE))

detachAllPackages <- function() {
  basic.packages.blank <-  c("stats",
                             "graphics",
                             "grDevices",
                             "utils",
                             "datasets",
                             "methods",
                             "base")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")

  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,
                                  TRUE,
                                  FALSE)]

  package.list <- setdiff(package.list, basic.packages)

  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

detachAllPackages()

if(!require(tidyverse)) {
  install.packages("tidyverse", repos="http://cloud.r-project.org")
  require(tidyverse)
}
if(!require(haven)) {
  install.packages("haven", repos="http://cloud.r-project.org")
  require(haven)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}
if(!require(readxl)) {
  install.packages("readxl", repos = "https://cloud.r-project.org/")
  require(readxl)
}
if(!require(srvyr)) {
  install.packages("srvyr", repos = "https://cloud.r-project.org/")
  require(srvyr)
}
if(!require(rlang)) {
  install.packages("rlang", repos = "https://cloud.r-project.org/")
  require(rlang)
}
if(!require(fastDummies)) {
  install.packages("fastDummies", repos = "https://cloud.r-project.org/")
  require(fastDummies)
}
if(!require(ggrepel)) {
  install.packages("ggrepel", repos = "https://cloud.r-project.org/")
  require(ggrepel)
}
if(!require(ggtext)) {
  install.packages("ggtext", repos="http://cloud.r-project.org")
  require(ggtext)
}
if(!require(treemap)) {
  install.packages("treemap", repos="http://cloud.r-project.org")
  require(ggtext)
}

library(broom)
library(clarify)
library(RColorBrewer)
library(patchwork)
library(modelr)
library(zoo)
library(kableExtra)

