list.of.packages <- c("tidyverse", "ggplot2", "drake")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(ggplot2)
library(drake)