# to run one line of command in visual studio code editor cmd + enter
install.packages("tidyverse")
library("tidyverse")
# set run directory
setwd(file.path(getwd(), "MetaRange_Jana/run_mytest"))
# need to understand how this input framework is designed
test_input <- read_csv("./input/environment/temperature.csv")
View(test_input)
test_output <- read_csv("./output/default at 24.09.2023 16-14-09/test.csv")
