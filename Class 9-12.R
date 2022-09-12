library(tidyverse)
library(causaldata)
library(fixest)


mcaid.data <- read_tsv("https://raw.githubusercontent.com/imccart/Insurance-Access/master/data/output/acs_medicaid.txt")

reg.dat <- causaldata::gapminder %>%
  mutate(lgdp_pc=log(gdpPercap))

feols(lifeExp ~ lgdp_pc | country, data=reg.dat)

