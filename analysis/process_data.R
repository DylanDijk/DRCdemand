library(tidyverse)
load(file = "data/Irish.RData")

# Shift dateTime column by one hour
Irish_adj = Irish
Irish_adj$extra$dateTime = Irish$extra$dateTime + 60*60

# Fixing toy
Irish_adj$extra$toy = as.numeric(format(Irish_adj$extra$dateTime, "%j")) /365

# make tod between 0 and 1
# Irish_adj$extra$tod = sin((pi*Irish_adj$extra$tod)/24) 
# keep tod as it is for now


# Format survey data
Irish_adj$survey <- as_tibble(Irish$survey) %>%
  readr::type_convert(
    col_types = list(
      OWNERSHIP = readr::col_factor(),
      HEAT.HOME = readr::col_factor(),
      HEAT.WATER = readr::col_factor(),
      WINDOWS.doubleglazed = readr::col_factor()
    )
  ) %>% # Change variables to factors
  dplyr::mutate(HOME.whitegoods = as.factor(Irish$survey$HOME.APPLIANCE..White.goods.)) %>% # Rename variable
  dplyr::select(-c(Code, HOME.APPLIANCE..White.goods.)) # Remove redundant variables

save(Irish_adj, file = "data/Irish_adj.RData")


