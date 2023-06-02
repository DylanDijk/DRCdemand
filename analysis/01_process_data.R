library(tidyverse)
library(lubridate)
if( !require(electBook) ){
  library(devtools)
  install_github("mfasiolo/electBook")
}
library(electBook)
data("Irish")

# Shift dateTime column by one hour
Irish_adj = Irish
Irish_adj$extra$dateTime = Irish$extra$dateTime + 60*60

# Fixing toy
Irish_adj$extra$toy = as.numeric(format(Irish_adj$extra$dateTime, "%j")) /365
# Irish_adj$extra$dateTime[4081:4085]
# make tod between 0 and 1
# Irish_adj$extra$tod = sin((pi*Irish_adj$extra$tod)/24)
# keep tod as it is for now


# Format survey data
Irish_adj$survey <- tibble::as_tibble(Irish$survey) %>%
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



# train and test split
# random 2 days from each month
# need to split both indCons and extra
Irish_adj_train = Irish_adj[c("extra", "indCons")]
Irish_adj_test = Irish_adj[c("extra", "indCons")]
# not using 2009 data in test set
Irish_adj_test$indCons = Irish_adj_test$indCons[!(year(Irish_adj_test$extra$dateTime) == 2009),]
Irish_adj_test$extra = Irish_adj_test$extra[!(year(Irish_adj_test$extra$dateTime) == 2009),]

test_ind = as.POSIXct(character())
set.seed(3)
for(i in 1:12){
  sample_dates = unique(as.Date(Irish_adj_test$extra$dateTime[(month(Irish_adj_test$extra$dateTime)) == i]))
  test_ind = c(test_ind, sample(sample_dates, 2, replace = F))
  test_ind = as.Date(test_ind)
}

test_ind_all = as.Date(Irish_adj_test$extra$dateTime) %in% test_ind
Irish_adj_test$indCons = Irish_adj_test$indCons[test_ind_all,]
Irish_adj_test$extra = Irish_adj_test$extra[test_ind_all,]

Irish_adj_train$indCons = Irish_adj_train$indCons[!(as.Date(Irish_adj_train$extra$dateTime) %in% test_ind),]
Irish_adj_train$extra = Irish_adj_train$extra[!(as.Date(Irish_adj_train$extra$dateTime) %in% test_ind),]

save(Irish_adj_train, file = "data/Irish_adj_train.RData")
save(Irish_adj_test, file = "data/Irish_adj_test.RData")
save(test_ind, file = "data/test_dates.RData")

