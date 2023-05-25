url <- "https://github.com/mfasiolo/electBook/raw/master/data/Irish.RData"
load(url(url))

View(Irish$extra)
Irish$extra$dateTime[4080:4090]
