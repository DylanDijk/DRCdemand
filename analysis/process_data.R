load(file = "data/Irish.RData")

# Shift dateTime column by one hour
Irish_adj = Irish
Irish_adj$extra$dateTime = Irish$extra$dateTime + 60*60

# Fixing toy
Irish_adj$extra$toy = as.numeric(format(Irish_adj$extra$dateTime, "%j")) /365

# make tod between 0 and 1
# Irish_adj$extra$tod = sin((pi*Irish_adj$extra$tod)/24) 
# keep tod as it is for now

save(Irish_adj, file = "data/Irish_adj.RData")


