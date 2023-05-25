load(file = "data/Irish.RData")

View(Irish$survey)
dim(Irish$survey)

any(Irish$extra$holy)
View(Irish$extra)


dim(Irish$indCons)
View(Irish$indCons)
matplot(Irish$indCons[1:(48 * 300), 5], type = 'l')

sum(Irish$indCons)


