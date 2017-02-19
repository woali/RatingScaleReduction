diffExamples <-
function(attribute){
m1 <-unique(attribute)
u <-nrow(m1)
d <-nrow(as.matrix(attribute))-u
return(list(total.examples=nrow(as.matrix(attribute)), dif.examples=u, dup.examples=d))
}
