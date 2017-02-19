grayExamplesN <-
function(attribute, D, N){
df1 <-data.frame(D,attribute)

df3 <-NULL
num <-c()
for (j in 1:length(D)){
if (all(df1[N,-1] == df1[j,-1]) && j!=N)
{df3 <-rbind(df3, df1[j,])
num <-cbind(num,j)}}

df3 <-rbind(df1[N,],df3)
num <-cbind(N,num)
return(list(examp=df3,examp.nr=num))
}
