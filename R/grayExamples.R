grayExamples <-
function(attribute, D){
df1 <-data.frame(D,attribute)

for (i in 1:nrow(df1)){

df3=NULL
num <-c()
for (j in i:nrow(df1)){
if (all(df1[i,-1] == df1[j,-1]) && j!=i)
{df3 <-rbind(df3, df1[j,])
num <-cbind(num,j)
print(rbind(df1[i,], df1[j,]))}
}

}
return('end of set')
}
