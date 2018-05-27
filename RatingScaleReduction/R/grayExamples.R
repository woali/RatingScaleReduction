grayExamples <-
function(attribute, D) {
    df1 <- data.frame(D, attribute)
    
    # compare each values of attributes in every examples 
    for (i in 1:nrow(df1)) {
        # set df3 is null
        df3 <- NULL
        # set num is c()
        num <- c()
        
        # iterate for all attributes   
        for (j in i:nrow(df1)) {  
            if (all(df1[i, -1] == df1[j, -1]) && j != i) {

                #row bind df1 to df3
                df3 <- rbind(df3, df1[j, ])

                #column bind j to num
                num <- cbind(num, j)
                
                # print eery example for which there are another examples (one or more) with the same values of
                # attributes
                
                print(rbind(df1[i, ], df1[j, ]))
            }
        }
    }
    return("end of set")
}
