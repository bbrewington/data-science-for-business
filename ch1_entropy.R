## Function: entropy
## Calculates the entropy, given a dataset and a variable
## ISSUE: I think I need to include 2 variables in the function call: property variable and target variable.
##        Currently, it's only finding the entropy of the variable on itself, which may not make sense
##        (getting >1 values testing on dataset "iris")

entropy <- function(data, variable) {
     p_overall <- 0
     
     for (i in 1:length(levels(as.factor(data[,which(names(data) == variable)])))){
          p_level <- (length(data[,which(names(data) == variable)]
                         [data[,which(names(data) == variable)] == levels(as.factor(data[,which(names(data) == variable)]))[i]])) / 
               length(data[,which(names(data) == variable)])
          p_overall <- p_overall - (p_level*log2(p_level))
          print(paste("numerator ",i," = ",length(data[,which(names(data) == variable)]
                                                  [data[,which(names(data) == variable)] == levels(as.factor(data[,which(names(data) == variable)]))[i]])))
          print(paste("denominator ",i," = ",length(data[,which(names(data) == variable)])))
          print(paste("p_level_",i," = ", p_level*log2(p_level)))
     }
     
     p_overall
}
