## Function: entropy
## Calculates the entropy, given a dataset and a variable

entropy <- function(data, variable) {
     p_overall <- 0
     
     for (i in 1:length(levels(as.factor(data[,which(names(data) == variable)])))){
          p_level <- (length(data[,which(names(data) == variable)]
                         [data[,which(names(data) == variable)] == levels(as.factor(data[,which(names(data) == variable)]))[i]])) / 
               length(data[,which(names(data) == variable)])
          p_overall <- p_overall - (p_level*log2(p_level))
     }
     
     p_overall
}
