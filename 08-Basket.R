dados<-PB_dados[,c("ID_ENTREVISTA","term")]

library(arules)
library(arulesViz)



trans <- as(split( dados[,"term"] , dados[,"ID_ENTREVISTA"]), "transactions")


rules <- apriori(data=trans, parameter=list (supp=0.05,conf = 0.1), appearance = list (default="lhs",rhs="valor"), control = list (verbose=T)) 
rules_conf <- sort (rules, by="count", decreasing=TRUE) 
inspectDT(rules_conf)






