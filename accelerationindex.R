Acc.index <- function(p){
    nn <- dim(p)[1]
    # Cumulated
    tests.aa <- p[,1] # Cumulated tests by date
    cases.aa <- p[,2] # Cumulated positive tests by date
    # daily
    d.tests<- tests.aa[2:nn]-tests.aa[1:(nn-1)]
    d.cases<- cases.aa[2:nn]-cases.aa[1:(nn-1)]
    ddd <<- cbind(d.tests,d.cases)
    #
    EE    <- rep(0,nn-1)
    #
    for (i in 1:(nn-1)){
        dtests<- d.tests[i] / tests.aa[i]
        dcases<- d.cases[i] / cases.aa[i]
        EE[i] <- dcases / dtests}
    return(EE)}
