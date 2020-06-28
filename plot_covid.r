# function for interactive plotting for COVID-19 time series

plot_covid <- function(x, cum=TRUE, incidence=TRUE, names=NULL, timing=NULL){

    options(warn = -1) # disable some spurious plot_ly warning

    if (!is.null(names)){
        names <- unique(names)
        x <- x[x$NAME %in% names,]
        x$NAME <- droplevels(x$NAME)
        x <- split(x, x$NAME)
    } else {
        x <- list(x)        
    }
    
    date_lim <- c(as.POSIXct("2020/01/01"), Sys.time())

    lab = sprintf("COVID-19 %s", 
      if (incidence) "incidence" else "cases")
    
    n <- length(x)
    maxcases <- 0

    p <- plot_ly()

    for (i in 1:n){
        
        x_ <- x[[i]]
        x_$NAME <- droplevels(x_$NAME)
        
        date_uni_ <- sort(unique(x_$T))
        daily_ <- sapply(split(x_$N, x_$T), sum)
        
        cases <- if (cum) cumsum(daily_) else daily_
            
        pop = sum(sapply(split(x_$POP, x_$NAME), mean))/100000
        if (incidence) cases <- cases/pop

        maxcases <- max(maxcases,cases)
        
        uNAME <- unique(x_$NAME)
        nNAME <- length(uNAME)
        label <- if (nNAME > 1) "All districts" else uNAME

        
        p <- p %>% add_trace(x=date_uni_, y=cases, name=label, type="scatter", mode="lines")
    }
    
    p <- p %>% layout(hovermode = "x unified")
    p <- p %>% layout(showlegend = TRUE)
    p <- p %>% layout(xaxis = list(range = date_lim, showline = TRUE))
    p <- p %>% layout(yaxis = list(title = lab, showline = TRUE, rangemode = "nonnegative", ticks = "outside"))

    if (!is.null(timing)) p <- p %>% layout(shapes=list(type="rect",
                   fillcolor="rgb(255, 222, 95)", line=list(color="rgb(255, 222, 95)"), opacity = 0.2,
                   x0 = timing[1], x1 = timing[2], y0 = 0, y1 = maxcases))
                                
    return(p)
}
