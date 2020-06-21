identify_cluster <- function(x, LK){

    cluster <- integer(0)

    for (k in 1:length(LK)){

        pos <- grep(LK[k], x$LK)

        if (length(pos) == 0){
            
            cat(LK[k], "is not in a cluster\n")
            
        } else {

            clus  <- unique(x$cluster[pos])
            nclus <- length(clus)

            for (i in 1:nclus) cat(LK[k], "is in cluster", clus[i], "\n")

            cluster <- c(cluster, clus)
        
        }

    }

    return(cluster)

}
    

plot_cases <- function(x, cum=FALSE, incidence=FALSE, LK=NULL, timing=NULL){

    options(warn = -1) # disable some spurious plot_ly warning

    if (!is.null(LK)){
        LK <- unique(LK)
        x <- x[x$LK %in% LK,]
        x$LK <- droplevels(x$LK)
        x <- split(x, x$LK)
    } else {
        x <- list(x)        
    }
    
    date_lim <- c(as.POSIXct("2020/01/01"), Sys.time())

    lab = sprintf("total COVID-19 %s", 
      if (incidence) "incidence" else "cases")
    
    n <- length(x)
    maxcases <- 0

    p <- plot_ly()

   # if (!is.null(timing)) p <- p %>% add_trace(x=timing, y=1e6, name='Cluster Timing', fill='tozeroy')

    for (i in 1:n){
        
        x_ <- x[[i]]
        
        date_uni_ <- sort(unique(x_$T))
        daily_ <- sapply(split(x_$N, x_$T), sum)
        
        cases <- if (cum) cumsum(daily_) else daily_
            
        pop = sum(sapply(split(x_$POP, x_$ID), mean))/100000
        if (incidence) cases <- cases/pop

        maxcases <- max(maxcases,cases)
        
        uLK <- unique(x_$LK)
        nLK <- length(uLK)
        name <- if (nLK > 1) "All LKs" else uLK

        
        p <- p %>% add_trace(x=date_uni_, y=cases, name=name, type="scatter", mode="lines")
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
