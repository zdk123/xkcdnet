library(xkcd)
library(reshape2)
library(network)


 xkcdfilledcircle <-  function (mapping, data, mask = FALSE, ...) {

    requiredaesthetics <- c("x", "y", "diameter")
    segementmapdat <- xkcd:::createdefaultmappinganddata(mapping, data, 
        requiredaesthetics)
    data <- segementmapdat$data
    mapping <- segementmapdat$mapping
    nsegments <- dim(data)[1]
    datafun <- data
    argList <- list(...)
    argsfcntt <- names(formals(xkcd:::pointscircunference))
    argsfcn <- argsfcntt[argsfcntt != "..."]
    for (i in intersect(argsfcn, names(argList))) {
        if (!(is.null(argList[i]) == TRUE)) {
            if (length(argList[[i]]) == 1) 
                datafun[, i] <- unlist(rep(argList[[i]], nsegments))
            if (length(argList[[i]]) == nsegments) 
                datafun[, i] <- argList[[i]]
        }
    }
    listofinterpolates <- xkcd:::doforeachrow(datafun, fun, FALSE, ...)
    listofinterpolateswithillustrativedata <- lapply(1:nsegments, 
        function(i) {
            dti <- listofinterpolates[[i]]
            illustrativevariables <- names(datafun)[!names(datafun) %in% 
                names(dti)]
            dti[, illustrativevariables] <- datafun[i, illustrativevariables]
            dti
        })
  
    listofpaths <- lapply(listofinterpolateswithillustrativedata, 
        function(x, mapping, mask, ...) {
            pathmask <- NULL
            if (mask) {
                argList <- list(...)
                for (i in intersect(c("color", "colour"), names(argList))) argList[i] <- NULL
                argList$mapping <- mapping
                argList$data <- x
                argList$colour <- "white"
                if (is.null(argList$size) == TRUE) 
                  argList$size <- 3
                if (argList$size <= 3) 
                  argList$size <- 3
                else argList$size <- argList$size * 2
                pathmask <- do.call("geom_polygon", argList)
            }
            c(pathmask, geom_polygon(mapping = mapping, data = x, 
                ...))
        }, mapping = mapping, mask = mask, ... = ...)
    listofpaths
}


plotxkcdGraph <- function(graph, coord, layoutfun=NULL, dia=.1, vertex.labels=FALSE) {

    if (!is.null(layoutfun))
        coord <- layoutfun(network(graph), NULL)



    if (is.null(rownames(graph))) lab <- 1:nrow(graph)
    else lab <- rownames(graph)

    data.graph <- data.frame(x=coord[,1], y=coord[,2], diam=dia, lab=lab)
    gdat <- melt(as.matrix(graph))
    gdat <- gdat[which(gdat$value != 0),]
    gdat$xbeg <- data.graph$x[gdat[,1]]
    gdat$ybeg <- data.graph$y[gdat[,1]]
    gdat$xend <- data.graph$x[gdat[,2]]
    gdat$yend <- data.graph$y[gdat[,2]]
    temp <- unlist(lapply(1:nrow(gdat), function(i) paste(sort(c(gdat[i,1], gdat[i,2])), collapse="_")))
    gdatedg <-  gdat[order(temp)[!duplicated( sort(temp))],]
    gdatedg <- gdatedg[which(gdatedg$Var1 != gdatedg$Var2), ]

    p <- ggplot()
    p <-   p + xkcdline(mapping=aes(x=xbeg, y=ybeg, xend=xend, yend=yend), data=gdatedg, size=1, mask=FALSE)
         + xkcdfilledcircle(mapping=aes(x=x, y=y, diameter=dia, fill=lab), data=data, colour='black', size=1)
         + geom_text(aes(x=jitter(x, amount=.02), y=jitter(y, amount=.02), label = lab), data=data, size=20, family="xkcd")
         + theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank(),
        panel.background = element_blank())
         
    if (vertex.labels) {
        gdlab <- data.graph[data.graph$lab %in% c(gdatedg$Var1, gdatedg$Var2),]
        ang <- atan2(y=gdlab$y, x=gdlab$x) # * (180/pi)
        gdlab$newx <- (cos(ang) * (gdlab$diam/2+min(gdlab$diam))) + gdlab$x
        gdlab$newy <- (sin(ang) * (gdlab$diam/2+min(gdlab$diam))) + gdlab$y
        p <- p + geom_text(aes(x=jitter(newx, 1/10), 
                        y=jitter(newy, 5/10), label = lab), size=6, data=gdlab, family="xkcd")
    }
     return(p)
}


