# from semvar
clusterqualSIL <- function (dmx, classes) 
{
    if (!is.factor(classes)) {
        classes <- as.factor(classes)
    }
    if (!is.matrix(dmx)) {
        dmx <- as.matrix(dmx)
    }
    nr <- nrow(dmx)
    diag(dmx) <- NA
    classlevels <- levels(classes)
    nclasslevels <- length(classlevels)
    numclasses <- as.numeric(classes)
    retval <- list()
    retval[["classfreqs"]] <- table(classes)
    retval[["pointclass"]] <- classes
    dst2class <- apply(dmx, 1, function(x) by(x, list(classes), 
        mean, na.rm = TRUE))
    row.classes <- matrix(rep(classlevels, nr), nrow = nclasslevels)
    col.classes <- matrix(rep(classes, nclasslevels), nrow = nclasslevels, 
        byrow = T)
    same.class <- (row.classes == col.classes)
    A.dst <- dst2class[same.class]
    orderedlevels <- matrix(apply(dst2class, 2, order), nrow = nclasslevels)
    orderedvals <- matrix(apply(dst2class, 2, sort), nrow = nclasslevels)
    B.level <- ifelse(orderedlevels[1, ] == numclasses, orderedlevels[2, 
        ], orderedlevels[1, ])
    B.dst <- ifelse(orderedlevels[1, ] == numclasses, orderedvals[2, 
        ], orderedvals[1, ])
    retval[["pointqual"]] <- (B.dst - A.dst)/pmax(A.dst, B.dst)
    retval[["A.distance"]] <- A.dst
    retval[["B.distance"]] <- B.dst
    retval[["B.class"]] <- as.factor(classlevels[B.level])
    retval[["classqual"]] <- aggregate(retval[["pointqual"]], 
        list(classes), mean, na.rm = TRUE)[, 2]
    names(retval[["classqual"]]) <- classlevels
    retval[["globqual"]] <- mean(retval[["pointqual"]], na.rm = TRUE)
    retval[["meanclassqual"]] <- mean(retval[["classqual"]], 
        na.rm = TRUE)
    class(retval) <- "clustqualSIL"
    return(retval)
}
