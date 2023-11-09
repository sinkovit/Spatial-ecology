dist <- function(x1, y1, x2, y2) {
    # find distance between two points in the plane
    d <- sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))
    return(d)
}

for (gname in unique(tidydta1$group)) {
    print(gname)
    x1 <-  tidydta1$lat[which(tidydta1$group == gname)]
    y1 <-  tidydta1$long[which(tidydta1$group == gname)]
    x2 <- x1[2:length(x1)]
    y2 <- y1[2:length(y1)]

#    print(x1)
#    print(y1)

    if (x1[1] == x1[length(x1)] && y1[1] == y1[length(y1)]) {
       print("agree")
    } else {
       print("disagree")
    }

    x1 <- x1[-length(x1)] # remove last element
    y1 <- y1[-length(y1)] # remove last element
    d <- mapply(dist, x1, y1, x2, y2)

#    print(d)
#    print(min(d))
#    print(max(d))
}
