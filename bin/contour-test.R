good <- TRUE
for (gname in unique(tidydta1$group)) {
    x1 <-  tidydta1$lat[which(tidydta1$group == gname)]
    y1 <-  tidydta1$long[which(tidydta1$group == gname)]

    if (x1[1] != x1[length(x1)] && y1[1] != y1[length(y1)]) {
       good <- FALSE
    }
}

print(good)