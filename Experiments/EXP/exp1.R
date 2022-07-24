library(mkde)
library(raster)

gpsmovebank <- read.csv("Condor-movebank-trunc.csv", header=TRUE)
gpsnoutm <- read.csv("Condor-movebank-trunc-noutm.csv", header=TRUE)
gpstxt <- read.table("pandabob.txt", header=TRUE)

expectedMovebankHeadings <-c(
"event.id",               "visible",
"timestamp",              "location.long",
"location.lat",           "ground.speed",
"heading",                "height.raw",
"sensor.type",            "individual.taxon.canonical.name",
"tag.local.identifier",   "individual.local.identifier",
"study.name",             "utm.easting",
"utm.northing",           "utm.zone")

expectedMovebankHeadingsNoUTM <-c(
"event.id",               "visible",
"timestamp",              "location.long",
"location.lat",           "ground.speed",
"heading",                "height.raw",
"sensor.type",            "individual.taxon.canonical.name",
"tag.local.identifier",   "individual.local.identifier",
"study.name")

if (setequal(expectedMovebankHeadings, colnames(gpsmovebank))) {
   print("Movebank format with UTM coordinates")
} else if (setequal(expectedMovebankHeadingsNoUTM, colnames(gpsmovebank))) {
   print("Movebank format without UTM coordinates")
} else {
   print("Not Movebank format")
}

if (setequal(expectedMovebankHeadings, colnames(gpsnoutm))) {
   print("Movebank format with UTM coordinates")
} else if (setequal(expectedMovebankHeadingsNoUTM, colnames(gpsnoutm))) {
   print("Movebank format without UTM coordinates")
} else {
   print("Not Movebank format")
}

if (setequal(expectedMovebankHeadings, colnames(gpstxt))) {
   print("Movebank format with UTM coordinates")
} else if (setequal(expectedMovebankHeadingsNoUTM, colnames(gpstxt))) {
   print("Movebank format without UTM coordinates")
} else {
   print("Not Movebank format")
}
