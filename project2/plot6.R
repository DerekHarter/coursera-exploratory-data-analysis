## read in the data files
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")


## Q6:  Compare emissions from motor vehicle sources in
##      Baltimore City with emissions from motor vehicle
##      sources in Los Angeles County, California
##      (fips == "06037").  Which city has seen greater
##      changes over time in motor vehicle emissions?

# subset out for both of the regions/cities we want this
# time
cities <- subset(nei, (fips == "24510" | fips == "06037" ) & type == "ON-ROAD")

# aggregrate by both year and the city/region this time
totalbyyear <- aggregate(Emissions ~ year + fips, sum, data = cities)
totalbyyear

totalbyyear$log10emissions <- log10(totalbyyear$Emissions)
g <- ggplot(totalbyyear, aes(year, log10emissions))
g + geom_line(aes(color=fips)) + geom_point(aes(color=fips))