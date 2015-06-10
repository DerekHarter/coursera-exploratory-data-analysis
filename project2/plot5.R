## read in the data files
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")


## Q5: How have emissions from motor vehicle sources changed
##     from 1999-2008 in Baltimore City?

# Need to subset for only Baltimore City fips/location, and
# for particular sources, motor vehicle only, using
# scc source classification codes
# Actually all motor vehicle sources from scc appear to
# be classified as Onroad.  The Nonroad are sources like trains
# and boats, but we want to concentrate on motor vehicle
# sources here.  So we will assume we can use
# the ON-ROAD category to subset out only the
# motor vehicle sources
baltimore <- subset(nei, fips == "24510" & type == "ON-ROAD")

# this is basically the ON-ROAD type sorces from
# question 3
totalbyyear <- aggregate(Emissions ~ year, sum, data = baltimore)
totalbyyear
barplot(totalbyyear$Emissions, names.arg = totalbyyear$year)
