## read in the data files
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

## Q2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
##     (fips == "24510") from 1999 to 2008.  Use the base plotting system to
##     make a plot to answer this question.

# This question is basically the same as our Q1, but we want to look
# at the totals only for the subset of observations from Baltimore City
# First, construct a subset of those observations in nei data only from
# Baltimore City
# There appear to be a total of only 2096 observations for baltimore city
# over these years.
baltimore <- subset(nei, fips == "24510")

# ok, lets look at the total by year for this subset of observations
# in BaltimoreCity
totalbyyear <- with(baltimore, tapply(Emissions, year, sum, na.rm = TRUE))
totalbyyear
totalbyyear <- aggregate(Emissions ~ year, sum, data = baltimore)
totalbyyear

# here the total number of observations is much smaller, so large differences
# in the number of observations could have big effects on what we see
# We do see a big change from 320 observations in 1999 to almost 700 in 2008,
# but again the number of observations is going up so if the totals go down
# it would only seem to strengthen argument that emission totals are going
# down for the city
obsperyear <- with(baltimore, tapply(Emissions, year, length))
obsperyear

# and construct a barplot again of the basic totals from all sources for the city
# so the trend is messier, with 2005 looking like the total increased back to
# almost 1999 levels.
png('plot2.png',  width=480, height=480)
par(mar=c(4,4,4,1))
barplot(totalbyyear, 
        main = 'Total Emissions of PM_{2.5}\nBaltimore City (fips == 24510)')
dev.off()
