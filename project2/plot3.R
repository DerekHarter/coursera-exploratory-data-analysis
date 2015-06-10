## read in the data files
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

## Q3: Of the four types of sources indicated by the type (point,
##     nonpoint, onroad, nonroad) variable, which of these four sources
##     have seen decreases in emissions from 1999-2008 for Baltimore City?
##     Which have seen increases in emissions from 1999-2008?
##     Use the ggplot2 plotting system to make a plot answer this question.

# First, construct a subset of those observations in nei data only from
# Baltimore City
# There appear to be a total of only 2096 observations for baltimore city
# over these years.
baltimore <- subset(nei, fips == "24510")


library(ggplot2)
# a quick look, use basic qplot to create linear smoothed model of
# the four types of sources.  We see that on-road appear to be smallest
# and decreasing, while point type emissions were the largest and have seen
# dramatic decreases.  All types appear to be decreasing, but
# nonpoint and non-road perhaps not as much as the other types.
baltimore$Emissions <- baltimore$Emissions + 0.01 # 128 observations have 0 emissions, add a bit so can take log
qplot(year, log(Emissions), data=baltimore, color=type, geom = c("point", "smooth"), method="lm")

# But the previous plot would be problematic.  We get a sense of the
# average emissions by type for each year, but this really doesn't
# give us a sense of the total amount of polution for each type, so
# I believe we still need to look at the totals by type if we want
# to really answer the question of which of the sources is
# decreasing/increasing.
# lets try just looking at the sum of the source by type and year
byyeartype <- aggregate(Emissions ~ year + type, sum, data = baltimore)
byyeartype
qplot(year, Emissions, data=byyeartype, color=type, geom=c("line"))

g <- ggplot(byyeartype, aes(year, Emissions))
#g + geom_bar(stat='identity', aes(fill=type))
g + geom_line(aes(color=type)) + geom_point(aes(color=type))