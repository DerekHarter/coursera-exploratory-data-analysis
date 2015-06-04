## read in the data files
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

# summary shows a mean of emissions at 3.4, but a max value
# that is suspiciously large, an outlier?
# I would guess that some values were reported not in tons but in simple
# lbs for some data?
summary(nei)

# there are no missing values for emissions
sum(is.na(nei$Emissions))

# since mean is 3.4 (tons), lets see how many values look suspiciously large
# there are >170k values greater than 10, this is only 2% of the values
# there are >35k values greater than 100, this represents only 1/2 a percent
# For the question we are answering, these outliers probably don't effect
# the result, but we might think about trimming extreme outliers in any case
sum(nei$Emissions > 10)
mean(nei$Emissions > 10)

sum(nei$Emissions > 10)
mean(nei$Emissions > 10)

## Q1: Have total emissions from PM2.5 decreased in the 
##     United States from 1999 to 2008? Using the base plotting system, 
##     make a plot showing the total PM2.5 emission from all sources for
##     each of the years 1999, 2002, 2005, and 2008.

# split the data by year, and summarize
# means do go down, apparently significantly, by year
# from 6.6 in 1999 to 1.75 in 2008
meanbyyear <- with(nei, tapply(Emissions, year, mean, na.rm = TRUE))
meanbyyear

# lets try and boxplot the data
# so many outliers causing plotting system to grind to a halt
# so have boxplot not display the extreme values, only the interquartile
# range
#boxplot(nei$Emissions, by = nei$year)
boxplot(nei$Emissions ~ nei$year, outline = FALSE)

# The ranges definitely look significant, but since everything is down
# close to 0 hard to see how means change, especially for the years other
# than 1999.  Try looking at log of emission values.
# It appears that the 2002-2008 are significantly different
# from the 1999 value, but even though the median and mean value of
# 2008 was down a bit from 2002 and 2005, the boxplot appears to show
# the change hasn't been significant from 2002 to 2008
boxplot(log10(nei$Emissions) ~ nei$year, outline = FALSE)


# ok, lets specifically look at the total emissions in each year from
# all sources.  We can plot this as a simple bar chart for the totals
# looking at the numbers we see that the totals have droped a lot from
# 1999, though not much difference between 2002 and 2005
totalbyyear <- with(nei, tapply(Emissions, year, sum, na.rm = TRUE))
totalbyyear

# I'm curious what the number of observations were for each year.  If they
# are equal, then it makes sense to compare the yearly totals, but if not...
# The number of observations is increasing, there are over 800k more
# observations in 2008 than 1999, which makes the conclusion that
# the total pollution being emitted is decreasing even stronger
obsperyear <- with(nei, tapply(Emissions, year, length))
obsperyear

# create a simple bar chart of the total emissions from all sources by year
# as asked for for plot 1, save this plot to a png file
png('plot1.png',  width=480, height=480)
barplot(totalbyyear, main = 'Total Emissions of PM_{2.5}\nNationwide, all sources')
dev.off()
