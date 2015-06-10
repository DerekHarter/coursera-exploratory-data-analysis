## read in the data files
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

## Q4: Across the United States, how have emissions from coal
##     combustion-related sources changed from 1999-2008?

# Here we need to use the source classification code file to
# pull out source classifications that are related to
# coal combustion sources.  This yields 230
# names with Coal in them.
# There seem to be a lot of names with Coal in them but
# that aren't related to combustion, for example,
# Coal Mining, Cleaning & Material Handling, etc.
coal.index <- grep('Coal', scc$Short.Name, fixed=TRUE)

# I will limit to obvious combustion sources, the main ones
# being Ext Comb, Int Comb, Stationary Fuel Comb.
# This yields 91 names with Comb and Coal in them,
# which I believe are probably the main coal combustion
# sources from the data.
coal.index <- grep('.*Comb.*Coal.*', scc$Short.Name)

# subset the nei data using scc codes we found related
# to Coal Combustion sources
scc.codes = scc$SCC[coal.index]
coal.combustion <- subset(nei, SCC %in% scc.codes)

# now do totals by year 
totalbyyear <- aggregate(Emissions ~ year, sum, data = coal.combustion)
totalbyyear
barplot(totalbyyear$Emissions, names.arg = totalbyyear$year)
