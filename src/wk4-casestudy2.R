# read in 1999 data.  We use x0 variable names for the 1999 data
pm0 <- read.table('RD_501_88101_1999-0.txt', 
                  comment.char = "#", 
                  header= FALSE,
                  sep = "|", na.strings = "")
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed=TRUE)
names(pm0) <- make.names(cnames[[1]])
x0 <- pm0$Sample.Value
summary(x0)
mean(is.na(x0)) # ratio of missing values, is 11% going to be a problem?

# read in 2012 data
pm1 <- read.table('RD_501_88101_2012-0.txt', 
                  comment.char = "#", 
                  header= FALSE,
                  sep = "|", na.strings = "")
names(pm1) <- make.names(cnames[[1]])
x1 <- pm1$Sample.Value
summary(x1)
mean(is.na(x1)) # ratio of missing values, is 11% going to be a problem?

# first visualization, look at boxplots comparing median and spread
# of 1999 and 2012
boxplot(x0, x1) # hard to see, look at log...
boxplot(log10(x0), log10(x1))

# why are there negative values in 2012 data, is that
# a problem?
negative <- x1 < 0
sum(negative, na.rm=TRUE)
mean(negative, na.rm=TRUE) # ratio of negative values

#dates <- pm1$Date
dates <- as.Date(as.character(dates), "%Y%m%d")
hist(dates, "month")
hist(dates[negative], "month")

# explore pm levels for a single station in NY state
site0 <- unique(subset(pm0, State.Code == 36, 
                       c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, 
                       c(County.Code, Site.ID)))
site0 <- paste(site0[,1], site0[,2], sep=".")
site1 <- paste(site1[,1], site1[,2], sep=".")
both <- intersect(site0, site1)
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep="."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep="."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

# get number of observations at each site in 1999 and 2012
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

# pick county 63 site 2008 (video used )
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & 
                     Site.ID == 2008)
dim(pm0sub)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & 
                     Site.ID == 2008)
dim(pm1sub)

dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

dates1 <- pm1sub$Date
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)

par(mfrow = c(1,2), mar=c(4,4,2,1))
rng <- range(x0sub, x1sub, na.rm=T) # get y range for plotting
plot(dates0, x0sub, pch=20, ylim = rng) 
abline(h = median(x0sub, na.rm=T))
plot(dates1, x1sub, pch=20, ylim = rng)
abline(h = median(x1sub, na.rm=T))

# pick county 1 site 5 
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 1 & 
                     Site.ID == 5)
dim(pm0sub)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 1 & 
                     Site.ID == 5)
dim(pm1sub)

dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

dates1 <- pm1sub$Date
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)

par(mfrow = c(1,2), mar=c(4,4,2,1))
rng <- range(x0sub, x1sub, na.rm=T) # get y range for plotting
plot(dates0, x0sub, pch=20, ylim = rng) 
abline(h = median(x0sub, na.rm=T))
plot(dates1, x1sub, pch=20, ylim = rng)
abline(h = median(x1sub, na.rm=T))


# exploring change at the state level
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm=T))
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm=T))
summary(mn1)

d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")

par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[,2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012, 52), mrg[,3], xlim = c(1998, 2013)))
segments(rep(1999, 52), mrg[,2], rep(2012, 52), mrg[,3])
