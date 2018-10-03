#Import ggplot2 library
library(ggplot2)

#Read in data files

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#There are considerably different numbers of measurements between these years.
#It is therefore not reasonable to simply sum all the measurement locations. 
#We will sum only the SCC values that are common to all 4 years, and use that to
#plot the relative accumulated emissions. 

#Create subsets by year
NEI99 = NEI[NEI$year == 1999,]
NEI02 = NEI[NEI$year == 2002,]
NEI05 = NEI[NEI$year == 2005,]
NEI08 = NEI[NEI$year == 2008,]

#Determine which measurement locations (SCC values) were common to all years
commonSCC <- Reduce(intersect, list(NEI99$SCC, NEI02$SCC, NEI05$SCC, NEI08$SCC)) 

#Subset the common measurement locations that are in Baltimore
BaltOnly <- NEI[NEI$fips == "24510" & NEI$SCC %in% commonSCC,]

#Aggregate BaltOnly Emissions by year and type
ByTypeAndYear <- aggregate(BaltOnly$Emissions, by=list(BaltOnly$type,BaltOnly$year), FUN=sum, na.rm=TRUE)

#Fix up names which were lost in aggregate for some reason
names(ByTypeAndYear) = c("type", "year", "Emissions")

#Create png device
png("plot3.png", width=480, height=480)

#Plot total emissions by year
plt <- ggplot(ByTypeAndYear, aes(ByTypeAndYear$year, ByTypeAndYear$Emissions, xlab = "year", ylab = "Relative Total Emissions", color = type))
plt + geom_line()+xlab("Year") + ylab("Relative Total Emissions") 


#Close plot device
dev.off()
