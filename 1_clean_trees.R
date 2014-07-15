library(ggplot2)
library(dplyr)
library(lubridate)

load('H:/Data/TEAM_Database_Downloads/veg_data2014-07-10.gzip')
trees <- result$tree
sitecode_key <- read.csv("Site_Code_Key.csv")
trees$sitecode <- sitecode_key$Site.Name.Code[match(trees$SiteName, sitecode_key$Site.Name.Database)]
trees$ObservationDate <- as.Date(trees$ObservationDate)

trees <- tbl_df(trees)

###############################################################################
# Correct Diameter -> NewDiameter issue for BCI, NAK, NNN, and RNF

# BCI incorrectly uses zeros instead of NAs when NewDiameter does not apply.  
trees$POMHeight[trees$NewDiameter == 0] <- NA
trees$NewDiameter[trees$NewDiameter == 0] <- NA
trees$NewDiameter[trees$POMHeight == 0] <- NA

# Add an identifier to trees coding the sampling period number on a per site 
# basis, with 1 assigned to the first sampling period in each site
SamplingPeriods <- summarize(group_by(trees, sitecode, SamplingPeriod))
SamplingPeriods <- SamplingPeriods[order(SamplingPeriods$sitecode, SamplingPeriods$SamplingPeriod), ]
SamplingPeriods <- mutate(SamplingPeriods, SamplingPeriodNumber=seq(1, length(SamplingPeriod)))
trees$SamplingPeriodNumber <- SamplingPeriods$SamplingPeriodNumber[match(paste(trees$sitecode, trees$SamplingPeriod),
                                                                         paste(SamplingPeriods$sitecode, SamplingPeriods$SamplingPeriod))]

###############################################################################
# Add condition code columns. First need to clean the ConditionCodes field
trees$ConditionCodes[trees$ConditionCodes == " "] <- ""
trees$ConditionCodes[is.na(trees$ConditionCodes)] <- ""
trees$ConditionCodes[grepl(' ', trees$ConditionCodes)]
trees$ConditionCodes <- gsub('^,', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub('[,.]$', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub(' ', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub('[.]', ',', trees$ConditionCodes)

table(grepl('[.]', trees$ConditionCodes))
table(grepl('[ ]', trees$ConditionCodes))
table(grepl('[.,]$', trees$ConditionCodes))
table(grepl('^[.,]', trees$ConditionCodes))

ConditionCodes <- c('B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 
                    'N', 'O', 'P', 'R', 'S', 'T', 'U', 'V', 'W')
for (ConditionCode in ConditionCodes) {
    this_code <- unlist(lapply(trees$ConditionCodes, function(x) ConditionCode %in% x))
    trees <- cbind(trees, this_code)
    names(trees)[ncol(trees)] <- paste0('ConditionCode_', ConditionCode)
}

###############################################################################
# Correct for sites that are entering all new data after year 1 in the 
# NewDiameter column - this applies to 47,190 records across 4 sites
newdia_rows <- (trees$sitecode %in% c('NAK', 'NNN', 'RNF')) &
               (trees$SamplingPeriodNumber != 1)

hist(trees[newdia_rows, ]$NewDiameter)
table(is.na(trees[newdia_rows, ]$NewDiameter))
table(trees[newdia_rows, ]$Diameter == trees[newdia_rows, ]$NewDiameter)

hist(trees[newdia_rows, ]$NewPOMHeight)
table(is.na(trees[newdia_rows, ]$NewPOMHeight))
table(trees[newdia_rows, ]$POMHeight == trees[newdia_rows, ]$NewPOMHeight)

trees[newdia_rows, ]$Diameter <- trees[newdia_rows, ]$NewDiameter
# Also copy over the NewPOMHeight column to the POMHeight column since we are 
# using the measurement taken at that POM
trees[newdia_rows, ]$POMHeight <- trees[newdia_rows, ]$NewPOMHeight

###############################################################################
# Use NewDiameter column in sites that DID follow the protocol

# Also exclude CSN here, until I figure out what is going on at CSN
newdia_rows <- !(trees$sitecode %in% c('NAK', 'NNN', 'RNF', 'CSN')) &
               !(is.na(trees$NewDiameter))

hist(trees[newdia_rows, ]$NewDiameter)
table(is.na(trees[newdia_rows, ]$NewDiameter))
# Some NewDiameters match the old Diameter columns (234 do, 907 don't)
table(trees[newdia_rows, ]$Diameter == trees[newdia_rows, ]$NewDiameter)

hist(trees[newdia_rows, ]$NewPOMHeight)
table(is.na(trees[newdia_rows, ]$NewPOMHeight))
# Most NewPOMHeights do not match the POMHeight rows (only 2 match)
table(trees[newdia_rows, ]$POMHeight == trees[newdia_rows, ]$NewPOMHeight)

trees[newdia_rows, ]$Diameter <- trees[newdia_rows, ]$NewDiameter
# Also copy over the NewPOMHeight column to the POMHeight column since we are 
# using the measurement taken at that POM
trees[newdia_rows, ]$POMHeight <- trees[newdia_rows, ]$NewPOMHeight

###############################################################################
# There are multiple observations per sampling period for the same tree. This 
# should not occur except in cause of remeasurement or multiple stems. Multiple 
# stems however should have unique SamplingUnitName values.
trees <- mutate(group_by(trees, sitecode, SamplingUnitName, SamplingPeriod),
                         n_obs=rep(length(Diameter), length(Diameter)),
                         obs_num=seq(1:length(Diameter)))
# Make a dataframe of trees with multiple measurements
mult_obs_trees <- select(filter(trees, n_obs > 1), ObservationDate, Diameter, POMHeight, SamplingUnitName, ConditionCodes, obs_num)
mult_obs_trees <- mult_obs_trees[order(mult_obs_trees$SamplingUnitName, mult_obs_trees$ObservationDate), ]
write.csv(mult_obs_trees, file="trees_with_multiple_obs_per_period.csv", row.names=FALSE)

mean(trees$n_obs)
table(trees$n_obs)
table(trees[trees$n_obs > 1,]$sitecode, trees[trees$n_obs > 1,]$SamplingPeriod)

###############################################################################
# TODO: Figure out what is going on in 2013 CSN data

# Correct 999 used to code missing or dead trees in CSN per Jul 10 email from 
# Jimmy. This value codes missing or dead trees. Code as NA so these are 
# dropped from growth calculations. TODO: Need to recode these in the condition 
# code column.
trees[which((trees$sitecode == 'CSN') & (trees$Diameter == 999)), ]$Diameter <- NA

###############################################################################
# Site manager said BCI 2012 dbh was entered in mm - this doesn't appear to be 
# the case, so don't do any corrections here.

###############################################################################
# Exclude stems under 10 cm dbh
trees <- filter(trees, Diameter >= 10)

summ_stats <- summarize(group_by(trees, sitecode, SamplingPeriod),
                        dbh_mean=mean(Diameter, na.rm=TRUE),
                        dbh_min=min(Diameter, na.rm=TRUE),
                        dbh_max=max(Diameter, na.rm=TRUE),
                        dbh_sd=sd(Diameter, na.rm=TRUE),
                        POM_mean=mean(POMHeight, na.rm=TRUE),
                        POM_min=min(POMHeight, na.rm=TRUE),
                        POM_max=max(POMHeight, na.rm=TRUE),
                        POM_sd=sd(POMHeight, na.rm=TRUE),
                        newdbh_mean=mean(NewDiameter, na.rm=TRUE),
                        newdbh_min=min(NewDiameter, na.rm=TRUE),
                        newdbh_max=max(NewDiameter, na.rm=TRUE),
                        newdbh_sd=sd(NewDiameter, na.rm=TRUE),
                        newPOM_mean=mean(NewPOMHeight, na.rm=TRUE),
                        newPOM_min=min(NewPOMHeight, na.rm=TRUE),
                        newPOM_max=max(NewPOMHeight, na.rm=TRUE),
                        newPOM_sd=sd(NewPOMHeight, na.rm=TRUE))

###############################################################################
# Review other issues noted in summary stats
# COU 2011 has a very high max DBH
ggplot(filter(trees, sitecode == 'COU')) + geom_bar(aes(Diameter)) + facet_wrap(~ SamplingPeriod, scales="free")

# KRP 2011 has a very high max DBH
ggplot(filter(trees, sitecode == 'KRP')) + geom_bar(aes(Diameter)) + facet_wrap(~ SamplingPeriod, scales="free")

# PSH 2013 has a very high max DBH
ggplot(filter(trees, sitecode == 'PSH')) + geom_bar(aes(Diameter)) + facet_wrap(~ SamplingPeriod, scales="free")

n_obs <- summarize(group_by(trees, sitecode, SamplingPeriod),
                 n_obs=length(Diameter))
ggplot(n_obs) +
    geom_bar(aes(SamplingPeriod, n_obs), stat="identity") +
    facet_wrap(~ sitecode) +
    xlab("Sampling period") +
    ylab("Number of observations") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("trees_n_observations.png", width=14, height=7.5, dpi=300)

names(trees) <- gsub('1haPlot', 'OnehaPlot', names(trees))

save(trees, file='trees_clean.RData')
