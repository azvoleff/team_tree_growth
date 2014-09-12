library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(stringr)

load("trees_clean.RData")

# Some sites have repeated measurements for the same tree. Until this is fixed 
# in the database, just use the first observation.
trees <- filter(trees, obs_num == 1)

#trees <- filter(trees, sitecode == 'COU')

calc_growth <- function(piece) {
    piece <- piece[order(piece$ObservationDate), ]
    if (nrow(piece) == 1) {
        SamplingPeriodEnd <- ''
        SamplingPeriodNumber <- NA
        # Below is a kludge - dplyr won't concatenate NAs and Date objects, so 
        # below can't be NA
        period_end <- piece$ObservationDate
        n_days <- NA
        diameter_start <- NA
        diameter_end <- NA
        growth <- NA
        growth_ann <- NA
        pom_start <- NA
        pom_end <- NA
        pom_change <- NA
    } else {
        SamplingPeriodEnd <- piece$SamplingPeriod[2:nrow(piece)]
        SamplingPeriodNumber <- piece$SamplingPeriodNumber[2:nrow(piece)]
        period_end <- piece$ObservationDate[2:nrow(piece)]
        n_days <- as.numeric(diff(piece$ObservationDate), units="days")
        diameter_start <- piece$Diameter[1:(nrow(piece) - 1)]
        diameter_end <- piece$Diameter[2:nrow(piece)]
        growth <- diff(piece$Diameter)
        growth_ann <- (growth / n_days) * 365.25
        pom_start <- piece$POMHeight[1:(nrow(piece) - 1)]
        pom_end <- piece$POMHeight[2:nrow(piece)]
        pom_change <- diff(piece$POMHeight)
    }
    return(data.frame(SamplingUnitName=piece$SamplingUnitName[1],
                      SamplingPeriodEnd=SamplingPeriodEnd,
                      SamplingPeriodNumber=SamplingPeriodNumber,
                      period_end=period_end,
                      n_days=n_days,
                      diameter_start=diameter_start,
                      diameter_end=diameter_end,
                      growth=growth,
                      growth_ann=growth_ann,
                      pom_start=pom_start,
                      pom_end=pom_end,
                      pom_change=pom_change,
                      Family=piece$Family[1],
                      Genus=piece$Genus[2],
                      Species=piece$Species[1],
                      sitecode=piece$sitecode[1],
                      SiteName=piece$SiteName[1],
                      OnehaPlotNumber=piece$OnehaPlotNumber[1],
                      OnehaPlotXCoordinate=piece$OnehaPlotXCoordinate[1],
                      OnehaPlotYCoordinate=piece$OnehaPlotYCoordinate[1]))
}

timestamp()
samplingunits <- group_by(trees, SamplingUnitName)
growth <- do(samplingunits, calc_growth(.))
# Drop NA rows - these are trees with only one year of data
growth <- growth[!is.na(growth$n_days), ]
timestamp()

# Calculate relative growth rate (rgh)
growth$growth_rgr <- (log(growth$diameter_end) - log(growth$diameter_start)) / growth$n_days

save(growth, file='growth_dirty.RData')
