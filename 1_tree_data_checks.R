library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)

load('H:/Data/TEAM_Database_Downloads/veg_data2014-07-10.gzip')
trees <- result$tree
sitecode_key <- read.csv("Site_Code_Key.csv")
trees$sitecode <- sitecode_key$Site.Name.Code[match(trees$SiteName, sitecode_key$Site.Name.Database)]
trees <- tbl_df(trees)

# First check the number of new diameter and repeat diameter measurements for 
# each site for each period
trees <- group_by(trees, sitecode, SamplingPeriod)
pct_dbh_measures <- summarize(trees,
                            pct_dbh=sum(!is.na(Diameter))/length(Diameter), 
                            pct_dbh_missing=sum(is.na(Diameter))/length(Diameter), 
                            pct_newdbh=sum(!is.na(NewDiameter))/length(Diameter), 
                            pct_newdbh_missing=sum(is.na(NewDiameter))/length(Diameter))
pct_dbh_measures_long <- melt(pct_dbh_measures)

ggplot(pct_dbh_measures_long) +
    geom_bar(aes(SamplingPeriod, value, fill=variable), stat='identity') + 
    facet_wrap(~ sitecode) +
    xlab("Sampling Period") +
    ylab("Fraction of measurements") +
    theme_grey(base_size=10, ) +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("dbh_measurement_summary.png", width=14, height=7.5, dpi=300)

# Now check the number of new diameter measurements by site and period that 
# coexist int the same row with original diameter measurements. Do the same for 
# POM heights
pct_coexist <- summarize(trees,
                       n_dbh_and_newdbh=sum(!is.na(Diameter) & !is.na(NewDiameter))/length(Diameter),
                       n_dbh_and_newdbh_match=sum(Diameter == NewDiameter, na.rm=TRUE)/length(Diameter))

ggplot(pct_coexist) +
    geom_bar(aes(SamplingPeriod, n_dbh_and_newdbh), stat='identity') +
    facet_wrap(~ sitecode)

ggplot(pct_coexist) +
    geom_bar(aes(SamplingPeriod, n_dbh_and_newdbh_match), stat='identity') +
    facet_wrap(~ sitecode)

# This is good - from above plot we can see that, for the most part, when 
# Diameter and NewDiameter coexist in the same year, they do not match. This 
# suggests that the Diameter column is probably repeated from the prior year. 
# Check for this:
dbh_match_old_newdbh <- function(piece) {
    piece <- piece[order(piece$ObservationDate), ]
    if (nrow(piece) == 1) {
        dbh_match_old_newdbh <- NA
        newdbh_match_old_dbh <- NA
    } else {
        # Note the NA below because no comparison is possible in the first year
        dbh_match_old_newdbh <- (c(NA, piece$Diameter[2:nrow(piece)] == piece$NewDiameter[1:(nrow(piece) - 1)]))
        newdbh_match_old_dbh <- (c(NA, piece$NewDiameter[2:nrow(piece)] == piece$Diameter[1:(nrow(piece) - 1)]))
    }
    return(data.frame(sitecode=piece$sitecode[1],
                      SamplingPeriod=piece$SamplingPeriod[1],
                      ObservationDate=piece$ObservationDate[1],
                      dbh_match_old_newdbh=dbh_match_old_newdbh,
                      newdbh_match_old_dbh=newdbh_match_old_dbh))
}
test_new_old_dbh_match <- do(group_by(trees, SamplingUnitName), 
                             dbh_match_old_newdbh(.))

summarize(group_by(test_new_old_dbh_match, sitecode, SamplingPeriod),
          n_dbh_match_old_newdbh=sum(dbh_match_old_newdbh, na.rm=TRUE),
          n_newdbh_match_old_dbh=sum(newdbh_match_old_dbh, na.rm=TRUE))



table(trees$POMHeight == 1.3)







summarize(trees,
          n_dbh_match_old_newdbh=Diameter[2:nrow(Diameter)]==NewDiameter[1:(nrow(piece) - 1)]

summarize(trees,
          dbh_min=min(Diameter, na.rm=TRUE),
          dbh_max=max(Diameter, na.rm=TRUE),
          dbh_mean=mean(Diameter, na.rm=TRUE),
          new_dbh_min=min(NewDiameter, na.rm=TRUE),
          new_dbh_max=max(NewDiameter, na.rm=TRUE),
          new_dbh_mean=mean(NewDiameter, na.rm=TRUE))

trees$ObservationDate <- as.Date(trees$ObservationDate)
