library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)

load('growth_dirty.RData')

growth <- tbl_df(growth)

# Below is based on trim.growth in CTFSRPackage.rdata
slope <- 0.006214
intercept <- 0.9036
err_limit <- 4
maxgrow <- 7.5 # Defined in cm for use with TEAM data (CTFS uses mm)
pomcut <- 0.05
mindbh <- 10
dbhunit <- "cm"
if (dbhunit == "cm") intercept <- intercept/10

stdev_dbh1 <- slope * growth$diameter_start + intercept

growth$ctfs_pomdiff_gr_pomcut <- abs(growth$pom_change / growth$pom_start) > pomcut
growth$ctfs_bad_posgrow <- growth$growth_ann > maxgrow
growth$ctfs_bad_neggrow <- growth$diameter_end <= (growth$diameter_start - err_limit * stdev_dbh1)
growth$ctfs_na_growth <- is.na(growth$growth_ann)
growth$ctfs_na_dbh_start <- is.na(growth$diameter_start)
growth$ctfs_na_dbh_end <- is.na(growth$diameter_end)
growth$ctfs_na_pom_start <- is.na(growth$pom_start)
growth$ctfs_na_pom_end <- is.na(growth$pom_end)
growth$ctfs_dbh_start_lt_mindbh <- growth$diameter_start < mindbh
growth$ctfs_dbh_end_lt0 <- growth$diameter_end < 0
growth$ctfs_accept <- with(growth, (!ctfs_pomdiff_gr_pomcut) &
                                   (!ctfs_bad_posgrow) &
                                   (!ctfs_bad_neggrow) &
                                   (!ctfs_na_growth) &
                                   (!ctfs_na_dbh_start) &
                                   (!ctfs_na_dbh_end) &
                                   (!ctfs_na_pom_start) &
                                   (!ctfs_na_pom_end) &
                                   (!ctfs_dbh_start_lt_mindbh) &
                                   (!ctfs_dbh_end_lt0))
save(growth, file='growth_ctfsflagged.RData')

ctfs_overall_summary <- group_by(growth, sitecode) %>%
    summarize(ctfs_pomdiff_gr_pomcut=sum(ctfs_pomdiff_gr_pomcut, na.rm=TRUE),
              ctfs_bad_posgrow=sum(ctfs_bad_posgrow),
              ctfs_bad_neggrow=sum(ctfs_bad_neggrow),
              ctfs_na_growth=sum(ctfs_na_growth), 
              ctfs_na_dbh_start=sum(ctfs_na_dbh_start), 
              ctfs_na_dbh_end=sum(ctfs_na_dbh_end), 
              ctfs_na_pom_start=sum(ctfs_na_pom_start), 
              ctfs_na_pom_end=sum(ctfs_na_pom_end), 
              ctfs_dbh_start_lt_mindbh=sum(ctfs_dbh_start_lt_mindbh), 
              ctfs_dbh_end_lt0=sum(ctfs_dbh_end_lt0))
ctfs_overall_summary

ctfs_check_summary <- group_by(growth, sitecode, SamplingPeriodEnd) %>%
    summarize(ctfs_pomdiff_gr_pomcut=sum(ctfs_pomdiff_gr_pomcut)/length(diameter_end),
              ctfs_bad_posgrow=sum(ctfs_bad_posgrow)/length(diameter_end), 
              ctfs_bad_neggrow=sum(ctfs_bad_neggrow)/length(diameter_end), 
              ctfs_na_growth=sum(ctfs_na_growth)/length(diameter_end), 
              ctfs_na_dbh_start=sum(ctfs_na_dbh_start)/length(diameter_end), 
              ctfs_na_dbh_end=sum(ctfs_na_dbh_end)/length(diameter_end), 
              ctfs_na_pom_start=sum(ctfs_na_pom_start)/length(diameter_end), 
              ctfs_na_pom_end=sum(ctfs_na_pom_end)/length(diameter_end), 
              ctfs_dbh_start_lt_mindbh=sum(ctfs_dbh_start_lt_mindbh)/length(diameter_end), 
              ctfs_dbh_end_lt0=sum(ctfs_dbh_end_lt0)/length(diameter_end))
ctfs_check_summary <- melt(ctfs_check_summary)
ggplot(ctfs_check_summary) +
    geom_bar(aes(x=SamplingPeriodEnd, y=value, fill=variable), stat="identity", 
             position="dodge") +
    facet_wrap(~sitecode) +
    ylab("Fraction of all measurements") +
    xlab("Sampling period") +
    ggtitle("CTFS Rejections") + 
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("ctfs_growth_trim_summary.png", width=14, height=7.5, dpi=300)

ctfs_check_frac_notaccept <- group_by(growth, sitecode, SamplingPeriodEnd) %>%
    summarize(not_accept=sum(!ctfs_accept)/length(diameter_end))
ggplot(ctfs_check_frac_notaccept) +
    geom_bar(aes(x=SamplingPeriodEnd, y=not_accept), stat="identity", 
             position="dodge") +
    facet_wrap(~sitecode) +
    ggtitle("CTFS Rejections") + 
    xlab("Sampling period") +
    ylab("Fraction of all measurements") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("ctfs_growth_trim_notaccept.png", width=14, height=7.5, dpi=300)

growth$OnehaPlotID <- str_extract(growth$SamplingUnitName, '^[A-Z]*-[A-Z]*-[0-9]*')
growth$OnehaPlotID <- as.numeric(gsub('-', '', str_extract(growth$OnehaPlotID, '[0-9]*$')))
growth$OnehaPlotID <- as.factor(gsub('-', '', str_extract(growth$OnehaPlotID, '[0-9]*$')))
n_obs_per_plot <- filter(growth, ctfs_accept == 1) %>%
    group_by(sitecode, OnehaPlotID, SamplingPeriodEnd) %>%
    summarize(n_obs_per_plot=length(diameter_end))
ggplot(n_obs_per_plot) +
    geom_bar(aes(SamplingPeriodEnd, n_obs_per_plot, fill=OnehaPlotID), colour="black", position="dodge", stat="identity") +
    facet_wrap(~ sitecode) +
    xlab("Sampling period") +
    ylab("Number of observations") +
    ggtitle("Number of observations by one ha plot ID (filtered)") + 
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("growth_n_observations_per_plot.png", width=14, height=7.5, dpi=300)

# What is going on with Korup??
select(filter(growth, sitecode == "KRP"), sitecode, SamplingPeriodEnd, SamplingUnitName)

n_obs <- group_by(growth, sitecode, SamplingPeriodEnd) %>%
    summarize(n_obs=length(diameter_end))
ggplot(n_obs) +
    geom_bar(aes(SamplingPeriodEnd, n_obs), stat="identity") +
    facet_wrap(~ sitecode) +
    xlab("Sampling period") +
    ylab("Number of observations") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("growth_n_observations.png", width=14, height=7.5, dpi=300)
