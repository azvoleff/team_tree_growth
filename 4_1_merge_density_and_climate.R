library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(lubridate)

load('growth_ctfsflagged.RData')

# Import wood density data from Lydia's work:
WDdata <- read.csv("GlobalWoodDensityData.csv")

WDdata <- WDdata[,2:5]
Genus <- WDdata$Binomial
Genus <- sub(" .*","", Genus)
WDdata <- cbind(WDdata, Genus)

# Calculate genus and family level wood densities
gWD <- summarise(group_by(WDdata, Genus), WD=mean(WD))
fWD <- summarise(group_by(WDdata, Family), WD=mean(WD))

# Extract WD values for genera in TEAM plots
growth$WD <- gWD$WD[match(growth$Genus, gWD$Genus)]
table(is.na(growth$WD))

# Fill in missing genus values with family wood density value
# Families for each genus
growth$WD[is.na(growth$WD)] <- gWD$WD[match(growth$Family[is.na(growth$WD)], fWD$Family)]
table(is.na(growth$WD))

# Check 
#table(growth$OnehaPlotNumber, is.na(growth$WD))
table(growth$Family == "Unknown", is.na(growth$WD))

growth$plot_ID_num <- str_extract(growth$OnehaPlotNumber, "[0-9]*$")
frac_unk <- group_by(growth, sitecode,
                     SamplingPeriodNumber, plot_ID_num) %>%
    summarize(frac_unk_family=sum(Family == "Unknown") / length(Family),
              frac_unk_species=sum(Species == "unknown") / length(Species),
              frac_unk_genus=sum(Genus == "Unknown") / length(Genus))

dim(growth)
# Throw out plots where greater than 20% of stems are "Unknown" family
growth <- filter(growth, frac_unk$frac_unk_family[match(paste0(growth$SamplingPeriodNumber, growth$plot_ID_num),
                                                        paste0(frac_unk$SamplingPeriodNumber, frac_unk$plot_ID_num))] <.2)
dim(growth)

# Make plots of percent unknown species and family
ggplot(filter(frac_unk, !(sitecode %in% c("CSN", "NAK", "YAN")))) +
    geom_bar(aes(x=SamplingPeriodNumber, y=frac_unk_genus,
                 fill=plot_ID_num), stat="identity", 
             position="dodge") +
    facet_wrap(~sitecode) +
    ylab("Fraction observations with unknown genus") +
    xlab("Sampling period")
ggsave("growth_unknown_genus.png", width=14, height=7.5, dpi=300)

ggplot(filter(frac_unk, !(sitecode %in% c("CSN", "NAK", "YAN")))) +
    geom_bar(aes(x=SamplingPeriodNumber, y=frac_unk_species,
                 fill=plot_ID_num), stat="identity", 
             position="dodge") +
    facet_wrap(~sitecode) +
    ylab("Fraction observations with unknown species") +
    xlab("Sampling period")
ggsave("growth_unknown_species.png", width=14, height=7.5, dpi=300)

ggplot(filter(frac_unk, !(sitecode %in% c("CSN", "NAK", "YAN")))) +
    geom_bar(aes(x=SamplingPeriodNumber, y=frac_unk_family,
                 fill=plot_ID_num), stat="identity", 
             position="dodge") +
    facet_wrap(~sitecode) +
    ylab("Fraction observations with unknown family") +
    xlab("Sampling period")
ggsave("growth_unknown_family.png", width=14, height=7.5, dpi=300)

n_species <- group_by(growth, sitecode) %>%
    summarize(n_species=length(unique(Species)),
              n_family=length(unique(Family)),
              n_genus=length(unique(Genus))) %>%
    melt()
n_species$variable <- ordered(n_species$variable,
                              levels=c("n_species", "n_genus", "n_family"))
ggplot(filter(n_species, !(sitecode %in% c("CSN", "NAK", "YAN")))) +
    geom_bar(aes(x=sitecode, y=value,
                 fill=sitecode), stat="identity", 
             position="dodge") +
    facet_grid(variable~.) +
    ylab("n") +
    xlab("Site")
ggsave("growth_n_species_genus_family.png", width=7.5, height=7.5, dpi=300)

# Plot percent negative growth by site and year
frac_neg <- group_by(growth, sitecode, year=year(period_end)) %>%
    summarize(fraction_negative=sum(growth_ann < 0)/length(growth_ann))
frac_neg$year <- factor(frac_neg$year)
ggplot(filter(frac_neg, !(sitecode %in% c("CSN", "NAK", "YAN")))) +
    geom_bar(aes(x=year, y=fraction_negative, fill=sitecode),
             stat="identity") +
    facet_wrap(~sitecode) +
    ylab("Fraction of growth observations that are negative") +
    xlab("Year") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) 
ggsave("growth_frac_negative.png", width=14, height=7.5, dpi=300)


# Fill in remaining NAs wood densities (stems with "Unknown" Family) with mean 
# wood density value for that plot.
table(is.na(growth$WD))
mean_wd <- summarise(group_by(growth, SamplingPeriodNumber, OnehaPlotNumber), 
                              WD=mean(WD, na.rm=TRUE))
mean_wd_rows <- match(paste0(growth$SamplingPeriodNumber, growth$OnehaPlotNumber),
                             paste0(mean_wd$SamplingPeriodNumber, mean_wd$OnehaPlotNumber))
growth$WD[is.na(growth$WD)] <- mean_wd$WD[mean_wd_rows][is.na(growth$WD)]
table(is.na(growth$WD))

growth$dbh_class <- cut(growth$diameter_start, c(10, 20, 40, 500), include.lowest=TRUE)
growth$WD_class <- cut(growth$WD, c(0, .4, .6, .8, 2))

###############################################################################
# Merge climate data
load('../CHIRPS/vg_plot_spis.RData')
spis <- tbl_df(spis)

spis$plot_ID <- as.character(spis$plot_ID)
growth$plot_ID <- gsub("-", "", growth$OnehaPlotNumber)
table(spis$plot_ID)
table(growth$plot_ID[!(growth$plot_ID %in% spis$plot_ID)])
# Note that some PSH plot IDs are missing - check this with Jimmy. The spatial 
# data has the PSH plot IDs ranging from 4-9.

# Calculate closest SPI end point to growth period end point
growth$period_end_month <- round_date(growth$period_end, "month")
spis$period_end_month <- spis$date

spi_6 <- filter(spis, spi_period == 6) %>% select(plot_ID, period_end_month, spi)
names(spi_6)[names(spi_6) == "spi"] <- "spi_6"
spi_12 <- filter(spis, spi_period == 12) %>% select(plot_ID, period_end_month, spi)
names(spi_12)[names(spi_12) == "spi"] <- "spi_12"
spi_24 <- filter(spis, spi_period == 24) %>% select(plot_ID, period_end_month, spi)
names(spi_24)[names(spi_24) == "spi"] <- "spi_24"
growth <- merge(growth, spi_6)
growth <- merge(growth, spi_12)
growth <- merge(growth, spi_24)

save(growth, file="growth_ctfsflagged_merged.RData")
