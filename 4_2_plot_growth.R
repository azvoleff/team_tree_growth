library(dplyr)
library(ggplot2)

load('growth_ctfsflagged.RData')

frac_zero_growth_ann <- filter(growth, ctfs_accept == 1) %>%
    group_by(sitecode, SamplingPeriodEnd) %>%
    summarize(frac_zero_growth_ann=sum(growth_ann == 0) / length(growth_ann))
ggplot(frac_zero_growth_ann) +
    geom_bar(aes(SamplingPeriodEnd, frac_zero_growth_ann), stat="identity") +
    facet_wrap(~ sitecode) +
    xlab("Sampling period") +
    ylab("Fraction of observations") +
    ggtitle("Fraction of trees with zero growth (filtered)") + 
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("growth_filtered_n_zero_growth_ann.png", width=14, height=7.5, dpi=300)

growth_ann_mean <- filter(growth, ctfs_accept == 1) %>%
    group_by(sitecode, SamplingPeriodEnd) %>%
    summarize(growth_ann_mean=mean(growth_ann, na.rm=TRUE))
ggplot(growth_ann_mean) +
    geom_bar(aes(SamplingPeriodEnd, growth_ann_mean), stat="identity") +
    facet_wrap(~ sitecode) +
    xlab("Sampling period") +
    ylab("Mean annual growth (cm)") +
    ggtitle("Mean annual growth (filtered)") + 
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("growth_filtered_ann_mean.png", width=14, height=7.5, dpi=300)

ggplot(filter(growth, ctfs_accept == 1)) +
    geom_boxplot(aes(SamplingPeriodEnd, growth_ann)) +
    facet_wrap(~ sitecode) +
    xlab("Sampling period") +
    ylab("Annual growth (cm)") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("growth_annual.png", width=14, height=7.5, dpi=300)

ggplot(filter(growth, ctfs_accept == 1)) +
    geom_point(aes(diameter_start, growth_ann)) +
    facet_wrap(~ sitecode) +
    xlab("DBH") +
    ylab("Annual growth (cm)") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("growth_vs_dbh.png", width=14, height=7.5, dpi=300)

ggplot(filter(growth, ctfs_accept == 1, SamplingPeriodNumber == 2)) +
    geom_histogram(aes(diameter_start)) +
    facet_wrap(~ sitecode, scales="free_y") +
    xlab("DBH") +
    ylab("Frequency") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("dbh_histogram.png", width=14, height=7.5, dpi=300)

growth$dbh_class <- cut(growth$diameter_start, seq(10, 150, by=2), 
                        include.lowest=TRUE)
growth_by_dbh <- filter(growth, ctfs_accept == 1) %>%
    group_by(sitecode, dbh_class) %>%
    summarize(growth_ann_mean=mean(growth_ann))
growth_by_dbh$dbh_class_min <- seq(10, 150, by=2)[as.numeric(growth_by_dbh$dbh_class)]
growth_by_dbh$dbh_class_max <- seq(10, 150, by=2)[as.numeric(growth_by_dbh$dbh_class) + 1]
growth_by_dbh$dbh_class_midpoint <- rowMeans(cbind(growth_by_dbh$dbh_class_min, growth_by_dbh$dbh_class_max))
ggplot(filter(growth_by_dbh, dbh_class_max < 100), aes(dbh_class_midpoint, growth_ann_mean)) +
    geom_line() + geom_smooth() +
    facet_wrap(~ sitecode) +
    xlab("DBH class") +
    ylab("Mean annual growth (cm)") +
    coord_cartesian(ylim=c(0, 1.5))
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("growth_by_dbh.png", width=14, height=7.5, dpi=300)
