library(Hmisc) # For smean.cl.boot
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(mgcv)
library(grid) # For facetAdjust function

img_height <- 6.5
img_width <- 6.5
img_dpi <- 300

load("growth_ctfsflagged_merged.RData")

# Only use clean data when developing the models
growth_clean <- filter(growth, ctfs_accept)

#sites <- read.csv('H:/Data/TEAM/Sitecode_Key/sitecode_key.csv')
sites <- read.csv('C:/Users/azvoleff/Desktop/Sitecode_Key/sitecode_key.csv')
growth_clean <- merge(growth_clean, sites)

growth_data_summary <- group_by(growth_clean, sitecode) %>%
    summarise(n_plots=length(unique(OnehaPlotNumber)),
              first_year=min(year(period_end)),
              last_year=max(year(period_end)),
              n_years=length(unique(SamplingPeriodNumber)))
write.csv(growth_data_summary, file="growth_data_summary.csv", row.names=FALSE)

###############################################################################
# Below is for adding x axis labels on facet grid plots. Taken from: 
# http://bit.ly/1o0Wf6Phttp://bit.ly/1o0Wf6P

facetAdjust <- function(x, pos = c("up", "down")) {
    pos <- match.arg(pos)
    p <- ggplot_build(x)
    gtable <- ggplot_gtable(p); dev.off()
    dims <- apply(p$panel$layout[2:3], 2, max)
    nrow <- dims[1]
    ncol <- dims[2]
    panels <- sum(grepl("panel", names(gtable$grobs)))
    space <- ncol * nrow
    n <- space - panels
    if(panels != space){
        idx <- (space - ncol - n + 1):(space - ncol)
        gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
        if(pos == "down"){
            rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                         gtable$layout$name)
            lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
            gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
        }
    }
    class(gtable) <- c("facetAdjust", "gtable", "ggplot"); gtable
}

print.facetAdjust <- function(x, newpage = is.null(vp), vp = NULL) {
    if(newpage)
        grid.newpage()
    if(is.null(vp)){
        grid.draw(x)
    } else {
        if (is.character(vp)) 
            seekViewport(vp)
        else pushViewport(vp)
        grid.draw(x)
        upViewport()
    }
    invisible(x)
}

################################################################################
# Model with continuous diameter variable, gam smoother
# GAM with random effect:
#overall <- gam(growth_ann ~ s(diameter_start, bs="cs") + s(sitecode, bs="re"), 
#data=growth)
# overall <- gam(growth_ann ~ s(diameter_start, bs="cs"), select=TRUE, 
#                method="REML", data=growth)
# overall_preds <- data.frame(Model="Overall",
#                             diameter_start=test_data$diameter_start,
#                             growth_ann=predict(overall, newdata=test_data))
gr_models <- group_by(growth_clean, sitename_short) %>%
    select(growth_ann, diameter_start) %>%
    do(mod = gam(growth_ann ~ s(diameter_start, bs="cs"), select=TRUE, 
                 method="REML", data= .))

make_newdata <- function(sitename_short) {
    min_diameter_start <- min(growth_clean$diameter_start[growth_clean$sitename_short == sitename_short])
    max_diameter_start <- max(growth_clean$diameter_start[growth_clean$sitename_short == sitename_short])
    data.frame(diameter_start=seq(min_diameter_start, max_diameter_start, by=.5))
}

gr_preds <- gr_models %>%
    do(data.frame(Model="Site-specific",
                  sitename_short=.$sitename_short,
                  diameter_start=make_newdata(.$sitename_short),
                  growth_ann=predict(.$mod, newdata=make_newdata(.$sitename_short))))
# Find maximum diameter_start for each site to plot this as well
diameter_start_max <- group_by(growth_clean, sitename_short) %>% summarise(max=max(diameter_start))
pgam <- ggplot(gr_preds) + 
    theme_bw(base_size=10) +
    geom_point(aes(diameter_start, growth_ann), alpha=.05, data=growth_clean) +
    geom_line(aes(diameter_start, growth_ann), color="blue")  +
    facet_wrap(~sitename_short) +
    xlab("DBH (cm)") +
    ylab("Annual growth (cm)") + 
    coord_cartesian(ylim=c(-.5, 2)) +
    theme(legend.position = c(.65, .1))
pgam <- facetAdjust(pgam)
ggsave("growth_trends_continuous_gam.png", pgam, width=img_width, 
       height=img_height, dpi=img_dpi)

pgam_fully <- ggplot(gr_preds) + 
    theme_bw(base_size=10) +
    geom_point(aes(diameter_start, growth_ann), alpha=.05, data=growth_clean) +
    geom_line(aes(diameter_start, growth_ann), color="blue")  +
    facet_wrap(~sitename_short) +
    xlab("DBH (cm)") +
    ylab("Annual growth (cm)") + 
    theme(legend.position = c(.65, .1))
pgam_fully <- facetAdjust(pgam_fully)
ggsave("growth_trends_continuous_gam_full_ylims.png", pgam_fully, 
       width=img_width, height=img_height, dpi=img_dpi)

pgam_gg <- ggplot(growth_clean, aes(diameter_start, growth_ann)) +
    theme_bw(base_size=10) +
    geom_point(alpha=.2) + geom_smooth() +
    facet_wrap(~sitename_short) +
    coord_cartesian(ylim=c(-.5, 2)) +
    xlab("DBH (cm)") +
    ylab("Annual growth (cm)")
pgam_gg <- facetAdjust(pgam_gg)
ggsave("growth_trends_continuous_gam_ggplot.png", pgam_gg, width=img_width, 
       height=img_height, dpi=img_dpi)

###############################################################################
# Perform detrending

growth$growth_ann_pred <- NA
for (sitename_short in unique(growth$sitename_short)) {
    this_model <- gr_models[gr_models$sitename_short == sitename_short, ]$mod[[1]]
    site_rows <- growth$sitename_short == sitename_short
    growth[site_rows, ]$growth_ann_pred <- predict(this_model, newdata=growth[site_rows, ])
}
table(is.na(growth$growth_ann_pred))
growth$growth_ann_index <- growth$growth_ann / growth$growth_ann_pred
table(is.na(growth$growth_ann_index))

growth$growth_ann_index_std <- (growth$growth_ann_index - mean(growth$growth_ann_index)) /  sd(growth$growth_ann_index)
growth$growth_ann_index_cent <- growth$growth_ann_index - mean(growth$growth_ann_index)

save(growth, file="growth_ctfsflagged_merged_detrended.RData")
