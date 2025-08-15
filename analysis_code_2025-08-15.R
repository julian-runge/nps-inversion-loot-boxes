# add packages to the list as needed
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest", "haven", "xtable", "effects", "stargazer" #, "ggside", "tidyverse", "tidyquant"
)

# install packages in list
lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# set wd
setwd("")

# read in xsection
xsection <- read.csv("nps_lootboxes_xsection.csv", header = TRUE)

######################################################################################################
###################### How do NPS ratings associate with consumption behavior? #######################
######################################################################################################

# create cross-tab plot for both survey responses (figure for Appendix B)

# factorize and retain NA as a visible level for plotting
xsection$nps_rating1_f <- factor(xsection$nps_rating1, exclude = NULL)
xsection$nps_rating2_f <- factor(xsection$nps_rating2, exclude = NULL)

# create full count table (including NAs)
tab_counts <- table(xsection$nps_rating1_f, xsection$nps_rating2_f, useNA = "ifany")
df_counts <- as.data.frame(tab_counts)
colnames(df_counts) <- c("nps_rating1", "nps_rating2", "count")

# create a table excluding rows/cols where either dimension is NA
non_na_data <- na.omit(xsection[, c("nps_rating1", "nps_rating2")])
tab_props <- prop.table(table(non_na_data$nps_rating1, non_na_data$nps_rating2))
df_props <- as.data.frame(tab_props)
colnames(df_props) <- c("nps_rating1", "nps_rating2", "proportion")

# convert to factor with same levels as df_counts for merging
df_props$nps_rating1 <- factor(df_props$nps_rating1, levels = levels(xsection$nps_rating1_f))
df_props$nps_rating2 <- factor(df_props$nps_rating2, levels = levels(xsection$nps_rating2_f))

# merge full counts with non-NA proportions
df_combined <- merge(df_counts, df_props, by = c("nps_rating1", "nps_rating2"), all.x = TRUE)

# create plot
p <- ggplot(df_combined, aes(x = nps_rating2, y = nps_rating1)) +
  geom_tile(aes(fill = proportion), color = "white") +
  geom_text(aes(label = count), size = 3, color = "black") +
  scale_fill_gradient(
    low = "white", high = "steelblue", 
    labels = scales::percent_format(),
    na.value = "grey90"  # color for NA proportions
  ) +
  theme_minimal() +
  labs(
    #title = "Cross-tabulation of NPS Ratings (Proportions exclude missing)",
    x = "NPS rating second survey",
    y = "NPS rating first survey",
    fill = "Share of sample"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# save plot
ggsave("nps_cross_tab_excl_na_prop.png", plot = p, width = 8, height = 6, dpi = 300, bg = "white")


# generate table 1 and 2
table1 <- xsection %>%
  dplyr::group_by(nps1) %>%
  dplyr::summarize(
    segment_size = n(),
  
    t1_lootboxspend_week_before1_mean = mean(lootboxspend_week_before1, na.rm=TRUE),
    t1_lootboxspend_during1_mean = mean(lootboxspend_during1, na.rm=TRUE),
    t1_lootboxspend_week_after1_mean = mean(lootboxspend_week_after1, na.rm=TRUE),
    
    t1_playtime_week_before1_mean = mean(playtime_week_before1, na.rm=TRUE),
    t1_playtime_during1_mean = mean(playtime_during1, na.rm=TRUE),
    t1_playtime_week_after1_mean = mean(playtime_week_after1, na.rm=TRUE),
    
    t1_revenue_week_before1_mean = mean(revenue_week_before1, na.rm=TRUE),
    t1_revenue_during1_mean = mean(revenue_during1, na.rm=TRUE),
    t1_revenue_week_after1_mean = mean(revenue_week_after1, na.rm=TRUE),
    )

table2 <- xsection %>%
  dplyr::group_by(nps2) %>%
  dplyr::summarize(
    segment_size = n(),
    
    t2_lootboxspend_week_before2_mean = mean(lootboxspend_week_before2, na.rm=TRUE),
    t2_lootboxspend_during2_mean = mean(lootboxspend_during2, na.rm=TRUE),
    t2_lootboxspend_week_after2_mean = mean(lootboxspend_week_after2, na.rm=TRUE),
  
    t2_playtime_week_before2_mean = mean(playtime_week_before2, na.rm=TRUE),
    t2_playtime_during2_mean = mean(playtime_during2, na.rm=TRUE),
    t2_playtime_week_after2_mean = mean(playtime_week_after2, na.rm=TRUE),
    
    t2_revenue_week_before2_mean = mean(revenue_week_before2, na.rm=TRUE),
    t2_revenue_during2_mean = mean(revenue_during2, na.rm=TRUE),
    t2_revenue_week_after2_mean = mean(revenue_week_after2, na.rm=TRUE)
  )

table1$nps1 <- factor(table1$nps1, levels = c("Promoter", "Passive", "Detractor", "Severe detr.", "NaN"))

table1_sorted <- table1 %>%
  arrange(nps1) 

table2$nps2 <- factor(table2$nps2, levels = c("Promoter", "Passive", "Detractor", "Severe detr.", "NaN"))

table2_sorted <- table2 %>%
  arrange(nps2) 

colnames(table1_sorted) <- c("NPS bucket", "Users", "Week bef.", "During", "Week aft.", "Week bef.", "During", "Week aft.", "Week bef.", "During", "Week aft.")
colnames(table2_sorted) <- c("NPS bucket", "Users", "Week bef.", "During", "Week aft.", "Week bef.", "During", "Week aft.", "Week bef.", "During", "Week aft.")

# define multicolumn row
header_row <- paste0(
  "\\multicolumn{2}{c}{ } & ",
  "\\multicolumn{3}{c}{Loot box spend} & ",
  "\\multicolumn{3}{c}{Playtime} & ",
  "\\multicolumn{3}{c}{Spend in $} \\\\ \\hline\n"
)

# print tables with manual header insertion
print(
  xtable(table1_sorted),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  add.to.row = list(
    pos = list(-1),
    command = header_row
  )
)

print(
  xtable(table2_sorted),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  add.to.row = list(
    pos = list(-1),
    command = header_row
  )
)

# generate standard errors tables
table1_se <- xsection %>%
  dplyr::group_by(nps1) %>%
  dplyr::summarize(
    segment_size = n(),
    
    t1_lootboxspend_week_before1_se = sd(lootboxspend_week_before1, na.rm = TRUE) / sqrt(sum(!is.na(lootboxspend_week_before1))),
    t1_lootboxspend_during1_se = sd(lootboxspend_during1, na.rm = TRUE) / sqrt(sum(!is.na(lootboxspend_during1))),
    t1_lootboxspend_week_after1_se = sd(lootboxspend_week_after1, na.rm = TRUE) / sqrt(sum(!is.na(lootboxspend_week_after1))),
    
    t1_playtime_week_before1_se = sd(playtime_week_before1, na.rm = TRUE) / sqrt(sum(!is.na(playtime_week_before1))),
    t1_playtime_during1_se = sd(playtime_during1, na.rm = TRUE) / sqrt(sum(!is.na(playtime_during1))),
    t1_playtime_week_after1_se = sd(playtime_week_after1, na.rm = TRUE) / sqrt(sum(!is.na(playtime_week_after1))),
    
    t1_revenue_week_before1_se = sd(revenue_week_before1, na.rm = TRUE) / sqrt(sum(!is.na(revenue_week_before1))),
    t1_revenue_during1_se = sd(revenue_during1, na.rm = TRUE) / sqrt(sum(!is.na(revenue_during1))),
    t1_revenue_week_after1_se = sd(revenue_week_after1, na.rm = TRUE) / sqrt(sum(!is.na(revenue_week_after1)))
  )

table2_se <- xsection %>%
  dplyr::group_by(nps2) %>%
  dplyr::summarize(
    segment_size = n(),
    
    t2_lootboxspend_week_before1_se = sd(lootboxspend_week_before1, na.rm = TRUE) / sqrt(sum(!is.na(lootboxspend_week_before1))),
    t2_lootboxspend_during1_se = sd(lootboxspend_during1, na.rm = TRUE) / sqrt(sum(!is.na(lootboxspend_during1))),
    t2_lootboxspend_week_after1_se = sd(lootboxspend_week_after1, na.rm = TRUE) / sqrt(sum(!is.na(lootboxspend_week_after1))),
    
    t2_playtime_week_before1_se = sd(playtime_week_before1, na.rm = TRUE) / sqrt(sum(!is.na(playtime_week_before1))),
    t2_playtime_during1_se = sd(playtime_during1, na.rm = TRUE) / sqrt(sum(!is.na(playtime_during1))),
    t2_playtime_week_after1_se = sd(playtime_week_after1, na.rm = TRUE) / sqrt(sum(!is.na(playtime_week_after1))),
    
    t2_revenue_week_before1_se = sd(revenue_week_before1, na.rm = TRUE) / sqrt(sum(!is.na(revenue_week_before1))),
    t2_revenue_during1_se = sd(revenue_during1, na.rm = TRUE) / sqrt(sum(!is.na(revenue_during1))),
    t2_revenue_week_after1_se = sd(revenue_week_after1, na.rm = TRUE) / sqrt(sum(!is.na(revenue_week_after1)))
  )

table1_se$nps1 <- factor(table1_se$nps1, levels = c("Promoter", "Passive", "Detractor", "Severe detr.", "NaN"))

table1_se_sorted <- table1_se %>%
  arrange(nps1) 

table2_se$nps2 <- factor(table2_se$nps2, levels = c("Promoter", "Passive", "Detractor", "Severe detr.", "NaN"))

table2_se_sorted <- table2_se %>%
  arrange(nps2) 

colnames(table1_se_sorted) <- c("NPS bucket", "Users", "Week bef.", "During", "Week aft.", "Week bef.", "During", "Week aft.", "Week bef.", "During", "Week aft.")
colnames(table2_se_sorted) <- c("NPS bucket", "Users", "Week bef.", "During", "Week aft.", "Week bef.", "During", "Week aft.", "Week bef.", "During", "Week aft.")

# define multicolumn row
header_row <- paste0(
  "\\multicolumn{2}{c}{ } & ",
  "\\multicolumn{3}{c}{Loot box spend} & ",
  "\\multicolumn{3}{c}{Playtime} & ",
  "\\multicolumn{3}{c}{Spend in $} \\\\ \\hline\n"
)

# print tables with manual header insertion
print(
  xtable(table1_se_sorted),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  add.to.row = list(
    pos = list(-1),
    command = header_row
  )
)

print(
  xtable(table2_se_sorted),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  add.to.row = list(
    pos = list(-1),
    command = header_row
  )
)

# plot distributions of log-transformed playtime, spend on loot boxes, and revenue after survey by nps rating

# create data for plotting
plot_df <- bind_rows(
  data.frame(
    nps = xsection$nps1,
    value = log1p(xsection$lootboxspend_week_after1),
    source = "First NPS survey",
    metric = "Spend on loot boxes (logged)"
  ),
  data.frame(
    nps = xsection$nps2,
    value = log1p(xsection$lootboxspend_week_after2),
    source = "Second NPS survey",
    metric = "Spend on loot boxes (logged)"
  ),
  data.frame(
    nps = xsection$nps1,
    value = log1p(xsection$playtime_week_after1),
    source = "First NPS survey",
    metric = "Playtime (logged)"
  ),
  data.frame(
    nps = xsection$nps2,
    value = log1p(xsection$playtime_week_after2),
    source = "Second NPS survey",
    metric = "Playtime (logged)"
  ),
  data.frame(
    nps = xsection$nps1,
    value = log1p(xsection$revenue_week_after1),
    source = "First NPS survey",
    metric = "Revenue (logged)"
  ),
  data.frame(
    nps = xsection$nps2,
    value = log1p(xsection$revenue_week_after2),
    source = "Second NPS survey",
    metric = "Revenue (logged)"
  )
)

# ensure metrics appear in the desired facet row order
plot_df$metric <- factor(plot_df$metric, levels = c(
  "Playtime (logged)",
  "Spend on loot boxes (logged)",
  "Revenue (logged)"
))

# create the plot
consumption_by_nps <- ggplot(plot_df, aes(x = value, color = nps)) +
  geom_density(size = 0.5) +
  facet_grid(metric ~ source) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0, 13)) +
  theme_minimal() +
  labs(
    #title = "Post-survey game consumption by NPS rating category",
    x = "Consumption in the week after the survey (playtime, spend, revenue)",
    y = "Density",
    color = "NPS rating category"
  ) +
  theme(legend.position = "bottom")

# save the plot
ggsave("consumption_by_nps.png",
       plot = consumption_by_nps,
       width = 7,
       height = 8,
       dpi = 300,
       bg = "white")


######################################################################################################
##### Are negative evaluations followed by successful regulation of consumption to lower levels? #####
######################################################################################################

# some plotting and exploration of distributions
# loot box spend
df_plot1 <- rbind(
  data.frame(metric = "Loot Box Spend Week Before First", value = log1p(xsection$lootboxspend_week_before1)),
  data.frame(metric = "Loot Box Spend Week Before Second", value = log1p(xsection$lootboxspend_week_before2)),
  data.frame(metric = "Loot Box Spend Week During First", value = log1p(xsection$lootboxspend_during1)),
  data.frame(metric = "Loot Box Spend Week During Second", value = log1p(xsection$lootboxspend_during2)),
  data.frame(metric = "Loot Box Spend Week After First", value = log1p(xsection$lootboxspend_week_after1)),
  data.frame(metric = "Loot Box Spend Week After Second", value = log1p(xsection$lootboxspend_week_after2))
)

# remove NA values (not needed as there are none but to be sure)
#df_plot1 <- df_plot1 %>% filter(!is.na(value))

# plot density curves
ggplot(df_plot1, aes(x = value, color = metric)) +
  geom_density(size = 0.5) +
  theme_minimal() +
  labs(
    title = "Density of Logged Regression Vars",
    x = "Value",
    y = "Density",
    color = "Metric"
  )

## some histograms for the outcome distributions
# for loot box spend
xsection$diff_lootboxspend1 <- xsection$lootboxspend_week_after1 - xsection$lootboxspend_week_before1
xsection$diff_lootboxspend2 <- xsection$lootboxspend_week_after2 - xsection$lootboxspend_week_before2

hist(xsection$diff_lootboxspend1, 10000, xlim = c(-5000,5000))
hist(xsection$diff_lootboxspend2, 10000, xlim = c(-5000,5000))

hist(filter(xsection,xsection$lootboxspend_week_before1>0)$diff_lootboxspend1, 10000, xlim = c(-8000,8000))
hist(filter(xsection,xsection$lootboxspend_week_before2>0)$diff_lootboxspend2, 10000, xlim = c(-8000,8000))

hist(filter(xsection,xsection$lootboxspend_week_after1>0)$diff_lootboxspend1, 10000, xlim = c(-8000,8000))
hist(filter(xsection,xsection$lootboxspend_week_after2>0)$diff_lootboxspend2, 10000, xlim = c(-8000,8000))

hist(filter(xsection,xsection$lootboxspend_week_before1>0)$lootboxspend_week_before1, 10000, xlim = c(-1,10000))
hist(filter(xsection,xsection$lootboxspend_week_before2>0)$lootboxspend_week_before2, 10000, xlim = c(-1,10000))

hist(filter(xsection,xsection$lootboxspend_week_before1>0)$lootboxspend_week_after1, 10000, xlim = c(-1,10000))
hist(filter(xsection,xsection$lootboxspend_week_before2>0)$lootboxspend_week_after2, 10000, xlim = c(-1,10000))

hist(log(filter(xsection,xsection$lootboxspend_week_before1>0)$lootboxspend_week_before1+1), 10000, xlim = c(3,10))
hist(log(filter(xsection,xsection$lootboxspend_week_before2>0)$lootboxspend_week_before2+1), 10000, xlim = c(3,10))

hist(log(filter(xsection,xsection$lootboxspend_week_before1>0)$lootboxspend_week_after1+1), 10000, xlim = c(3,10))
hist(log(filter(xsection,xsection$lootboxspend_week_before2>0)$lootboxspend_week_after2+1), 10000, xlim = c(3,10))

# for playtime
xsection$diff_playtime1 <- xsection$playtime_week_after1 - xsection$playtime_week_before1
xsection$diff_playtime2 <- xsection$playtime_week_after2 - xsection$playtime_week_before2

hist(xsection$diff_playtime1, 10000, xlim = c(-4000,4000))
hist(xsection$diff_playtime2, 10000, xlim = c(-4000,4000))

hist(filter(xsection,xsection$playtime_week_before1>0)$diff_playtime1, 10000, xlim = c(-20000,20000))
hist(filter(xsection,xsection$playtime_week_before2>0)$diff_playtime2, 10000, xlim = c(-20000,20000))

hist(filter(xsection,xsection$playtime_week_before1>0)$playtime_week_before1, 10000, xlim = c(-1,30000))
hist(filter(xsection,xsection$playtime_week_before2>0)$playtime_week_before2, 10000, xlim = c(-1,30000))

hist(filter(xsection,xsection$playtime_week_before1>0)$playtime_week_after1, 10000, xlim = c(-1,30000))
hist(filter(xsection,xsection$playtime_week_before2>0)$playtime_week_after2, 10000, xlim = c(-1,30000))

hist(log(filter(xsection,xsection$playtime_week_before1>0)$playtime_week_before1+1), 1000, xlim = c(3,13))
hist(log(filter(xsection,xsection$playtime_week_before2>0)$playtime_week_before2+1), 1000, xlim = c(3,13))

hist(log(filter(xsection,xsection$playtime_week_before1>0)$playtime_week_after1+1), 1000, xlim = c(0,15))
hist(log(filter(xsection,xsection$playtime_week_before2>0)$playtime_week_after2+1), 1000, xlim = c(0,15))

# some histograms for revenue
xsection$diff_revenue1 <- xsection$revenue_week_after1 - xsection$revenue_week_before1
xsection$diff_revenue2 <- xsection$revenue_week_after2 - xsection$revenue_week_before2

hist(xsection$diff_revenue1, 1000, xlim = c(-100,100))
hist(xsection$diff_revenue2, 1000, xlim = c(-100,100))

hist(filter(xsection,xsection$revenue_week_before1>0)$diff_revenue1, 1000, xlim = c(-100,100))
hist(filter(xsection,xsection$revenue_week_before2>0)$diff_revenue2, 1000, xlim = c(-100,100))

hist(filter(xsection,xsection$revenue_week_before1>0)$revenue_week_before1, 10000, xlim = c(-4000,4000))
hist(filter(xsection,xsection$revenue_week_before2>0)$revenue_week_before2, 10000, xlim = c(-4000,4000))

hist(filter(xsection,xsection$revenue_week_before1>0)$revenue_week_after1, 10000, xlim = c(-4000,4000))
hist(filter(xsection,xsection$revenue_week_before2>0)$revenue_week_after2, 10000, xlim = c(-4000,4000))

# code variables appropriately
xsection$nps1 <- factor(xsection$nps1,
                        levels = c("Promoter", "Passive", "Detractor", "Severe detr.", "NaN"),
                        ordered = FALSE
)
xsection$nps1 <- relevel(xsection$nps1, ref = "Promoter")

xsection$nps2 <- factor(xsection$nps2,
                        levels = c("Promoter", "Passive", "Detractor", "Severe detr.", "NaN"),
                        ordered = FALSE
)
xsection$nps2 <- relevel(xsection$nps2, ref = "Promoter")

# histogram of outcome
hist(xsection$lootboxspend_week_after1, 100)
hist(xsection$lootboxspend_week_after2, 100)

# share of zero outcomes
mean(xsection$lootboxspend_week_after1 == 0)
mean(xsection$lootboxspend_week_after2 == 0)

# histogram of log-outcomes for positive values — visualizes distribution of second part of the model
hist(log(xsection[xsection$lootboxspend_week_after1 > 0, "lootboxspend_week_after1"]), 100)
hist(log(xsection[xsection$lootboxspend_week_after2 > 0, "lootboxspend_week_after2"]), 100)

# rescale loot box spend in the week before for better interpretability
xsection$lootboxspend_week_before1_1000 <- xsection$lootboxspend_week_before1/1000
xsection$lootboxspend_week_before2_1000 <- xsection$lootboxspend_week_before2/1000

# estimate two-part models
tp_1 <- tpm(lootboxspend_week_after1 ~ nps1 * lootboxspend_week_before1_1000,
            data = xsection,
            link_part1 = "logit",
            family_part2 = gaussian(link = "log"))

tp_2 <- tpm(lootboxspend_week_after2 ~ nps2 * lootboxspend_week_before2_1000,
            data = xsection,
            link_part1 = "logit",
            family_part2 = gaussian(link = "log"))

summary(tp_1)
summary(tp_2)
AME(tp_1)
AME(tp_2)
margin(tp_1)
margin(tp_2)

# extract sub-models
fp1 <- tp_1@model_part1
sp1 <- tp_1@model_part2
fp2 <- tp_2@model_part1
sp2 <- tp_2@model_part2

# rename coefficients to match across models
names(fp2$coefficients) <- gsub("nps2", "nps1", names(fp2$coefficients))
names(sp2$coefficients) <- gsub("nps2", "nps1", names(sp2$coefficients))
names(fp2$coefficients) <- gsub("lootboxspend_week_before2", "lootboxspend_week_before1", names(fp2$coefficients))
names(sp2$coefficients) <- gsub("lootboxspend_week_before2", "lootboxspend_week_before1", names(sp2$coefficients))

# export via stargazer
stargazer(fp1, sp1, fp2, sp2,
          type = "latex",
          dep.var.labels.include = FALSE,
          column.labels = c("Surv. 1: Logit", "Surv. 1: Log-Gaussian", "Surv. 2: Logit", "Surv. 2: Log-Gaussian"),
          covariate.labels = c(
            "Passive",
            "Detractor",
            "Severe detractor",
            "Non-responder",
            "Prior spend",
            "Passive × prior spend",
            "Detractor × prior spend",
            "Severe detr. × pr. spend",
            "Non-resp. × prior spend",
            "Intercept"
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE)


# estimate models with playtime instead of loot box spend (for appendix A)

# rescale playtime in the week before for better interpretability
xsection$playtime_week_before1_1000 <- xsection$playtime_week_before1/1000
xsection$playtime_week_before2_1000 <- xsection$playtime_week_before2/1000

tp_1_playtime <- tpm(playtime_week_after1 ~ nps1 * playtime_week_before1_1000,
            data = xsection,
            link_part1 = "logit",
            family_part2 = gaussian(link = "log"))

tp_2_playtime <- tpm(playtime_week_after2 ~ nps2 * playtime_week_before2_1000,
            data = xsection,
            link_part1 = "logit",
            family_part2 = gaussian(link = "log"))

summary(tp_1_playtime)
summary(tp_2_playtime)
AME(tp_1_playtime)
AME(tp_2_playtime)
margin(tp_1_playtime)
margin(tp_2_playtime)

# extract sub-models
fp1_playtime <- tp_1_playtime@model_part1
sp1_playtime <- tp_1_playtime@model_part2
fp2_playtime <- tp_2_playtime@model_part1
sp2_playtime <- tp_2_playtime@model_part2

# rename coefficients to match across models
names(fp2_playtime$coefficients) <- gsub("nps2", "nps1", names(fp2$coefficients))
names(sp2_playtime$coefficients) <- gsub("nps2", "nps1", names(sp2$coefficients))
names(fp2_playtime$coefficients) <- gsub("playtime_week_before2", "playtime_week_before1", names(fp2$coefficients))
names(sp2_playtime$coefficients) <- gsub("playtime_week_before2", "playtime_week_before1", names(sp2$coefficients))

# export via stargazer
stargazer(fp1_playtime, sp1_playtime, fp2_playtime, sp2_playtime,
          type = "latex",
          dep.var.labels.include = FALSE,
          column.labels = c("Surv. 1: Logit", "Surv. 1: Log-Gaussian", "Surv. 2: Logit", "Surv. 2: Log-Gaussian"),
          covariate.labels = c(
            "Passive",
            "Detractor",
            "Severe detractor",
            "Non-responder",
            "Prior playtime",
            "Passive × prior playtime",
            "Detractor × prior playtime",
            "Severe detr. × pr. playtime",
            "Non-resp. × prior playtime",
            "Intercept"
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE)


######################################################################################################
##### How prevalent is NPS inversion? #####
######################################################################################################

#### detractors with nonzero use (either loot box spend or playtime)
### loot box spend
## first survey

# sample size
sum(xsection$lootboxspend_week_before1>0)

# prevalence
sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=6
    & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
    & xsection$lootboxspend_week_before1>0, na.rm=TRUE) /
  sum(xsection$lootboxspend_week_before1>0)

# avg. money spent in week after by users in this segment in $
mean(xsection$revenue_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                  & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                  & xsection$lootboxspend_week_before1>0], na.rm=TRUE) 

# avg. playtime in week after by users in this segment in minutes
mean(xsection$playtime_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                  & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                  & xsection$lootboxspend_week_before1>0], na.rm=TRUE) / 60

## second survey

# sample size
sum(xsection$lootboxspend_week_before2>0)

# prevalence
sum(xsection$nps_rating2>=0 & xsection$nps_rating2<=6
    & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
    & xsection$lootboxspend_week_before2>0, na.rm=TRUE) /
  sum(xsection$lootboxspend_week_before2>0)


# avg. money spent in week after by users in this segment in $
mean(xsection$revenue_week_after2[xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                  & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                  & xsection$lootboxspend_week_before2>0], na.rm=TRUE) 

# avg. playtime in week after by users in this segment in minutes
mean(xsection$playtime_week_after2[xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                   & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                   & xsection$lootboxspend_week_before2>0], na.rm=TRUE) / 60

## both surveys jointly

# sample size
sum(xsection$lootboxspend_week_before1>0
    & xsection$lootboxspend_week_before2>0)

# prevalence
sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=6
    & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
    & xsection$lootboxspend_week_before1>0
    & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
    & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
    & xsection$lootboxspend_week_before2>0, na.rm=TRUE) /
  sum(xsection$lootboxspend_week_before1>0
      & xsection$lootboxspend_week_before2>0)

# avg. money spent in week after by users in this segment in $
(sum(xsection$revenue_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                  & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                  & xsection$lootboxspend_week_before1>0
                                  & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                  & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                  & xsection$lootboxspend_week_before2>0], na.rm=TRUE) +
    sum(xsection$revenue_week_after2[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                     & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                     & xsection$lootboxspend_week_before1>0
                                     & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                     & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                     & xsection$lootboxspend_week_before2>0], na.rm=TRUE))/2 /
  sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=6
      & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
      & xsection$lootboxspend_week_before1>0
      & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
      & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
      & xsection$lootboxspend_week_before2>0, na.rm=TRUE)

# avg. playtime in week after by users in this segment in minutes
(sum(xsection$playtime_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                   & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                   & xsection$lootboxspend_week_before1>0
                                   & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                   & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                   & xsection$lootboxspend_week_before2>0], na.rm=TRUE) +
    sum(xsection$playtime_week_after2[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                      & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                      & xsection$lootboxspend_week_before1>0
                                      & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                      & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                      & xsection$lootboxspend_week_before2>0], na.rm=TRUE))/2 /
  sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=6
      & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
      & xsection$lootboxspend_week_before1>0
      & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
      & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
      & xsection$lootboxspend_week_before2>0, na.rm=TRUE) / 60

### playtime

## first survey

# sample size
sum(xsection$playtime_week_before1>0)

# prevalence
sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=6
    & xsection$playtime_week_after1>=xsection$playtime_week_before1
    & xsection$playtime_week_before1>0, na.rm=TRUE) /
  sum(xsection$playtime_week_before1>0)


# avg. money spent in week after by users in this segment in $
mean(xsection$revenue_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                  & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                  & xsection$playtime_week_before1>0], na.rm=TRUE) 

# avg. playtime in week after by users in this segment in minutes
mean(xsection$playtime_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                   & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                   & xsection$playtime_week_before1>0], na.rm=TRUE) / 60

## second survey

# sample size
sum(xsection$playtime_week_before2>0)

# prevalence
sum(xsection$nps_rating2>=0 & xsection$nps_rating2<=6
    & xsection$playtime_week_after2>=xsection$playtime_week_before2
    & xsection$playtime_week_before2>0, na.rm=TRUE) /
  sum(xsection$playtime_week_before2>0)

# avg. money spent in week after by users in this segment in $
mean(xsection$revenue_week_after2[xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                  & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                  & xsection$playtime_week_before2>0], na.rm=TRUE) 

# avg. playtime in week after by users in this segment in minutes
mean(xsection$playtime_week_after2[xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                   & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                   & xsection$playtime_week_before2>0], na.rm=TRUE) / 60

## both surveys jointly

# sample size
sum(xsection$playtime_week_before1>0
    & xsection$playtime_week_before2>0)

# prevalence
sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=6
    & xsection$playtime_week_after1>=xsection$playtime_week_before1
    & xsection$playtime_week_before1>0
    & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
    & xsection$playtime_week_after2>=xsection$playtime_week_before2
    & xsection$playtime_week_before2>0, na.rm=TRUE) /
  sum(xsection$playtime_week_before1>0
      & xsection$playtime_week_before2>0)

# avg. money spent in week after by users in this segment in $
(sum(xsection$revenue_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                  & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                  & xsection$playtime_week_before1>0
                                  & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                  & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                  & xsection$playtime_week_before2>0], na.rm=TRUE) +
    sum(xsection$revenue_week_after2[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                     & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                     & xsection$playtime_week_before1>0
                                     & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                     & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                     & xsection$playtime_week_before2>0], na.rm=TRUE))/2 /
  sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=6
      & xsection$playtime_week_after1>=xsection$playtime_week_before1
      & xsection$playtime_week_before1>0
      & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
      & xsection$playtime_week_after2>=xsection$playtime_week_before2
      & xsection$playtime_week_before2>0, na.rm=TRUE)

# avg. playtime in week after by users in this segment in minutes
(sum(xsection$playtime_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                   & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                   & xsection$playtime_week_before1>0
                                   & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                   & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                   & xsection$playtime_week_before2>0], na.rm=TRUE) +
    sum(xsection$playtime_week_after2[xsection$nps_rating1>=0 & xsection$nps_rating1<=6
                                      & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                      & xsection$playtime_week_before1>0
                                      & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
                                      & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                      & xsection$playtime_week_before2>0], na.rm=TRUE))/2 /
  sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=6
      & xsection$playtime_week_after1>=xsection$playtime_week_before1
      & xsection$playtime_week_before1>0
      & xsection$nps_rating2>=0 & xsection$nps_rating2<=6
      & xsection$playtime_week_after2>=xsection$playtime_week_before2
      & xsection$playtime_week_before2>0, na.rm=TRUE) / 60


#### severe detractors with nonzero use (either loot box spend or playtime)
### loot box spend
## first survey

# sample size
sum(xsection$lootboxspend_week_before1>0)

# prevalence
sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=2
    & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
    & xsection$lootboxspend_week_before1>0, na.rm=TRUE) /
  sum(xsection$lootboxspend_week_before1>0)

# avg. money spent in week after by users in this segment in $
mean(xsection$revenue_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                  & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                  & xsection$lootboxspend_week_before1>0], na.rm=TRUE) 

# avg. playtime in week after by users in this segment in minutes
mean(xsection$playtime_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                   & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                   & xsection$lootboxspend_week_before1>0], na.rm=TRUE) / 60

## second survey

# sample size
sum(xsection$lootboxspend_week_before2>0)

# prevalence
sum(xsection$nps_rating2>=0 & xsection$nps_rating2<=2
    & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
    & xsection$lootboxspend_week_before2>0, na.rm=TRUE) /
  sum(xsection$lootboxspend_week_before2>0)


# avg. money spent in week after by users in this segment in $
mean(xsection$revenue_week_after2[xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                  & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                  & xsection$lootboxspend_week_before2>0], na.rm=TRUE) 

# avg. playtime in week after by users in this segment in minutes
mean(xsection$playtime_week_after2[xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                   & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                   & xsection$lootboxspend_week_before2>0], na.rm=TRUE) / 60

## both surveys jointly

# sample size
sum(xsection$lootboxspend_week_before1>0
    & xsection$lootboxspend_week_before2>0)

# prevalence
sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=2
    & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
    & xsection$lootboxspend_week_before1>0
    & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
    & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
    & xsection$lootboxspend_week_before2>0, na.rm=TRUE) /
  sum(xsection$lootboxspend_week_before1>0
      & xsection$lootboxspend_week_before2>0)

# avg. money spent in week after by users in this segment in $
(sum(xsection$revenue_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                  & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                  & xsection$lootboxspend_week_before1>0
                                  & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                  & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                  & xsection$lootboxspend_week_before2>0], na.rm=TRUE) +
    sum(xsection$revenue_week_after2[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                     & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                     & xsection$lootboxspend_week_before1>0
                                     & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                     & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                     & xsection$lootboxspend_week_before2>0], na.rm=TRUE))/2 /
  sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=2
      & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
      & xsection$lootboxspend_week_before1>0
      & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
      & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
      & xsection$lootboxspend_week_before2>0, na.rm=TRUE)

# avg. playtime in week after by users in this segment in minutes
(sum(xsection$playtime_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                   & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                   & xsection$lootboxspend_week_before1>0
                                   & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                   & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                   & xsection$lootboxspend_week_before2>0], na.rm=TRUE) +
    sum(xsection$playtime_week_after2[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                      & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
                                      & xsection$lootboxspend_week_before1>0
                                      & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                      & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
                                      & xsection$lootboxspend_week_before2>0], na.rm=TRUE))/2 /
  sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=2
      & xsection$lootboxspend_week_after1>=xsection$lootboxspend_week_before1
      & xsection$lootboxspend_week_before1>0
      & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
      & xsection$lootboxspend_week_after2>=xsection$lootboxspend_week_before2
      & xsection$lootboxspend_week_before2>0, na.rm=TRUE) / 60

### playtime

## first survey

# sample size
sum(xsection$playtime_week_before1>0)

# prevalence
sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=2
    & xsection$playtime_week_after1>=xsection$playtime_week_before1
    & xsection$playtime_week_before1>0, na.rm=TRUE) /
  sum(xsection$playtime_week_before1>0)


# avg. money spent in week after by users in this segment in $
mean(xsection$revenue_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                  & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                  & xsection$playtime_week_before1>0], na.rm=TRUE) 

# avg. playtime in week after by users in this segment in minutes
mean(xsection$playtime_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                   & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                   & xsection$playtime_week_before1>0], na.rm=TRUE) / 60

## second survey

# sample size
sum(xsection$playtime_week_before2>0)

# prevalence
sum(xsection$nps_rating2>=0 & xsection$nps_rating2<=2
    & xsection$playtime_week_after2>=xsection$playtime_week_before2
    & xsection$playtime_week_before2>0, na.rm=TRUE) /
  sum(xsection$playtime_week_before2>0)

# avg. money spent in week after by users in this segment in $
mean(xsection$revenue_week_after2[xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                  & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                  & xsection$playtime_week_before2>0], na.rm=TRUE) 

# avg. playtime in week after by users in this segment in minutes
mean(xsection$playtime_week_after2[xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                   & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                   & xsection$playtime_week_before2>0], na.rm=TRUE) / 60

## both surveys jointly

# sample size
sum(xsection$playtime_week_before1>0
    & xsection$playtime_week_before2>0)

# prevalence
sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=2
    & xsection$playtime_week_after1>=xsection$playtime_week_before1
    & xsection$playtime_week_before1>0
    & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
    & xsection$playtime_week_after2>=xsection$playtime_week_before2
    & xsection$playtime_week_before2>0, na.rm=TRUE) /
  sum(xsection$playtime_week_before1>0
      & xsection$playtime_week_before2>0)

# avg. money spent in week after by users in this segment in $
(sum(xsection$revenue_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                  & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                  & xsection$playtime_week_before1>0
                                  & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                  & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                  & xsection$playtime_week_before2>0], na.rm=TRUE) +
    sum(xsection$revenue_week_after2[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                     & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                     & xsection$playtime_week_before1>0
                                     & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                     & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                     & xsection$playtime_week_before2>0], na.rm=TRUE))/2 /
  sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=2
      & xsection$playtime_week_after1>=xsection$playtime_week_before1
      & xsection$playtime_week_before1>0
      & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
      & xsection$playtime_week_after2>=xsection$playtime_week_before2
      & xsection$playtime_week_before2>0, na.rm=TRUE)

# avg. playtime in week after by users in this segment in minutes
(sum(xsection$playtime_week_after1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                   & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                   & xsection$playtime_week_before1>0
                                   & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                   & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                   & xsection$playtime_week_before2>0], na.rm=TRUE) +
    sum(xsection$playtime_week_after2[xsection$nps_rating1>=0 & xsection$nps_rating1<=2
                                      & xsection$playtime_week_after1>=xsection$playtime_week_before1
                                      & xsection$playtime_week_before1>0
                                      & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
                                      & xsection$playtime_week_after2>=xsection$playtime_week_before2
                                      & xsection$playtime_week_before2>0], na.rm=TRUE))/2 /
  sum(xsection$nps_rating1>=0 & xsection$nps_rating1<=2
      & xsection$playtime_week_after1>=xsection$playtime_week_before1
      & xsection$playtime_week_before1>0
      & xsection$nps_rating2>=0 & xsection$nps_rating2<=2
      & xsection$playtime_week_after2>=xsection$playtime_week_before2
      & xsection$playtime_week_before2>0, na.rm=TRUE) / 60
