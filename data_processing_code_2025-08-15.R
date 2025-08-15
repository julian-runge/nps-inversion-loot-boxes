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

# read in data exported from database via sql in two tranches
data1 <- read.csv("daily_behavior_nps until 2019-3-25.csv", header = TRUE)
data2 <- read.csv("daily_behavior_nps after 2019-3-25.csv", header = TRUE)

# look at data
head(data1)
head(data2)

# generate numeric id
data1 <- transform(data1,id=as.numeric(factor(uid)))
data2 <- transform(data2,id=as.numeric(factor(uid)))

# format dates
data1$daily_date <- as.Date(data1$daily_date, format = "%Y-%m-%d")
data2$daily_date <- as.Date(data2$daily_date, format = "%Y-%m-%d")

# drop columns we don't need, including hashed user identifiers
data1 = subset(data1, select = -c(X, uid, ranks_gained, viplvl, dmo, mission_battlestart,
                                  guild_message, friend_thank, friend_request_sent, friend_invite,
                                  error_client, error_network, ad_videofinished, survey_name) )
data2 = subset(data2, select = -c(X, uid, ranks_gained, viplvl, dmo, mission_battlestart,
                                  guild_message, friend_thank, friend_request_sent, friend_invite,
                                  error_client, error_network, ad_videofinished, survey_name) )

# narrow to relevant time periods
data1 <- filter(data1,
                data1$daily_date>='2019-01-01' &
                  data1$daily_date<'2019-03-25')

data2 <- filter(data2, data2$daily_date>='2019-03-25' &
                  data2$daily_date<='2019-04-25')

# check that daily number of entries is reasonable over time
daily_count_1 <- data1 %>%
  dplyr::group_by(daily_date) %>%
  dplyr::summarize(count = n(),
                   survey = mean(rating_response, na.rm=TRUE)
  )
daily_count_2 <- data2 %>%
  dplyr::group_by(daily_date) %>%
  dplyr::summarize(count = n(),
                   survey = mean(rating_response, na.rm=TRUE)
  )

# append datasets and drop previous dfs
data <- rbind(data1,data2)

# check that daily number of entries matches
daily_count_3 <- data %>%
  dplyr::group_by(daily_date) %>%
  dplyr::summarize(count = n(),
                   survey = mean(rating_response, na.rm=TRUE)
  )

# remove unmerged datasets
rm(data1)
rm(data2)

# adjust nps rating to usual scale
data$rating_response <- data$rating_response - 1

# store dataset
write.csv(data, "nps_lootboxes_panel_reduced_combined.csv")

# some data formatting
data$ujd <- as.Date(data$ujd, format = "%Y-%m-%d")
data$survey_date <- as.Date(data$survey_date, format = "%Y-%m-%d")

# aggregate into cross-section
xsection <- data %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(
    days_in_game = as.numeric((as.Date("2019-03-03")-min(ujd))),
    platform = first(dos),
    country = first(co),
    
    revenue_2019_before1 = sum(total_daily_gross_revenue[daily_date<="2019-03-03" & daily_date>="2019-01-01"], na.rm=TRUE),
    
    revenue_month_before1 = sum(total_daily_gross_revenue[daily_date<="2019-03-03" & daily_date>="2019-02-02"], na.rm=TRUE),
    revenue_week_before1 = sum(total_daily_gross_revenue[daily_date<="2019-03-03" & daily_date>="2019-02-25"], na.rm=TRUE),
    revenue_during1 = sum(total_daily_gross_revenue[daily_date<="2019-03-06" & daily_date>="2019-03-04"], na.rm=TRUE),
    revenue_week_after1 = sum(total_daily_gross_revenue[daily_date<="2019-03-13" & daily_date>="2019-03-07"], na.rm=TRUE),
    
    revenue_month_before2 = sum(total_daily_gross_revenue[daily_date<="2019-04-07" & daily_date>="2019-03-09"], na.rm=TRUE),
    revenue_week_before2 = sum(total_daily_gross_revenue[daily_date<="2019-04-07" & daily_date>="2019-04-01"], na.rm=TRUE),
    revenue_during2 = sum(total_daily_gross_revenue[daily_date<="2019-04-10" & daily_date>="2019-04-08"], na.rm=TRUE),
    revenue_week_after2 = sum(total_daily_gross_revenue[daily_date<="2019-04-17" & daily_date>="2019-04-11"], na.rm=TRUE),
    
    lootboxspend_2019_before1 = sum(gem_spend[daily_date<="2019-03-03" & daily_date>="2019-01-01"], na.rm=TRUE),
    
    lootboxspend_month_before1 = sum(gem_spend[daily_date<="2019-03-03" & daily_date>="2019-02-02"], na.rm=TRUE),
    lootboxspend_week_before1 = sum(gem_spend[daily_date<="2019-03-03" & daily_date>="2019-02-25"], na.rm=TRUE),
    lootboxspend_during1 = sum(gem_spend[daily_date<="2019-03-06" & daily_date>="2019-03-04"], na.rm=TRUE),
    lootboxspend_week_after1 = sum(gem_spend[daily_date<="2019-03-13" & daily_date>="2019-03-07"], na.rm=TRUE),
    
    lootboxspend_month_before2 = sum(gem_spend[daily_date<="2019-04-07" & daily_date>="2019-03-09"], na.rm=TRUE),
    lootboxspend_week_before2 = sum(gem_spend[daily_date<="2019-04-07" & daily_date>="2019-04-01"], na.rm=TRUE),
    lootboxspend_during2 = sum(gem_spend[daily_date<="2019-04-10" & daily_date>="2019-04-08"], na.rm=TRUE),
    lootboxspend_week_after2 = sum(gem_spend[daily_date<="2019-04-17" & daily_date>="2019-04-11"], na.rm=TRUE),
    
    playtime_2019_before1 = sum(total_play_time[daily_date<="2019-03-03" & daily_date>="2019-01-01"], na.rm=TRUE),
    
    playtime_month_before1 = sum(total_play_time[daily_date<="2019-03-03" & daily_date>="2019-02-02"], na.rm=TRUE),
    playtime_week_before1 = sum(total_play_time[daily_date<="2019-03-03" & daily_date>="2019-02-25"], na.rm=TRUE),
    playtime_during1 = sum(total_play_time[daily_date<="2019-03-06" & daily_date>="2019-03-04"], na.rm=TRUE),
    playtime_week_after1 = sum(total_play_time[daily_date<="2019-03-13" & daily_date>="2019-03-07"], na.rm=TRUE),
    
    playtime_month_before2 = sum(total_play_time[daily_date<="2019-04-07" & daily_date>="2019-03-09"], na.rm=TRUE),
    playtime_week_before2 = sum(total_play_time[daily_date<="2019-04-07" & daily_date>="2019-04-01"], na.rm=TRUE),
    playtime_during2 = sum(total_play_time[daily_date<="2019-04-10" & daily_date>="2019-04-08"], na.rm=TRUE),
    playtime_week_after2 = sum(total_play_time[daily_date<="2019-04-17" & daily_date>="2019-04-11"], na.rm=TRUE),
    
    nps_rating1 = mean(rating_response[is.na(rating_response)==FALSE
                                         & is.na(survey_date)==FALSE
                                         & survey_date<=as.Date("2019-03-07")]),
    nps_rating2 = mean(rating_response[is.na(rating_response)==FALSE
                                         & is.na(survey_date)==FALSE
                                         & survey_date>=as.Date("2019-04-07")]),
    nps_rating1_check = max(rating_response[is.na(rating_response)==FALSE
                                             & is.na(survey_date)==FALSE
                                             & survey_date<=as.Date("2019-03-07")]),
    nps_rating2_check = max(rating_response[is.na(rating_response)==FALSE
                                             & is.na(survey_date)==FALSE
                                             & survey_date>=as.Date("2019-04-07")])
  )

# check that survey rating was generated correctly (resulting dataframe should be empty)
nps_rating_check <- filter(xsection, xsection$nps_rating1!=xsection$nps_rating1_check | xsection$nps_rating2!=xsection$nps_rating2_check)

# map nps ratings to behavioral intention segments
xsection$nps1 <- NaN
xsection$nps1[xsection$nps_rating1==10 | xsection$nps_rating1==9] <- "Promoter"
xsection$nps1[xsection$nps_rating1==8 | xsection$nps_rating1==7] <- "Passive"
xsection$nps1[xsection$nps_rating1>=3 & xsection$nps_rating1<=6] <- "Detractor"
xsection$nps1[xsection$nps_rating1>=0 & xsection$nps_rating1<=2] <- "Severe detr."

xsection$nps2 <- NaN
xsection$nps2[xsection$nps_rating2==10 | xsection$nps_rating2==9] <- "Promoter"
xsection$nps2[xsection$nps_rating2==8 | xsection$nps_rating2==7] <- "Passive"
xsection$nps2[xsection$nps_rating2>=3 & xsection$nps_rating2<=6] <- "Detractor"
xsection$nps2[xsection$nps_rating2>=0 & xsection$nps_rating2<=2] <- "Severe detr."

# store dataset
write.csv(xsection, "nps_lootboxes_xsection.csv")

# read in xsection
#xsection <- read.csv("nps_lootboxes_xsection.csv", header = TRUE)

# check segment size by country, platform and survey rating
country_count <- xsection %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(count = n(),
                   survey = mean(nps_rating1, na.rm=TRUE),
                   survey = mean(nps_rating2, na.rm=TRUE)
  )

segment_sizes <- xsection %>%
  dplyr::group_by(country, platform, nps1, nps2) %>%
  dplyr::summarize(count = n()
  )

countries_grouped <- data.frame(unique(filter(segment_sizes,count<30)$country))

# create country grouping as US and ROW
xsection$country_group <- "ROW"
xsection$country_group[xsection$country=="US"] <- "US"


######################################################################################################
############################## create publicly shareable dataset #####################################
######################################################################################################

data_public <- xsection %>%
  dplyr::group_by(country_group, platform, nps1, nps2) %>%
  dplyr::summarize(
    segment_size = n(),
    
    revenue_2019_before1_mean = mean(revenue_2019_before1, na.rm=TRUE),
    
    revenue_month_before1_mean = mean(revenue_month_before1, na.rm=TRUE),
    revenue_week_before1_mean = mean(revenue_week_before1, na.rm=TRUE),
    revenue_during1_mean = mean(revenue_during1, na.rm=TRUE),
    revenue_week_after1_mean = mean(revenue_week_after1, na.rm=TRUE),
    
    revenue_month_before2_mean = mean(revenue_month_before2, na.rm=TRUE),
    revenue_week_before2_mean = mean(revenue_week_before2, na.rm=TRUE),
    revenue_during2_mean = mean(revenue_during2, na.rm=TRUE),
    revenue_week_after2_mean = mean(revenue_week_after2, na.rm=TRUE),
    
    lootboxspend_2019_before1_mean = mean(lootboxspend_2019_before1, na.rm=TRUE),
    
    lootboxspend_month_before1_mean = mean(lootboxspend_month_before1, na.rm=TRUE),
    lootboxspend_week_before1_mean = mean(lootboxspend_week_before1, na.rm=TRUE),
    lootboxspend_during1_mean = mean(lootboxspend_during1, na.rm=TRUE),
    lootboxspend_week_after1_mean = mean(lootboxspend_week_after1, na.rm=TRUE),
    
    lootboxspend_month_before2_mean = mean(lootboxspend_month_before2, na.rm=TRUE),
    lootboxspend_week_before2_mean = mean(lootboxspend_week_before2, na.rm=TRUE),
    lootboxspend_during2_mean = mean(lootboxspend_during2, na.rm=TRUE),
    lootboxspend_week_after2_mean = mean(lootboxspend_week_after2, na.rm=TRUE),
    
    playtime_2019_before1_mean = mean(playtime_2019_before1, na.rm=TRUE),
    
    playtime_month_before1_mean = mean(playtime_month_before1, na.rm=TRUE),
    playtime_week_before1_mean = mean(playtime_week_before1, na.rm=TRUE),
    playtime_during1_mean = mean(playtime_during1, na.rm=TRUE),
    playtime_week_after1_mean = mean(playtime_week_after1, na.rm=TRUE),
    
    playtime_month_before2_mean = mean(playtime_month_before2, na.rm=TRUE),
    playtime_week_before2_mean = mean(playtime_week_before2, na.rm=TRUE),
    playtime_during2_mean = mean(playtime_during2, na.rm=TRUE),
    playtime_week_after2_mean = mean(playtime_week_after2, na.rm=TRUE),
    
    revenue_2019_before1_sd = sd(revenue_2019_before1, na.rm=TRUE),
    
    revenue_month_before1_sd = sd(revenue_month_before1, na.rm=TRUE),
    revenue_week_before1_sd = sd(revenue_week_before1, na.rm=TRUE),
    revenue_during1_sd = sd(revenue_during1, na.rm=TRUE),
    revenue_week_after1_sd = sd(revenue_week_after1, na.rm=TRUE),
    
    revenue_month_before2_sd = sd(revenue_month_before2, na.rm=TRUE),
    revenue_week_before2_sd = sd(revenue_week_before2, na.rm=TRUE),
    revenue_during2_sd = sd(revenue_during2, na.rm=TRUE),
    revenue_week_after2_sd = sd(revenue_week_after2, na.rm=TRUE),
    
    lootboxspend_2019_before1_sd = sd(lootboxspend_2019_before1, na.rm=TRUE),
    
    lootboxspend_month_before1_sd = sd(lootboxspend_month_before1, na.rm=TRUE),
    lootboxspend_week_before1_sd = sd(lootboxspend_week_before1, na.rm=TRUE),
    lootboxspend_during1_sd = sd(lootboxspend_during1, na.rm=TRUE),
    lootboxspend_week_after1_sd = sd(lootboxspend_week_after1, na.rm=TRUE),
    
    lootboxspend_month_before2_sd = sd(lootboxspend_month_before2, na.rm=TRUE),
    lootboxspend_week_before2_sd = sd(lootboxspend_week_before2, na.rm=TRUE),
    lootboxspend_during2_sd = sd(lootboxspend_during2, na.rm=TRUE),
    lootboxspend_week_after2_sd = sd(lootboxspend_week_after2, na.rm=TRUE),
    
    playtime_2019_before1_sd = sd(playtime_2019_before1, na.rm=TRUE),
    
    playtime_month_before1_sd = sd(playtime_month_before1, na.rm=TRUE),
    playtime_week_before1_sd = sd(playtime_week_before1, na.rm=TRUE),
    playtime_during1_sd = sd(playtime_during1, na.rm=TRUE),
    playtime_week_after1_sd = sd(playtime_week_after1, na.rm=TRUE),
    
    playtime_month_before2_sd = sd(playtime_month_before2, na.rm=TRUE),
    playtime_week_before2_sd = sd(playtime_week_before2, na.rm=TRUE),
    playtime_during2_sd = sd(playtime_during2, na.rm=TRUE),
    playtime_week_after2_sd = sd(playtime_week_after2, na.rm=TRUE)
  )

# store dataset
write.csv(data_public, "nps_lootboxes_data_public.csv")