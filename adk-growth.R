# Checks for, installs, & loads packages (no warning prior to install):
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[req == FALSE]
  if(length(need) > 0){ 
    install.packages(need)
    lapply(need, require, character.only = TRUE)
  }
}

using("tidyverse", "laselva", "maps", "caret", "Rborist", "extrafont", "knitr")
# laselva fetches FIA data: https://github.com/ropenscilabs/laselva
loadfonts("win", quiet = TRUE)



##############################
# Import FIA data
##############################


# Define NY counties (FIPS codes) in Adirondack region --------

counties <- c(75, 65, 49, 45, 89, 43, 35, 41, 33, 31, 19, 113)


# Fetch FIA tree, growth, plot, & condition data for New York 
# (this may take a few minutes)

ny_trees <- fia_fetch(state = "NY")
ny_growth <- fia_fetch("NY", "TREE_GRM_COMPONENT")
ny_plots <- fia_fetch("NY", "PLOT")
ny_conds <- fia_fetch("NY", "COND")


# Filter nf_trees to keep only data from Adirondack counties 

adk_trees <- ny_trees$NY_tree %>%
  filter(COUNTYCD %in% counties)


# Calculates overtopping basal area (BAL) assuming all input trees are in same plot
# and ba is adjusted based on tpa:
# THIS IS A REMNANT FROM AN ATTEMPT THAT WAS DISCARDED!
pbal <- function(dbh, ba){
  sapply(dbh, function(x){
    index <- dbh > x
    return(sum(ba[index]))
  })
}


# For each FIA table, filter, format and select vectors that might be useful
adk_trees <- adk_trees %>%
  # keep live, non-cull, non-seedling trees:
  filter(DIAHTCD == 1, 
         TREECLCD == 2, 
         STATUSCD == 1) %>% 
  mutate(TRE_CN = CN,
         SPCD = factor(SPCD),
         COUNTY = factor(COUNTYCD)) %>%
  select(TRE_CN, PLT_CN, COUNTY, SPCD, HT, CCLCD, TREEGRCD, CULL, UNCRCD, 
         CR, CDENCD, CDIEBKCD, TRANSCD, TREECLCD_NERS, DAMLOC1, DAMLOC2, 
         DAMTYP1, DAMTYP2, DAMSEV1, DAMSEV2, INVYR, DIA)

ny_growth <- ny_growth$NY_TREE_GRM_COMPONENT %>%
  select(TRE_CN, DIA_BEGIN, DIA_MIDPT, DIA_END, ANN_DIA_GROWTH) 

ny_plots <- ny_plots$NY_PLOT %>%
  mutate(PLT_CN = CN) %>%
  select(PLT_CN, LAT, LON)

ny_conds <- ny_conds$NY_COND %>%
  select(FORTYPCD, SITECLCD, SLOPE, ASPECT, PHYSCLCD, GSSTKCD,
         BALIVE, LIVE_CANOPY_CVR_PCT, NBR_LIVE_STEMS, PLT_CN)


# Join FIA tables -------------------------------------------------------

adk_fia <- inner_join(adk_trees, ny_growth, by = "TRE_CN") 

adk_fia <- right_join(ny_plots, adk_fia, by = "PLT_CN")

adk_fia <- right_join(ny_conds, adk_fia, by = "PLT_CN")


# filter out observations w/o diameter growth measurements (response variable),
# remove database key, bring response to front

adk_fia <- adk_fia %>% filter(!is.na(ANN_DIA_GROWTH)) %>%
  select(-(TRE_CN)) %>% 
  select(ANN_DIA_GROWTH, everything())


# clean up ---------------------------------------------------------------

remove(adk_trees, ny_conds, ny_growth, ny_plots, ny_trees, counties, 
       pbal, using)

n_trees1 <- nrow(adk_fia)
n_plots1 <- length(unique(adk_fia$PLT_CN))
n_spp1 <- length(unique(adk_fia$SPCD))



##############################
# Preprocess
##############################


# # Examine vectors ----------------------------------------------------------
# 
# str(adk_fia)
# 
# summary(adk_fia)
# 
# adk_fia <- adk_fia %>%
#   arrange(ANN_DIA_GROWTH)
# 
# View(head(adk_fia))
# 
# View(tail(adk_fia))
# 
# levels(adk_fia$COUNTY) # 12 NY counties: all accounted for
# 
# 
# # Distribution of growth rates -------------------------------------------
# 
# adk_fia %>%
#   group_by(SPCD) %>%
#   filter(n()>=20) %>%
#   ungroup() %>%
#   ggplot(aes(ANN_DIA_GROWTH)) +
#   geom_density(bw = .008, fill = "dark gray") +
#   geom_vline(xintercept = median(adk_fia$ANN_DIA_GROWTH),
#              col = "dark green", size = 1)
# 
# adk_fia %>% ggplot(aes(sample = ANN_DIA_GROWTH)) + geom_qq()
# 
# # Grouped by species:
# adk_fia %>%
#   group_by(SPCD) %>%
#   filter(n()>=20) %>%
#   ungroup() %>%
#   ggplot(aes(ANN_DIA_GROWTH)) +
#   geom_density(bw = .008, fill = "dark gray") +
#   geom_vline(xintercept = median(adk_fia$ANN_DIA_GROWTH),
#              col = "dark green", size = 1) +
#   facet_wrap(~ SPCD, ncol = 5)
# 
# 
# # Look for plots w/o condition data --------------------------------------
# 
# View(adk_fia %>%
#        filter(is.na(FORTYPCD)) %>%
#        group_by(PLT_CN) %>%
#        summarize(nasite = sum(is.na(SITECLCD))/n(),
#                  naslope = sum(is.na(SLOPE))/n(),
#                  naaspect = sum(is.na(ASPECT))/n(),
#                  naphys = sum(is.na(PHYSCLCD))/n(),
#                  nastock = sum(is.na(GSSTKCD))/n(),
#                  naba = sum(is.na(BALIVE))/n()))
# 
# View(adk_fia %>%
#        filter(!is.na(FORTYPCD)) %>%
#        group_by(PLT_CN) %>%
#        summarize(nasite = sum(is.na(SITECLCD))/n(),
#                  naslope = sum(is.na(SLOPE))/n(),
#                  naaspect = sum(is.na(ASPECT))/n(),
#                  naphys = sum(is.na(PHYSCLCD))/n(),
#                  nastock = sum(is.na(GSSTKCD))/n(),
#                  naba = sum(is.na(BALIVE))/n()))
# 
# View(unique((adk_fia %>% filter(is.na(FORTYPCD)))$COUNTY))
# # Plots w/o condition data exist in all counties


# Fix problems -------------------------------------------------------------

bad_plots <- unique((filter(adk_fia, is.na(FORTYPCD)))$PLT_CN) #w/o condition data
n_plots2 <- n_plots1 - length(bad_plots)

adk_fia <- adk_fia  %>%
  # reformat factors:
  mutate(FORTYPCD = factor(FORTYPCD),
         PHYSCLCD = factor(PHYSCLCD)) %>%
  # remove observations missing key predictors:
  filter(#!is.na(bal),
    !(PLT_CN %in% bad_plots)) %>%
  # remove empty & almost empty vectors:
  select(ANN_DIA_GROWTH:BALIVE, LAT:SPCD, CCLCD, CR, INVYR, DIA_MIDPT)

n_spp2 <- length(unique(adk_fia$SPCD))


# Make names and factor levels match PBF inventory input -------------------

species_codes <- c(12, 43, 68, 70, 71, 91, 94, 95, 96, 97, 105, 123, 125, 126, 129, 
                   130, 136, 202, 221, 241, 261, 310, 313, 314, 315, 316, 317, 318, 
                   319, 320, 331, 341, 355, 356, 357, 367, 370, 371, 372, 373, 375, 
                   379, 391, 400, 402, 403, 407, 409, 421, 462, 491, 500, 531, 540, 
                   541, 543, 544, 546, 552, 601, 602, 621, 651, 655, 660, 661, 663, 
                   680, 693, 701, 712, 731, 741, 742, 743, 744, 746, 760, 761, 762, 
                   763, 764, 771, 802, 804, 806, 816, 823, 832, 833, 837, 901, 920, 
                   922, 923, 926, 934, 935, 936, 937, 950, 951, 970, 972, 975, 977, 
                   999)

species <- c("fir", "other softwood", "cedar", "tamarack", "tamarack", 
             "norway spruce", "spruce", "spruce", "spruce", "spruce", "other softwood", 
             "other softwood", "red pine", "other softwood", "white pine", "scots pine", 
             "other softwood", "other softwood", "other softwood", "cedar", "hemlock", 
             "other hardwood", "soft maple", "hard maple", "striped maple", "soft maple",
             "soft maple", "hard maple", "other hardwood", "hard maple", 
             "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
             "other hardwood", "other hardwood", "other hardwood", "yellow birch", 
             "other hardwood", "other hardwood", "paper birch", "other hardwood", 
             "other hardwood", "hickory", "hickory", "hickory", "hickory", "hickory", 
             "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
             "beech", "ash", "ash", "ash", "ash", "ash", "other hardwood", "butternut", 
             "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
             "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
             "other hardwood", "hophornbeam", "other hardwood", "other hardwood", 
             "aspen", "cottonwood", "aspen", "cottonwood", "aspen", "other hardwood", 
             "other hardwood", "black cherry", "other hardwood", "other hardwood", 
             "other hardwood", "white oak", "white oak", "red oak", "white oak", 
             "white oak", "white oak", "red oak", "red oak", "other hardwood", 
             "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
             "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
             "basswood", "basswood", "elm", "elm", "elm", "elm", "other hardwood")

spp <- data.frame(SPCD = factor(species_codes), spp = species)

adk_fia <- left_join(adk_fia, spp, by = "SPCD")


forest_type_codes <- c(101, 102, 103, 104, 105, 121, 122, 123, 124, 125, 126, 127, 
                       167, 171, 381, 384, 385, 401, 402, 409, 503, 505, 509, 512, 
                       513, 516, 517, 519, 520, 701, 702, 703, 704, 705, 706, 707, 
                       708, 709, 801, 802, 805, 809, 901, 902, 903, 904, 905, 962, 
                       995, 999)

forest_types <- c("Red pine", "Red pine", "White pine", "Mixed softwood", "Hemlock",
                  "Spruce-fir", "Spruce-fir", "Spruce-fir", "Spruce-fir", "Spruce-fir",
                  "Larch", "Cedar", "Mixed softwood", "Mixed softwood", "Scots pine", 
                  "Norway spruce", "Larch", "Pine-hardwood", "Mixedwood",
                  "Pine-hardwood", "Oak-hickory", "Oak-hickory", "Oak-hickory", 
                  "Transition hardwood", "Transition hardwood", "Transition hardwood", 
                  "Transition hardwood", "Northern hardwood", "Northern hardwood", 
                  "Northern hardwood", "Transition hardwood", "Cottonwood", "Other", 
                  "Other", "Other", "Northern hardwood", "Northern hardwood", 
                  "Cottonwood", "Northern hardwood", "Northern hardwood", 
                  "Northern hardwood", "Northern hardwood", "Northern hardwood", 
                  "Northern hardwood", "Northern hardwood", "Northern hardwood", 
                  "Northern hardwood", "Other", "Other", "Nonstocked")

forests <- data.frame(FORTYPCD = factor(forest_type_codes), forest_type = forest_types)

adk_fia <- left_join(adk_fia, forests, by = "FORTYPCD")


landscape_codes <- c(11, 12, 13, 21, 22, 23, 24, 25, 29, 31, 32, 34, 39)

landscapes <- c("dry tops", "dry slopes", "deep sands", "flatwoods", "rolling uplands",
                "moist slopes & coves", "narrow floodplains/bottomlands",
                "broad floodplains/bottomlands", "other mesic", "swamps/bogs", 
                "small drains", "beaver ponds", "other hydric")

lands <- data.frame(PHYSCLCD = factor(landscape_codes), landscape = landscapes)

adk_fia <- left_join(adk_fia, lands, by = "PHYSCLCD")

remove(forests, lands, spp, bad_plots, forest_type_codes, forest_types, landscape_codes,
       landscapes, species, species_codes)


# Rename variables ---------------------------------------------------------

adk_fia <- adk_fia %>%
  mutate(stocking = GSSTKCD, crown_class = CCLCD)

adk_fia <- adk_fia %>%
  select(-SPCD, -FORTYPCD, -PHYSCLCD, -GSSTKCD, -CCLCD, -INVYR) %>%
  rename(diam_growth = ANN_DIA_GROWTH, site_class = SITECLCD, slope = SLOPE, 
         aspect = ASPECT, ba = BALIVE, lat = LAT, lon = LON, county = COUNTY, 
         cr = CR, dbh = DIA_MIDPT)

n_trees2 <- nrow(adk_fia)
n_spp3 <- length(unique(adk_fia$spp))



##############################
# Partition data
##############################

test_size <- .2

set.seed(10)
index <- createDataPartition(adk_fia$diam_growth, times = 1, p = test_size, list = FALSE)

train <- adk_fia[-index,]
test <- adk_fia[index,]



##############################
# EDA
##############################

# train %>%
#   ggplot(aes(diam_growth)) +
#   geom_density(bw = .01)
# 
# # PLot relationships to growth -------------------------------------------
# 
# train %>%
#   filter(!is.na(forest_type)) %>%
#   mutate(forest_type = reorder(forest_type, -diam_growth, FUN = median)) %>%
#   ggplot(aes(forest_type, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# train %>%
#   filter(!is.na(site_class)) %>%
#   mutate(site_class = factor(site_class)) %>%
#   ggplot(aes(site_class, diam_growth)) +
#   geom_boxplot(fill = "gray")
# # Why isn't there a clear relationship to site class?
# 
# train %>%
#   filter(!is.na(site_class)) %>%
#   mutate(site_class = factor(site_class)) %>%
#   ggplot(aes(site_class, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   facet_wrap(~ spp) +
#   scale_y_continuous(trans = "log")
# # not enough to show underlying influence of site class
# 
# train %>%
#   filter(!is.na(site_class)) %>%
#   mutate(site_class = factor(site_class)) %>%
#   ggplot(aes(site_class, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   facet_wrap(~ forest_type) +
#   scale_y_continuous(trans = "log")
# # not enough to show underlying influence of site class
# 
# train %>%
#   filter(cr >= 50) %>%
#   ggplot(aes(factor(site_class), diam_growth)) +
#   geom_violin(fill = "gray") +
#   facet_wrap(~ spp) +
#   scale_y_continuous(trans = "log")
# # still nothing
# 
# View(train %>%
#        filter(spp == "yellow birch",
#               cr > 50,
#               stocking > 3) %>%
#        group_by(factor(site_class)) %>%
#        summarize(n = n(),
#                  mean_growth = mean(diam_growth),
#                  sd_growth = sd(diam_growth)))
# 
# train %>%
#   group_by(site_class) %>%
#   summarize(n = n(),
#             mean_stock = mean(stocking),
#             stock_sd = sd(stocking))
# # perhaps slight underlying neg relationship btween site class & stocking that could
# # confound diam growth/site class relationship, but doesn't look significant.
# 
# train %>%
#   filter(!is.na(slope)) %>%
#   ggplot(aes(slope, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "#f0027f")
# 
# train %>%
#   filter(!is.na(aspect)) %>%
#   ggplot(aes(aspect, diam_growth)) +
#   geom_jitter(alpha = .05) +
#   geom_smooth(col = "red")
# 
# train %>%
#   filter(!is.na(landscape)) %>%
#   mutate(landscape = reorder(landscape, -diam_growth, FUN = median)) %>%
#   ggplot(aes(landscape, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# # almost no relationship, but what's there is unexpected
# 
# train %>%
#   group_by(landscape) %>%
#   summarize(n = n())
# # doesn't seem to be a regularization problem
# 
# View(train %>%
#        group_by(landscape) %>%
#        summarize(mean_cr = mean(cr),
#                  mean_stocking = mean(stocking),
#                  mean_siteclass = mean(site_class),
#                  mean_dbh = mean(dbh)))
# train %>%
#   filter(!is.na(landscape)) %>%
#   ggplot(aes(landscape, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_wrap(~ stocking)
# # this fixes it
# 
# train %>%
#   filter(!is.na(stocking)) %>%
#   ggplot(aes(factor(stocking), diam_growth)) +
#   geom_boxplot(fill = "gray")
# 
# train %>%
#   filter(!is.na(ba)) %>%
#   ggplot(aes(ba, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red")
# # What's with the plots w/ BA>500?
# 
# train %>%
#   filter(ba > 400) %>%
#   group_by(forest_type) %>%
#   summarize(n = n(), mean_growth = mean(diam_growth))
# # high ba are all white pine stands
# 
# train %>%
#   mutate(ba_high = if_else(ba > 400, 1, 0)) %>%
#            group_by(ba_high) %>%
#            summarize(mean_growth = mean(diam_growth),
#                      mean_dbh = mean(dbh),
#                      mean_cr = mean(cr),
#                      mean_site = mean(site_class))
# train %>%
#   mutate(ba_low = if_else(ba <30, 1, 0)) %>%
#   group_by(ba_low) %>%
#   summarize(mean_growth = mean(diam_growth),
#             mean_dbh = mean(dbh),
#             mean_cr = mean(cr),
#             mean_site = mean(site_class))
# # low ba are poorer sites w/ smaller trees
# 
# train %>%
#   ggplot(aes(lat, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red")
# 
# train %>%
#   ggplot(aes(lon, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red")
# 
# train %>%
#   mutate(county = reorder(county, -diam_growth, FUN = median)) %>%
#   ggplot(aes(county, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# #slight relationship, w/ highland counties having lower growth
# 
# View(train %>% group_by(county) %>% summarize(n = n()))
# 
# train %>%
#   mutate(spp = reorder(spp, -diam_growth, FUN = median)) %>%
#   ggplot(aes(spp, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_y_continuous(trans = "log",
#                      breaks = c(.01, .02, .04, .08, .16, .32, .64),
#                      name = "diameter growth (in/yr)") +
#   scale_x_discrete(name = "species group")
# 
# View(train %>%
#        mutate(spp = reorder(spp, -diam_growth, FUN = mean))%>%
#        group_by(spp) %>%
#        summarize(n = n(),
#                  mean_growth = mean(diam_growth),
#                  sd_growth = sd(diam_growth)))
# 
# train %>%
#   ggplot(aes(factor(crown_class), diam_growth)) +
#   geom_boxplot(fill = "gray")
# 
# # why is open-grown slow growing?
# train %>%
#   filter(crown_class == 1) %>%
#   group_by(site_class) %>%
#   summarize(n = n(), mean_growth = mean(diam_growth))
# # maybe they are poor situations not captured by site class...
# 
# train %>%
#   group_by(factor(crown_class)) %>%
#   summarize(mean_dbh = mean(dbh))
# # or they include a lot of small trees that don't grow as fast
# 
# train %>%
#   ggplot(aes(cr, diam_growth)) +
#   geom_jitter(width = 2.5, height = .005, alpha = .07) +
#   geom_smooth(col = "purple") +
#   scale_y_continuous(limits = c(0, .3))
# 
# train %>%
#   filter(!is.na(dbh)) %>%
#   group_by(spp) %>%
#   #filter(n() > 100) %>%
#   ungroup() %>%
#   ggplot(aes(dbh, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red") +
#   facet_wrap(~ spp) +
#   scale_y_continuous(limits = c(0, .4),
#                      name = "diameter growth (in/yr)")
# 
# train %>%
#   filter(spp != "fir") %>%
#   group_by(spp) %>%
#   filter(n() > 2300) %>%
#   ungroup() %>%
#   mutate(spp = reorder(spp, -diam_growth, FUN = mean)) %>%
#   ggplot(aes(dbh, diam_growth, col = spp)) +
#   geom_smooth(method.args = list(gamma =4)) +
#   scale_y_continuous(name = expression(Delta~'dbh (in/yr)')) +
#   scale_color_brewer(type = "qual",
#                      name = "species")
# 
# train %>%
#   filter(!is.na(bal),
#          bal > 0) %>%
#   ggplot(aes(bal, diam_growth)) +
#   geom_jitter(alpha = .05) +
#   geom_smooth(col = "red")
# 
# 
# # Plot relationships between predictors ---------------------------------
# 
# # Geographic -----------------------
# train %>%
#   ggplot(aes(lon, lat)) +
#   geom_polygon(data = map_data("state"),
#                aes(x = long, y = lat, group = group),
#                fill = NA, col = "dark gray") +
#   geom_point(size = .2) +
#   coord_fixed(xlim = c(-77, -72), ylim = c(42.4, 45.5), ratio = 1.3) +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank())
# 
# # Cuts continuous var into bins & returns midpoint value:
# cut2 <- function(x, breaks) {
#   r <- range(x)
#   b <- seq(r[1], r[2], length=2*breaks+1)
#   brk <- b[0:breaks*2+1]
#   mid <- b[1:breaks*2]
#   brk[1] <- brk[1]-0.01
#   k <- cut(x, breaks=brk, labels=FALSE)
#   mid[k]
# }
# 
# # 2d bin smoothed mapping of diameter growth
# train %>%
#   mutate(groupx = cut2(lon, 10), #binning lat & lon
#          groupy = cut2(lat, 10)) %>%
#   group_by(groupx, groupy) %>%
#   summarize(growth = mean(diam_growth)) %>%
#   ggplot(aes(groupx, groupy)) +
#   geom_raster(aes(fill = growth), interpolate = TRUE) +
#   geom_polygon(data = map_data("state"),
#                aes(x = long, y = lat, group = group),
#                fill = NA, col = "gray 60") +
#   scale_fill_viridis_c(option = "plasma", name = "in/year") +
#   coord_fixed(xlim = c(-77, -72), ylim = c(42.4, 45.5), ratio = 1.3) +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank())
# 
# # Site class relationships ---------
# train %>%
#   mutate(landscape = reorder(landscape, -site_class, FUN = mean)) %>%
#   group_by(landscape) %>%
#   summarize(mean = mean(site_class),
#             min = mean - 1.96*(sd(site_class)/sqrt(n())),
#             max = mean + 1.96*(sd(site_class)/sqrt(n()))) %>%
#   arrange(desc(mean)) %>%
#   ggplot(aes(landscape, ymin = min, ymax = max)) +
#   geom_errorbar(col = "gray 50") +
#   geom_point(aes(landscape, mean), size = 3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         axis.title.x = element_blank()) +
#   scale_y_continuous(name = "site class")
# 
# train %>%
#   mutate(forest_type = reorder(forest_type, -site_class, FUN = mean)) %>%
#   group_by(forest_type) %>%
#   summarize(mean = mean(site_class),
#             min = mean - 1.96*(sd(site_class)/sqrt(n())),
#             max = mean + 1.96*(sd(site_class)/sqrt(n()))) %>%
#   arrange(desc(mean)) %>%
#   ggplot(aes(forest_type, ymin = min, ymax = max)) +
#   geom_errorbar(col = "gray 50") +
#   geom_point(aes(forest_type, mean), size = 3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         axis.title.x = element_blank()) +
#   scale_y_continuous(name = "site class")
# #fastest growing forest types are on poorer sites
# 
# train %>%
#   mutate(county = reorder(county, -site_class, FUN = mean)) %>%
#   group_by(county) %>%
#   summarize(mean = mean(site_class),
#             min = mean - 1.96*(sd(site_class)/sqrt(n())),
#             max = mean + 1.96*(sd(site_class)/sqrt(n()))) %>%
#   arrange(desc(mean)) %>%
#   ggplot(aes(county, ymin = min, ymax = max)) +
#   geom_errorbar(col = "gray 50") +
#   geom_point(aes(county, mean), size = 3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         axis.title.x = element_blank()) +
#   scale_y_continuous(name = "site class")
# 
# train %>%
#   mutate(spp = reorder(spp, -site_class, FUN = mean)) %>%
#   group_by(spp) %>%
#   summarize(mean = mean(site_class),
#             min = mean - 1.96*(sd(site_class)/sqrt(n())),
#             max = mean + 1.96*(sd(site_class)/sqrt(n()))) %>%
#   arrange(desc(mean)) %>%
#   ggplot(aes(spp, ymin = min, ymax = max)) +
#   geom_errorbar(col = "gray 50") +
#   geom_point(aes(spp, mean), size = 3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         axis.title.x = element_blank()) +
#   scale_y_continuous(name = "site class")
# # fastest growing species tend to grow on poorer sites
# 
# train %>%
#   mutate(stocking = reorder(stocking, -site_class, FUN = mean)) %>%
#   group_by(stocking) %>%
#   summarize(mean = mean(site_class),
#             min = mean - 1.96*(sd(site_class)/sqrt(n())),
#             max = mean + 1.96*(sd(site_class)/sqrt(n()))) %>%
#   arrange(desc(mean)) %>%
#   ggplot(aes(stocking, ymin = min, ymax = max)) +
#   geom_errorbar(col = "gray 50") +
#   geom_point(aes(stocking, mean), size = 3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_y_continuous(name = "site class")
# 
# # Competition indecies -------------
# train %>%
#   ggplot(aes(ba, cr)) +
#   geom_jitter(alpha = .07, height = 2.5) +
#   geom_smooth(col = "red")
# 
# train %>%
#   ggplot(aes(factor(crown_class), cr)) +
#   geom_boxplot(fill = "gray")
# 
# train %>%
#   ggplot(aes(factor(stocking), cr)) +
#   geom_boxplot(fill = "gray")
# 
# train %>%
#   mutate(spp = reorder(spp, -cr, FUN = median)) %>%
#   ggplot(aes(spp, cr)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# train %>%
#   mutate(forest_type = reorder(forest_type, -cr, FUN = median)) %>%
#   ggplot(aes(forest_type, cr)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# train %>%
#   mutate(landscape = reorder(landscape, -cr, FUN = median)) %>%
#   ggplot(aes(landscape, cr)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# train %>%
#   ggplot(aes(factor(site_class), cr)) +
#   geom_boxplot(fill = "gray")
# 
# train %>%
#   ggplot(aes(dbh, cr)) +
#   geom_jitter(alpha = .07, height = 2.5) +
#   geom_smooth(col = "red")
# # measurement error related to treating saplings differently?



##############################
# Train model
##############################


# calculates RMSE:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu <- mean(train$diam_growth)


# Train regularization of naive terms --------------------------------------

# regularization params to try for each predictor
l1 <- seq(0, 10, 2)
l2 <- seq(0, 10, 2)
l3 <- seq(0, 10, 2)

ls <- expand.grid(l1, l2, l3)

# K folds for cross validation
set.seed(10)
folds <- createFolds(train$diam_growth, k=5, list = FALSE)

# calculate RMSE for each combo of possible params
rmses <- vector(length = nrow(ls))
for(i in 1:nrow(ls)){
  
  res <- vector(length = length(unique(folds)))
  for(j in 1:length(unique(folds))){
    
    b_s <- train[folds != j,] %>% 
      group_by(spp) %>%
      summarize(b_s = mean(diam_growth - mu)*(n()/(n() + ls[i, 1])))
    
    b_f <- train[folds != j,] %>%
      left_join(b_s, by = "spp") %>%
      group_by(forest_type) %>%
      summarize(b_f = mean(diam_growth - mu - b_s)*(n()/(n() + ls[i, 2])))
    
    b_l <- train[folds != j,] %>%
      left_join(b_s, by = "spp") %>%
      left_join(b_f, by = "forest_type") %>%
      group_by(landscape) %>%
      summarize(b_l = mean(diam_growth - mu - b_s - b_f)*(n()/(n() + ls[i, 3])))
    
    pred <- train[folds == j,] %>%
      left_join(b_s, by = "spp") %>%
      left_join(b_f, by = "forest_type") %>%
      left_join(b_l, by = "landscape") %>%
      mutate(b_s = if_else(is.na(b_s), 0, b_s),
             b_f = if_else(is.na(b_f), 0, b_f),
             b_l = if_else(is.na(b_l), 0, b_l),
             pred = mu + b_s + b_f + b_l)
    
    res[j] <- RMSE(train$diam_growth[folds == j], pred$pred)
  }
  
  rmses[i] <- mean(res)
}

reg_index <- which.min(rmses)


# Record results after each term addition ----------------------------------

results <- data.frame(model = "mu", 
                      RMSE = RMSE(train$diam_growth, mu))

b_s <- train %>% 
  group_by(spp) %>%
  summarize(b_s = mean(diam_growth - mu)*(n()/(n() + ls[reg_index, 1])))

pred <- train %>%
  left_join(b_s, by = "spp") %>%
  mutate(b_s = if_else(is.na(b_s), 0, b_s),
         pred = mu + b_s)

results <- bind_rows(results,
                     data.frame(model = "mu + bs", 
                                RMSE = RMSE(train$diam_growth,
                                            pred$pred)))

b_f <- train %>%
  left_join(b_s, by = "spp") %>%
  group_by(forest_type) %>%
  summarize(b_f = mean(diam_growth - mu - b_s)*(n()/(n() + ls[reg_index, 2])))

pred <- train %>%
  left_join(b_s, by = "spp") %>%
  left_join(b_f, by = "forest_type") %>%
  mutate(b_s = if_else(is.na(b_s), 0, b_s),
         b_f = if_else(is.na(b_f), 0, b_f),
         pred = mu + b_s + b_f)

results <- bind_rows(results,
                     data.frame(model = "mu + bs + bf", 
                                RMSE = RMSE(train$diam_growth,
                                            pred$pred)))

b_l <- train %>%
  left_join(b_s, by = "spp") %>%
  left_join(b_f, by = "forest_type") %>%
  group_by(landscape) %>%
  summarize(b_l = mean(diam_growth - mu - b_s - b_f)*(n()/(n() + ls[reg_index, 3])))

pred <- train %>%
  left_join(b_s, by = "spp") %>%
  left_join(b_f, by = "forest_type") %>%
  left_join(b_l, by = "landscape") %>%
  mutate(b_s = if_else(is.na(b_s), 0, b_s),
         b_f = if_else(is.na(b_f), 0, b_f),
         b_l = if_else(is.na(b_l), 0, b_l),
         pred = mu + b_s + b_f + b_l)

results <- bind_rows(results,
                     data.frame(model = "mu + bs + bf + bl", 
                                RMSE = RMSE(train$diam_growth,
                                            pred$pred)))


# New training set with residuals from naive model -------------------------

train2 <- train %>%
  mutate(resid = diam_growth - pred$pred) %>%
  select(-spp, -forest_type, -landscape, -county, -diam_growth) %>%
  select(resid, everything())


# Fit RF to training data --------------------------------------------------
# This may take up to an hour

set.seed(10)
fit_rf <- train(resid ~ ., data = train2, method = "Rborist", nTree = 200,
                tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 18, by = 4)))

# Final predictions on training data ---------------------------------------

pred <- train %>%
  left_join(b_s, by = "spp") %>%
  left_join(b_f, by = "forest_type") %>%
  left_join(b_l, by = "landscape") %>%
  mutate(b_s = if_else(is.na(b_s), 0, b_s),
         b_f = if_else(is.na(b_f), 0, b_f),
         b_l = if_else(is.na(b_l), 0, b_l),
         rf = predict(fit_rf, newdata = train),
         pred = mu + b_s + b_f + b_l + rf)

results <- bind_rows(results,
                     data.frame(model = "mu + bs + bf + bl + rf", 
                                RMSE = RMSE(train$diam_growth, pred$pred)))


# # RF without less important predictors -------------------------------------
# 
# varImp(fit_rf)$importance
# 
# train_minimal <- train2 %>%
#   select(-slope, -aspect, -ba, -stocking, -site_class, -crown_class)
# 
# 
# fit_rf_minimal <- train(resid ~ ., data = train_minimal, method = "Rborist", 
#                         nTree = 200,
#                         tuneGrid = 
#                           data.frame(predFixed = 2, minNode = seq(2, 18, by = 4)))
# 
# pred_minimal <- train %>%
#   left_join(b_s, by = "spp") %>%
#   left_join(b_f, by = "forest_type") %>%
#   left_join(b_l, by = "landscape") %>%
#   mutate(b_s = if_else(is.na(b_s), 0, b_s),
#          b_f = if_else(is.na(b_f), 0, b_f),
#          b_l = if_else(is.na(b_l), 0, b_l),
#          rf_m = predict(fit_rf_minimal, newdata = train),
#          pred = mu + b_s + b_f + b_l + rf_m)
# 
# results <- bind_rows(results,
#                      data.frame(model = "mu + bs + bf + bl + rf_min", 
#                                 RMSE = RMSE(train$diam_growth, pred_minimal$pred)))


# # Examine residuals of training data ---------------------------------------
# 
# outcomes <- pred %>%
#   mutate(actual = diam_growth,
#          predicted = pred,
#          naive = mean(diam_growth),
#          error = actual - predicted,
#          naive_error = actual - naive)
# 
# outcomes %>% ggplot(aes(error)) +
#   geom_density(bw = .008, fill = "dark gray")
# 
# outcomes %>% 
#   group_by(spp) %>%
#   mutate(xint = mean(error)) %>%
#   ungroup() %>%
#   ggplot(aes(error)) +
#   geom_density(bw = .008, fill = "dark gray") +
#   geom_vline(xintercept = 0, col = "#386cb0", size = 1) +
#   geom_vline(aes(xintercept = xint), col = "#bf5b17", size = 1) +
#   facet_wrap(~ spp, ncol = 4) +
#   xlab("error (in/yr)")
# 
# outcomes %>% ggplot(aes(naive_error)) +
#   geom_density(bw = .008, fill = "dark gray") +
#   scale_x_continuous(limits = c(-.1,.3))
# 
# View(top_n(outcomes %>% arrange(desc(error)), 30))
# 
# outcomes %>% ggplot(aes(diam_growth)) + geom_density()



##############################
# Estimate accuracy of final
##############################


pred_final <- test %>%
  left_join(b_s, by = "spp") %>%
  left_join(b_f, by = "forest_type") %>%
  left_join(b_l, by = "landscape") %>%
  mutate(b_s = if_else(is.na(b_s), 0, b_s),
         b_f = if_else(is.na(b_f), 0, b_f),
         b_l = if_else(is.na(b_l), 0, b_l),
         rf = predict(fit_rf, newdata = test),
         pred = mu + b_s + b_f + b_l + rf,
         error = diam_growth - pred)

RMSE_final <- RMSE(pred_final$diam_growth, pred_final$pred)
RMSE_final



# ##############################
# # Retrain on entire dataset
# ##############################
# 
# 
# b_s <- adk_fia %>% 
#   group_by(spp) %>%
#   summarize(b_s = mean(diam_growth - mu)*(n()/(n() + ls[reg_index, 1])))
# 
# b_f <- adk_fia %>%
#   left_join(b_s, by = "spp") %>%
#   group_by(forest_type) %>%
#   summarize(b_f = mean(diam_growth - mu - b_s)*(n()/(n() + ls[reg_index, 2])))
# 
# b_l <- train %>%
#   left_join(b_s, by = "spp") %>%
#   left_join(b_f, by = "forest_type") %>%
#   group_by(landscape) %>%
#   summarize(b_l = mean(diam_growth - mu - b_s - b_f)*(n()/(n() + ls[reg_index, 3])))
# 
# adk_resid <- adk_fia %>%
#   left_join(b_s, by = "spp") %>%
#   left_join(b_f, by = "forest_type") %>%
#   left_join(b_l, by = "landscape") %>%
#   mutate(b_s = if_else(is.na(b_s), 0, b_s),
#          b_f = if_else(is.na(b_f), 0, b_f),
#          b_l = if_else(is.na(b_l), 0, b_l),
#          pred = mu + b_s + b_f + b_l,
#          resid = diam_growth - pred) %>%
#   select(-spp, -forest_type, -landscape, -county, -diam_growth,
#          -pred, -b_s, -b_f, -b_l) %>%
#   select(resid, everything())
# 
# x <- adk_resid %>% select(-resid)
# y <- adk_resid$resid
# 
# fit_rf_final <- Rborist(x, y, nTree = 200, 
#                         predFixed = fit_rf$bestTune$predFixed, 
#                         minNode = fit_rf$bestTune$minNode)
