###### The utility of terrestrial leeches as indicator species varies with rainfall

library(tidyverse)
library(vegan)

## Preparing Leech data from Nelaballi et al. 2022 
## https://github.com/andrewjohnmarshall/leech_distribution

#read csv - leech data from Nelaballi et al. 2022
l <- read.csv(file = "data/leech_tidied_data.csv", header = TRUE)

#dropping first column 
l <- l[,-1]

#dropping the numbers preceding entries in the FT column
l$FT <- gsub("^.{0,2}", "", l$FT)

#ordering forest type (FT) and forest type partition (partition) columns
l$FT <- factor(l$FT, levels = c("PS", "FS", "AB", "LS", "LG", "UG", "MO"))
l$partition <- factor(l$partition, levels = c("PS.I", "FS.I", "AB.I", "AB.II", "LS.I", "LS.II", "LG.I", 
                                              "LG.II", "UG.I", "UG.II", "MO.I", "MO.II", "MO.III"))

#changing Date column type to date (from character)
l$Date <- as.Date(l$Date)

#eight observations (potentially from same transect) have missing Date values. Removing for now
l[is.na(l$Date) == TRUE,]
l <- l[is.na(l$Date) == FALSE,]

#setting colors for plotting based on Nelaballi et al. (2021)
H.picta.col <- "orange"
H.zeylanica.col <- "brown"


####### plotting raw leech counts forest type and by species #############

#total counts
sum(l$H.picta)                      #10,610
sum(l$H.zeylanica)                  #5,449
sum(l$H.picta) + sum(l$H.zeylanica) #16,059

l %>%
  group_by(FT) %>%
  summarise(H.picta = sum(H.picta),
            H.zeylanica = sum(H.zeylanica)) %>%
  pivot_longer(cols = c(H.picta, H.zeylanica),
               names_to = "species",
               values_to = "obs") %>%
  ggplot(., aes(x = FT, y = obs, fill = species)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  coord_flip() +
  theme_classic() +
  scale_fill_manual(name = "Leech species (total n)", 
                    values = c("H.picta" = H.picta.col, "H.zeylanica" = H.zeylanica.col),
                    labels = c(bquote(italic("H. picta") ~ "(" * .(formatC(sum(l$H.picta), format = "d", big.mark = ",")) * ")"),
                               bquote(italic("H. zeylanica") ~ "(" * .(formatC(sum(l$H.zeylanica), format = "d", big.mark = ",")) * ")"))) +
  labs(x = "forest type", y = "n leeches") + 
  theme(legend.position = c(0.8, 0.7)) + 
  scale_x_discrete(
    labels = c("MO" = "Montane",
               "UG" = "Upland\ngranite",
               "LG" = "Lowland\ngranite",
               "LS" = "Lowland\nsandstone",
               "AB" = "Alluvial\nbench",
               "FS" = "Freshwater\nswamp",
               "PS" = "Peat\nswamp"))

#ggsave("LeechCountsByFT.jpg", plot = last_plot(), width = 6, height = 4, dpi = 350, device = "jpeg")

####### plotting leech count rates per meter by forest type and by species #############

#leech count rate (n/m) by forest type by species (forest types listed in order by descending elevation)
l %>%
  group_by(FT) %>%
  summarise(H.picta = sum(H.picta)/sum(Distance),
            H.zeylanica = sum(H.zeylanica)/sum(Distance)) %>%
  pivot_longer(cols = c(H.picta, H.zeylanica),
               names_to = "species",
               values_to = "obs") %>%
  ggplot(., aes(x = FT, y = obs, fill = species)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("H.picta" = H.picta.col, "H.zeylanica" = H.zeylanica.col)) +
  labs(title = "Haemadipsa spp. Leech Counts at Cabang Panti", subtitle = "Nov 2012 - Sep 2020", 
       x = "forest type", y = "counts per meter") +
  theme(legend.position = c(0.8, 0.77))

#leech count rate by forest type (same as above, but stacked bar plot)
l %>%
  group_by(FT) %>%
  summarise(H.picta = sum(H.picta)/sum(Distance),
            H.zeylanica = sum(H.zeylanica)/sum(Distance)) %>%
  pivot_longer(cols = c(H.picta, H.zeylanica),
               names_to = "species",
               values_to = "obs") %>%
  ggplot(aes(x = FT, y = obs, fill = species)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("H.picta" = H.picta.col, "H.zeylanica" = H.zeylanica.col)) +
  labs(title = "Haemadipsa spp. Leech Counts at Cabang Panti", subtitle = "Nov 2012 - Sep 2020", 
       x = "forest type", y = "counts per meter") +
  theme(legend.position = c(0.8, 0.7))

#leech count rate by forest type partition
l %>%
  group_by(partition) %>%
  summarise(H.picta = sum(H.picta)/sum(Distance),
            H.zeylanica = sum(H.zeylanica)/sum(Distance)) %>%
  pivot_longer(cols = c(H.picta, H.zeylanica),
               names_to = "species",
               values_to = "obs") %>%
  ggplot(., aes(x = partition, y = obs, fill = species)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("H.picta" = H.picta.col, "H.zeylanica" = H.zeylanica.col)) +
  labs(title = "Haemadipsa spp. Leech Counts at Cabang Panti", subtitle = "Nov 2012 - Sep 2020", 
       x = "forest type partition", y = "counts per meter") +
  theme(legend.position = c(0.8, 0.77))


#######  calculating leech counts by 30-day sampling periods  #######

#data range in the original leech data - 9 Nov 2012 - 28 Sep 2020
#camera trap data study period is Jul 2015 - Sep 2020)
range(l$Date)

#read in sampling period file that lists 30-day sampling blocks and associated dates
sp <- read.csv(file = "data/30-daysamplingperiods-byday.csv", header = TRUE)
sp$date <- as.Date(sp$date)
head(sp)

#read in a similar file showing sampling period date ranges
sp2 <- read.csv(file = "data/30-day-samplingperiods.csv", header = TRUE)
head(sp2)

#adding sampling period data to leech table
names(l)[names(l) == 'Date'] <- 'date'
l <- left_join(l, sp[,c("date", "samplingperiod")], by = "date")

#removing rows with NA in 'samplingperiod' column, removing leech data with dates outside CT survey period
l <- l[is.na(l$samplingperiod) == FALSE, ]


######  secular trends in leech counts  #######

#plotting count rates for both species over whole study area and period 
l %>% 
  group_by(samplingperiod) %>%
  summarise(H.picta.per.m = sum(H.picta)/sum(Distance),
            H.zeylanica.per.m = sum(H.zeylanica)/sum(Distance)) %>%
  ggplot() +
  geom_line(aes(x = samplingperiod, y = H.picta.per.m, color = "H. picta"), linewidth = 1.5) +
  geom_line(aes(x = samplingperiod, y = H.zeylanica.per.m, color = "H. zeylanica"), linewidth = 1.5) +
  labs(title = "Secular Trends of Haemadipsa Leech Counts at CPRS", subtitle = "Jul 2015 - Sep 2020",
       x = "sampling period", y = "count/m") +
  theme_classic() +
  scale_color_manual(name = "species", values = c("H. picta" = H.picta.col, "H. zeylanica" = H.zeylanica.col)) +
  theme(legend.position = c(0.8, 0.77)) +
  guides(color = guide_legend(title = NULL, label.theme = element_text(face = "italic")))

#plotting sampling effort across study period based on distance of transects walked
l %>%
  group_by(samplingperiod) %>%
  summarise(sampling.effort = sum(Distance)/1000) %>%
  ggplot(aes(x = samplingperiod, y = sampling.effort)) +
  geom_line(size = 1.5) +
  theme_minimal() +
  labs(title = "Leech Sampling Effort", subtitle = "Jul 2015 - Sep 2020",
       x = "sampling period", y = "transect distance walked (km)")


#######  creating leech count rate tables for use in subsequent mammal modeling  ############

#creating 'leech count per meter' (count/m) values for both leech species by sampling period by forest type
n_per_m_ft <- l %>%
  group_by(samplingperiod, FT) %>%
  summarise(H.picta.per.m = sum(H.picta)/sum(Distance),
            H.zeylanica.per.m = sum(H.zeylanica)/sum(Distance))
#adding sampling period date information 
n_per_m_ft <- left_join(n_per_m_ft, sp2, by = "samplingperiod")
head(n_per_m_ft)

#does this make sense? 64 sampling periods X 7 forest types = 448. 1 is missing
dim(n_per_m_ft)
#FS sampling period 60 is missing
table(n_per_m_ft$samplingperiod, n_per_m_ft$FT)

#creating count/m values for both species by sampling period by partition - combining MO.I & MO.II
n_per_m_part <- l %>%
  mutate(partition = if_else(partition == "MO.II", "MO.I", partition)) %>%
  group_by(samplingperiod, partition) %>%
  summarise(H.picta.per.m = sum(H.picta)/sum(Distance),
            H.zeylanica.per.m = sum(H.zeylanica)/sum(Distance))
#adding sampling period date information 
n_per_m_part <- left_join(n_per_m_part, sp2, by = "samplingperiod")
head(n_per_m_part)

#does this make sense? 64 sampling periods X 12 forest type partitions = 768. Four are missing.
table(n_per_m_part$samplingperiod, n_per_m_part$partition)
#sp60 is missing for AB1, FS1, LG1, and LS1


#plotting secular trends of both leech species by forest type
ggplot(data = n_per_m_ft) +
  geom_line(aes(x = samplingperiod, y = H.picta.per.m, color = "H. picta"), size = 1.25) + 
  geom_line(aes(x = samplingperiod, y = H.zeylanica.per.m, color = "H. zeylanica"), size = 1.25) + 
  facet_wrap(~fct_rev(FT), ncol = 1) +
  labs(title = "Secular Trends of Leech Counts at CPRS by Forest Type", subtitle = "Jul 2015 - Sep 2020",
       x = "sampling period", y = "count/m") +
  theme_minimal() +
  scale_color_manual(name = "", values = c("H. picta" = H.picta.col, "H. zeylanica" = H.zeylanica.col)) +
  theme(legend.position = c(0.8, 0.725)) +
  guides(color = guide_legend(label.theme = element_text(face = "italic")))


#######################################################################################################################
###### Exploring camera trap data of mammals at CPRS (Jul 2015 - Sep 2020)

#reading in camera trap observation data - all species 
d <- read.csv(file = "data/ofp_videos-2021-04-20.csv")
head(d)

#reading in camera trap deployment data - forest types and partitions
#fixing errors in ct file 
ct <- read.csv(file = "data/cameradata_updatedZJ-ajm.csv", header = TRUE)
ct$habitat[ct$location == "TL 26 S 40"] <- "Lowland Granite"
ct$latitude[ct$locationID == 342] <- "-1.2153447"
ct$longitude[ct$locationID == 342] <- "110.1277471"

ftp <- read.csv(file = "data/CTlocations_partitions.csv", header = TRUE) #partitions

ct <- left_join(ct[, -c(11,12)], ftp[,c("locationID","partition","on_off_trail")], 
                  by = "locationID")

#selecting only mammals
#also, column Project.ID is all 'OFP', Class all 'Mammalia', Blank all FALSE, 
#and Group.Size, Age, and Sex columns are empty, so dropping these columns here
table(d$Class)
d <- d[d$Class == "Mammalia", !names(d) %in% c("Project.ID", "Class", "Group.Size", "Age", "Sex", "Blank")]
head(d)

#create 'date' column from the Date_Time column
d$date <- as.Date(gsub('.{6}$', '', d$Date_Time.Captured))
range(d$date)

#changing 'locationID' and on_off_trail column names to match that in the CT table
names(d)[names(d) == 'Deployment.Location.ID'] <- 'locationID'
names(ct)[names(ct) == 'on_off_trail'] <- 'on.off.trail'


#adding camera trap location data, including forest types (habitat) and partitions
d <- left_join(d, ct[!duplicated(ct$locationID),
                     c("locationID", "location", "latitude", "longitude", 
                       "habitat","partition", "altitude", "on.off.trail")],
               by = "locationID")

#ordering forest types and partitions
d$habitat <- factor(d$habitat, levels = c("Peat Swamp", "Freshwater Swamp", "Alluvial Bench", 
                                          "Logged Alluvial Bench", "Kerangas", "Lowland Sandstone", 
                                          "Lowland Granite", "Upland Granite", "Montane"))

d$partition <- factor(d$partition, levels = c("PS1", "FS1", "AB1", "AB2", "Logged Alluvial Bench",
                                           "Kerangas", "LS1", "LS2", "LG1", "LG2", "UG1", "UG2",
                                          "MO1", "MO2"))

#adding sampling period data
d <- left_join(d, sp, by = "date")

#excluding observations outside of the sampling period dates
d <- d[is.na(d$samplingperiod) == FALSE, ]
head(d)

### how many observations by genus and by species do we have? 

#by genus - mostly humans, then Sus, rats, Tragulus, & Macaca
d %>%
  count(Genus, sort = TRUE)

#excluding humans, the top 5 most observed species account for 64% of all mammal observations (15,464/24,153)
#they are: S.barbatus (bearded pig), Un-ID'd rats, T.javanicus (Java mouse-deer), 
#M.nemestrina (Pig-tailed macaque), and V.tangalunga (Malayan Civet)
d %>%
  filter(Genus != "Homo") %>%
  count(Species, sort = TRUE)

#some entries are missing Genus - all are 'unidentified rats' (Muridae family) 
d[d$Genus == "",]
table(d$Species[d$Genus == ""])

#what to do with 'Confused Squirrel or Treeshrew' (17 obs) - observer seemingly couldn't ID to family?
table(d$Family)
d[d$Family == "Confused Squirrel or Treeshrew",]

##################################################################################################################
### add CT survey effort (# of CT days) and filter repeat mammal observations 
#using Andy's code from 'vert_detections.Rmd'

#pruning data first
#removing forest types not used in analysis - kerangas (heath forest) and logged alluvial bench
#removing repeat observations by same location in the same hour, keeping observation with most individuals
pruned <- d %>%
  filter(Genus != "Homo") %>%
  filter(!habitat %in% c("Kerangas", "Logged Alluvial Bench")) %>%
  mutate(day = date(Date_Time.Captured),
         hour = hour(Date_Time.Captured)) %>%
  group_by(Species, locationID, day, hour) %>% 
  filter(Number.of.Animals == max(Number.of.Animals)) %>% 
  slice(1)

#building camera trap table - adding survey effort 
deployments <- read.csv("data/ofp_deployments-2021-11-04.csv")

#selecting variables of interest and changing column names to match other tables
CTtable <-  deployments %>% 
  select(Location, Deployment.ID, Deployment.Location.ID, Treatment.Strata, Latitude, Longitude, 
         Camera.Deployment.Begin.Date, Camera.Deployment.End.Date, X..days.active, Feature.Type) %>% 
  rename(placementID = Deployment.ID,
         locationID   = Deployment.Location.ID,
         start     = Camera.Deployment.Begin.Date,
         stop      = Camera.Deployment.End.Date,
         habitat   = Treatment.Strata,
         days.active  = X..days.active,
         on.off.trail = Feature.Type) %>% 
  arrange(locationID, placementID)

#creating a column for number of active CT days for each CT location
CTtable$CT.days <- as.numeric(difftime(as.Date(CTtable$stop), as.Date(CTtable$start), units = "days"))

#fixing errors
CTtable$habitat[CTtable$Location == "TL 26 S 40"] <- "Lowland Granite"
CTtable$Latitude[CTtable$locationID == 342] <- "-1.2153447"
CTtable$Longitude[CTtable$locationID == 342] <- "110.1277471"

#active CT days by habitat
#of the forest types in our analyses (all but Kerangas and LAB) peat swamps were surveyed the least
CTtable %>%
  group_by(habitat) %>%
  summarise(CT.days = sum(CT.days))

#active CT days by CT location
ActDays_Summary <- CTtable %>%
  filter(!(habitat %in% c("Kerangas","Logged Alluvial Bench"))) %>%
  group_by(locationID) %>%
  summarise(CT.days = sum(CT.days)) %>%
  arrange(desc(CT.days)) %>%
  print(n = Inf)

#summary stats of camera trap days per location ID
mean(ActDays_Summary$CT.days)
sd(ActDays_Summary$CT.days)
range(ActDays_Summary$CT.days)
hist(ActDays_Summary$CT.days, breaks = 50)

#177 unique ct placement sites in our study area
length(unique(CTtable$locationID[!CTtable$habitat %in% c("Kerangas","Logged Alluvial Bench")]))

#fixing 'Number.of.Animals' column - making numeric 
pruned$Number.of.Animals <- as.numeric(pruned$Number.of.Animals)
pruned$Number.of.Animals[pruned$Number.of.Animals == "NULL"] <- 0
hist(pruned$Number.of.Animals)

#adding CT locations covariate data (from Andrew Marshall) to pruned mammal observation data 
#total rainfall, max. and min. temps, and amount of ripe fruit per CT location per sampling period
covs <- read.csv(file = "data/site.covariates.bysurvey.all.formatted.csv", header = TRUE)
head(covs)

covs$habitat_7 <- factor(covs$habitat_7, levels = c("Peat Swamp", "Freshwater Swamp", "Alluvial Bench", 
                                                    "Logged Alluvial Bench", "Kerangas", "Lowland Sandstone", 
                                                    "Lowland Granite", "Upland Granite", "Montane"))

pruned <- left_join(pruned, covs[,c("locationID", "samplingperiod", "z_rain", "z_max", 
                          "z_min", "z_pheno", "z_elev", "z_elev_sq")],
          by = c("locationID", "samplingperiod"))

## adding leech data to pruned mammal data 
#by FT
n_per_m_ft <- n_per_m_ft %>%
  rename(habitat = FT) %>%
  mutate(habitat = case_match(habitat, 
                          "PS" ~ "Peat Swamp",
                          "FS" ~ "Freshwater Swamp",
                          "AB" ~ "Alluvial Bench",
                          "LS" ~ "Lowland Sandstone",
                          "LG" ~ "Lowland Granite",
                          "UG" ~ "Upland Granite",
                          "MO" ~ "Montane"))

#using custom scaling function - Gelman et al. (2021)
scale2 <- function(x){
  (x - mean(x))/(2 * sd(x))
}

scale2.na <- function(x){
  (x - mean(x, na.rm = TRUE))/(2 * sd(x, na.rm = TRUE))
}

n_per_m_ft$z_picta <- scale2(n_per_m_ft$H.picta.per.m)
n_per_m_ft$z_zeylanica <- scale2(n_per_m_ft$H.zeylanica.per.m)

pruned <- left_join(pruned, n_per_m_ft[,c("samplingperiod", "habitat", "z_picta", "z_zeylanica")],
          by = c("samplingperiod", "habitat"))

#by partition
n_per_m_part <- n_per_m_part %>%
  mutate(partition = case_match(partition, 
                              "PS.I" ~ "PS1",
                              "FS.I" ~ "FS1",
                              "AB.I" ~ "AB1",
                              "AB.II" ~ "AB2",
                              "LS.I" ~ "LS1",
                              "LS.II" ~ "LS2",
                              "LG.I" ~ "LG1",
                              "LG.II" ~ "LG2",
                              "UG.I" ~ "UG1",
                              "UG.II" ~ "UG2",
                              "MO.I" ~ "MO1",
                              #"MO.II" ~ "MO1",
                              "MO.III" ~ "MO2"))

n_per_m_part$z_picta_part <- scale2(n_per_m_part$H.picta.per.m)
n_per_m_part$z_zeylanica_part <- scale2(n_per_m_part$H.zeylanica.per.m)

pruned <- left_join(pruned, n_per_m_part[,c("samplingperiod", "partition", "z_picta_part", "z_zeylanica_part")],
          by = c("samplingperiod", "partition"))

### adding active CT days per locationID per sampling period (to eventually control for survey effort)
active_days <- CTtable %>%
  mutate(start_date = start, end_date = stop) %>%
  cross_join(sp2) %>%
  filter(start_date.x <= end_date.y & end_date.x >= start_date.y) %>%
  group_by(locationID, samplingperiod) %>%
  summarise(active.days = sum(as.numeric(pmin(as.Date(end_date.y), as.Date(end_date.x)) - 
                                pmax(as.Date(start_date.y), as.Date(start_date.x)) + 1), 
                              na.rm = TRUE))

#locationID 169 has been changed to 176 by AJM (both were close and active during the same exact period)
#so need to divide locationID 176 active days by 2
active_days$active.days[active_days$locationID == 176] <- active_days$active.days[active_days$locationID == 176]/2

#add active days data to pruned observation table
pruned <- left_join(pruned, active_days, by = c("locationID", "samplingperiod"))

#and create a column of animal observations controlling for survey effort
pruned$n.per.ad <- pruned$Number.of.Animals/pruned$active.days

#fixing values in the lat and long columns that have S or E instead of a negative sign
pruned$latitude <- gsub("S 0", "-", trimws(gsub("°", "", pruned$latitude)))
pruned$latitude <- as.numeric(pruned$latitude)

pruned$longitude <- gsub("E ", "", trimws(gsub("°", "", pruned$longitude)))
pruned$longitude <- as.numeric(pruned$longitude)

#creating a summary table showing number of animals by species by CT location
mammals_by_loc <- pruned %>%
  group_by(locationID) %>%
  summarise(n.all.ind = sum(na.omit(Number.of.Animals)))

mammals_by_loc <- pruned %>%
  group_by(locationID, Species) %>%
  summarise(n.all.ind = sum(Number.of.Animals)) %>%
  pivot_wider(names_from = Species, values_from = n.all.ind, values_fill = 0) %>%
  left_join(mammals_by_loc, by = "locationID") %>%
  left_join(CTtable[!duplicated(CTtable$locationID), c("locationID", "Latitude", "Longitude")], 
            by = "locationID") %>%
  select("locationID", "Latitude", "Longitude", "n.all.ind", everything())

##########################################################################################################
## mammal data visualization
library(corrplot)

pruned$habitat <- factor(pruned$habitat,
                        levels = c("Peat Swamp", "Freshwater Swamp", "Alluvial Bench",
                                "Lowland Sandstone", "Lowland Granite", "Upland Granite", "Montane"))

pruned$partition <- factor(pruned$partition, levels = c("PS1", "FS1", "AB1", "AB2", "LS1", "LS2", 
                                                        "LG1", "LG2", "UG1", "UG2", "MO1", "MO2"))

#active CT days by sampling period - CT survey effort variation over the study period
ad_sp <- active_days %>%
  group_by(samplingperiod) %>%
  summarise(ad = sum(active.days))

ad_sp %>%
  ggplot(aes(x = samplingperiod, y = ad)) +
  geom_line(lwd = 1.25) + 
  labs(title = "Camera Trap Survey Effort Across Study Period", 
       x = "sampling period", y = "total CT days") + 
  theme_minimal()

#number of unique CT locations active during each sampling period
active_days %>%
  group_by(samplingperiod) %>%
  summarise(n.CT = n_distinct(locationID)) %>%
  ggplot(aes(x = samplingperiod, y = n.CT)) +
  geom_line(lwd = 1.25) + 
  labs(title = "Camera Trap Survey Effort Across Study Period", 
       x = "sampling period", y = "# active CT's") + 
  theme_minimal()

#all mammal observations (number of individuals) over study period, controlling for CT survey effort
pruned %>%
  group_by(samplingperiod) %>%
  summarise(total.n = sum(na.omit(Number.of.Animals))) %>%
  left_join(ad_sp, by = "samplingperiod") %>%
  mutate(n.per.ad = total.n/ad) %>%
  ggplot(aes(x = samplingperiod, y = n.per.ad)) +
  geom_line(lwd = 1.25) + 
  labs(title = "All Mammal Observations (n individuals)", 
       x = "sampling period", y = "# individuals / CT days") + 
  theme_minimal()

#all mammal observations (number of videos) over study period, controlling for CT survey effort
pruned %>%
  group_by(samplingperiod) %>%
  summarise(total.obs = n_distinct(Image.ID)) %>%
  left_join(ad_sp, by = "samplingperiod") %>%
  mutate(obs.per.ad = total.obs/ad) %>%
  ggplot(aes(x = samplingperiod, y = obs.per.ad)) +
  geom_line(lwd = 1.25) + 
  labs(title = "All Mammal Observations (n videos)", 
       x = "sampling period", y = "# videos / CT days") + 
  theme_minimal()

#top mammal species observations over time (all have over 500 observations)
pruned %>%
  filter(Species %in% c("Sus barbatus", "Unid Rat", "Tragulus javanicus", 
                        "Macaca nemestrina", "Viverra tangalunga", "Muntiacus muntjak",
                        "Hystrix brachyura", "Trichys fasciculata", "Hemigalus derbyanus",
                        "Muntiacus atherodes", "Lariscus insignis", "Neofelis nebulosa")) %>%
  group_by(Species, samplingperiod) %>%
  summarise(total.n = sum(na.omit(Number.of.Animals))) %>%
  left_join(ad_sp, by = "samplingperiod") %>%
  mutate(n.per.ad = total.n/ad) %>%
  ggplot(aes(x = samplingperiod, y = n.per.ad)) +
  geom_line(lwd = 1.25) +
  facet_wrap(~Species, ncol = 2) + 
  labs(title = "Mammal Species Observations - n individuals (n videos > 500)", 
       x = "sampling period", y = "# individuals / CT days") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "italic"))

#top mammal species observations over time, w/ free scales
pruned %>%
  filter(Species %in% c("Sus barbatus", "Unid Rat", "Tragulus javanicus", 
                        "Macaca nemestrina", "Viverra tangalunga", "Muntiacus muntjak",
                        "Hystrix brachyura", "Trichys fasciculata", "Hemigalus derbyanus",
                        "Muntiacus atherodes", "Lariscus insignis", "Neofelis nebulosa")) %>%
  group_by(Species, samplingperiod) %>%
  summarise(total.n = sum(na.omit(Number.of.Animals))) %>%
  left_join(ad_sp, by = "samplingperiod") %>%
  mutate(n.per.ad = total.n/ad) %>%
  ggplot(aes(x = samplingperiod, y = n.per.ad)) +
  geom_line(lwd = 1.25) +
  facet_wrap(~Species, ncol = 2, scales = "free") + 
  labs(title = "Mammal Species Observations - n individuals (n videos > 500)", 
       x = "sampling period", y = "# individuals / CT days") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "italic"))

#average number of individuals in CT videos by species over study period
pruned %>%
  group_by(Species) %>%
  summarise(av.no.ind = mean(Number.of.Animals)) %>%
  arrange(desc(av.no.ind)) %>%
  print(n = 59)

#all mammal observations by forest type over study period
pruned %>%
  group_by(samplingperiod, habitat) %>%
  summarise(total.n = sum(na.omit(Number.of.Animals))) %>%
  left_join(ad_sp, by = "samplingperiod") %>%
  mutate(n.per.ad = total.n/ad) %>%
  ggplot(aes(x = samplingperiod, y = n.per.ad)) +
  geom_line(lwd = 1.25) +
  facet_wrap(~fct_rev(habitat), ncol = 1) + 
  theme_minimal()

#mammal species observations by forest type over study period, for 7 top species by observations
pruned %>%
  filter(Species %in% c("Sus barbatus", "Unid Rat", "Tragulus javanicus", 
                        "Macaca nemestrina", "Viverra tangalunga", "Muntiacus muntjak",
                        "Hystrix brachyura")) %>%
  group_by(habitat, Species) %>%
  summarise(total.n = sum(na.omit(Number.of.Animals))) %>%
  ggplot(aes(fill = reorder(Species, -total.n), y = total.n, x = habitat)) +
  geom_bar(position = "stack", stat = "identity") + 
  coord_flip() +
  labs(title = "Mammal Species Observations by Forest Type", x = "", y = "# individuals/CT days") +
  theme_minimal() +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank(),  
    legend.text = element_text(face = "italic"))

#species co-occurence plot for top species by observations
M <- cor(na.omit(mammals_by_loc[,c("Sus barbatus", "Unid Rat", "Tragulus javanicus", 
                                   "Macaca nemestrina", "Viverra tangalunga", "Muntiacus muntjak",
                                   "Hystrix brachyura", "Trichys fasciculata", "Hemigalus derbyanus",
                                   "Muntiacus atherodes", "Lariscus insignis", "Neofelis nebulosa",
                                   "Tragulus napu", "Rheithrosciurus macrotis", "Martes flavigula")]))
corrplot(M, method = "color", 
         type = "upper", 
         order = "hclust",
         tl.col = "black", tl.srt = 45, tl.cex = 0.6,
         diag = FALSE)

#species richness by FT - calculated by CT location (not controlling for CT survey effort)
pruned %>%
  group_by(locationID) %>%
  summarise(Rich = length(unique(Species))) %>%
  left_join(., CTtable[!duplicated(CTtable$locationID), 
                       c("locationID", "habitat")], by = "locationID") %>%
  mutate(habitat = factor(habitat,
                          levels = c("Peat Swamp", "Freshwater Swamp", "Alluvial Bench",
                                     "Lowland Sandstone", "Lowland Granite", "Upland Granite", "Montane"))) %>%
  ggplot(aes(x = habitat, y = Rich)) + 
  geom_boxplot() +
  geom_jitter(width = 0.15) +
  coord_flip() + 
  theme_classic() +
  labs(title = "Mammal Species Richness by Forest Type",
       x = "", y = "number of species observed")

#species richness by sampling period by FT (not controlling for CT survey effort)
pruned %>%
  group_by(samplingperiod, habitat) %>%
  summarise(Rich = length(unique(Species))) %>%
  ggplot(aes(x = samplingperiod, y = Rich)) + 
  geom_line(linewidth = 1.25) +
  theme_minimal() +
  labs(title = "Mammal Species Richness Across Study Period by Forest Type",
       x = "sampling period", y = "n species observed") + 
  facet_wrap(~fct_rev(habitat), ncol = 1)

#species diversity by forest type (not controlling for CT survey effort)
shannon_index <- diversity(table(pruned$locationID, pruned$Species), index = "shannon")
shannon_index <- rownames_to_column(as.data.frame(shannon_index), var = "locationID")
shannon_index$locationID <- as.integer(shannon_index$locationID)

shannon_index %>%
  left_join(., CTtable[!duplicated(CTtable$locationID), 
                       c("locationID", "habitat")], by = "locationID") %>%
  mutate(habitat = factor(habitat,
                          levels = c("Peat Swamp", "Freshwater Swamp", "Alluvial Bench",
                                     "Lowland Sandstone", "Lowland Granite", "Upland Granite", "Montane"))) %>%
  ggplot(aes(x = habitat, y = shannon_index)) +
  geom_boxplot() +
  geom_jitter(width = 0.15) +
  coord_flip() + 
  theme_classic() +
  labs(title = "Mammal Species Diversity by Forest Type",
       x = "", y = "Shannon Diversity Index")

#species diversity by sampling period (not controlling for CT survey effort)
shannon_index2 <- diversity(table(pruned$samplingperiod, pruned$Species), index = "shannon")
shannon_index2 <- rownames_to_column(as.data.frame(shannon_index2), var = "samplingperiod")
shannon_index2$samplingperiod <- as.integer(shannon_index2$samplingperiod)

shannon_index2 %>%
  ggplot(aes(x = samplingperiod, y = shannon_index2)) +
  geom_line(linewidth = 1.25) +
  theme_minimal() +
  labs(title = "Mammal Species Diversity Across Study Period",
       x = "sampling period", y = "Shannon Diversity Index")

#how many unique on- and off-trail CT locations are in the study area?
table(CTtable$on.off.trail[!duplicated(CTtable$locationID) & 
                       CTtable$habitat != c("Kerangas","Logged Alluvial Bench")])

#summary table for species obs (n individuals) by forest type, with total and cumulative total columns
mammal_table <- pruned %>%
  group_by(habitat, Species) %>%
  summarise(n_obs = sum(Number.of.Animals)) %>%
  pivot_wider(names_from = habitat, values_from = n_obs) %>%
  ungroup() %>%
  mutate(total = rowSums(select(., -Species), na.rm = TRUE),
         percentage_of_total = round((total / sum(total)) * 100, 2)) %>%
  arrange(desc(total)) %>%
  mutate(cumulative_percentage = round(cumsum(percentage_of_total), 2)) %>%
  print()
