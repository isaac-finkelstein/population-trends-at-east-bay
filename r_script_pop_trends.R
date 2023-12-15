## looking into the population trends at East Bay
library(ggplot2)
library(tidyverse)
library(dplyr)


species_log<-read.csv("raw_data/specieslog_EBM_allyears.csv")
obs_hours <- read.csv("raw_data/specieslog_observer_Effort_EBM_allyears.csv")
#filter out the "Number of Observers" column and the year 2020 (no data because of pandemic)
filtered_obs_hours <- obs_hours %>%
  filter(log_type != "Number of Observers",
         obsYear != 2020)

#filter for just SNGO
species_log_sngo <- subset(species_log, species_code== "SNGO")
#the species log goes back to 1997, but the obs hours only goes back to 2004, also 2020 has no data (because of the pandemic)
filtered_species_log_sngo <- species_log_sngo %>%
  filter(obsYear >= 2004 & obsYear != 2020)

#sum of snow geese per year
sngo_sums <- rowSums(filtered_species_log_sngo [, 5:72], na.rm = TRUE)
filtered_species_log_sngo$total_sum<-sngo_sums
#I calculated by hand a few rows by hand and yes, it worked.

#sum of effort hours per year
hours_sums <- rowSums(filtered_obs_hours[, 4:71], na.rm = TRUE)
filtered_obs_hours$total_sum<-hours_sums
#Yes, row 1 and 2 are correct

#now divide by the sum of snow geese per row by the sum of obs hours per row
#I multiply by 8 to make it number of snow geese per 8 hours -- following indices from lit
#I don't think this matters though.
sngo_indice <- (filtered_species_log_sngo$total_sum/filtered_obs_hours$total_sum)*8
filtered_species_log_sngo$sngo_indice <- sngo_indice

#snow goose indice
sngo_indice <- (filtered_species_log_sngo$total_sum/filtered_obs_hours$total_sum)*8
filtered_species_log_sngo$sngo_indice <- sngo_indice


#Now lets do Brant
#filter for just BRAN
species_log_bran <- subset(species_log, species_code== "BRAN")
filtered_species_log_bran <- species_log_bran %>%
  filter(obsYear >= 2004 & obsYear != 2020)

#sum of Brant per year
bran_sums <- rowSums(filtered_species_log_bran [, 5:72], na.rm = TRUE)
filtered_species_log_bran$total_sum<-bran_sums

#brant indice
bran_indice <- (filtered_species_log_bran$total_sum/filtered_obs_hours$total_sum)*8
filtered_species_log_bran$bran_indice <- bran_indice


#Now lets do Canada Goose
#filter for just CANG
species_log_cang <- subset(species_log, species_code== "CANG")
filtered_species_log_cang <- species_log_cang %>%
  filter(obsYear >= 2004 & obsYear != 2020)

#sum of Canada goose per year
cang_sums <- rowSums(filtered_species_log_cang [, 5:72], na.rm = TRUE)
filtered_species_log_cang$total_sum<-cang_sums

#Canada goose indice
cang_indice <- (filtered_species_log_cang$total_sum/filtered_obs_hours$total_sum)*8
filtered_species_log_cang$cang_indice <- cang_indice

#now let's do CACG
#filter for just CACG
species_log_cacg <- subset(species_log, species_code== "CACG")
#the species log goes back to 1997, but the obs hours only goes back to 2004, also 2020 has no data (because of the pandemic)
filtered_species_log_cacg <- species_log_cacg %>%
  filter(obsYear >= 2004 & obsYear != 2020)
#sum of cackling geese per year
cacg_sums <- rowSums(filtered_species_log_cacg [, 5:72], na.rm = TRUE)
filtered_species_log_cacg$total_sum<-cacg_sums
#I calculated by hand a few rows by hand and yes, it worked.

#Because cacg is only back to 2013, I need to filter out years before 2013
filtered_obs_hours_cacg <- filtered_obs_hours %>%
  filter(obsYear >= 2013 & obsYear != 2020)

#cackling goose indice
cacg_indice <- (filtered_species_log_cacg$total_sum/filtered_obs_hours_cacg$total_sum)*8
filtered_species_log_cacg$cacg_indice <- cacg_indice


# Visualize this -- just cackling geese
ggplot(filtered_species_log_cacg, aes(x = obsYear, y = cacg_indice)) +
  geom_point() +
  geom_line() +
  labs(title = "Cackling Goose Indice Over Years",
       x = "Year",
       y = "Cackling Goose Indice")

# Visualize this -- just snow geese
ggplot(filtered_species_log_sngo, aes(x = obsYear, y = sngo_indice)) +
  geom_point() +
  geom_line() +
  labs(title = "Snow Goose Indice Over Years",
       x = "Year",
       y = "Snow Goose Indice")

# Visualize this -- just canada geese
ggplot(filtered_species_log_cang, aes(x = obsYear, y = cang_indice)) +
  geom_point() +
  geom_line() +
  labs(title = "Canada Goose Indice Over Years",
       x = "Year",
       y = "Canada Goose Indice")

# Visualize this -- just brant
ggplot(filtered_species_log_bran, aes(x = obsYear, y = bran_indice)) +
  geom_point() +
  geom_line() +
  labs(title = "Brant Indice Over Years",
       x = "Year",
       y = "Brant Indice")

# Visualize this -- just cackling goose
ggplot(filtered_species_log_cacg, aes(x = obsYear, y = cacg_indice)) +
  geom_point() +
  geom_line() +
  labs(title = "Cackling Goose Indice Over Years",
       x = "Year",
       y = "Cackling Goose Indice")

# Visualize all goose species
combined_indices <- data.frame(
  obsYear = filtered_species_log_sngo$obsYear,  # Assuming obsYear is a common column
  sngo_indice = filtered_species_log_sngo$sngo_indice,
  bran_indice = filtered_species_log_bran$bran_indice,
  cang_indice = filtered_species_log_cang$cang_indice,
  cacg_indice = filtered_species_log_cacg$cacg_indice
)
# Reshape the data to long format
combined_indices_long <- tidyr::pivot_longer(combined_indices, 
                                             cols = c("sngo_indice", "bran_indice", "cang_indice", "cacg_indice"),
                                             names_to = "species", values_to = "indice")

# Visualize the line graph for all species
ggplot(combined_indices_long, aes(x = obsYear, y = indice, color = species)) +
  geom_point() +
  geom_line() +
  labs(title = "Goose Indices Over Years",
       x = "Year",
       y = "Indice",
       color = "Species")
