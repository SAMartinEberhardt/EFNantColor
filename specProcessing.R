#  script for processing and plotting uv-vis spectra

library(pavo)
library(tidyverse)

#### materials testing ####
  #### cleaning EFN data ####
    #### Impatiens ####
imp <- read.csv("raw_data/spectrometer_PLECO_data.csv")

imp <- imp %>% dplyr::select(c(1, seq(from = 2, to = length(names(imp)), by = 2)))

# convert to an rspec object
imp <- as.rspec(imp)

# pare down the range to be 300 to 700 nm
imp <- imp[which(imp$wl == 300):which(imp$wl == 700),]

# only need to run this if some are negative 
#imp <-procspec(imp, fixneg = "addmin")


# check which smoothing fn you like best. 
# plotsmooth(imp,
#            minsmooth = 0.05,
#            maxsmooth = 0.5,
#            curves = 4,
#            ask = FALSE
# )

# add favorite span val and smooth out all the specs
imp <- procspec(imp, opt = "smooth", span = 0.1)  
plot(imp)


# aggregate
  # examine the blanks (Pelco tabs)
procspec(imp, opt = c("min", "max")) %>% 
  select(wl, c("pelco_blank_1", "pelco_blank_1.1", 
               "pelco_blank_2", "pelco_blank_2.1")) %>% 
  explorespec(by = 2, lwd = 2)

  # examine dataset without blanks
procspec(imp, opt = c("min", "max")) %>% 
  dplyr::select(wl, !c(pelco_blank_1, pelco_blank_1.1, 
                       pelco_blank_2, pelco_blank_2.1, 
                       g.2)) %>% 
  explorespec(by = 3, lwd = 2)

# need to remove g.2 and r (high uv ref, practically zero elsewhere)
imp3 <- imp %>% 
  dplyr::select(!contains("pelco") & !c("g.2", "r"))

  # average every three specra
imp <- aggspec(imp3, by = 3, FUN = mean)

# save it!
write.csv(imp, "clean_data/specs_Impatiens_clean.csv", row.names = F)

    #### Gossypium ####
goss <- read.csv("raw_data/spectrometer_data_Gossypium.csv")

goss <- goss %>% dplyr::select(c(1, seq(from = 2, to = length(names(goss)), by = 2)))

# convert to an rspec object and check it worked
goss <- as.rspec(goss)

# pare down the range to be 300 to 700 nm
goss <- goss[which(goss$wl == 300):which(goss$wl == 700),]

# only need to run this if some are negative 
#goss <-procspec(goss, fixneg = "addmin")


# check which smoothing fn you like best. 
# plotsmooth(goss,
#            minsmooth = 0.05,
#            maxsmooth = 0.5,
#            curves = 4,
#            ask = FALSE
# )

# add favorite span val and smooth out all the specs
goss <- procspec(goss, opt = "smooth", span = 0.1)  
plot(goss)

# aggregate
goss %>%   explorespec(by = 3, lwd = 2)

# remove aus_nectar2.1 and 2.2 (second and third measurements) since droplet of nectar was stuck to inside of black block after taking 2nd measurement
goss3 <- aggspec(goss %>% dplyr::select(!c("aus_nectar2", "aus_nectar2.1", "aus_nectar2.2")),
                by = 3, FUN = mean)

goss <- full_join(goss3, goss %>% dplyr::select(wl, aus_nectar2), by = "wl")

write.csv(goss, "clean_data/specs_Gossypium_clean.csv", row.names = F)


  #### cleaning materials data ####
setwd("~/aResearch/Rprojects/EFNantColor/")

  # downloaded googlesheet named trialEFNmaterials as a csv
sp <- read.csv("raw_data/trialEFNmaterials.csv")

  # drops repeated wavelength columns -- artifact from saving readings one at a time from free version of OceanView
sp <- sp %>% dplyr::select(c(1, seq(from = 2, to = length(names(sp)), by = 2)))

# convert dataframe to an rspec object (specific to pavo package) 
sp <- as.rspec(sp)

# pare down the range to be 300 to 700 nm -- insect visual range
sp <- sp[which(sp$wl == 300):which(sp$wl == 700),]

plot(sp)
# only need to run this if some are negative 
#sp <-procspec(sp, fixneg = "addmin")


#check which smoothing fn you like best.
plotsmooth(sp,
           minsmooth = 0.05,
           maxsmooth = 0.5,
           curves = 4,
           ask = FALSE
)

# add favorite span val and smooth out all the spec
sp <- procspec(sp, opt = "smooth", span = 0.1)  
plot(sp) # have removed UV in this plot to see sublter variation in rest of spectrum (artificial materiasl reflect so much UV)


# aggregate 
  # first check for replicate measurements that are totally different from other two reps
explorespec(sp, by = 3, lwd = 2)

  # returns mean of every three spectra
sp <- aggspec(sp, by = 3, FUN = mean)

# calculate hexadecimal color in human colorspace described by each spectrum
rgb <- spec2rgb(sp)
rgb <- data.frame(sample = names(sp)[2:length(names(sp))], 
                            rgb = rgb)

# save as csv (will need to be converted back to rspec object when read in as dataframe)
write.csv(sp, "clean_data/specs_materials_clean.csv", row.names = F)



  #### combine EFN and materials datasets ####
mat <- read.csv("clean_data/specs_materials_clean.csv")
imp <- read.csv("clean_data/specs_Impatiens_clean.csv")
goss <- read.csv("clean_data/specs_Gossypium_clean.csv")
  # convert dataframes to rspec objects
mat <- as.rspec(mat)
imp <- as.rspec(imp)
goss <- as.rspec(goss)


# how different are different days?
dates <- data.frame(sample = names(mat)[2:length(names(mat))], 
                    date = c(rep(1, 7), rep(2, 11), rep(3, 5), rep(4, 8)))
mat %>% 
  pivot_longer(greenfolder:greenvinylmattenp, 
               names_to = "sample", 
               values_to = "Reflectance") %>% 
  mutate(material = case_when(grepl("vinyl", sample) ~ "vinyl",
                              grepl("inside", sample) ~ "folder_inside",
                              grepl("leaf", sample) ~ "leaf",
                              grepl("foam", sample) ~ "foam",
                              grepl("swatch", sample) ~ "swatch",
                              grepl("paper", sample) ~ "paper",
                              grepl("bag", sample) ~ "cloth",
                              TRUE ~ "folder_outside"), 
         date = case_when(sample %in% c(dates$sample[dates$date == 1]) ~ "1", 
                         sample %in% c(dates$sample[dates$date == 2]) ~ "2",
                         sample %in% c(dates$sample[dates$date == 3]) ~ "3",
                         sample %in% c(dates$sample[dates$date == 4]) ~ "4"),  
         color = case_when(grepl("darkgreen", sample) ~ "darkgreen",
                           grepl("red", sample) ~ "red",
                           TRUE ~ "green")) %>% 
  filter(date == 4) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = sample, color = sample)) + 
  geom_line() + 
  theme_bw()


  #### plots ####
# can rerun with different normalization approaches
  # isolate which spectra have reflectances over 100% (or some other threshold)
mx <- data.frame(names = names(sp), 
                 max = rep(NA, length(names)))
for (i in 1:length(names(sp))) {
  mx$max[i] <- max(sp[, i]) 
}

# convert to long plotting format
matl <- mat %>% 
  pivot_longer(2:ncol(mat), 
               names_to = "sample", 
               values_to = "Reflectance") %>% 
  #filter(sample %in% c(mx$names[mx$max < 100])) %>% # drop materials with reflectances over 100%
  mutate(material = case_when(grepl("vinyl", sample) ~ "vinyl",
                              grepl("inside", sample) ~ "folder_inside",
                              grepl("leaf", sample) ~ "leaf",
                              grepl("foam", sample) ~ "foam",
                              grepl("swatch", sample) ~ "swatch",
                              grepl("paper", sample) ~ "paper",
                              grepl("bag", sample) ~ "cloth",
                              TRUE ~ "folder_outside"), 
            color = case_when(grepl("darkgreen", sample) ~ "darkgreen",
                              grepl("red", sample) ~ "red",
                              TRUE ~ "green"))

gossl <- goss %>% 
  pivot_longer(sturt_leaf:aus_nectar2, 
               names_to = "sample", 
               values_to = "Reflectance") %>% 
  mutate(type = case_when(grepl("leaf", sample) ~ "leaf", 
                          grepl("nectar", sample) ~ "nectar",
                          grepl("dryefn", sample) ~ "dryefn"), 
         variety = case_when(grepl("aus", sample) ~ "Gossypium_australe", 
                             grepl("sturt", sample) ~ "Gossypium_sturtianum")
  )

impl <- imp %>% 
  pivot_longer(gt_s:r, 
               names_to = "sample", 
               values_to = "Reflectance") %>% 
  mutate(type = case_when(grepl("_s", sample) ~ "stem", 
                         # sample == "pelco_blank" ~ "background", 
                          TRUE ~ "EFN"
  ), 
  variety = case_when(grepl("gt", sample) ~ "true green",
                      grepl("r", sample) ~ "red", 
                      grepl("g", sample) & !grepl("t", sample) ~ "green"))
impl$genus <- rep("Impatiens", nrow(impl))
gossl$genus <- rep("Gossypium", nrow(gossl))
plants <- rbind(gossl, impl)


# plot all the natural specs together in a single plot
plants %>% 
  filter(variety != "Gossypium_sturtianum") %>% 
  mutate(type2 = ifelse(type %in% c("leaf", "stem"), "surrounds", "EFN")) %>% 
  ggplot(aes(x = wl, y = Reflectance, 
             group = sample, 
             linetype = type2, 
             color = variety))  + 
  geom_line(lwd = 1.5) + 
  scale_color_manual(values = c("tomato", #"olivedrab", 
                                "olivedrab4",
                                "tomato4", "olivedrab2")) +
  labs(y = "Standardized Reflectance", x = "wavelength", 
       title = "EFNs and surrounding tissue (G. australe and Impatiens)") + 
  theme_bw()

ggsave("figures/EFNs_Leaves_Stems_no_addneg.png")


# plot all materials together
matl %>% 
 # filter(!material %in% c("vinyl", "swatch", "paper", "folder_outside")) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = sample, color = material)) +
  geom_line(lwd = 1.5) + 
  theme_bw()
ggsave("figures/all_materials_below_100.png")


#### iterative plots ####
    #### surrounds ####
for (i in 1:length(unique(matl$sample[matl$color %in% c("green", "darkgreen")]))) {
plants %>% 
  filter(variety != "Gossypium_sturtianum" & type %in% c("leaf", "stem")) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = sample, color = genus))  + 
  geom_line(lwd = 1.5)  +
  scale_color_manual(values = c("olivedrab4", "olivedrab2")) + 
  geom_line(aes(x = wl, y = Reflectance, 
                group = sample, 
                color = sample), 
            data = matl %>% filter(sample == unique(matl$sample[matl$color %in% c("green", "darkgreen")])[i]), 
            lwd = 1.5, 
            color = "steelblue") +
  labs(y = "Reflectance", x = "wavelength", 
       title = unique(matl$sample[matl$color %in% c("green", "darkgreen")][i])) + 
  theme_bw()

path <- paste0("figures/comparisons/surrounds_", unique(matl$sample[matl$color %in% c("green", "darkgreen")])[i], ".png")
ggsave(path)
}

    #### red EFNs ####
for (i in 1:length(unique(matl$sample[matl$color %in% c("red")]))) {
  plants %>% 
    filter(variety %in% c("Gossypium_australe", "red") & 
             type %in% c("EFN", "dryefn")) %>% 
    ggplot(aes(x = wl, y = Reflectance, group = sample, color = genus))  + 
    geom_line(lwd = 1.5)  +
    scale_color_manual(values = c("firebrick", "tomato")) + 
    geom_line(aes(x = wl, y = Reflectance, 
                  group = sample, 
                  color = sample), 
              data = matl %>% filter(sample == unique(matl$sample[matl$color %in% c("red")])[i]), 
              lwd = 1.5, 
              color = "steelblue") +
    labs(y = "Reflectance", x = "wavelength", 
         title = unique(matl$sample[matl$color %in% c("red")][i])) + 
    theme_bw()
  
  path <- paste0("figures/comparisons/redEFN_", unique(matl$sample[matl$color %in% c("red")])[i], ".png")
  ggsave(path)
}

    #### green EFNs ####
for (i in 1:length(unique(matl$sample[matl$color %in% c("green", "darkgreen")]))) {
  plants %>% 
    filter(variety %in% c("Gossypium_sturtianum", "green", "true green") & 
             type %in% c("EFN", "dryefn")) %>% 
    ggplot(aes(x = wl, y = Reflectance, group = sample, color = genus))  + 
    geom_line(lwd = 1.5)  +
    scale_color_manual(values = c("olivedrab1", "olivedrab2")) + 
    geom_line(aes(x = wl, y = Reflectance, 
                  group = sample, 
                  color = sample), 
              data = matl %>% filter(sample == unique(matl$sample[matl$color %in% c("green", "darkgreen")])[i]), 
              lwd = 1.5, 
              color = "steelblue") +
    labs(y = "Reflectance", x = "wavelength", 
         title = unique(matl$sample[matl$color %in% c("green", "darkgreen")][i])) + 
    theme_bw()
  
  path <- paste0("figures/comparisons/greenEFN_", unique(matl$sample[matl$color %in% c("green", "darkgreen")])[i], ".png")
  ggsave(path)
}


  #### comparison of selected materials ####
plants %>% 
  mutate(salient = case_when(variety %in% c("Gossypium_australe", "red") & type %in% c("EFN", "dryefn") ~"red EFN",
                             variety %in% c("Gossypium_sturtianum", "green", "true green") &type %in% c("EFN", "dryefn") ~ "green EFN",
                             variety != "Gossypium_sturtianum" & type %in% c("leaf", "stem") ~ "surrounds")) %>% 
  filter(!is.na(salient)) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = salient, color = salient))  + 
  geom_line(lwd = 1.5, alpha = .3) +
  geom_line(aes(x = wl, y = Reflectance,
                group = sample,
                color = sample),
            data = matl %>% filter(sample %in% c("insideredfoldermattenp", "greenleafgrapeupper", "greenleafgrapelower")),
            lwd = 1.5) +
  scale_color_manual(values = c("olivedrab",  "olivedrab", "darkgreen","tomato", "tomato", "darkgreen")) + 
  labs(y = "Reflectance", x = "wavelength", color = "") + 
  theme_bw()
ggsave("figures/selected_materials_example.png")

### selected materials ####

plants %>% 
 # filter(genus == "Impatiens" & variety %in% c("true green", "green")) %>% 
  filter(variety == "Gossypium_sturtianum" & type %in% c("dryefn", "leaf")) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = sample, color = type))  + 
  geom_line(lwd = 1.5)  +
  scale_color_manual(values = c("darkgreen", "chartreuse", "olivedrab3", "olivedrab4")) + 
  geom_line(aes(x = wl, y = Reflectance,
                group = sample,
                color = sample),
          # data = matl %>% filter(sample %in% c("darkgreenvinylmattenp", "redvinylmattenp")),
          #  data = matl %>% filter(sample %in% c("greenleafgrapelower", "insideredfoldermattenp")),
            data = matl %>% filter(sample %in% c("greenleafgrapelower", "darkgreenvinylmattenp")),
            lwd = 1.5) +
  labs(y = "Reflectance", x = "wavelength", 
       title = "Green Impatiens vinyl and silk leaf") + 
  theme_bw()
ggsave("figures/Comparison_green_impatiens.png")


##### **************** #####

matl %>% 
  filter(color %in% c("green", "darkgreen")) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = sample, color = material))  + 
  geom_line(lwd = 1.5) + 
  geom_line(aes(x = wl, y = Reflectance, group = sample, color = type), 
            data = impl %>% filter(variety %in% c("green", "true green")), 
            lwd = 1.5) + 
  # scale_color_manual(values = c("mediumorchid", "steelblue", "steelblue1", 
  #                                "darkgrey", "steelblue4")) + 
  labs(y = "Standardized Reflectance", x = "wavelength", 
       title = "Green Impatiens and Plastics") + 
  theme_bw()

ggsave("figures/Greens_impatiens_maxtransf.png")

  # Gossypium
foldl %>% 
  filter(color %in% c("green", "darkgreen")) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = sample, color = material))  + 
  geom_line(lwd = 1.5) + 
  geom_line(aes(x = wl, y = Reflectance, group = sample, color = type), 
            data = gossl %>% filter(varietys == "Gossypium_sturtianum" |
                                      sample == "aus_leaf"), 
            lwd = 1.5) +
  scale_color_manual(values = c("mediumorchid", "steelblue", "steelblue1", 
                                 "darkgrey", "mediumorchid2", "steelblue4")) + 
  labs(y = "Standardized Reflectance", x = "wavelength", 
       title = "Green Gossypium and Plastics") + 
  theme_bw()
ggsave("figures/Greens_Gossypium_maxtransf.png")

# red tissue and surrounding tissue and red plastics
  # impatiens only
foldl %>% 
  filter(color %in%  c("red")) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = sample, color = material))  + 
  geom_line(lwd = 1.5) + 
  geom_line(aes(x = wl, y = Reflectance, group = sample, color = type), 
            data = impl %>% filter(variety %in% c("red")), 
            lwd = 1.5) + 
  scale_color_manual(values = c("mediumorchid", "steelblue", "steelblue1", 
                                "darkgrey", "steelblue4")) + 
  labs(y = "Standardized Reflectance", x = "wavelength", 
       title = "Red Impatiens and Plastics") + 
  theme_bw()

ggsave("figures/Reds_impatiens_maxtransf.png")

foldl %>% 
  filter(color %in% c("red")) %>% 
  ggplot(aes(x = wl, y = Reflectance, group = sample, color = material))  + 
  geom_line(lwd = 1.5) + 
  geom_line(aes(x = wl, y = Reflectance, group = sample, color = type), 
            data = gossl %>% filter(variety == "Gossypium_australe" |
                                      sample == "sturt_leaf"), 
            lwd = 1.5) +
  scale_color_manual(values = c("mediumorchid3", "steelblue", "steelblue1", 
                                "darkgrey", "mediumorchid1", "steelblue4")) + 
  labs(y = "Standardized Reflectance", x = "wavelength", 
       title = "Red Gossypium and Plastics") + 
  theme_bw()
ggsave("figures/Reds_Gossypium_maxtransf.png")


### outtakes #####
# convert to long plotting format
avl <- av3 %>% 
  pivot_longer(gt_s:r, 
               names_to = "sample", 
               values_to = "Reflectance") %>% 
  mutate(type = case_when(grepl("_s", sample) ~ "stem", 
                          TRUE ~ "EFN"
  ), 
  variety = case_when(grepl("gt", sample) ~ "true green",
                      grepl("r", sample) ~ "red", 
                      grepl("g", sample) & !grepl("t", sample) ~ "green"))
avgl <- avg %>% 
  pivot_longer(sturt_leaf:aus_nectar2, 
               names_to = "sample", 
               values_to = "Reflectance") %>% 
  mutate(type = case_when(grepl("leaf", sample) ~ "leaf", 
                          grepl("nectar", sample) ~ "nectar",
                          grepl("dryefn", sample) ~ "dryefn"), 
         variety = case_when(grepl("aus", sample) ~ "Gossypium_australe", 
                             grepl("sturt", sample) ~ "Gossypium_sturtianum")
  )

# human colors efns
rgbg <- spec2rgb(goss)

avgl %>% 
  # filter(type == "nectar") %>% 
  ggplot(aes(x = wl, y = Reflectance, 
             group = sample, 
             col = sample)) + 
  geom_line(lwd = 2) + 
  # geom_line(aes(group = sample, 
  #               col = varietyor), 
  #           data = aplot %>% filter(type == "EFN"), lwd = 1.5) + 
  scale_color_manual(values = rgbg) + 
  #  scale_linetype_manual(values = c("solid", "longdash", "solid", "dotdash")) +
  labs(x = "Wavelength", y = "Reflectance", 
       col = "", linetype = "") + 
  theme_bw() 


# compare tissue types
avgl %>% 
  ggplot(aes(x = wl, y = Reflectance, 
             group = sample, 
             col = type)) + 
  geom_line(lwd = 2) + 
  # geom_line(aes(group = sample, 
  #               col = varietyor), 
  #           data = aplot %>% filter(type == "EFN"), lwd = 1.5) + 
  # scale_color_manual(values = rgb) + 
  #  scale_linetype_manual(values = c("solid", "longdash", "solid", "dotdash")) +
  labs(x = "Wavelength", y = "Reflectance", 
       col = "", linetype = "") + 
  theme_bw() + 
  facet_wrap(~variety)

  #### normalization ####

# two different normalization approaches
# 1)  normalize with min/max (_mm for "min/max")
imp_mm <- procspec(imp, opt = c("min"))
plot(imp)
plot(procspec(imp, opt = "center"))
goss_mm <- procspec(goss, opt = c("min", "max"))
mat_mm <- procspec(mat, opt = c("min", "max"))

# 2) normalize by setting max global value to 100 and min global value to 0 (_mt for "global maximum transofmration")
maxtransf <- function(dataframe){
  globalmax <- max(dataframe[, 2:ncol(dataframe)])
  refl <- dataframe[2:ncol(dataframe)]
  refl <- refl/globalmax
  new <- cbind(dataframe[,1], refl)
  names(new)[1] <- "wl"
  new <- as.rspec(new)
  return(new)
}

goss_mt <- maxtransf(goss)
imp_mt <- maxtransf(imp)
mat_mt <- maxtransf(mat)