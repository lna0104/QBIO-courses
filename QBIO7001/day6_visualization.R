
library(ggplot2)
library(boilrdata)
library(tidyverse)

##### Geom  -----

### Geom plot
# version 1:
ggplot(marsupials) +
  geom_point(aes(Mass, MaxLifeSpan))

# version 2:
ggplot(marsupials, aes(Mass, MaxLifeSpan)) +
  geom_point()

# version 3:
ggplot() +
  geom_point(aes(Mass, MaxLifeSpan), marsupials)

# add transparent

ggplot(marsupials) +
  geom_point(aes(Mass, MaxLifeSpan), alpha = 0.2)


# use colors for family
ggplot(marsupials) +
  geom_point(aes(Mass, MaxLifeSpan, col=Family)) +
  geom_smooth(aes(Mass, MaxLifeSpan), method = "lm")


### Line plot

nectarYeastReduced <- nectarYeast |>
  filter(strain %in% c("Mr", "Mk") & sucrose == 30 & validity == "YES" & exp == "comp1") |>
  select(strain, rep, treat.bio, timepoint, CFUs)

ggplot(nectarYeastReduced |> filter(rep==1)) +
  geom_line(mapping = aes(timepoint, CFUs, col=strain, linetype=treat.bio))

# treat each rep as individual line and different color
ggplot(nectarYeastReduced) +
  geom_line(mapping = aes(timepoint, CFUs, 
                          col=interaction(rep, strain), 
                          linetype=treat.bio)
  )

# treat each rep as individual line but same color
ggplot(nectarYeastReduced) +
  geom_line(mapping = aes(timepoint, CFUs, 
                          group=interaction(strain, rep, treat.bio), 
                          col=strain, 
                          linetype=treat.bio)
  )

# add log scale 
ggplot(nectarYeastReduced) +
  geom_line(mapping = aes(timepoint, CFUs, 
                          group=interaction(strain, rep, treat.bio), 
                          col=strain, 
                          linetype=treat.bio)
  ) +
  scale_y_log10()

##### Bar charts

treesWithFamily <- oxley$trees |>
  left_join(oxley$species, by = "species_map_code") |>
  select(species_map_code, species, family, alive_first_monitor)
head(treesWithFamily)

ggplot(treesWithFamily) +
  geom_bar(aes(species))

ggplot(treesWithFamily) +
  geom_bar(aes(species)) +
  coord_flip()

ggplot(treesWithFamily) +
  geom_bar(aes(species, fill = factor(alive_first_monitor))) +
  coord_flip()

ggplot(treesWithFamily) +
  geom_bar(aes(species, fill = factor(alive_first_monitor)),
           position = "fill") +
  coord_flip()

treesWithFamily <- oxley$trees |>
  left_join(oxley$species, by = "species_map_code") |>
  select(species, family, height_cm_when_mapped, height_cm_first_monitor)
head(treesWithFamily)

ggplot(treesWithFamily) +
  geom_histogram(aes(height_cm_when_mapped))

ggplot(treesWithFamily) +
  geom_histogram(aes(height_cm_when_mapped), fill = rgb(1, 0, 0, 0.2)) +
  geom_histogram(aes(height_cm_first_monitor), fill = rgb(0, 0, 1, 0.2))

ggplot(treesWithFamily) +
  geom_boxplot(aes(family, height_cm_when_mapped)) +
  coord_flip()


##### Combining and splitting plots -----

ggplot(treesWithFamily) +
  geom_boxplot(aes(family, height_cm_when_mapped)) +
  geom_point(aes(family, height_cm_when_mapped)) +
  coord_flip()

ggplot(treesWithFamily) +
  geom_boxplot(aes(family, height_cm_when_mapped), outlier.alpha = 0) +
  geom_point(aes(family, height_cm_when_mapped), 
             size = 0.1, position = "jitter", alpha = 0.1) +
  coord_flip()

ggplot(treesWithFamily) +
  geom_point(aes(height_cm_when_mapped, height_cm_first_monitor), size = 0.2, alpha = 0.2) +
  facet_wrap(vars(family), nrow = 5)

### Compound figures
plotA <- ggplot(marsupials) +
  geom_point(aes(Mass, MaxLifeSpan, col=Family))

plotB <- ggplot(marsupials) +
  geom_point(aes(Mass, PopDensity, col=Family)) +
  scale_y_log10()

plotA
plotB

library(cowplot)
library(patchwork)

plot_grid(plotA, plotB)

legend <- get_legend(plotA)

plot_grid(plotA + theme(legend.position="none"), 
          plotB + theme(legend.position="none"),
          legend,
          nrow = 1,
          labels = c("A", "B"),
          rel_widths = c(1, 1, 0.5))

plotA + (plotA / plotB)

##### The final touch -----

treesWithFamily <- oxley$trees |>
  left_join(oxley$species, by = "species_map_code") |>
  select(species_map_code, species, family, alive_first_monitor)
ggplot(treesWithFamily) +
  geom_bar(aes(species, fill = factor(alive_first_monitor))) +
  coord_flip()

ggplot(treesWithFamily) +
  geom_bar(aes(species, fill = factor(alive_first_monitor))) +
  coord_flip() +
  theme_wsj()


ggplot(treesWithFamily) +
  geom_bar(aes(species, fill = factor(alive_first_monitor))) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1", 
                    na.value = "grey50",
                    labels = c("dead", "alive", "unknown"),
                    limits = c("1", "0", NA)) +
  labs(x = "Species", y = "Number of seedlings", fill ="") +
  theme_bw() +
  theme(legend.position="top", axis.text.y = element_text(face = "italic"))

##### Exercises -----

#6.1.
oxleySurvivalData <- oxley$trees |>
  group_by(plot) |>
  summarise(survivalRate = mean(alive_first_monitor, na.rm = TRUE))
  

ggplot(oxleySurvivalData, aes(plot, survivalRate)) +
  geom_bar(stat="identity")


oxleySurvivalData <- oxley$trees |>
  group_by(plot) |>
  summarise(survivalRate = mean(alive_first_monitor, na.rm = TRUE)) |>
  left_join(oxley$plots, by = "plot") |>
  select(plot, survivalRate, mix) 

ggplot(oxleySurvivalData) +
  geom_bar(aes(plot, survivalRate, fill = factor(mix)), stat="identity") +
  scale_x_continuous(breaks=seq(1,20,1)) + 
  scale_fill_brewer(palette = "Set1",
                    labels = c("Fast", "Hardy"),
                    limits = c("F", "H")) +
  labs(x = "Plot" , y = "Survival Rate", fill = "Mix") +
  theme(legend.position = c(0.9,0.85))

#6.2

ggplot(senecio) +
  geom_point(aes(x = Area, y = Circularity, col = Ecotype)) +
  labs(x = "Leaf area [mm2]", y= "Leaf circularity")


ggplot(senecio) +
  geom_boxplot(aes(x=Population, y=MSD, fill = Ecotype)) + 
  theme_classic() +
  labs(y="Main stem diameter [mm]")

ggplot(senecio, aes(VegHeight, fill = Ecotype)) +
  geom_density(adjust=1, , alpha=0.4) +
  theme_bw() +
  labs(x ="Vegetable height [mm]", y = "Density")

ggplot(senecio) +
  geom_point(aes(SB, VegHeight)) +
  geom_smooth(aes(SB, VegHeight), method = "lm") +
  facet_wrap(vars(Ecotype), nrow =2) + 
  theme_light()+
  labs(x ="Number of secondary branches", y = "Vegetable height [mm]")

#6.3

selectedOxley <- oxley$trees |> select(height_cm_when_mapped, height_cm_first_monitor)

ggplot(oxley$trees, aes(x= height_cm_when_mapped, y= height_cm_first_monitor)) +
  geom_bin_2d()+
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(oxley$trees) +
  geom_density_2d_filled(aes(x = height_cm_when_mapped, y = height_cm_first_monitor)) +
  scale_x_continuous(c(0,60)) +
  scale_y_continuous(c(0,100))
  

ggplot(oxley$trees, aes(x= height_cm_when_mapped, y= height_cm_first_monitor)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.5)

drosophilaWingG

tidydrosophilaWingG <- drosophilaWingG |> 
  as.data.frame() |>
  mutate(trait1 = rownames(drosophilaWingG)) |>
  pivot_longer(cols = 1:10,
               names_to = "trait2",
               values_to = "value"
               )


ggplot(tidydrosophilaWingG, aes(x= trait1, y= trait2, fill = value)) +
  geom_tile()
  
