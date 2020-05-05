## Analysis of Pacific Environmental Dataset ##

#### Packages ####

library(tidyverse)

#### Data Input ####

data <- read_csv("Data/SO248-254_all_for statistic _v20200428_SO254-5-20_del.csv") %>%
  select(c(6:9,15:17,19:29,31:93, 95:97)) %>%
  mutate_all(as.numeric) %>%
  rename("Depth" = "Depth water [m]")

### Correlation Analysis #### 

pdf("Plots/CorrelationMatrixComplete.pdf",
    width = 20, height = 20,
    pointsize = 15)

plotCorrelationMatrix(data, orderParams = T, ytextOffset = 3.75)

dev.off()

png("Plots/CorrelationMatrixDeep.png",
    width = 1200, height = 1200,
    pointsize = 10,
    res = 120)

data %>%
  select(1,2,4,5,7,10,11,12,13,15,16,17,18,19,20,21,28,29,30,31,32,33,38,62,68,69,70,71,72,73,81,82,83,84) %>%
  filter(Depth > 100) %>% 
  select(-3) %>%
  plotCorrelationMatrix(., plotHalf = T, ytextOffset = 1.3, main = "Correlation Table - >100m Depth")

dev.off()

par(mar = c(5,4,4,1))

#### PCA Analysis ####

province <- read_csv("~/PhD/Data_Storage/Meta_Data/Pacific/Raw/SO248-254_all_for statistic _v20200428_SO254-5-20_del.csv") %>%
  select(10) %>%
  as_vector() %>%
  as.factor()

data_use <- data %>%
  select(1,2,4,5,7,11,12,15,16,17,18,19,20,21,28,29,30,31,32,33,38,62,68,69,70,71,72,73) %>%
  mutate(Province = province,
         Depth_Grp = ordered(ifelse(Depth <= 100, "<= 100m",
                                    ifelse(Depth <= 200, "100-200m",
                                           ifelse(Depth <= 500, "200-500m",
                                                  ifelse(Depth <= 1000, "500-1000m", ">1000m")))),
                             levels = c("<= 100m", "100-200m", "200-500m", "500-1000m", ">1000m")))

png("Plots/PCA_Complete.png",
    width = 1600, height = 800,
    pointsize = 10,
    res = 120)

PCA_complete <- data_use %>%
  pca_wrapper(modelParam = .$Latitude) 

p1 <- ggplot(PCA_complete$data, aes(x = pca_1, y = pca_2)) +
  geom_point(aes(fill = Depth_Grp), shape = 21, size = 3) +
  ggsci::scale_fill_locuszoom() +
  labs(x = paste("PCA 1 - ", complete$explVar[1], "% expl. Var", sep = ""), 
       y = paste("PCA 2 - ", complete$explVar[2], "% expl. Var", sep = ""),
       title = "Principal Component Analysis", subtitle = "Complete dataset",fill = "Depth")

p2 <- ggplot(PCA_complete$data, aes(x = pca_1, y = pca_2)) +
  geom_point(aes(fill = Province), shape = 21, size = 3) +
  ggsci::scale_fill_simpsons() +
  labs(x = paste("PCA 1 - ", explVar[1], "% expl. Var", sep = ""), 
       y = paste("PCA 2 - ", explVar[2], "% expl. Var", sep = ""),
       title = "Principal Component Analysis", subtitle = "Complete dataset",fill = "Province")

cowplot::plot_grid(p1, p2)

dev.off()

png("Plots/PCA_100m.png",
    width = 900, height = 800,
    pointsize = 12,
    res = 120)

PCA_subset_Depth <- data_use %>%
  filter(Depth_Grp == "<= 100m") %>%
  pca_wrapper(modelParam = abs(.$Latitude))

ggplot(PCA_subset_Depth$data, aes(x = pca_1, y = pca_2))  +
  stat_contour(data = PCA_subset_Depth$ordiModel, aes(x = x, y = y, z = z, colour = ..level..),
               binwidth = 5) +
  geom_point(aes(fill = as.factor(Province)), shape = 21, size = 3) +
  ggsci::scale_fill_simpsons() +
  labs(x = paste("PCA 1 - ", explVar[1], "% expl. Var", sep = ""), 
       y = paste("PCA 2 - ", explVar[2], "% expl. Var", sep = ""),
       title = "Principal Component Analysis", subtitle = "Subset: <= 100m",fill = "Province", colour = "Absolute Latitude")

dev.off()

png("Plots/PCA_deep.png",
    width = 900, height = 800,
    pointsize = 12,
    res = 120)

PCA_subset_Depth <- data_use %>%
  filter(Depth_Grp >= "<= 100m") %>%
  pca_wrapper(modelParam = abs(.$Latitude))

ggplot(PCA_subset_Depth$data, aes(x = pca_1, y = pca_2))  +
  #stat_contour(data = subset_Depth$ordiModel, aes(x = x, y = y, z = z, colour = ..level..),
  #             binwidth = 5) +
  geom_point(aes(fill = as.factor(Province)), shape = 21, size = 3) +
  ggsci::scale_fill_simpsons() +
  labs(x = paste("PCA 1 - ", explVar[1], "% expl. Var", sep = ""), 
       y = paste("PCA 2 - ", explVar[2], "% expl. Var", sep = ""),
       title = "Principal Component Analysis", subtitle = "Subset: > 100m",fill = "Province")

dev.off()
