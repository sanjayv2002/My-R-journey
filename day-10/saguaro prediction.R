# Species distribution modeling for saguaro
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-23

library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")

# getting data
bioclim_data <- getData(name ="worldclim",
                        var = "bio",
                        res = 2.5,
                        path = "day-10/data/")


#Reading data
obs_data <- read.csv(file = 'day-10/data/Carnegiea-gigantea-GBIF.csv')


# dropping na's
obs_data <- obs_data[!is.na(obs_data$latitude),]

# finding the latitudinal and longitudinal boundaries
max_lat <- ceiling(max(obs_data$latitude))
min_lat <- floor(min(obs_data$latitude))
max_lon <- ceiling(max(obs_data$longitude))
min_lon <- floor(min(obs_data$longitude))
geographic_extent <- extent(x = c(min_lon, max_lon, min_lat, max_lat))


# Load the data to use for our base map
data("wrld_simpl")

# plot the base map
plot(wrld_simpl,
     xlim = c(min_lon, max_lon),
     ylim = c(min_lat, max_lat),
     axes = TRUE,
     col = "grey95")

# Adding points
points(x = obs_data$longitude,
       y = obs_data$latitude,
       col = "olivedrab",
       pch = 20,
       cex = 0.75)

# adding a box boundary
box()



# getting data within geographic extent
bioclim_data <- crop(x = bioclim_data, y = geographic_extent)


# droping unused columns to fit bioclim model
obs_data <- obs_data[,c("longitude", "latitude")]
# Build species distribution model
bc_model <- bioclim(x = bioclim_data, p = obs_data)

# predict presence from model
predict_presence <- dismo::predict(object =  bc_model,
                                   x = bioclim_data,
                                   ext = geographic_extent)



# Plot base map
plot(wrld_simpl, 
     xlim = c(min_lon, max_lon),
     ylim = c(min_lat, max_lat),
     axes = TRUE, 
     col = "grey95")

# Add model probabilities
plot(predict_presence, add = TRUE)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")

# Add original observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
box()




# Using the bioclim data files for sampling resolution
bil_files <- list.files(path = "day-10/data/wc2-5",
                        pattern = "*.bil$",
                        full.names = TRUE)

# taking the first file from the list of files
mask <- raster(bil_files[1])

# setting the seed for random-num generator to ensure  results are similar
set.seed(20210707)

# Randomly sample points
background <- randomPoints(mask = mask,
                           n = nrow(obs_data),
                           ext = geographic_extent,
                           extf = 1.25)


# Plot the base map
plot(wrld_simpl, 
     xlim = c(min_lon, max_lon),
     ylim = c(min_lat, max_lat),
     axes = TRUE, 
     col = "grey95",
     main = "Presence and pseudo-absence points")

# Add the background points
points(background, col = "grey30", pch = 1, cex = 0.75)

# Add the observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

box()


# Arbitrarily assign group 1 as the testing data group
testing_group <- 1

# Create vector of group memberships
group_presence <- kfold(x = obs_data, k = 5) # kfold is in dismo package


# Separate observations into training and testing groups
presence_train <- obs_data[group_presence != testing_group, ]
presence_test <- obs_data[group_presence == testing_group, ]

# Repeat the process for pseudo-absence points
group_background <- kfold(x = background, k = 5)
background_train <- background[group_background != testing_group, ]
background_test <- background[group_background == testing_group, ]


# Build a model using training data
bc_model <- bioclim(x = bioclim_data, p = presence_train)

# Predict presence from model (same as previously, but with the update model)
predict_presence <- dismo::predict(object = bc_model, 
                                   x = bioclim_data, 
                                   ext = geographic_extent)


# Use testing data for model evaluation
bc_eval <- evaluate(p = presence_test,   # The presence testing data
                    a = background_test, # The absence testing data
                    model = bc_model,    # The model we are evaluating
                    x = bioclim_data)    # Climatic variables for use by model

# Determine minimum threshold for "presence"
bc_threshold <- threshold(x = bc_eval, stat = "spec_sens")


# Plot base map
plot(wrld_simpl, 
     xlim = c(min_lon, max_lon),
     ylim = c(min_lat, max_lat),
     axes = TRUE, 
     col = "grey95")

# Only plot areas where probability of occurrence is greater than the threshold
plot(predict_presence > bc_threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab"))

# And add those observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "black",
       pch = "+", 
       cex = 0.6)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")
box()
