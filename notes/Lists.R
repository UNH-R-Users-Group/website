
# R Users Group Stuff
# March 11th, 2025

# Example 1
Year1Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 18, 5),
                        nitrogen_mg_per_kg = rnorm(9, 4.8, 0.5),
                        Year = 1)
Year2Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 19, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5, 0.5),
                        Year = 2)
Year3Data <- data.frame(Depth_cm = rep(c("0-10", "10-20", "20-40"), 3),
                        Site = rep(c("Forest", "Pasture", "Corn_Field"), each = 3),
                        carbon_mg_per_kg = rnorm(9, 20, 5),
                        nitrogen_mg_per_kg = rnorm(9, 5.2, 0.5),
                        Year = 3)
MyData <- list(Year1Data = Year1Data, Year2Data = Year2Data, Year3Data = Year3Data)

# Example 2
Corn_Field <- data.frame(phosphorus_mg_per_kg = 1.8, potassium_mg_per_kg = 2.3)
Forest <- data.frame(phosphorus_mg_per_kg = 0.9, potassium_mg_per_kg = 2.1)
Pasture <- data.frame(phosphorus_mg_per_kg = 1.2, potassium_mg_per_kg = 1.9)
(Metadata <- list(Corn_Field = Corn_Field, Forest = Forest, Pasture = Pasture))

# Example 3
MyData <- list(Year1Data = Year1Data, Year2Data = Year2Data, Year3Data = Year3Data)
NestedList <- list(MyData, MyData, MyData)

# Example 4
MetadataNested <- list(Metadata, Metadata, Metadata)
