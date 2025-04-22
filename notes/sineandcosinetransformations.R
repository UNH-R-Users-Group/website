
# Sine and Cosine Transformations

# R Users Group Presentation
# David Moore
# davidblakneymoore@gmail.com
# April 22nd, 2025


# Introduction to Sines and Cosines

dev.off()
par(mar = c(7, 3, 1, 1))
x <- seq(-(2 * pi), (4 * pi), 0.01)
y1 <- sin(x)
y2 <- cos(x)
matplot(x, cbind(y1, y2), type = "l", lty = 1, lwd = 2, xlab = "", ylab = "")
abline(h = 0, col = 4)
abline(v = 0, col = 4)
legend("bottom", inset = c(0, -0.2), horiz = T, xpd = T, title = expression(paste(underline("Function"))), legend = c("y = sin(x)", "y = cos(x)"), lty = 1, lwd = 2, col = 1:2)

# These are the same two functions separated by 90 ˚ (pi / 2 rad)

y3 <- sin(x + (pi / 2))
lines(x, y3, col = 4, type = "l", lwd = 2)

# Why would you use sine or cosine transformations?

# How do you use them? When would you use more than one together?

# What varies about sine and cosine functions?

# 1. Period
# 2. Amplitude
# 3. Phase Shift
# 4. Vertical Shift

# 1. Period
y1 <- sin(x * 2)
y2 <- sin(x)
y3 <- sin(x / 2)
matplot(x, cbind(y1, y2, y3), type = "l", lty = 1, lwd = 2, xlab = "", ylab = "")
abline(h = 0, col = 4)
abline(v = 0, col = 4)
legend("bottom", inset = c(0, -0.2), horiz = T, xpd = T, title = expression(paste(underline("Period"))), legend = c("pi", "2 * pi", "4 * pi"), lty = 1, lwd = 2, col = 1:3)

# 2. Amplitude
y1 <- sin(x)
y2 <- 2 * sin(x)
y3 <- 3 * sin(x)
matplot(x, cbind(y1, y2, y3), type = "l", lty = 1, lwd = 2, xlab = "", ylab = "")
abline(h = 0, col = 4)
abline(v = 0, col = 4)
legend("bottom", inset = c(0, -0.2), horiz = T, xpd = T, title = expression(paste(underline("Amplitude"))), legend = c("1", "2", "3"), lty = 1, lwd = 2, col = 1:3)

# 3. Phase Shift
y1 <- sin(x)
y2 <- sin(x - (pi / 2))
y3 <- sin(x - pi)
matplot(x, cbind(y1, y2, y3), type = "l", lty = 1, lwd = 2, xlab = "", ylab = "")
abline(h = 0, col = 4)
abline(v = 0, col = 4)
legend("bottom", inset = c(0, -0.2), horiz = T, xpd = T, title = expression(paste(underline("Phase Shift"))), legend = c("0", "pi / 2", "pi"), lty = 1, lwd = 2, col = 1:3)

# 4. Vertical Shift
y1 <- sin(x)
y2 <- sin(x) + 1
y3 <- sin(x) + 2
matplot(x, cbind(y1, y2, y3), type = "l", lty = 1, lwd = 2, xlab = "", ylab = "")
abline(h = 0, col = 4)
abline(v = 0, col = 4)
legend("bottom", inset = c(0, -0.2), horiz = T, xpd = T, title = expression(paste(underline("Vertical Shift"))), legend = c("0", "1", "2"), lty = 1, lwd = 2, col = 1:3)

# Note that there are some overlap between the four - one example is that
# flipping the sine curve over the horizontal axis (making its amplitude -1) is
# the same thing as a phase shift of 90 ˚ (or pi rad)
y1 <- sin(x)
y2 <- -sin(x)
y3 <- sin(x - pi)
matplot(x, cbind(y1, y2, y3), type = "n", lty = 1, lwd = 2, xlab = "", ylab = "")
abline(h = 0, col = 4)
abline(v = 0, col = 4)
lines(x, y1, col = 1, lwd = 2) # Pause here and explain
lines(x, y2, col = 2, lwd = 2)
lines(x, y3, col = 3, lwd = 2)

# We are primarily interested in phase shift and period

# Examples


# Example 1: Moss growth on trees is affected by orientation or direction

(Moss_Data <- data.frame(Tree_Number = rep(1:5, each = 4), Orientations = rep(seq(0, 360, length.out = 5)[-5], 5), Moss_Score = c(0.9, 0.2, 0.3, 0.2, 0.7, 0.5, 0.4, 0.5, 0.5, 0, 0.1, 0.1, 0.4, 0.1, 0, 0, 0.5, 0.2, 0.3, 0.4)))

# Plot These Data
par(mar = c(10, 4, 4, 2))
plot(Moss_Score ~ Orientations, data = Moss_Data, col = Tree_Number, pch = 19, main = "Moss Scores", xlab = "Orientation", ylab = "Moss Score")
legend("bottom", xpd = T, horiz = T, inset = c(0, -0.35), title = expression(paste(underline("Tree Number"))), legend = levels(factor(Moss_Data$Tree_Number)), col = 1:5, pch = 19)

# We can't just find a linear trendline since 360 ˚ and 0 ˚ mean the same
# thing.

# Let's transform the orientations to make it so that 0 ˚ and 360 ˚ mean the
# same thing.

# The 'sin' and 'cos' functions in R require that the input is in radians, so
# We will have to convert our orientations into rad.

Converted_Orientations <- Moss_Data$Orientations * ((2 * pi) / 360)

# 0 ˚ is due north. We know that moss grows the most on north sides of trees.
# Therefore, we will want our sine transformation to give north-facing
# orientations the highest values (near 1) and south-facing values the lowest
# values (-1). If we take the sine of 0, we will get 0, but we want to get 1.
# Therefore, let's incorporate a phase shift. What do we have to add or
# subtract from our converted orientations to make north-facing orientations
# be near 1?

sin(0)
sin(pi / 2)
sin(pi)
sin(3 * pi / 2)

Shifted_Orientations <- Converted_Orientations + (pi / 2)

# Now, we can use the sine function.

North_South_Transformed_Orientations <- sin(Shifted_Orientations)

# Did we do it correctly?
par(mar = c(5, 4, 4, 2) + 0.1)
plot(North_South_Transformed_Orientations ~ Moss_Data$Orientations, pch = 19, xlab = "Orientation (˚)", ylab = "Sine-Transformed Value")
# Yes - let's think about these transformed values for a minute. They treat
# north-facing orientations as 1s, south-facing orientations as -1s, and both
# east-and west-facing orientations as 0s. Therefore, they treat east and west
# as being equivalent, and they only look at differences along the north-south
# gradient.

# Why did we add and not subtract pi / 2?

# Let's use these transformed values in our model.

summary(lm(Moss_Data$Moss_Score ~ North_South_Transformed_Orientations))

# What if we want to include a term representing an east-west gradient in our
# model too?

# By the way, it's important to note that we can add this other term because
# the east-west gradient and the north-south gradient are orthogonal
# (perpendicular, or completely independent). If they weren't, they wouldn't be
# independent; there could be some collinearity, and we'd be 'double-dipping'
# on our data, as Iago Hale says.

# To look at an east-west gradient, let's assign 1 to east-facing orientations,
# 0 to north- and south-facing orientations, and -1 to west-facing
# orientations.

# In this particular case, the phase shift will be 0 since east-facing
# orientations are pi / 2 radians into the cycle, and since the sine function
# already peaks at pi / 2. In nearly all cases, you'll always have to consider
# the phase shift, but in this particular case, since we've thought through it
# thoroughly and determined it isn't necessary, we can proceed with the
# 'Converted_Orientations' object we created before.

East_West_Transformed_Orientations <- sin(Converted_Orientations)

# Let's check to make sure we did it correctly.
par(mar = c(5, 4, 4, 2) + 0.1)
plot(East_West_Transformed_Orientations ~ Moss_Data$Orientations, pch = 19, xlab = "Orientation (˚)", ylab = "Sine-Transformed Value")
# Here, we're treating north- and south-facing orientations the same (as 0s)
# while east-facing orientations are treated as 1s and west-facing orientations
# are treated as -1s.

# It looks good. Now let's model.
summary(lm(Moss_Data$Moss_Score ~ North_South_Transformed_Orientations + East_West_Transformed_Orientations))

# We could also look at the interaction between these two. In other words, are
# larger north-south-gradient values associated with larger east-west-gradient
# values in any way?
summary(lm(Moss_Data$Moss_Score ~ North_South_Transformed_Orientations * East_West_Transformed_Orientations))
# # ?interaction.plot


# Example 2: Temperature data here in Durham varies on yearly and daily cycles

# Import Some Data
Data_From_2024 <- read.table("https://www.ncei.noaa.gov/pub/data/uscrn/products/hourly02/2024/CRNH0203-2024-NH_Durham_2_SSW.txt")
Data_From_2023 <- read.table("https://www.ncei.noaa.gov/pub/data/uscrn/products/hourly02/2023/CRNH0203-2023-NH_Durham_2_SSW.txt")
Data_From_2022 <- read.table("https://www.ncei.noaa.gov/pub/data/uscrn/products/hourly02/2022/CRNH0203-2022-NH_Durham_2_SSW.txt")
Data_Frame_Information <- readLines("https://www.ncei.noaa.gov/pub/data/uscrn/products/hourly02/headers.txt")
Data <- rbind(Data_From_2022, Data_From_2023, Data_From_2024)
colnames(Data) <- strsplit(Data_Frame_Information[2], " ")[[1]]
Data$LST_DATE <- as.Date(as.character(Data$LST_DATE), format = "%Y%m%d")
Data$LST_TIME <- Data$LST_TIME / 100
Data$TIMESTAMP <- as.POSIXct(paste(Data$LST_DATE, Data$LST_TIME), format = "%Y-%m-%d %H")
Data$T_HR_AVG[which(Data$T_HR_AVG == -9999)] <- NA

# Look at These Data
plot(T_HR_AVG ~ TIMESTAMP, Data, pch = 20, main = "Temperatures at Thompson Farm", xlab = "Date and Time", ylab = "Temperature (˚ C)")
plot(T_HR_AVG ~ TIMESTAMP, Data[1:500, ], pch = 20, main = "Temperatures at Thompson Farm", xlab = "Date and Time", ylab = "Temperature (˚ C)")

# Let's transform the data to account for the annual cycle. In the moss
# example, it's easy to see the relationship between orientation and angles (in
# radians) for the sine transformation since orientations are already angles.
# Now, though, we have temporal data, not spatial data, that are cyclical, but
# because these data are cyclical, we can still think about these times as
# angles.

# Warmest temperatures are in the summer and coldest temperatures are in the
# winter, so let's use a sine transformation and have the cycle start on the
# first day of the summer (June 21st)

Numeric_Times <- as.numeric(Data$TIMESTAMP)
Numeric_Times[1:10]

# These times represent the number of seconds since January 1st, 1970 at
# 00:00:00 UTC. Ultimately, we'll need to convert them into rad, because the
# sine and cosine functions take angles in rad as an input. First, though,
# we'll need to take into account the different time zones. EST is 5 hr (18000
# s) behind (less than) UCT. This is a minor point for annual cycles, but it
# will be very important when we look at daily cycles.

Numeric_Times <- Numeric_Times - 18000

# Now, let's convert them into rad.

# Since the times are in seconds, the number of seconds in a year will be our
# period.
Number_of_Seconds_in_a_Year <- 365.25 * 24 * 60 * 60

# We have to convert our numeric times to rad. One cycle in s is 31557600
# whereas one cycle in rad is 2 * pi.
Converted_Times <- Numeric_Times * ((2 * pi) / Number_of_Seconds_in_a_Year)

# We know the period, but we still have to figure out what the phase shift will
# be. Warmest temperatures are in the summer and coldest temperatures are in
# the winter, so let's use a sine transformation and have the cycle start on
# the first day of the spring (March 20th). That way, the function will peak at
# the first of the summer, when days are the longest.

# Spring starts around 6750000 s into the year (in a non-leap year, assuming
# March 20th at 03:00 UTC is when spring starts).
Phase_Shift <- 6750000

# Remember that this phase shift must be in radians too.
Converted_Phase_Shift <- Phase_Shift * ((2 * pi) / Number_of_Seconds_in_a_Year)

# Now we can do the transformation.
Annually_Sine_Transformed_Times <- sin(Converted_Times - Converted_Phase_Shift)

plot(Annually_Sine_Transformed_Times ~ Data$TIMESTAMP)
abline(h = 0, col = 4)
abline(v = as.POSIXct("2023-03-20 3:00:00"), col = 4) # This line represents
# the first day of spring in 2023 (for reference)
abline(v = as.POSIXct("2024-12-21 3:00:00"), col = 4) # This line represents
# the first day of winter in 2024 (also for reference)

# Since we know temperature varies on a daily scale too, let's do the same
# thing to account for daily variability.

# Let's start here again.
Numeric_Times

# Conovert these times (in seconds) to rad.
Number_of_Seconds_in_a_Day <- 24 * 60 * 60
Converted_Times <- Numeric_Times * ((2 * pi) / Number_of_Seconds_in_a_Day)

# Let's have the sine curve start at 6:00 AM (21600 s into each day) so it
# peaks at noon and has its minimum values at midnight. Without any phase
# shift, the midnight values would be 0, but we want the values at 6:00 AM to
# be 0.
Phase_Shift <- 21600

# Remember that this phase shift must be in rad too.
Converted_Phase_Shift <- Phase_Shift * ((2 * pi) / Number_of_Seconds_in_a_Day)

# Since 6:00 AM is a quarter of the way through a day, this converted phase
# shift should equal pi / 2 rad.
Converted_Phase_Shift == pi / 2

# Now we can do the transformation.
Daily_Sine_Transformed_Times <- sin(Converted_Times - Converted_Phase_Shift)

# Let's just look at 3 d.
plot(Daily_Sine_Transformed_Times[1:(24 * 3)] ~ Data$TIMESTAMP[1:(24 * 3)], pch = 20)
abline(h = 0, col = 4)
abline(v = as.POSIXct("2022-01-02 12:00:00"), col = 4) # This line represents
# noon on the second day - it should be at day 2's highest point.

# We can use these values in a model too.
summary(lm(Data$T_HR_AVG ~ Daily_Sine_Transformed_Times + Annually_Sine_Transformed_Times))

# By the way, we could also consider the two corresponding orthogonal
# components of these cycles if we wanted to. We've accounted for the summer-
# winter gradient component of the annual cycle and the day-night component of
# the daily cycle, but there are also spring-autumn and morning-evening
# components we could consider (again, because they are both orthogonal to what
# we've already done). I'll leave it as an exercise for you to figure out.

# Last Remarks

# First, I've heard before that you should use both sine and cosine functions
# to transform data. The reason is because, as we discussed earlier, sine and
# cosine functions differ by 90 ˚ - they are orthogonal to each other - and
# therefore when you use both of them, they pick up on different components of
# the cycle, as long as you don't mess with the phase shift. In my examples, we
# messed with the phase shift since wit's good practice but also because for
# the annual cycle the cycle itself doesn't neatly start at 0 like it does
# with the daily cycle and the orientations from the moss example - the first
# day of the year is not also the first day of spring.

# Second, if two sine or cosine functions have the same period, you can add
# them together and get another sine or cosine function of that same period.
# For this reason, if you allowed the phase shift to be flexible, you could
# fit one sine or cosine function to the data and use those values to model
# periodicity and to create one predictor variable instead of two (one for each
# orthogonal component like we did earlier). This method will allow you to look
# for a periodic component to the data, but it will not allow you to look at
# two specific, separate, orthogonal components of interest.

# We can use the weather data as an example of this last idea. We'll focus on
# the annual component here.

dev.off()
plot(T_HR_AVG ~ TIMESTAMP, Data, pch = 20, main = "Temperatures at Thompson Farm", xlab = "Date and Time", ylab = "Temperature (˚ C)")

# We know that the period will be 325.25 d (31557600 s), but we don't know if
# the sine or cosine function will perfectly align with the start days and
# times for each season, so let's allow the phase shift to vary and see what
# the 'nls' function comes up with.

# Let's use the 'Numeric_Times' object since we've already adjusted it for its
# time zone.

Temperatures <- Data$T_HR_AVG

# I can get some rough initial parameter estimates from the figure.
Fitted_Curve <- nls(Temperatures ~ (Amplitude * sin((Numeric_Times - Phase_Shift) * ((2 * pi) / Number_of_Seconds_in_a_Year))) + Vertical_Shift, 
                    start = c(Amplitude = 12, Phase_Shift = 6750000, Vertical_Shift = 10))
# Notice how I've specified one of the four parameters (period) but not the
# other four (amplitude, phase shift, and vertical shift).

# Here, our phase shift will be in units of s (not rad) because of how I
# converted the phase shift inside of the sine function. Note that I did the
# conversion differently here than I did in my previous examples to make sure
# we get practice converting from whatever unit your periodic data are in to
# radians! We'll have to use this method for the rest of this example since we
# used it to estimate the phase shift now.

Fitted_Curve
summary(Fitted_Curve)
coef(Fitted_Curve)
Amplitude <- coef(Fitted_Curve)["Amplitude"]
Phase_Shift <- coef(Fitted_Curve)["Phase_Shift"]
Vertical_Shift <- coef(Fitted_Curve)["Vertical_Shift"]

# What does this fitted curve look like?
x <- seq(par("usr")[1], par("usr")[2], 3600)
lines(x, (Amplitude * sin((x - Phase_Shift) * ((2 * pi) / Number_of_Seconds_in_a_Year))) + Vertical_Shift, col = 2, lwd = 2)
# It's not a bad fit for empirical temperature data!

# I really just care about the phase shift.
Phase_Shift <- coef(Fitted_Curve)["Phase_Shift"]

# Interestingly, it's not that close to what we used in the original model
# (6750000 s). It's because the hottest time of the year isn't exactly the end
# of the summer and the coldest time of the year isn't exactly the end of the
# winter. Instead, the hottest and coldest times are typically sometime during
# the middle of the summer and the winter, respectfully.
Phase_Shift

# Let's use it in our model.
Sine_Transformed_Times <- sin((Numeric_Times - Phase_Shift) * ((2 * pi) / Number_of_Seconds_in_a_Year))
summary(lm(Data$T_HR_AVG ~ Sine_Transformed_Times))
# Clearly, the sine-transformed time variable is an extremely important
# predictor of temperature.

# Because we found this best-fitting phase shift, the corresponding orthogonal
# component will be 0 - we've already captured everything from this annual
# cycle that we can possibly get from the data. Also, if we had estimated both
# the winter-summer and spring-autumn gradients from these data and added those
# two functions together, we would have gotten this function.

# It's probably better if you used daylength, temperature, and other variables
# that are periodic with an annual cycle directly in your model instead of
# using sine- or cosine-transformed times, but I think that sine- or cosine-
# transformed times could still be used in a model with daylength,
# temperatures, and other predictor variables that have annual cycles - the
# sine- or cosine-transformed predictor variable may pick up on some other
# aspect of the annual cycle that daylength, temperature, or other variables
# miss.

# Finally, time-series data analysis may be worth considering too - it
# considers other factors too, like upward or downward trends in data. There
# are also ways to account for changes in periods or amplitudes over time.
