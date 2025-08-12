# Description: Creates a multi plot image for every year and converts PNG files to mp4 video

# Calculate the anomaly by finding the mean of each country during reference period, and then subtract from observations
co[, grep("^X(19|20)", names(co))] <- (co[, grep("^X(19|20)", names(co))] - 273)
years_subset <- as.character(1931:1960)
co_num <- co[, paste0("X", years_subset)]
no_geo <- st_drop_geometry(co_num)
mean_of_years <- apply(no_geo, 1, mean)

all_years <- as.character(1901:2012)
co[, grep("^X(19|20)", names(co))] <- (co[, grep("^X(19|20)", names(co))] - mean_of_years)

# Creating breaks and assigning colors to values
breaks <- c(-3, -2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3)
colors <- c("#010076","#110f96", "#2332be", "#415ddb", "#869ae9", "#efefef", "#f0c3c4", "#d95b61", "#c40011", "#a90005", "#8f0002")

# Ask ChatGPT: How do I create a loop to assign values to colors over many years
for (year in all_years) {
  col_name <- paste0("X", year)
  co[[paste0("color_", year)]] <- cut(co[[col_name]], breaks = breaks, labels = FALSE, right = TRUE)
  co[[paste0("color_", year)]] <- colors[co[[paste0("color_", year)]]]
}

xvalue <- c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)

# Create function that plots figure based on year
f <- function(year, mean_year_values, years_for_mean) {
  # PNG setup and image layout
  picture <- paste0("Temp_Anomaly_", year, ".png")
  png(picture , width = 600, height = 750)
  layout(matrix(c(1:3), nrow = 3, ncol = 1), heights = c(0.2, 0.02, 0.2), widths = c(1, 0.5, 1))
  
  # Plots maps
  par(mar=c(1, 2, 1, 2))
  plot(co[,paste0("X", year)], reset = FALSE, breaks=breaks, col = co[[paste0("color_", year)]], main = "")
  mtext(as.character(year), side = 3, line = -3, font = 2, cex = 2.5)

  # Plots custom legend
  par(mar=c(1, 7.5, 1, 2))
  plot(xvalue, y = NULL, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-3, 3), ylim = c(0, 0.5), main = "")
  mtext("Temperature Anomaly (°C)", side = 3, line = 0.5, font = 1, cex = 1.5)
  for (i in 1:(length(xvalue) - 1)) {
    rect(xleft = xvalue[i], xright = xvalue[i] + 0.5, ybottom = 0, ytop = 0.5, border = "transparent", col = colors[i]) 
  }
  axis(1, las=1, at=c(-2.5,-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2), lwd=0, labels=c("-2.5","-2", "-1.5", "-1", "-0.5", "0.5", "1", "1.5", "2", "2.5"), cex.axis = 2)
  abline(v=c(-3, -2.5,-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5), lty=1)
  
  mean_year <- mean(co[[paste0("X", year)]])

  # Ask ChatGPT: How do I save values in a function to graph in an mp4?
  mean_year_values <- c(mean_year_values, mean_year)
  years_for_mean <- c(years_for_mean, year)
  
  # Plots average global anomaly
  par(mar=c(3, 6, 3, 1))
  plot(years_for_mean, mean_year_values, pch = 19, , ylab = "Global Temperature Anomaly (°C)", xlab = "", xlim = c(1900, 2020), ylim = c(-0.4, 0.8), axes = FALSE, main = "", cex.lab = 2)
  lines(years_for_mean, mean_year_values, lwd = 2)
  abline(h = 0, col = "black", lty = 2)
  axis(1, cex.axis = 1.5)
  axis(2, cex.axis = 1.5)

  
  dev.off()
  return(list(mean_year_values = mean_year_values, years_for_mean = years_for_mean))
}


# Use wrapper to create video from vector of years
# Ask ChatGPT: How do I use av package in R to create an mp4 file?
video <- function(years_value, output_video = "Global_Temperature_Anomalies.mp4") {
  mean_year_values <- c()
  years_for_mean <-c()
  pngfiles <- vector("character", length = length(years_value))
  
  for (i in 1:length(years_value)) {
    year <- years_value[i]
    result <- f(year, mean_year_values, years_for_mean)
    mean_year_values <- result$mean_year_values
    years_for_mean <- result$years_for_mean
    pngfiles[i] <- paste0("Temp_Anomaly_", year, ".png")
  }
  
  av_encode_video(pngfiles, output = output_video, framerate = 5)
}

years <- 1901:2012
video(years)


# The End
