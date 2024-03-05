# I need to create a plot of the colors as numbers and a swatch plot


library(ggplot2)
library(pracma)

source("R/01-functions.R")

# Define the size of the piece
size <- 40
circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

# Define the colors
background <- "#EDEFEE"

if (background == "#EDEFEE"){ # If background is paper white
  colors <- c(#"#EDEFEE", # Paper
    "#1A8BB3", # Teal - no longer teal, just bright blue
    "#0950AE", # Dark blue
    "#4DACE5", # Light blue
    "#126DDB", # Blue
    "#E48DC4", # Pink
    "#ABA9E8", # Light purple
    "#872791", # Purple
    "#6D1617", # Dark red
    "#B81634", # Red
    "#DF3B43", # Red orange
    "#E35C47", # Orange
    "#EB8749", # Light orange
    "#F6E254", # Yellow
    "#7B442D", # Brown
    "#000000", # Black
    "#1A6E7E", # Dark green - no longer dark green, now looks teal
    "#7CBF7B", # Green
    "#ADD2B8") # Light green
} else { # else (if black), remove black from colors
  colors <- c(#"#EDEFEE", # Paper
    "#1A8BB3", # Teal - no longer teal, just bright blue
    "#0950AE", # Dark blue
    "#4DACE5", # Light blue
    "#126DDB", # Blue
    "#E48DC4", # Pink
    "#ABA9E8", # Light purple
    "#872791", # Purple
    "#6D1617", # Dark red
    "#B81634", # Red
    "#DF3B43", # Red orange
    "#E35C47", # Orange
    "#EB8749", # Light orange
    "#F6E254", # Yellow
    "#7B442D", # Brown
    #"#000000", # Black
    "#1A6E7E", # Dark green - no longer dark green, now looks teal
    "#7CBF7B", # Green
    "#ADD2B8") # Light green
}



# Create a data frame for the grid coordinates
df <- expand.grid(x = 1:size, y = 1:size)

# Get the vector of probabilities based on the size of the piece
prob_vector <- get_prob_vector(circuits)

# Generate the color vector
color_vector <- get_color_vector(size, colors)

# Add the corresponding color to each grid cell coordinate
df$color <- color_vector

# Assign probabilities to matrix correctly
M <- get_prob_matrix(size, prob_vector)

# Apply prob matrix M to df as a vector
df$probs <- as.vector(M)

#####################################################################
# This is the point at which the colors could get converted to numbers
# and all plots could get made if the user wanted a "piece VII" style grid

#####################################################################

# Turn the piece VII vector into the piece III vector (white squares)
final_df <- get_kelly_III_vector(df, background)

#####################################################################
# Add numbers to the colors
convert_colors_to_numbers <- function(colors) {
  # Turn colors into numbers, but assign NA to background color
  colors <- as.numeric(factor(colors, levels = unique(colors[-which(colors == background)])))

  # Change all NA values to the letter "B"
  colors <- ifelse(is.na(colors), "B", colors)

  return(colors)
}

final_df$color_numbers <- convert_colors_to_numbers(final_df$color)

# Create the pbn plot
paint_by_numbers <-
  ggplot(final_df, aes(x = x, y = y, fill = color, label = color_numbers)) +
  geom_tile(alpha = 0.5, color = "black") +  # Add tiles
  geom_text(size = 2.6) +
  scale_x_continuous(position = "top",
                     expand = c(0,0),
                     breaks =  1:nrow(final_df)) +
  scale_y_reverse(expand = c(0,0),
                     breaks =  1:nrow(final_df)) +
  scale_fill_identity() +  # Use the colors stored as strings in the color column
  #theme_void() +  # Remove axis labels and background
  coord_equal() +  # Use equal aspect ratio
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90)
        axis.text.x.top = element_text(angle = 90,
                                       vjust = 0.5))
# https://github.com/tidyverse/ggplot2/issues/1878

paint_by_numbers

# Printing a size 40 grid at 8" by 8" works great for my printer
# with US letter sized paper. Tested up to size = 60 for now, it does ok

# Create the piece III plot
kelly_colors_III <-
  ggplot(final_df, aes(x = x, y = y, fill = color)) +
  geom_tile() +  # Add tiles
  scale_y_reverse() +
  scale_fill_identity() +  # Use the colors stored as strings in the color column
  theme_void() +  # Remove axis labels and background
  coord_equal()  # Use equal aspect ratio

# Print the plot
kelly_colors_III


# I like this plot for the swatches, using the hues package
library(hues)

hues::swatch(c(colors, background))
