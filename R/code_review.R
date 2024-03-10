# This script should run top to bottom without issues as long as "01-functions.R"
# is stored with it, but you may need to change the relative path of the source()

# Load packages
library(ggplot2)
library(pracma)

# Source the file containing all the functions, assumes files are in same R folder
source("R/01-functions.R")

# Set user inputs: size, colors, background color
# size of the desired grid and calculate number of circuits
size <- 40 # this is the number of units/squares on ONE side of the desired grid
circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2) # Num of concentric circuits

# Define the colors
background <- "#EDEFEE" # or can be "#000000" (black)

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
# One probability per circuit
prob_vector <- get_prob_vector(circuits)

# Generate the color vector - no more than two same colors touching
color_vector <- get_color_vector(size, colors)

# Add the corresponding color to each grid cell coordinate
df$color <- color_vector

# Assign probabilities to matrix correctly
M <- get_prob_matrix(size, prob_vector)

# Apply prob matrix M to df as a vector
df$probs <- as.vector(M)

# Turn the piece VII vector (all colors) into the piece III vector (add background)
final_df <- get_kelly_III_vector(df, background)

# Create a vector of numbers to represent colors
final_df$color_numbers <- convert_colors_to_numbers(final_df$color)

# Create a vector of the unique colors and their representative numbers to swatch
colors_and_numbers <- unique(final_df[,c('color','color_numbers')])

# Create the final plot of the art piece (this is for. making piece III)
kelly_colors_III <-
  ggplot(final_df, aes(x = x, y = y, fill = color)) +
  geom_tile() +  # Add tiles
  scale_fill_identity() +  # Use the colors stored as strings in the color column
  theme_void() +  # Remove axis labels and background
  coord_equal()  # Use equal aspect ratio

# Print the plot
kelly_colors_III # Prints with white lines in it if not displayed large enough

# Create the "paint-by-numbers" plot
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
        axis.text.x = element_text(angle = 90))

# Print the plot
paint_by_numbers # In app, this will be a pdf download of size 8" by 8"

# Create a "color swatch" plot of the hex values with their colors
# based on the hues::swatch function, just base R plotting here
# and the function is found in the functions file
swatch_plot <- plot_number_swatch(colors_and_numbers)

# Print the plot
swatch_plot # Wish I had a prettier monospaced font that is preinstalled in Linux

# Possible future addition - use something like https://www.colorhexa.com/ to
# return color names. I don't quite know how this website does this!
