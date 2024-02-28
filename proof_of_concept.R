# Load packages
library(tidyverse)

# Define a function to generate a random vector of colors
generate_color_vector <- function(size, colors) {

  # Create a size^2 vector filled with a random sample of colors from a color list
  color_vector <- sample(x = colors,
                         size = size * size,   # "size" is the # of squares on each side
                         replace = TRUE)

  return(color_vector)
}

# Set the size of the desired grid
# Ellsworth Kelly used 40 x 40 grids, so I will as well
size <- 40

# Define the colors
# I did my best to sample the unique colors from his work and this is what I got.
# I've read that the paper he used for color inspo came in 18 unique colors,
# but I've only identified 16 unique colors for now. I do think there are some
# more to be distinguished in there between the darker blues and red-oranges
colors <- c("#ece4cc",
            "#fccd02",
            "#b1c575",
            "#74aa3a",
            "#007b7b",
            "#0099d4",
            "#014d9c",
            "#0b0c18",
            "#9f1c6c",
            "#d2a1c0",
            "#952017",
            "#c3280b",
            "#eda29a",
            "#e4581c",
            "#955507",
            "#eb871b"
)

# Generate the color grid - time to use my custom function with my custom color list!
# This code actually creates a vector of colors that get shaped into a size^2 matrix
color_vector <- generate_color_vector(size, colors)

# Create a data frame for the grid coordinates
# A 40 x 40 grid is 1600 cells, so I am creating a data frame with 1600 rows.
# Each row of the data frame will contain a coordinate pair designating a grid cell
df <- expand.grid(x = 1:size, y = 1:size)

# Add the corresponding color to each grid cell coordinate
# This line of code adds a column to my data frame called "color"
# It contains the vector of 1600 colors we generated with our function above
df$color <- color_vector

# Now, we can plot the grid using the tile geom in ggplot2!
# Normally, ggplot would choose colors for us or we'd have to define them in the code
# but we can use scale_fill_identity() to have ggplot use the strings stored in the
# color column as the colors for the tiles.
kelly_colors_VII <-
  ggplot(df, aes(x = x, y = y, fill = color)) +
  geom_tile() +  # Add tiles
  scale_y_reverse() +
  scale_fill_identity() +  # Use the colors stored as strings in the color column
  theme_void() +  # Remove axis labels and background
  coord_equal()  # Use equal aspect ratio

# Print the plot
kelly_colors_VII

