library(ggplot2)
library(pracma)

source("R/01-functions.R")

# Define the size of the piece
size <- 40
circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

# Define the colors (these may be defined by user at some point)
background <- "#EDEFEE"
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

# Turn the piece VII vector into the piece III vector (white squares)
final_df <- get_kelly_III_vector(df, background)

#####################################################################
# Add numbers to the colors
convert_colors_to_numbers <- function(colors) {
  # Turn colors into numbers, but assign NA to background color
  color_nums <- as.numeric(factor(colors, levels = unique(colors[-which(colors == background)])))

  # Change all NA values to the letter "B"
  color_nums <- ifelse(is.na(color_nums), "B", color_nums)

  return(color_nums)
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
        axis.text.x = element_text(angle = 90))

# Get a df of just the unique colors and their numbers
colors_and_numbers <- unique(final_df[,c('color','color_numbers')])

# Function to plot the palette with it's numbers, based on hues::swatch()
plot_number_swatch <- function (x) # df containing two rows, color & color numbers
{
  # create labels vector
  labels <- paste0(x[[2]], ": ", x[[1]])

  # set plotting parameters
  par(

    # mai specifies the margins in inches as (bottom, left, top, right)
    mai = c(0.2, # bottom margin == .2"
            # left margin == the width in " of the longest color+number combo name + .4"
            max(strwidth(labels, "inch") + 0.4, na.rm = TRUE),
            0.2, # top margin == .2"
            0.4),# right margin == .4"
    family = "Courier") # Just trying to ensure monospaced font

  # create a bar plot
  barplot(
    rep(1, nrow(x)), # create as many bars (of height 1) as there are colors
    col = rev(x[[1]]), # color the bars using the vector of colors in reverse order
    space = 0.1, # keep 10% (.1) of each bar's height as space between the bars
    axes = FALSE, # Don't draw an axis line
    names.arg = rev(labels), # labels below each bar; colors vector in reverse order
    cex.names = 0.8, # expansion factor for bar labels; shrink to 80% (.8)
    horiz = T, # draw bars horizontally (first bar goes at the bottom)
    las = 1) # specify the orientation of the bar labels (1 = horizontal)

  return(invisible(NULL)) # don't print the plot if it's just being assigned
} # I'm putting this in the functions file

plot_number_swatch(colors_and_numbers)
