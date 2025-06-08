
# this function gives unwanted results below and above its mathematical
# constraints and that's ok/expected - the user will not be able to assign a size
# outside the appropriate range (still determining, but range = size 13 to size 60 for now)
get_prob_vector <- function(circuits){ # returns a vector of probs, 1 per circuit

  # create a vector that represents the probs of the outer 10% of circuits
  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1)

  # calculate the length of the last 90% based on the circuits leftover after 10%
  last90perc_length <- circuits - length(first10perc)

  # calculate the length of the last 10% based on the length of the last 90%
  last10perc_length <- length(first10perc) #round(last90perc_length * (1/9))

  # calculate the length of the middle 80% based on what's leftover
  middle80perc_length <- last90perc_length - last10perc_length

  # assign probabilities linearly to the middle 80% of circuits
  middle80perc <- seq(0.02857143, 1, length.out = middle80perc_length+2)[-c(1, middle80perc_length+2)]

  # assign a probability of 1 to all circuits in the last 10% (the center)
  last10perc <- rep(1, last10perc_length)

  # combine the vectors into one
  prob_vector <- c(first10perc, middle80perc, last10perc)

  return(prob_vector)
}

################################################################################
# Function to turn the probability vector into a matrix of probabilities in the right shape
# * using pracma::flip functions for literate clarity of what is happening -
# since pracma is built of very simple base, I don't mind it being a dependency
get_prob_matrix <- function(size, prob_vector){ # returns a matrix of probs

  # Calculate quad size same way as circuits
  quad_size <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

  # Create empty matrix for the quad
  M <- matrix(0, nrow = quad_size, ncol = quad_size)

  # For loop to assign prob_vector to correct cells in quadrant
  for (i in 1:quad_size){

    M[i, i:quad_size] <- prob_vector[i]
    M[i:quad_size, i] <- prob_vector[i]
  }

  # if size is even, we need to mirror the entire quad
  if(size %% 2 == 0){
    # mirror horizontally and column bind
    M_right <- pracma::fliplr(M) # fliplr means flip left-right
    M <- cbind(M, M_right)

    # then mirror vertically and row bind
    M_down <- pracma::flipud(M) # flipud means flip up-down
    M <- rbind(M, M_down)

  }else{ # if size is odd, then we leave the last col and row out of mirroring
    # mirror all but last col horizontally and col bind
    M_right <- pracma::fliplr(M[ , 1:(quad_size-1)]) # fliplr means flip left-right
    M <- cbind(M, M_right)

    # then mirror all but last row vertically and row bind
    M_down <- pracma::flipud(M[1:(quad_size-1), ]) # flipud means flip up-down
    M <- rbind(M, M_down)

  }

  return(M)
}

################################################################################
# This is something I may use to simulate a different Kelly piece within same app
get_color_vector_blobs_ok <- function(size, colors) {

  # Create a size^2 vector filled with a random sample of colors from a color list
  color_vector <- sample(x = colors,
                         size = size * size,   # "size" is the # of squares on each side
                         replace = TRUE)

  return(color_vector)
}

################################################################################
# This is the color vector function I'll use for pieces III and VII to ensure
# that no more than 2 of the same color are touching. It does this by checking
# the colors of the surrounding cells that have already been assigned a color
get_color_vector <- function(size, colors){

  # Define an empty size x size matrix
  color_matrix <- matrix("", nrow = size, ncol = size)

  # For loop to go row by row through the empty matrix
  for (i in 1:nrow(color_matrix)){

    # For loop to go column by column
    for (j in 1:ncol(color_matrix)){

      # If you're in the first (top left) cell, assign a random color
      if (i == 1 && j == 1){

        color_matrix[i,j] <- sample(x = colors,
                                    size = 1,
                                    replace = TRUE)

        # If you're in any other cell than the top left
      } else {

        # Get the colors of the five surrounding cells, if they exist
        # (the condition in the ifelse checks to see if they exist)
        left_color1 <- ifelse((j-1) > 0, color_matrix[i, j-1], "")
        left_color2 <- ifelse((j-2) > 0, color_matrix[i, j-2], "")
        up_color1 <- ifelse((i-1) > 0, color_matrix[i-1, j], "")
        up_color2 <- ifelse((i-2) > 0, color_matrix[i-2, j], "")
        up_left_color <- ifelse((j-1) > 0 && (i-1) > 1, color_matrix[i-1, j-1], "")
        up_right_color <- ifelse((i-1) > 0 && (j+1) < (ncol(color_matrix)+1),
                                 color_matrix[i-1, j+1], "")

        # Check to see if any of the cell colors in cells that touch match, and
        # if they do, add the matching color to a vector of colors to be excluded
        # from the options when we assign a color to the cell we're on
        matching <- vector(mode = "character", length = 8)

        matching[1] <- ifelse(left_color1 == left_color2, left_color1, "")
        matching[2] <- ifelse(left_color1 == up_color1, left_color1, "")
        matching[3] <- ifelse(up_color1 == up_color2, up_color1, "")
        matching[4] <- ifelse(up_left_color == left_color1, up_left_color, "")
        matching[5] <- ifelse(up_left_color == up_color1, up_left_color, "")
        matching[6] <- ifelse(up_right_color == up_color1, up_right_color, "")
        matching[7] <- ifelse(up_left_color == left_color1, up_color1, "")
        matching[8] <- ifelse(up_left_color == up_color1, left_color1, "")

        # get just the unique colors in the list of matching colors,
        # ignoring any blank elements
        matching <- unique(matching[which(matching != "")])

        # If there were no matches
        if (length(matching) == 0){

          # Assign any random color
          color_matrix[i,j] <- sample(x = colors,
                                      size = 1,
                                      replace = TRUE)

        } else { # If there WERE matches

          # Assign any other color than those in the "matches" vector
          color_matrix[i,j] <- sample(x = colors[-which(colors %in% matching)],
                                      size = 1,
                                      replace = TRUE)
        }
      }
    }
  }
  # return the color matrix as a vector
  return(as.vector(color_matrix))
}


################################################################################
# Function to assign background colors vs colors according to probability in cell
get_kelly_III_vector <- function(df, background){

  # Write a loop that iterates over each row in df
  for (i in 1:nrow(df)){

    # If the prob in the row is 0, assign the background color to $color col
    if (df$probs[i] == 0){
      df$color[i] <- background
    } else if (df$probs[i] == 1){ # if it's 1, leave it as the color it is
      df$color[i] <- df$color[i]
    } else { # if it's neither 0 nor 1, use probability to determine color/bkgrd

      # grab a random number between 0 and 1
      random <- runif(n = 1)

      # If the random is greater than probs, assign background,
      # (if not, do nothing)
      if (random > df$probs[i]){
        df$color[i] <- background
      }
    }
  }

  return(df)

}

################################################################################
# Create a vector of numbers to represent colors
convert_colors_to_numbers <- function(colors, background) {
  # Turn colors into numbers, but assign NA to background color
  color_numbers <- as.numeric(factor(colors, levels = unique(colors[-which(colors == background)])))

  # Change all NA values to the letter "B"
  color_numbers <- ifelse(is.na(color_numbers), "B", color_numbers)

  return(color_numbers)
}

################################################################################
# Plot the unique colors and their corresponding numbers
plot_number_swatch <- function (x) # df containing two rows, color & color numbers
{
  # create labels vector
  labels <- paste0(x[[2]], ": ", x[[1]])

  # set plotting parameters
  par(

    # mai specifies the margins in inches as (bottom, left, top, right)
    mai = c(0.5, # bottom margin
            2.5, # left margin
            0.5, # top margin
            0.5),# right margin
    family = "Courier") # Just trying to ensure monospaced font

  # create a bar plot
  barplot(
    rep(1, nrow(x)), # create as many bars (of height 1) as there are colors
    col = rev(x[[1]]), # color the bars using the vector of colors in reverse order
    space = 0.1, # keep 10% (.1) of each bar's height as space between the bars
    axes = FALSE, # Don't draw an axis line
    names.arg = rev(labels), # labels below each bar; colors vector in reverse order
    cex.names = 1.2, # expansion factor for bar labels;
    horiz = T, # draw bars horizontally (first bar goes at the bottom)
    las = 1) # specify the orientation of the bar labels (1 = horizontal)

  return(invisible(NULL)) # don't print the plot if it's just being assigned
}
