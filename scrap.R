library(tidyverse)

# Function to generate grid of colors
generate_color_grid <- function(size, colors) {
  # Create a size^2 matrix filled with a random sample of colors from a list
  color_vector <- sample(x = colors,
                         size = size * size,
                         replace = TRUE)
  grid <- matrix(color_vector,
                 nrow = size,
                 ncol = size)

  return(grid)
}

# Generate color grid

# # Set the size of the grid
size <- 40
# Define the colors

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
# Generate the color grid
color_grid <- generate_color_grid(size, colors)

# Create data frame for ggplot2

# Create a data frame for the grid coordinates
df <- expand.grid(x = 1:size, y = 1:size)
# Add the corresponding color to each grid coordinate
df$color <- color_grid[cbind(df$x, df$y)]
df$color <- color_vector

circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

# Plot using ggplot2
kelly_colors <-
  ggplot(df, aes(x = x, y = y, fill = color)) +
  geom_tile() +  # Add tiles
  scale_fill_identity() +  # Use the colors as specified
  theme_void() +  # Remove axis labels and background
  coord_equal() +  # Use equal aspect ratio
  theme(legend.position = "none")  # Remove the legend


# Analyze the probabilities found in piece III

# Create table of data from the piece
probs_table <-
  dplyr::tibble(
    circuit = 1:circuits,
    num_colored_in = c(0, 2, 4, 12, 19, 25, 30, 33, 37, 39,
                       39, 40, 39, 37, 34, 30, 25, 19, 12, 4),
    num_total = seq(from = 156, to = 4, by = -8)
)

# Calculate probabilities
probs_table <-
  probs_table |>
  mutate(prob_of_color = num_colored_in/num_total)

# Examine probability over circuits (outside in)
plot(probs_table$circuit, probs_table$prob_of_color)
# Looks pretty linear with just a slight sigmoid shape
# I could use a linearly increasing prob and just
# add a 0 at the front and a 1 at the end to simulate it

# Try simulating

sim_prob <- c(0, seq(0, 1, length.out = (circuits)-2), 1)

# Examine probability over circuits (outside in)
# vs my simulated probabilities
par(mfrow = c(1, 2))
plot(probs_table$circuit, probs_table$prob_of_color)
plot(probs_table$circuit, sim_prob)
par(mfrow = c(1, 1))

# It looks pretty good, but I could try just tacking the
# 1 on the end and not adding the 0 to the front

sim_prob2 <- c(seq(0, 1, length.out = (circuits)-1), 1)

# Examine probability over circuits (outside in)
# vs my simulated probabilities
par(mfrow = c(1, 2))
plot(probs_table$circuit, probs_table$prob_of_color)
plot(probs_table$circuit, sim_prob2)
par(mfrow = c(1, 1))
# Ok, it needs something in front. Maybe I can think of something
# else simple, but let's also play with the sigmoid shape of the probs

# for an even numbered grid
df_nls <- data.frame(x = 1:(circuits - 1),
                 y = jitter(probs_table$prob_of_color[1:circuits-1]), factor = 1)
# x1 <- 1:(circuits - 1)
upper_asymp <- 1 # upper asymptote
growth_rate <- round(1/(circuits-2),3) # growth rate (1/(length(probs_table$circuits)-2) if I'm going to add a 1 at the end
x_at_inflection <- (circuits/2) - 1 # time of maximum growth (x value at inflection, length(probs_table$circuits)/2 - 1)

fitmodel1 <- nls(probs_table$prob_of_color[1:circuits-1]~a/(1 + exp(-b * (x1-c))),
                start=list(a = upper_asymp,b = growth_rate,c = x_at_inflection))

fitmodel2 <- nls(y ~ I(a/(1 + exp(-b * (x-c)))),
                 data = df_nls,
                 start=list(a = 1.1, b = 0.280, c = 10))
# WHYYYY CAN'T I GET THIS TO WORK WHYYYYYYY!?
# I needed to add jitter to my y values because they were too perfect

params <- coef(fitmodel2)

# Using the info on circuits 1-20, let's try to fit this using sigmoid
estimated_points <- params[1] / (1 + exp(-params[2] * (probs_table$circuit[1:circuits-1] - params[3])))

y2 <- sigmoid(params,1:19)
y <- probs_table$prob_of_color[1:circuits-1]
plot(y2,type="l")
points(y)
# Doesn't fit the way I'd like, gonna modify params

params[1] <- 1.10
params[2] <- 0.280
params[3] <- 10.5

# Try plotting again
y2 <- c(NA, sigmoid(params,1:circuits), NA)
y <- probs_table$prob_of_color
plot(y2,type="l")
points(y)

# His code:
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}

x = 1:53
y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.1,0.18,0.18,0.18,0.33,0.33,0.33,0.33,0.41,
      0.41,0.41,0.41,0.41,0.41,0.5,0.5,0.5,0.5,0.68,0.58,0.58,0.68,0.83,0.83,0.83,
      0.74,0.74,0.74,0.83,0.83,0.9,0.9,0.9,1,1,1,1,1,1,1)

# fitting code
fitmodel <- nls(y~a/(1 + exp(-b * (x-c))), start=list(a=1,b=.5,c=25))

# visualization code
# get the coefficients using the coef function
params=coef(fitmodel)

y2 <- sigmoid(params,x)
plot(y2,type="l")
points(y)


# End of his code


library(logspline)

logspline(x = probs_table$prob_of_color, lbound = 0, ubound = 1, maxknots = 3)

fit <- logspline(x = probs_table$prob_of_color,
                 lbound = 0,
                 ubound = 1,
                 maxknots = 3)

fit2 <- logspline(x = probs_table$prob_of_color,
                  lbound = -0.00000001,
                  ubound = 1,
                  knots = c(0.09090909, 0.46428571, 0.51315789))

probs1 <- qlogspline(p = probs_table$prob_of_color, fit = fit)
probs2 <- plogspline(q = probs_table$prob_of_color, fit = fit)
probs3 <- qlogspline(p = probs_table$prob_of_color, fit = fit2)

y <- c(0, probs_table$prob_of_color[3:circuits-1], 1)

plot(probs,type="l")
points(y)

plot(probs2,type="l")
points

# What would the actual difference be at the scale of 40X40?
# Take the vector of total squares available and multiply it by the probs
# Then compare it to the actual number of squares colored
kellys <- probs_table$num_colored_in
p1 <- round(probs1 * probs_table$num_total)
p2 <- round(probs2 * probs_table$num_total)
p3 <- round(probs3 * probs_table$num_total)

par(mfrow = c(1, 3))
plot(probs_table$circuit, kellys)
plot(probs_table$circuit, p1)
plot(probs_table$circuit, p3)
par(mfrow = c(1, 1))

# This is really good, but wonder if I can get a better fit with a linear model
# for points 2:19

fitlm <- lm(probs_table$prob_of_color[3:circuits-1] ~ probs_table$circuit[3:circuits-1])

circuitlist <- list(probs_table$circuit[3:circuits-1]) # points 2 through 19
lm1_pred <- predict.lm(fitlm, circuitlist)

par(mfrow = c(1, 2))
plot(probs_table$circuit[3:circuits-1], probs_table$prob_of_color[3:circuits-1])
plot(probs_table$circuit[3:circuits-1], lm1_pred)
par(mfrow = c(1, 1))

# This does so great! But the prob for the second value is negative
# Values 3:19 are great. I still need a programmatic way to do this
# that will scale, so I don't want to hard code the first value as 0
# and the second value as 0.01351351

# let it use that first data point for the modeling
fitlm2 <- lm(probs_table$prob_of_color[2:circuits-1] ~ probs_table$circuit[2:circuits-1])
circuitlist2 <- list(probs_table$circuit[2:circuits-1])
predict.lm(fitlm2, circuitlist2)

# No, that was a bad idea

# So... is it just even? value 2:19 are evenly distributed?
1/18 # 0.05555556
# or is it 1/n-1 to get the number of breaks? 1/4 is .25, giving 0, .25, .5, .75, 1
# for the seq function seq(0, 1, length.out = 5)

# seq to get the probs, then?


# So, I'd make the prob vector start with 0 and end with 1,
# and then I'd create a division factor
div_factor <- 1/(circuits-2) # 0.05555556

# And I'd calculate those probs by starting with 0.01351351?
0.01351351

diff(probs_table$prob_of_color)

# > diff(probs_table$prob_of_color)
# [1] 0.01351351 0.01505792 0.06233766 0.06231672
# [5] 0.06229143 0.06226054 0.05222222 0.07217391
# [9] 0.06211180 0.04887218 0.07507740 0.06176471
# [13] 0.06153846 0.06118881 0.06060606 0.05952381
# [17] 0.05714286 0.05000000 0.00000000


# So, for the first 10% of the values (the first 2 of 20 here), the increase is
# about .014. Then, for the other 90% of values, the increase is about
mean(diff(probs_table$prob_of_color[3:20])) # 0.05714286

# HOW DO I MAKE THIS SCALE UP AND DOWN???????

# If the size is 20, for instance, there are 10 circuits.
# The prob vector would be 0, ..., 1 and there would be 8 values to fill in there
# So do I just
seq(0, 1, length.out = 9)
# [1] 0.000 0.125 0.250 0.375
# [5] 0.500 0.625 0.750 0.875
# [9] 1.000
# and then tack on a 1.0 at the end of the vector? Why not?

c(seq(0, 1, length.out = 19), 1)

par(mfrow = c(1, 2))
plot(probs_table$circuit, probs_table$prob_of_color)
plot(probs_table$circuit, c(seq(0, 1, length.out = circuits-1), 1))
par(mfrow = c(1, 1))

# The first 10 percent of 20 circuits is 2 circuits, so I could use
# A different seq for that one,
first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1)
last90perc <- seq(0.02857143, 1, length.out = round(circuits*.90)-1)[-1]

prob_vector <- c(first10perc, last90perc, 1)

return(prob_vector)

# I need to write in tests to prevent rounding issues where the lengths of these
# vectors end up wrong due to rounding percentages of weird numbers

get_prob_vector <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1)
  last90perc <- seq(0.02857143, 1, length.out = round(circuits*.90)-1)[-1]

  prob_vector <- c(first10perc, last90perc, 1)

  return(prob_vector)
}

get_prob_vector(45) # This results in a vector of length 44 nooooo
# Let's walk through why
circuits45 <- 45

first10perc <- seq(0, 0.02857143, length.out = round(circuits45*.10)+1)
# ^ That results in 5 values
last90perc <- seq(0.02857143, 1, length.out = round(circuits45*.90)-1)[-1]
# ^ That results in 38 values
prob_vector <- c(first10perc, last90perc, 1) # So I'm one short

# What if I used ceiling in the function?

get_prob_vector_c <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = ceiling(circuits*.10)+1)
  last90perc <- seq(0.02857143, 1, length.out = ceiling(circuits*.90)-1)[-1]

  prob_vector <- c(first10perc, last90perc, 1)

  return(prob_vector)
}

get_prob_vector_c(circuits45) # Results in 46 values, also noooooooo

# This comes from the uneven division of many numbers into 10% and 90% with one
# remainder (which I tack on at the end because I do 90% minus 1)
20*.10 # 2
(20*.90)-1 #17

# Circuits for a size 51 grid would be 52/2 = 26
26*.10 # 2.6
(26*.90)-1 #22.4

# If I rounded those in the schoolhouse way, it would be fine, but this is R,
# so they would both round down, giving me 24 when I need 25


45*.10 # 4.5
(45*.90)-1 # 39.5

# Two values ending in .5! I still need to round one up and one down, which it
# should do if it's rounding to even, right? I need a total of 44

round(45*.10) # 4
round(45*.90)-1 # This doesn't work because it ends up being 39
round((45*.90)-1) # But including the -1 in the round function gives me 40

# Let's try a function doing that
get_prob_vector_r <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1)
  last90perc <- seq(0.02857143, 1, length.out = round((circuits*.90)-1))[-1]

  prob_vector <- c(first10perc, last90perc, 1)

  return(prob_vector)
} # THIS ONE IS THE WINNER! VALID FOR ALL SIZES!

get_prob_vector_r(circuits45) # Gives me 45, good

prob_vectors_test <- map(1:65, get_prob_vector_r)

prob_vector_lengths <- map(prob_vectors_test, length)

identical(unlist(prob_vector_lengths[-1]), 2:65) #TRUE!

# Ok, that worked for circuits of 1 to 65!

# What about huge sizes?

prob_vectors_test <- map(66:200, get_prob_vector_r)

prob_vector_lengths <- map(prob_vectors_test, length)

identical(unlist(prob_vector_lengths), 66:200) # OMG ALSO TRUE I DID IT

# Can I plot the shape of the probabilities on Kelly's vs my 40 vs other?
par(mfrow = c(1, 3))
plot(probs_table$circuit, probs_table$prob_of_color, pch=16)
plot(probs_table$circuit, get_prob_vector_r(circuits), pch=16)
plot(1:65, get_prob_vector_r(65), pch=16)
par(mfrow = c(1, 1))

# Ok, this is suuuuuuper good. But I really need to mirror the last 10% being
# prob = 1. I have the first 10 percent being a seq from 0, so I am doing 10% +1
# to account for that extra 0 on the front, then I'm doing 90%-1 to account too.
# Do I instead want to do 10%+1, 80%, 10% all 1? Can I make that work and pass tests?

get_prob_vector_80 <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1)
  middle80perc <- seq(0.02857143, 1, length.out = round((circuits*.80)))[-1]
  last10perc <- rep(1, round(circuits*.10))

  prob_vector <- c(first10perc, middle80perc, last10perc)

  return(prob_vector)
}

get_prob_vector_80(45) # gives me 44, noooo

# This works with circuits == 20
first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1) # 3 values
middle80perc <- seq(0.02857143, 1, length.out = round((circuits*.80)))[-1] #15
last10perc <- rep(1, round(circuits*.10)) # 2

# But with circuits == 45
first10perc <- seq(0, 0.02857143, length.out = round(45*.10)+1) # 5 (4.5 rounded to 4 +1)
middle80perc <- seq(0.02857143, 1, length.out = round((45*.80)))[-1] # 35 (36-1, this is ok)
last10perc <- rep(1, round(45*.10)) # 4 (4.5 rounded to 4 but needs to be 5)

# What if I added the +1 into the round in first10perc?
get_prob_vector_80_r <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round((circuits*.10)+1))
  middle80perc <- seq(0.02857143, 1, length.out = round((circuits*.80)))[-1]
  last10perc <- rep(1, round(circuits*.10))

  prob_vector <- c(first10perc, middle80perc, last10perc)

  return(prob_vector)
}

get_prob_vector_80_r(45) # gives me 45, good

# Run test on 2:200
prob_vectors_test2 <- map(2:200, get_prob_vector_80_r)

prob_vector_lengths2 <- map(prob_vectors_test2, length)

identical(unlist(prob_vector_lengths2), 2:200) # FALSE BOO

# Ok, well, I know I can get 10% and 90% to work and that it's a pretty
# good approximation of Kelly's algorithm. Maybe I leave it at that.

# If I can get the length.out to work for 90%, then can I just take away
# a percentage of that? What percentage of 90% of a whole is 10% of that whole?
# For 100, that would be 10 out of 90 which is .11111repeating
# and that won't always be a round number

round90perc <- function(x){
  paste0("90% of ", x, " is ", x*.9, " and 10% of that is ", (x*.9)*.11111111)
}

map(2:10, round90perc)

# I would just need some portion of the 90% - as long as it stayed the same length
# total, it would work. I think.

get_prob_vector_r_prop <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1) # 3

  last90perc_length <- round((circuits*.90)-1) # 17

  last10perc_length <- round(last90perc_length * (1/9)) # 2

  middle80perc_length <- last90perc_length - last10perc_length # 15

  middle80perc <- seq(0.02857143, 1, length.out = middle80perc_length+2)[-c(1, middle80perc_length+2)]

  last10perc <- rep(1, last10perc_length)

  prob_vector <- c(first10perc, middle80perc, last10perc)

  return(prob_vector)
}

# Did that work?

# Run test on 2:200
prob_vectors_test3 <- map(2:200, get_prob_vector_r_prop)

prob_vector_lengths3 <- map(prob_vectors_test3, length)

identical(unlist(prob_vector_lengths3), 2:200) # TRUE OMG I DIE

# OMG I COULD HAVE JUST USED BASIC SUBTRACTION
#
get_prob_vector_r_prop2 <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1) # 3

  last90perc_length <- circuits - length(first10perc)

  last10perc_length <- round(last90perc_length * (1/9)) # 2

  middle80perc_length <- last90perc_length - last10perc_length # 15

  middle80perc <- seq(0.02857143, 1, length.out = middle80perc_length+2)[-c(1, middle80perc_length+2)]

  last10perc <- rep(1, last10perc_length)

  prob_vector <- c(first10perc, middle80perc, last10perc)

  return(prob_vector)
}

# Again, plot the shape of the probabilities on Kelly's vs my 40 vs larger
par(mfrow = c(1, 3))
plot(probs_table$circuit, probs_table$prob_of_color, pch=16)
plot(probs_table$circuit, get_prob_vector_r_prop2(circuits), pch=16)
plot(1:65, get_prob_vector_r_prop2(65), pch=16)
par(mfrow = c(1, 1))

# https://youtu.be/_6Vn32wboVA?si=L98EFNWVaFNZgdAX
# left off at 09:49 or something
#
# For a size by size matrix, the maximum value is size*size
# So he's starting with 1 and working in to the max value
#
# If the starting value is more than 1, say 3, then the max value increases
# by initial-1

# M is a square matrix of zeros, iv is an initial value
spiral_matrix <- function(M, iv){

  # Make sure user inputs a square matrix of zeros
  if(nrow(M) != ncol(M)){
    stop("The matrix must be square")
  }

  # Set initial value of top left cell (row 1, col 1)
  M[1, 1] <- iv

  i_start <- 1
  j_start <- 1
  i_end <- nrow(M)
  j_end <- ncol(M)
  start <- 1
  end <- ncol((M))

  count1 <- 0 # control number rows in loop 2 and loop 4
  count2 <- 1 # control number columns in loop 3

  # set condition to stop the loop
  Smax <- (nrow(M))^2 + (iv - 1) # max value
  SV <- sum(abs(iv:Smax)) # sum of the sequence from the initial value to the max
  SM <- sum(abs(M)) # sum of the balue in the matrix M

  # loop starts

  while(SM < SV){

    for(j in start:(end-1)){ # for init loop, this covers row 1 minus the last col

      if(M[i_start, (j + 1)] == 0){ # if the next number is 0

        M[i_start, (j + 1)] <- M[i_start, j] + 1 # make the next number this number plus 1
      }
    }
    # check sum

    SM <- sum(abs(M))
    if(SM == SV) break # break loop if condition is fulfilled

    # another loop
    for(i in (start + count1):(end - 1)){ # for init loop, this is 1:ncol-1, so all but the last value again

      M[(i + 1), j_end] <- M[i, j_end] + 1 # row i + 1 (row 2), end column, assign 1 more than the end col of row 1
    }

    # Check sum
    SM <- sum(abs(M)) # line 83
    if(SM == SV) break

    # third loop
    for(j in end:(start + count2)){

      M[i_end, (j - 1)] <- M[i_end, j] + 1
    }

    # check sum, line 93
    SM <- sum(abs(M))
    if(SM == SV) break

    # loop 4, line 97
    for(i in (end - 1):(start + 1 + count1)){

      M[i, j_start] <- M[(i + 1), j_start] + 1

    }

    # check sum, line 105
    SM <- sum(abs(M))
    if(SM == SV) break

    # update the initial values
    j_end <- j_end - 1
    i_end <- i_end - 1
    i_start <- i_start + 1
    j_start <- j_start + 1
    end <- end - 1

    count1 <- count1 + 1
    count2 <- count2 + 1

  } # end

  return(M)
}

M <- matrix(0, nrow = 5, ncol = 5)
M
spiral_matrix(M, 1)



##################shiny
library(ggplot2)
library(shinyWidgets)
library(DT)
library(grid)
library(gridExtra)

# Define UI
ui <- fluidPage(
  titlePanel("Random Lines Plot"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_lines", "Number of Lines to Plot", value = 1, min = 1),
      uiOutput("color_pickers"),
      actionButton("plot_btn", "Plot Lines"),
      downloadButton("download_plot", "Download Plot"),
      downloadButton("download_table", "Download Table")
    ),
    mainPanel(
      plotOutput("plot"),
      DTOutput("color_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Generate color pickers based on number of lines
  output$color_pickers <- renderUI({
    req(input$num_lines)
    lapply(1:input$num_lines, function(i) {
      shinyWidgets::colourInput(paste0("color_", i), label = paste("Line", i, "Color:"), value = "red")
    })
  })

  # Reactive values for storing line data
  lines_data <- reactiveValues(lines = NULL, colors = NULL)

  # Generate random lines data
  observeEvent(input$plot_btn, {
    lines_data$lines <- replicate(input$num_lines, {
      data.frame(x = 1:10, y = runif(10))
    })
    lines_data$colors <- lapply(1:input$num_lines, function(i) {
      input[[paste0("color_", i)]]
    })
  })

  # Plot lines
  output$plot <- renderPlot({
    if (!is.null(lines_data$lines)) {
      ggplot() +
        lapply(1:input$num_lines, function(i) {
          geom_line(data = lines_data$lines[[i]], aes(x = x, y = y, color = lines_data$colors[[i]]))
        }) +
        theme_minimal()
    }
  })

  # Generate color table
  output$color_table <- renderDT({
    if (!is.null(lines_data$colors)) {
      colors_df <- data.frame(Color = unlist(lines_data$colors), Hex_Code = sapply(unlist(lines_data$colors), colors))
      datatable(colors_df, rownames = FALSE, options = list(dom = 't'))
    }
  })

  # Download plot
  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      ggsave(file, plot = output$plot, width = 6, height = 5, units = "in", dpi = 300)
    }
  )

  # Download color table
  output$download_table <- downloadHandler(
    filename = function() {
      "color_table.pdf"
    },
    content = function(file) {
      pdf(file, width = 6, height = 4)
      grid.table(output$color_table$V)
      dev.off()
    }
  )
}


shinyApp(ui = ui, server = server)





##### Testing a shiny app

# Load packages
library(shiny)
library(bslib)
library(ggplot2)
library(pracma)
library(markdown)

# Source the file containing all the functions, assumes files are in same R folder
source("R/01-functions.R")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),

  # Center align all content
  tags$div(
    style = "text-align: center; max-width: 600px; margin: auto;",

    titlePanel("An Ode to Ellsworth Kelly"),

    HTML(markdown::markdownToHTML(text = "This is a very simple app with lots of fun math under the hood. Have you ever heard of [Ellsworth Kelly](https://ellsworthkelly.org/)? He created an amazing series called Spectrum Colors Arranged by Chance. He used random chance within constraints to create beautiful artwork. This app lets you recreate a grid in the style of two pieces from the series. Choose a white background to create something akin to Piece III, and choose a black background to create something like Piece IV. Each piece will be different, so if you like what you see, save it! Thanks for stopping by 👋 - [Libby Heeren](www.libbyheeren.com/blog)")),

    # Inputs
    sliderInput("size", "What size grid would you like?", min = 13, max = 60, value = 40, width = "100%"),
    selectInput("bg_color", "Which background color would you like?",
                choices = c("Paper White" = "#EDEFEE", "Black" = "black"),
                selected = "#EDEFEE", width = "100%"),

    # Button
    actionButton("generate", "Calculate Art Piece", class = "btn-primary"),

    # Spacer
    tags$br(), tags$br(),

    # Plot of the art piece
    plotOutput("art_plot", height = "600px"),

    # Plot of the paint-by-numbers version
    plotOutput("pbn_plot", height = "600px"),

    # Swatch
    plotOutput("swatch_plot", height = "600px")
  )
)

server <- function(input, output, session) {

  artwork_data <- eventReactive(input$generate, {
    size <- input$size
    background <- input$bg_color

    # Calculate number of circuits based on size
    circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2) # Num of concentric circuits

    # Define the colors
    all_colors <- c(#"#EDEFEE", # Paper
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

    if (background == "#EDEFEE"){ # If background is paper white
      colors <- all_colors
    } else { # else (if black), remove black from colors
      colors <- all_colors[all_colors != "#000000"]
    }

    # Add logic to create all the plots I want in the final app,
    # Even if I don't use them all right now

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

return(final_df)
})

output$art_plot <-
  renderPlot({
    final_df <- artwork_data()

    ggplot(final_df, aes(x = x, y = y, fill = color)) +
      geom_tile() +  # Add tiles
      scale_fill_identity() +  # Use the colors stored as strings in the color column
      theme_void() +  # Remove axis labels and background
      coord_equal()  # Use equal aspect ratio
  })

output$pbn_plot <-
  renderPlot({
    final_df <- artwork_data()

    ggplot(final_df, aes(x = x, y = y, fill = color, label = color_numbers)) +
      geom_tile(alpha = 0.5, color = "black") +  # Add tiles
      geom_text(size = 2.6) +
      scale_x_continuous(position = "top",
                         expand = c(0,0),
                         breaks =  1:nrow(final_df)) +
      # scale_y_reverse(expand = c(0,0),
      #                 breaks =  1:nrow(final_df)) +
      scale_fill_identity() +  # Use the colors stored as strings in the color column
      #theme_void() +  # Remove axis labels and background
      coord_equal() +  # Use equal aspect ratio
      labs(x = NULL, y = NULL) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 90))
  })

output$swatch_plot <-
  renderPlot({
    final_df <- artwork_data()

    plot_number_swatch(colors_and_numbers)
  })


}

shinyApp(ui, server)



#####################
####################
# Load packages
library(shiny)
library(bslib)
library(ggplot2)
library(pracma)
library(markdown)
library(grDevices) # For PDF creation

# Source the file containing all the functions, assumes files are in same R folder
source("R/01-functions.R")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),

  # Center align all content
  tags$div(
    style = "text-align: center; max-width: 600px; margin: auto;",

    titlePanel("An Ode to Ellsworth Kelly"),

    HTML(markdown::markdownToHTML(text = "This is a very simple app with lots of fun math under the hood. Have you ever heard of [Ellsworth Kelly](https://ellsworthkelly.org/)? He created an amazing series called Spectrum Colors Arranged by Chance. He used random chance within constraints to create beautiful artwork. This app lets you recreate a grid in the style of two pieces from the series. Choose a white background to create something akin to Piece III, and choose a black background to create something like Piece IV. Each piece will be different, so if you like what you see, save it! Thanks for stopping by 👋 - [Libby Heeren](www.libbyheeren.com/blog)")),

    # Inputs
    sliderInput("size", "What size grid would you like?", min = 13, max = 60, value = 40, width = "100%"),
    selectInput("bg_color", "Which background color would you like?",
                choices = c("Paper White" = "#EDEFEE", "Black" = "black"),
                selected = "#EDEFEE", width = "100%"),

    # Buttons row
    fluidRow(
      column(6, actionButton("generate", "Calculate Art Piece", class = "btn-primary")),
      column(6, downloadButton("download_pdf", "Download PDF", class = "btn-primary"))
    ),

    # Spacer
    tags$br(),

    # Note to scroll down!
    p("Scroll down 👇"),

    # Plot of the art piece
    plotOutput("art_plot", height = "800px"),

    # Plot of the paint-by-numbers version
    plotOutput("pbn_plot", height = "800px"),

    # Swatch
    plotOutput("swatch_plot", height = "600px")
  )
)

server <- function(input, output, session) {

  artwork_data <- eventReactive(input$generate, {
    size <- input$size
    background <- input$bg_color

    # Calculate number of circuits based on size
    circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2) # Num of concentric circuits

    # Define the colors
    all_colors <- c(#"#EDEFEE", # Paper
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

    if (background == "#EDEFEE"){ # If background is paper white
      colors <- all_colors
    } else { # else (if black), remove black from colors
      colors <- all_colors[all_colors != "#000000"]
    }

    # Add logic to create all the plots I want in the final app,
    # Even if I don't use them all right now

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
    final_df$color_numbers <- convert_colors_to_numbers(final_df$color, background)

    # Create a vector of the unique colors and their representative numbers to swatch
    colors_and_numbers <- unique(final_df[,c('color','color_numbers')])

    return(final_df)
  })

  # Function to create the art plot
  get_art_plot <- function(final_df) {

    ggplot(final_df, aes(x = x, y = y, fill = color)) +
      geom_tile() +  # Add tiles
      scale_fill_identity() +  # Use the colors stored as strings in the color column
      theme_void() +  # Remove axis labels and background
      coord_equal()  # Use equal aspect ratio
  }

  # Function to create the paint-by-numbers plot
  get_pbn_plot <- function(final_df) {
    grid_size <- sqrt(nrow(final_df))
    text_size <- 120 / grid_size


    ggplot(final_df, aes(x = x, y = y, fill = color, label = color_numbers)) +
      geom_tile(alpha = 0.5, color = "black") +  # Add tiles
      geom_text(size = text_size) +
      scale_x_continuous(position = "top",
                         expand = c(0,0),
                         breaks =  1:grid_size) +
      scale_y_continuous(expand = c(0,0),
                         breaks =  1:grid_size,
                         labels = rev(1:grid_size)) +
      scale_fill_identity() +  # Use the colors stored as strings in the color column
      coord_equal() +  # Use equal aspect ratio
      labs(x = NULL, y = NULL) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 90))
  }

  # Function to create the swatch plot
  get_swatch_plot <- function(final_df) {
    colors_and_numbers <- unique(final_df[,c('color','color_numbers')])

    plot_number_swatch(colors_and_numbers)
  }

  output$art_plot <- renderPlot({
    final_df <- artwork_data()
    get_art_plot(final_df)
  })

  output$pbn_plot <- renderPlot({
    final_df <- artwork_data()

    get_pbn_plot(final_df)
  }, res = 108)

  output$swatch_plot <- renderPlot({
    final_df <- artwork_data()
    get_swatch_plot(final_df)
  })

  # PDF download handler
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("EllsworthKelly-Artwork-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf", sep = "")
    },
    content = function(file) {
      final_df <- artwork_data()

      # Recreate the swatch data inside the download function
      colors_and_numbers <- unique(final_df[, c("color", "color_numbers")])

      # Open the PDF device
      pdf(file, width = 8.5, height = 11, onefile = TRUE)

      # Page 1: Art Plot
      print(get_art_plot(final_df))

      # Page 2: Paint-by-numbers Plot
      print(get_pbn_plot(final_df))

      # Page 3: Swatch Plot (base graphics)
      plot_number_swatch(colors_and_numbers)

      # Close the PDF
      dev.off()
    }
  )

}

shinyApp(ui, server)
