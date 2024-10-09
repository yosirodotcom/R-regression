pacman::p_load(dplyr, knitr, ggplot2)

x <- c(20, 20, 30, 30, 30, 40, 40, 50, 50, 60)
y <- c(16.3, 26.7, 39.2, 63.5, 51.3, 98.4, 65.7, 104.1, 155.6, 217.2)

modelling <- function(xdata, ydata) {
  model <- lm(xdata ~ ydata)
  r2 <- summary(model)$r.squared
  return(r2)
}
r2 <- modelling(xdata = x, ydata = y)

# Create the plot
p <- ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regression Plot", x = "X", y = "Y") +
  annotate("text", x = mean(x), y = mean(y), label = paste("R-squared =", round(r2, 2)))

print(p)

linear_healing <- function(x, y) {
  y_ <- log(y)
  x_ <- x
  r2 <- modelling(xdata = x_, ydata = y_)

  dat <- data.frame(
    Model = "A",
    r2 = r2
  )


  y_ <- log(y)
  x_ <- log(x)
  r2 <- modelling(xdata = x_, ydata = y_)

  dat <- rbind(dat, data.frame(
    Model = "B",
    r2 = r2
  ))

  y_ <- 1 / y
  x_ <- x
  r2 <- modelling(xdata = x_, ydata = y_)

  dat <- rbind(dat, data.frame(
    Model = "C",
    r2 = r2
  ))

  y_ <- 1 / sqrt(y)
  print(y_)
  x_ <- x
  r2 <- modelling(xdata = x_, ydata = y_)

  dat <- rbind(dat, data.frame(
    Model = "D",
    r2 = r2
  ))

  y_ <- 1 / y
  x_ <- 1 / (1 + x)
  r2 <- modelling(xdata = x_, ydata = y_)

  dat <- rbind(dat, data.frame(
    Model = "E",
    r2 = r2
  ))

  y_ <- y
  x_ <- sqrt(x)
  r2 <- modelling(xdata = x_, ydata = y_)

  dat <- rbind(dat, data.frame(
    Model = "F",
    r2 = r2
  ))

  y_ <- sqrt(y)
  x_ <- x
  r2 <- modelling(xdata = x_, ydata = y_)

  dat <- rbind(dat, data.frame(
    Model = "G",
    r2 = r2
  ))

  dat <- dat %>% arrange(desc(r2))
  modelnum <- dat %>%
    head(1) %>%
    select(Model) %>%
    as.character()

  if (modelnum == "A") {
    y_ <- log(y)
    x_ <- x
  } else if (modelnum == "B") {
    y_ <- log(y)
    x_ <- log(x)
  } else if (modelnum == "C") {
    y_ <- 1 / y
    x_ <- x
  } else if (modelnum == "D") {
    y_ <- 1 / sqrt(y)
    x_ <- x
  } else if (modelnum == "E") {
    y_ <- 1 / y
    x_ <- 1 / (1 + x)
  } else if (modelnum == "F") {
    y_ <- y
    x_ <- sqrt(x)
  } else if (modelnum == "G") {
    y_ <- sqrt(y)
    x_ <- x
  }

  model <- lm(x_ ~ y_)
  r2 <- summary(model)$r.squared

  p <- ggplot(data.frame(x_, y_), aes(x_, y_)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Regression Plot", x = "X", y = "Y") +
    annotate("text", x = mean(x_), y = mean(y_), label = paste("R-squared =", round(r2, 2)))

  print(p)
  print(kable(dat))
}

linear_healing(x = x, y = y)
