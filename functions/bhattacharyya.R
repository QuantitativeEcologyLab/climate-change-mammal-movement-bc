# functions to calculate the Bhattacharyya distance for distributions:

# Poisson ----
bhattacharyya_pois <- function(lambda_1, lambda_2, pois_max = NULL,
                               quiet = FALSE) {
  
  #' determine the range of the approximated domain if `range` is NULL
  if(is.null(pois_max)) {
    pois_max <- qpois(1 - .Machine$double.eps * 10, max(lambda_1, lambda_2))
    if(! quiet) message(paste0('Approximating the domain using seq(0, ',
                               pois_max, ', by = 1).'))
  }
  
  # approximate the domain
  x <- seq(0, pois_max, by = 1)
  
  # find the distributions over the approximated domain
  pois_1 <- dpois(x = x, lambda = lambda_1, log = FALSE)
  pois_2 <- dpois(x = x, lambda = lambda_2, log = FALSE)
  
  # calculate the distance
  b_d <- -log(sum(sqrt(pois_1 * pois_2)))
  return(b_d)
}

# check stability
if(FALSE) {
  bhattacharyya_pois(lambda_1 = 10, lambda_2 = 10)
  plot(
    seq(20, 100, by = 1),
    sapply(seq(20, 100, by = 1),
           \(y) bhattacharyya_pois(lambda_1 = 10, lambda_2 = 50,
                                   pois_max = y)),
    type = 'l', ylab = 'Estimated Bhattacharyya distance',
    xlab = 'Number of sample points')
}

# Bernoulli ----
bhattacharyya_ber <- function(p_1, p_2) {
  
  # exact (finite) domain is {0, 1}
  x <- c(0, 1)
  
  # find the distributions over the domain
  bern_1 <- dbinom(x, size = 1, prob = p_1, log = FALSE)
  bern_2 <- dbinom(x, size = 1, prob = p_2, log = FALSE)
  
  # calculate the distance
  b_d <- -log(sum(sqrt(bern_1 * bern_2)))
  return(b_d)
}

if(FALSE) {
  bhattacharyya_ber(0.01, 0.02) # small difference
  bhattacharyya_ber(0.02, 0.01) # exactly as above
  bhattacharyya_ber(0.01, 0.99) # large difference
  bhattacharyya_ber(0.99, 0.01) # exactly as above
}

# Gamma ----
bhattacharyya_gamma <- function(shape_1, shape_2, scale_1, scale_2,
                                steps = 1000, quiet = FALSE) {
  
  eps <- .Machine$double.eps * 100
  
  # determine the approximated domain of the gamma distributions
  low_1 <- qgamma(p = eps, shape = shape_1, scale = scale_1)
  low_2 <- qgamma(p = eps, shape = shape_2, scale = scale_2)
  high_1 <- qgamma(p = 1 - eps, shape = shape_1, scale = scale_1)
  high_2 <- qgamma(p = 1 - eps, shape = shape_2, scale = scale_2)
  min_x <- floor(min(low_1, low_2))
  max_x <- ceiling(max(high_1, high_2))
  dx <- min(high_1 - low_1, high_2 - low_2) / steps
  if(! quiet) {
    message(paste0('Approximating the domain using seq(from = ', min_x,
                   ', to = ', max_x, ', by =', dx, ').'))
  }
  
  # approximate the domain
  x <- seq(min_x, max_x, by = dx)
  
  # find the distributions over the approximated domain
  gamma_1 <- dgamma(x, shape = shape_1, scale = scale_1, log = FALSE)
  gamma_2 <- dgamma(x, shape = shape_2, scale = scale_2, log = FALSE)
  
  # calculate the distance
  b_d <- -log(sum(sqrt(gamma_1 * gamma_2) * dx))
  return(b_d)
}

# check stability
if(FALSE) {
  # checking identical distributions
  bhattacharyya_gamma(shape_1 = 10, shape_2 = 10, scale_1 = 5, scale_2 = 5)
  
  # checking order of distributions
  bhattacharyya_gamma(shape_1 = 10, shape_2 = 20, scale_1 = 2, scale_2 = 1)
  bhattacharyya_gamma(shape_1 = 20, shape_2 = 10, scale_1 = 1, scale_2 = 2)
  
  # checking very different distributions
  bhattacharyya_gamma(shape_1 = 1, shape_2 = 100, scale_1 = 5, scale_2 = 10)
  bhattacharyya_gamma(shape_1 = 100, shape_2 = 1, scale_1 = 10, scale_2 = 5)
  
  plot(
    seq(1, 100, by = 1),
    sapply(seq(1, 100, by = 1),
           \(y) bhattacharyya_gamma(shape_1 = 100, scale_2 = 200,
                                    scale_1 = 4, shape_2 = 4,
                                    steps = y, quiet = T)),
    type = 'l', ylab = 'Estimated Bhattacharyya distance',
    xlab = 'Domain sample size')
  plot(
    seq(50, 1000, by = 1),
    sapply(seq(50, 1000, by = 1),
           \(y) bhattacharyya_gamma(4, 2, 10, 10, steps = y, quiet = T)) %>%
      round(5),
    type = 'l', ylab = 'Estimated Bhattacharyya distance',
    xlab = 'Domain sample size')
}

# Hurdle Gamma ----
bhattacharyya_h_gamma <- function(p_1, p_2, shape_1, shape_2, scale_1,
                                  scale_2, steps = 1e3, quiet = FALSE) {
  
  eps <- .Machine$double.eps * 100
  
  # determine the approximated domain of the gamma distributions
  low_1 <- qgamma(p = eps, shape = shape_1, scale = scale_1)
  low_2 <- qgamma(p = eps, shape = shape_2, scale = scale_2)
  high_1 <- qgamma(p = 1 - eps, shape = shape_1, scale = scale_1)
  high_2 <- qgamma(p = 1 - eps, shape = shape_2, scale = scale_2)
  min_x <- min(low_1, low_2)
  max_x <- max(high_1, high_2)
  dx <- min(high_1 - low_1, high_2 - low_2) / steps
  if(! quiet) {
    message(paste0('Approximating the domain using seq(from = ', min_x,
                   ', to = ', max_x, ', by =', dx, ').'))
  }
  
  # approximate the domain
  x <- seq(min_x, max_x, by = dx)
  
  # find the distributions over the approximated domain
  gamma_1 <- dgamma(x, shape = shape_1, scale = scale_1, log = FALSE)
  gamma_2 <- dgamma(x, shape = shape_2, scale = scale_2, log = FALSE)
  
  # calculate the distance
  BD <-
    (1 - p_1) * (1 - p_2) + # fail (0) => no gamma
    sum(sqrt((p_1 * gamma_1) * (p_2 * gamma_2))) * dx # success (1) => Gamma
  b_d <- -log(BD)
  return(b_d)
}

# check stability
if(FALSE) {
  # checking identical distributions
  bhattacharyya_h_gamma(p_1 = 0.3, p_2 = 0.3,
                        shape_1 = 10, shape_2 = 10, scale_1 = 5, scale_2 = 5)
  
  # checking order of distributions
  bhattacharyya_h_gamma(p_1 = 0.3, p_2 = 0.4,
                        shape_1 = 10, shape_2 = 20, scale_1 = 2, scale_2 = 1)
  bhattacharyya_h_gamma(p_1 = 0.4, p_2 = 0.3,
                        shape_1 = 20, shape_2 = 10, scale_1 = 1, scale_2 = 2)
  
  # checking very different distributions
  bhattacharyya_h_gamma(p_1 = 0.1, p_2 = 0.95,
                        shape_1 = 1, shape_2 = 100, scale_1 = 5, scale_2 = 10)
  bhattacharyya_h_gamma(p_1 = 0.95, p_2 = 0.1,
                        shape_1 = 100, shape_2 = 1, scale_1 = 10, scale_2 = 5)
  
  # very different distributions
  plot(10:1e3,
       sapply(10:1e3,
              \(y) bhattacharyya_h_gamma(
                p_1 = 0.1, shape_1 = 4, scale_1 = 100, # highly variable
                p_2 = 0.9, shape_2 = 2, scale_2 = 10,  # less variable
                steps = y, quiet = TRUE)),
       type = 'l',
       ylab = 'Estimated Bhattacharyya distance',
       xlab = 'Number of sample points')
  
  # very similar distributions
  plot(10:1e3,
       sapply(10:1e3,
              \(y) bhattacharyya_h_gamma(
                p_1 = 0.1, shape_1 = 4, scale_1 = 10,
                p_2 = 0.15, shape_2 = 4, scale_2 = 10,
                steps = y, quiet = TRUE)),
       type = 'l',
       ylab = 'Estimated Bhattacharyya distance',
       xlab = 'Number of sample points')
}
