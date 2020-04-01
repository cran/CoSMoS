## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, eval = FALSE)

## ---- message = FALSE---------------------------------------------------------
#  library(CoSMoS)

## -----------------------------------------------------------------------------
#  marginaldist <- 'ggamma'
#  param <- list(scale = 1,
#                shape1 = .8,
#                shape2 = .8)

## -----------------------------------------------------------------------------
#  no <- 1000
#  ggamma_sim <- rggamma(n = no,
#                        scale = 1,
#                        shape1 = 1,
#                        shape2 = .5)
#  plot.ts(ggamma_sim, main = "")
#  par(mfrow = c(1, 2))
#  plot(density(ggamma_sim), main = "")
#  acf(ggamma_sim, main = "")

## -----------------------------------------------------------------------------
#  acf <- c(1, 0.8)
#  ggamma_sim <- generateTS(n = no,
#                           margdist = marginaldist,
#                           margarg = param,
#                           acsvalue = acf)
#  plot(ggamma_sim, main = "")
#  par(mfrow = c(1, 2))
#  plot(density(ggamma_sim[[1]]), main = "")
#  acf(ggamma_sim[[1]], main = "")

## -----------------------------------------------------------------------------
#  acf <- c(1, 0.5, 0.5, 0.4, 0.4) #up to lag-4
#  ggamma_sim <- generateTS(n = no,
#                           margdist = marginaldist,
#                           margarg = param,
#                           acsvalue = acf)
#  plot(ggamma_sim, main = "")
#  par(mfrow = c(1, 2))
#  plot(density(ggamma_sim[[1]]), main = "")
#  acf(ggamma_sim[[1]], main = "")

## -----------------------------------------------------------------------------
#  ## specify lag
#  lags <- 0:10
#  
#  ## get the ACS
#  f <- acs('fgn', t = lags, H = .75)
#  b <- acs('burrXII', t = lags, scale = 1, shape1 = .6, shape2 = .4)
#  w <- acs('weibull', t = lags, scale = 2, shape = 0.8)
#  p <- acs('paretoII', t = lags, scale = 3, shape = 0.3)
#  
#  ## visualize the ACS
#  dta <- data.frame(lags, f, b, w, p)
#  
#  m.dta <- melt(dta, id.vars = 'lags')
#  
#  ggplot(m.dta,
#         aes(x = lags,
#             y = value,
#             group = variable,
#             colour = variable)) +
#    geom_point(size = 2.5) +
#    geom_line(lwd = 1) +
#    scale_color_manual(values = c('steelblue4', 'red4', 'green4', 'darkorange'),
#                       labels = c('FGN', 'Burr XII', 'Weibull', 'Pareto II'),
#                       name = '') +
#    labs(x = bquote(lag ~ tau),
#         y = 'Acf') +
#    scale_x_continuous(breaks = t) +
#    theme_grey()

## -----------------------------------------------------------------------------
#  acf = acs(id = 'paretoII',
#            t = 0:30,
#            scale = 1,
#            shape = .75)
#  
#  ggamma_sim <- generateTS(n = no,
#                           margdist = marginaldist,
#                           margarg = param,
#                           acsvalue = acf)
#  par(mfrow = c(1, 1))
#  plot(ggamma_sim, main = "")

## -----------------------------------------------------------------------------
#  my_acf <- exp(seq(0, -2, -0.1))
#  ggamma_sim <- generateTS(n = no,
#                           margdist = marginaldist,
#                           margarg = param,
#                           acsvalue = my_acf)
#  par(mfrow = c(1, 1))
#  plot(ggamma_sim, main = "")
#  par(mfrow = c(1, 2))
#  acf(ggamma_sim[[1]])

## ---- fig.height = 7----------------------------------------------------------
#  prob_zero <- .9
#  ggamma_sim <- generateTS(n = no,
#                           margdist = marginaldist,
#                           margarg = param,
#                           acsvalue = acf,
#                           p0 = prob_zero,
#                           TSn = 5)
#  par(mfrow = c(1, 1))
#  plot(ggamma_sim, main = "")

## -----------------------------------------------------------------------------
#  checkTS(ggamma_sim)

## -----------------------------------------------------------------------------
#  data('precip')
#  plot(precip, type = 'l')
#  par(mfrow = c(1, 2))
#  plot(density(precip$value), main = "", xlim = c(0, 10)) #Does not plot extreme values
#  acf(precip$value, main = "", lag.max = 20)

## ---- fig.height = 9----------------------------------------------------------
#  precip_ggamma <- analyzeTS(TS = precip,
#                                     season = 'month',
#                                     dist = 'ggamma',
#                                     acsID = "weibull",
#                                     lag.max = 12)
#  reportTS(precip_ggamma, 'dist')
#  reportTS(precip_ggamma, 'acs')
#  reportTS(precip_ggamma, 'stat')

## ---- warning = FALSE, fig.height = 9-----------------------------------------
#  precip_pareto <- analyzeTS(TS = precip,
#                                     season = 'month',
#                                     dist = 'paretoII',
#                                     acsID = 'fgn',
#                                     lag.max = 12)
#  reportTS(precip_pareto, 'dist')
#  reportTS(precip_pareto, 'acs')

## -----------------------------------------------------------------------------
#  sim_precip <- simulateTS(aTS = precip_ggamma,
#                           from = as.POSIXct('1978-12-01 00:00:00'),
#                           to = as.POSIXct('2008-12-01 00:00:00'))
#  dta <- precip
#  dta[, id := 'observed']
#  sim_precip[, id := 'simulated']
#  
#  dta <- rbind(dta, sim_precip)
#  
#  ggplot(dta) +
#    geom_line(aes(x = date, y = value)) +
#    facet_wrap(~id, ncol = 1) +
#    theme_classic()

## -----------------------------------------------------------------------------
#  id <- '02135000'
#  dta_raw <- as.data.table(read.fwf(sprintf(
#    'ftp://hydrology.nws.noaa.gov/pub/gcip/mopex/US_Data/Us_438_Daily/%s.dly', id),
#                                    widths = c(8,10,10,10,10,10),
#                                    col.names = c('date', 'P', 'E', 'value', 'Tmax', 'Tmin')))
#  
#  dta_raw[, date := as.POSIXct(gsub(' ','0', date), format = '%Y%m%d')]
#  
#  daily_streamflow <- dta_raw[value >= 0, .(date, value)]
#  
#  str_burr <- analyzeTS(daily_streamflow,
#                        dist = 'ggamma',
#                        norm = 'N4',
#                        acsID = 'paretoII',
#                        lag.max = 50)
#  
#  sim_str <- simulateTS(str_burr)
#  
#  dta <- daily_streamflow
#  dta[, id := 'observed']
#  sim_str[, id := 'simulated']
#  
#  dta <- rbind(dta, sim_str)
#  
#  ggplot(dta) +
#    geom_line(aes(x = date, y = value)) +
#    facet_wrap(~id, ncol = 1) +
#    theme_classic()

