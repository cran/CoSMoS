## ---- message = FALSE----------------------------------------------------
library(CoSMoS)

marginaldist <- 'burrXII'
para <- list(scale = 2,
             shape1 = .75,
             shape2 = .1)

## ------------------------------------------------------------------------
p0 <- .9

## ------------------------------------------------------------------------
order <- 100
targetcorr <- acfparetoII(t = 0:order, scale = 15, shape = .3)

## ------------------------------------------------------------------------
p <- actpnts(marginaldist, para, p0 = p0)

## ------------------------------------------------------------------------
f <- fitactf(p)
plot(f)

## ------------------------------------------------------------------------
target <- ARp(margdist = marginaldist,
              margarg = para,
              actfpara = f,
              n = 100000,
              p0 = p0,
              p = order,
              acsvalue = targetcorr)

## ---- fig.show = 'hold'--------------------------------------------------
gauss <- attr(target, 'gaussian')

ggplot() +
  geom_point(aes(x = 0:order,
                 y = as.vector(acf(gauss, lag.max = order, plot = F)$acf)), colour = 'red2', alpha = .2) +
  geom_line(aes(x = 0:order,
                y = attr(target, 'trACS')), colour = 'red4') +
  theme_bw() +
  labs(x = 'Lag',
       y = 'ACF',
       title = 'Gaussian ACS')

ggplot() +
  geom_point(aes(x = 0:order,
                 y = as.vector(acf(target, lag.max = order, plot = F)$acf)), colour = 'steelblue2', alpha = .2) +
  geom_line(aes(x = 0:order,
                y = targetcorr), colour = 'steelblue4') +
  theme_bw() +
  labs(x = 'Lag',
       y = 'ACF',
       title = 'Target ACS')

## ---- comment = '>'------------------------------------------------------
checksimulation(target)

## ---- out.width = '95%', fig.asp = .75, results = 'asis'-----------------
chck <- checkmodel(margdist = marginaldist,
                   margarg = para,
                   n = 100000,
                   p = order,
                   p0 = p0,
                   acsvalue = targetcorr,
                   TSn = 15,
                   plot = T)

knitr::kable(chck)

