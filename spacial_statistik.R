## AktStat
## Woche 12
## Prof.Dr. Peter Ruckdeschel
#############################################################

##--------------------------------------------------------
### Exposure und räumliche Statistik
##--------------------------------------------------------

## gebrauchte Pakete
packages <- c("tidyverse", "mgcv", "dplyr",
              "classInt", "rgdal", "sp", "ggplot2",
              "RColorBrewer", "grid", "gridExtra", 
              "visreg", "sf", "tmap", "rgeos", 
              "mapview", "leaflet", "MASS",
              "maptools", "maps", "mapproj", 
              "RODBC",  "classInt", "PBSmapping", 
              "RgoogleMaps", "spdep", "dismo")

suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))

##INLA: 
if(FALSE){
  install.packages("INLA",repos=c(getOption("repos"),
                                  INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
  ## fuer R-4.1
  ## Download the INLA_22.04.16.zip file at https://inla.r-inla-download.org/R/stable/bin/windows/contrib/4.1/###
  ## In RStudio, under the Tools menu, select Install Packages, and Install from: Package Archive File. 
  ## Tell RStudio where the .zip file is located on your local machine. From there it installed fine for m}
  #### das zip File findet sich auch im Uebungs-Ordner
}
require(distr)
######## R-Script von K. Antonio (adaptiert) 
## Quellen: 
#  https://katrienantonio.github.io/PE-pricing-analytics/
#  https://github.com/katrienantonio/PE-Programming-R-for-data-science/tree/master/day\%202     
###

## entweder man macht einen pull-request in git oder 
## man lädt die Files einzeln händisch herunter

## benoetigte Files:
#  postcode.zip und P&Cdata.txt
#  Referenz Henckaerts et al 2018 SAJ.pdf

mtpl_orig <- read.table('katrienantonio/data/P&Cdata.txt',
                        header = TRUE)

#----------------------------------------------------------
### Kurze Einführung in tibbles / Pipe-Notation
#----------------------------------------------------------

mtpl_orig <- as_tibble(mtpl_orig)
mtpl_orig %>% slice(1:3) %>% select(-LONG, -LAT) 

mtpl <- mtpl_orig %>%
  # rename all columns 
  rename_all(function(.name) {
    .name %>% 
      # replace all names with the lowercase versions
      tolower 
    # replace all spaces with underscores is also useful, with `str_replace(" ", "-")`
  })
mtpl <- rename(mtpl, expo = exp)

## -----------------------------------------------------------------------------------
mean(mtpl$nclaims)
sum(mtpl$nclaims)/sum(mtpl$expo)
mtpl %>% summarize(emp_freq = sum(nclaims) / sum(expo)) 

## -----------------------------------------------------------------------------------
dim(mtpl)

## -----------------------------------------------------------------------------------
mtpl %>% summarize(emp_freq = sum(nclaims) / sum(expo)) 

## -----------------------------------------------------------------------------------
mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo))

## -----------------------------------------------------------------------------------


#----------------------------------------------------------
### Anwendung in ggplot:
#----------------------------------------------------------

KULbg <- "#116E8A" ## Farbe KU Leuven, Uni Oldenburg koennte man auch nehmen...

g <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(col = KULbg, fill = KULbg) + 
  labs(y = "Abs frequency") +
  ggtitle("MTPL - number of claims")
g

## -----------------------------------------------------------------------------------
## Beispiel: Nutzung aes (aethetics) fuer Saeulen
g <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g

## -----------------------------------------------------------------------------------
## Beispiel: Berechnung in ggplot

g <- ggplot(mtpl, aes(nclaims)) + theme_bw()
g + geom_bar(aes(y = (..count..)/sum(..count..)), 
             col = KULbg, fill = KULbg) + 
  labs(y = "Relative frequency") +
  ggtitle("MTPL - relative number of claims")

## -----------------------------------------------------------------------------------
g <- ggplot(mtpl, aes(bm)) + theme_bw()
g + geom_histogram(binwidth = 1, col = KULbg, fill = KULbg, alpha = .5)

## -----------------------------------------------------------------------------------
g <- ggplot(mtpl, aes(bm)) + theme_bw()
g + geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1, col = KULbg, fill = KULbg, alpha = 0.5) + labs(y = "Relative frequency")

### Nun eine auf einem Gitter angeordnete Schar von Graphiken
## dazu:
## aendern
col <- KULbg
fill <- KULbg
ylab <- "Relative frequency"

# wrapper functions
ggplot.bar <- function(DT, variable, xlab){
  ggplot(data = DT, aes(as.factor(variable))) + theme_bw() + 
    geom_bar(aes(y = (..count..)/sum(..count..)), col = col, fill = fill, alpha = 0.5) + labs(x = xlab, y = ylab)
}

ggplot.hist <- function(DT, variable, xlab, binwidth){
  ggplot(data = DT, aes(variable)) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = binwidth, col = col, fill = fill, alpha = 0.5) + 
    labs(x = xlab, y = ylab)
}


## -----------------------------------------------------------------------------------
# Targets: frequency, exposure and total severity
plot.eda.nclaims <- ggplot.bar(mtpl, variable = mtpl$nclaims, "nclaims")
plot.eda.exp <- ggplot.hist(mtpl, mtpl$expo, "expo", 0.05)

mtpl.sev <- mtpl %>% filter(amount > 0 & avg <= 81000) # see SAJ paper for motivation
## klassisches R
mtpl.sev <- mtpl[mtpl$amount>0 & mtpl$avg <= 81000,]
plot.eda.amount <- ggplot(data = mtpl.sev, aes(avg)) + geom_density(adjust = 3, col = col, fill = fill, alpha = 0.5) + xlim(0, 1e4) + ylab(ylab) + xlab("severity") + theme_bw()

# Bar plots of factor variables
plot.eda.coverage <- ggplot.bar(mtpl, mtpl$coverage, "coverage")
plot.eda.fuel <- ggplot.bar(mtpl, mtpl$fuel, "fuel")
plot.eda.sex <- ggplot.bar(mtpl, mtpl$sex, "sex")
plot.eda.use <- ggplot.bar(mtpl, mtpl$use, "use")
plot.eda.fleet <- ggplot.bar(mtpl, mtpl$fleet, "fleet")

# Histograms of continuous variables
plot.eda.ageph <- ggplot.hist(mtpl, mtpl$ageph, "ageph", 2)
plot.eda.agec <- ggplot.hist(mtpl, mtpl$agec, "agec", 1)
plot.eda.bm <- ggplot.bar(mtpl, mtpl$bm, "bm")
plot.eda.power <- ggplot.hist(mtpl, mtpl$power, "power", 10)

# Putting these together
grid.arrange(plot.eda.nclaims, plot.eda.exp, plot.eda.amount, 
             plot.eda.coverage, plot.eda.fuel, plot.eda.sex, plot.eda.use, 
             plot.eda.fleet, plot.eda.ageph, plot.eda.power, plot.eda.agec, 
             plot.eda.bm, ncol = 4)



## -----------------------------------------------------------------------------------
## Einstieg in raeumliche Statistik:
## zunaechst einplotten in Graphiken
## -----------------------------------------------------------------------------------

belgium_shape_sf <- st_read('katrienantonio/shape file Belgie postcodes/npc96_region_Project1.shp', quiet = TRUE)
## npc96_region_Project1.shp ist im zip file postcode.zip
belgium_shape_sf <- st_transform(belgium_shape_sf, CRS("+proj=longlat +datum=WGS84"))


## -----------------------------------------------------------------------------------
class(belgium_shape_sf)
belgium_shape_sf %>% as_tibble() %>% slice(1:3) 

## -----------------------------------------------------------------------------------
ggplot(belgium_shape_sf) +
  geom_sf() +
  ggtitle("Welcome to Belgium!") +
  theme_bw()

## -----------------------------------------------------------------------------------
library(tmap) ## erlaubt interaktive Graphiken

# qtm(belgium_shape_sf) # does not work
# shapefile slightly corrupted!

# slightly smooth the shapefile
sf_use_s2(FALSE) ## ergaenzt von PR
simple_shp <- st_simplify(belgium_shape_sf, 
                          dTolerance = 0.000001)

# and plot
qtm(simple_shp)

## -----------------------------------------------------------------------------------
tm_shape(simple_shp) +
  tm_borders(col = KULbg, lwd = 0.5) +
  tm_layout(main.title = 'Welcome to Belgium!', legend.outside = TRUE, frame = FALSE) 


## -----------------------------------------------------------------------------------
### Berechne Exposure je Postbezirk

post_expo <- mtpl %>% group_by(pc) %>% summarize(num = n(), total_expo = sum(expo)) 

post_expo %>% slice(1:5) 

## Wir joinen (left join) die Datensätze belgium_shape_sf und post_expo
## anhand der Variablen "POSTCODE" (belgium_shape_sf) und "pc" (post_expo)  
## und schreiben das Ergebnis wieder in belgium_shape_sf

belgium_shape_sf <- left_join(belgium_shape_sf, 
                              post_expo, 
                              by = c("POSTCODE" = "pc"))


## -----------------------------------------------------------------------------------
## Berechnung der Schadenfrequenz je Fläche

belgium_shape_sf$freq <- 
  belgium_shape_sf$total_expo/belgium_shape_sf$Shape_Area


## -----------------------------------------------------------------------------------
## drei Kategorien "low", "average", "high" aus freq gemäß Quantilen

belgium_shape_sf$freq_class <- cut(belgium_shape_sf$freq, 
                                   breaks = quantile(belgium_shape_sf$freq, c(0,0.2,0.8,1), na.rm = TRUE),
                                   right = FALSE, include.lowest = TRUE, 
                                   labels = c("low", "average", "high"))

## Plot Belgien in ggplot nach Variable freq_class gefärbt
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = belgium_shape_sf$freq_class), colour = "black", size = 0.1) +
  ggtitle("MTPL claim frequency data") + labs(fill = "Relative\nexposure") +
  scale_fill_brewer(palette = "Blues", na.value = "white") + 
  theme_bw()



library(tmap) ### erlaubt interaktive Plots

# slightly smooth the shapefile
belgium_shape_sf <- st_simplify(belgium_shape_sf, 
                                dTolerance = 0.00001)

# and plot
(mymap <- tm_shape(belgium_shape_sf) + 
    tm_borders(col = "black") + 
    tm_fill(col = "freq_class", style = "cont", palette = "Blues", colorNA = "white"))

## -----------------------------------------------------------------------------------
## leaflet ... toller interaktiver Effekt...
tmap_leaflet(mymap)


## -----------------------------------------------------------------------------------
## semiparametrische Regressionsmodelle mit räumlichen Prädiktoren
## -----------------------------------------------------------------------------------

## Regression models
library(tidyverse)
library(gridExtra)
library(mgcv)


KULbg <- "#116E8A"

## zunächst ein Plot
## -----------------------------------------------------------------------------------
g_freq <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg, alpha = .5) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g_freq

## -----------------------------------------------------------------------------------

g_sev <- ggplot(mtpl, aes(x = avg)) + theme_bw() +
  geom_histogram(bins = 30, boundary = 0, color = KULbg, fill = KULbg, alpha = .5) + 
  labs(x = "claim severity") +
  xlim(c(0, 20000))
g_sev

## -----------------------------------------------------------------------------------
### gibt es mehr Schäden bei Frauen?
## -----------------------------------------------------------------------------------
freq_by_gender <- mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) 
freq_by_gender 

## -----------------------------------------------------------------------------------
ggplot(freq_by_gender, aes(x = sex, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", col = KULbg, fill = KULbg, alpha = .5)

## -----------------------------------------------------------------------------------
### gibt es mehr Schäden bei Frauen?

freq_glm_1 <- glm(nclaims ~ sex, offset = log(expo), 
                  family = poisson(link = "log"), 
                  data = mtpl)
freq_glm_1 %>% broom::tidy() 

## -----------------------------------------------------------------------------------
exp(coef(freq_glm_1)[1])
exp(coef(freq_glm_1)[1] + coef(freq_glm_1)[2])

## -----------------------------------------------------------------------------------
KULbg <- "#116E8A"

# number 1
library(MASS)

## wir fitten nun einige gams an die Variable accel im Datensatz mcycle aus MASS
## genauer ist times ein nichtparametrischer Prädiktor
# 1 Spline mit zwei Basisfunktionen 
bias_model <- gam(accel ~ s(times, sp = 0, k = 2), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
(p_1 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
    geom_point(alpha = .3) +
    geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_x_continuous(expand = c(0, 0)) + 
    ggtitle("sp = 0 and k = 2"))

# 2  Spline mit 5 Basisfkt
bias_model <- gam(accel ~ s(times, sp = 0, k = 5), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
(p_2 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
    geom_point(alpha = .3) +
    geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle("sp = 0 and k = 5"))

# 3  Spline mit 55 Basisfct
bias_model <- gam(accel ~ s(times, sp = 0, k = 55), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
(p_3 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
    geom_point(alpha = .3) +
    geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_x_continuous(expand = c(0, 0)) + 
    ggtitle("sp = 0 and k = 15"))
# number 4
## Default von s() siehe ?gam -->optimales sp, k
library(MASS)
bias_model <- gam(accel ~ s(times), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
(p_4 <- ggplot(mcycle, aes(times, accel)) + theme_bw() + 
    geom_point(alpha = .3) +
    geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_x_continuous(expand = c(0, 0)) + 
    ggtitle("optimal sp and default k"))
# number 5
## sehr glatt
bias_model <- gam(accel ~ s(times, sp = 3), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
(p_5 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
    geom_point(alpha = .3) +
    geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_x_continuous(expand = c(0, 0)) + 
    ggtitle("sp = 3 and default k"))
# number 6
# extrem glatt
bias_model <- gam(accel ~ s(times, sp = 20), data = mcycle)
mcycle$predictions <- predict(bias_model, mcycle)
(p_6 <- ggplot(mcycle, aes(times, accel)) + theme_bw() +
    geom_point(alpha = .3) +
    geom_line(aes(times, predictions), size = 1.0, color = KULbg) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle("sp = 10 and default k"))

gridExtra::grid.arrange(p_1, p_2, p_3, p_4, p_5, p_6, nrow = 2)

## -----------------------------------------------------------------------------------
## kubische Spline
model <- gam(accel ~ s(times, sp = 1.2,
                       k = 5, bs = "cr"),
             family = gaussian, data = mcycle)

## -----------------------------------------------------------------------------------
## kubische Spline mit REML und Default Glättung
model <- gam(accel ~ s(times, bs = "cr"),
             method = "REML",
             family = gaussian, data = mcycle)

## -----------------------------------------------------------------------------------
print(model)

## -----------------------------------------------------------------------------------
model$sp

## -----------------------------------------------------------------------------------
## klassische Plots:
plot(model, pages = 1, scheme = 0)
plot(model, pages = 1, scheme = 1)

## -----------------------------------------------------------------------------------
## Schadenhaeufigkeit je Alter

mtpl %>% group_by(ageph) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) %>% 
  ggplot(aes(x = ageph, y = emp_freq)) + theme_bw() +
  geom_point(color = KULbg)

## -----------------------------------------------------------------------------------
a <- min(mtpl$ageph):max(mtpl$ageph)

## -----------------------------------------------------------------------------------
## Poisson-GLM fuer die Schadenhaeufigkeiten

## erst mit Alter als numerischem Prädiktor
freq_glm_age <- glm(nclaims ~ ageph, offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age <- predict(freq_glm_age, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age <- pred_glm_age$fit
l_glm_age <- pred_glm_age$fit - qnorm(0.975)*pred_glm_age$se.fit
u_glm_age <- pred_glm_age$fit + qnorm(0.975)*pred_glm_age$se.fit
df <- data.frame(a, b_glm_age, l_glm_age, u_glm_age)

## -----------------------------------------------------------------------------------
p_glm_age <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age <- p_glm_age + geom_line(aes(a, b_glm_age), size = 1, col = KULbg)   
p_glm_age <- p_glm_age + geom_line(aes(a, u_glm_age), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age), size = 0.5, linetype = 2, col = KULbg)
p_glm_age <- p_glm_age + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age

## -----------------------------------------------------------------------------------
## erst mit Alter als kategorieller Prädiktor
freq_glm_age_f <- glm(nclaims ~ as.factor(ageph), offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age_f <- predict(freq_glm_age_f, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age_f <- pred_glm_age_f$fit
l_glm_age_f <- pred_glm_age_f$fit - 
  qnorm(0.975)*pred_glm_age_f$se.fit
u_glm_age_f <- pred_glm_age_f$fit + 
  qnorm(0.975)*pred_glm_age_f$se.fit
df <- data.frame(a, b_glm_age_f, 
                 l_glm_age_f, u_glm_age_f)

## -----------------------------------------------------------------------------------
p_glm_age_f <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age_f <- p_glm_age_f + geom_line(aes(a, b_glm_age_f), size = 1, col = KULbg)   
p_glm_age_f <- p_glm_age_f + geom_line(aes(a, u_glm_age_f), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age_f), size = 0.5, linetype = 2, col = KULbg)
p_glm_age_f <- p_glm_age_f + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age_f

## -----------------------------------------------------------------------------------
## weniger Klassen durch Gruppierung
level <- seq(min(mtpl$ageph), max(mtpl$ageph), by = 5)
freq_glm_age_c <- glm(nclaims ~ cut(ageph, level), offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age_c <- predict(freq_glm_age_c, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age_c <- pred_glm_age_c$fit
l_glm_age_c <- pred_glm_age_c$fit - 
  qnorm(0.975)*pred_glm_age_c$se.fit
u_glm_age_c <- pred_glm_age_c$fit + 
  qnorm(0.975)*pred_glm_age_c$se.fit
df <- data.frame(a, b_glm_age_c, 
                 l_glm_age_c, u_glm_age_c)

## -----------------------------------------------------------------------------------
p_glm_age_c <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age_c <- p_glm_age_c + geom_line(aes(a, b_glm_age_c), size = 1, col = KULbg)   
p_glm_age_c <- p_glm_age_c + geom_line(aes(a, u_glm_age_c), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age_c), size = 0.5, linetype = 2, col = KULbg)
p_glm_age_c <- p_glm_age_c + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age_c

## -----------------------------------------------------------------------------------
## mit gam
freq_gam_age <- gam(nclaims ~ s(ageph), 
                    offset = log(expo), 
                    data = mtpl, 
                    family = poisson(link = "log"))
pred_gam_age <- predict(freq_gam_age, 
                        newdata = data.frame(ageph = a, expo = 1), 
                        type = "terms", se.fit = TRUE)
b_gam_age <- pred_gam_age$fit
l_gam_age <- pred_gam_age$fit -
  qnorm(0.975)*pred_gam_age$se.fit
u_gam_age <- pred_gam_age$fit +
  qnorm(0.975)*pred_gam_age$se.fit
df <- data.frame(a, b_gam_age, 
                 l_gam_age, u_gam_age)

## -----------------------------------------------------------------------------------
p_gam_age <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_gam_age <- p_gam_age + geom_line(aes(a, b_gam_age), size = 1, col = KULbg)   
p_gam_age <- p_gam_age + geom_line(aes(a, u_gam_age), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_gam_age), size = 0.5, linetype = 2, col = KULbg)
p_gam_age <- p_gam_age + xlab("ageph") + ylab("fit") + theme_bw()
p_gam_age

## -----------------------------------------------------------------------------------
## räumliche Modelle
## -----------------------------------------------------------------------------------
## mit thinplate-Splines (bs), tp=thinplate
### siehe auch
?smooth.terms 
freq_gam_spatial <- gam(nclaims ~ s(long, lat, 
                                    bs = "tp"), 
                        offset = log(expo), 
                        family = 
                          poisson(link = "log"), 
                        data = mtpl)
freq_gam_spatial$sp
plot(freq_gam_spatial) ## so erkennt man hier noch nicht wo etwas passiert
## -----------------------------------------------------------------------------------
## auch: Interaktion Alter, PS-Stärke => Tensorprodukt-Spline
##  ti: nur Interaktion (":"), te auch marginale ("*") , 
##  erlaubt individuelle Splinewahl hier "tp"
## -----------------------------------------------------------------------------------
freq_gam_inter <- gam(nclaims ~ s(ageph) + s(power) + 
                        ti(ageph, power, bs = "tp"), 
                      offset = log(expo), 
                      family = poisson(link = "log"), 
                      data = mtpl)
freq_gam_inter$sp
plot(freq_gam_inter) ## einzelne Komponenten

## -----------------------------------------------------------------------------------
## jedes Eck im Polygon unserer Karte hat Koordinaten;
## das hilft nicht; wir wollen für jeden Postbezirk repräsentative /zentral
## Koordinaten -> st_centroid

post_dt <- st_centroid(belgium_shape_sf)
post_dt$long <- do.call(rbind, post_dt$geometry)[,1]
post_dt$lat <- do.call(rbind, post_dt$geometry)[,2]


## -----------------------------------------------------------------------------------
## berechnet für jeden Bezirk die mit dem räuml. Modell vorhergesagte Häufigkeit
pred <- predict(freq_gam_spatial, newdata = post_dt, 
                type = "terms", terms = "s(long,lat)")


## -----------------------------------------------------------------------------------
dt_pred <- data.frame(pc = post_dt$POSTCODE, 
                      long = post_dt$long, 
                      lat = post_dt$lat, pred)
names(dt_pred)[4] <- "fit_spatial"


## -----------------------------------------------------------------------------------
belgium_shape_sf <- left_join(belgium_shape_sf, 
                              dt_pred, 
                              by = c("POSTCODE" =
                                       "pc"))

## -----------------------------------------------------------------------------------
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = fit_spatial), colour = NA) +
  ggtitle("MTPL claim frequency data") +
  scale_fill_gradient(low="#99CCFF", high="#003366") +
  theme_bw()

## -----------------------------------------------------------------------------------
tm_shape(belgium_shape_sf) + 
  tm_borders(col = 'white', lwd = .1) + 
  tm_fill("fit_spatial", style = "cont", palette = "RdBu", legend.reverse = TRUE, auto.palette.mapping = TRUE) + 
  tm_layout(legend.title.size = 1.0, legend.text.size = 1.0) 

###--- nun gehen wir nach Brasilien
require(CASdatasets)
### Verkehrsunfaelle im Februar 2011, in Belo Horizonte, (Stadt in Brasilien)
?brautocoll

#----------------------------------------------------
### R-Code aus Kapitel 5 Charpentier, angepasst, weil Files nicht (mehr) verfuegbar
#----------------------------------------------------
data(brgeomunicins)
sf_brgeomunicins <- st_as_sf(brgeomunicins)
ggplot(sf_brgeomunicins) +
  geom_sf() +
  ggtitle("Welcome to Brasil!") +
  theme_bw()

brasil_simple_sf <- st_simplify(sf_brgeomunicins, 
                                dTolerance = 0.00001)
ggplot(brasil_simple_sf) +
  geom_sf() +
  ggtitle("Welcome to Brasil!") +
  theme_bw()

# and plot
(mymapbras <- tm_shape(brasil_simple_sf) + 
    tm_borders(col = "black")     )

## -----------------------------------------------------------------------------------
## leaflet ... toller interaktiver Effekt...
tmap_leaflet(mymapbras)



data(brautocoll)
mat <- brautocoll[,4:5] 

crs <- CRS("+proj=longlat +ellps=WGS84")
events <- SpatialPoints(mat, proj4string = crs)
plot(events)
plot(events, axes=T, asp=2, pch=19, cex=0.8, col="dark grey")
summary(events)

#----------------------------------------------------------
### Arbeiten und Erzeugen von SpatialPointsDataFrame
#----------------------------------------------------------

## Anhängen weiterer Spalten im Format SpatialPointsDataFrame
bra.df <- as.data.frame(brautocoll[,c(1:3,6,7)])
events2 <- SpatialPointsDataFrame(mat, bra.df, proj4string = crs, match.ID=FALSE)
summary(events2)

events2[events2$Severity == "Fatal",c("Day","Hour","Type")]

events3 <- events2[order(events2$Type),]
events3$Day <- as.factor(events3$Day)
events3$Type <- as.factor(events3$Type)


plot(events3, axes=TRUE, pch = c(rep(1,sum(events3$Type == "Collision")),
                                 rep(19,sum(events3$Type == "Running over"))), 
     cex = as.numeric(events3$Day)/5,
     col=c(rep("black",sum(events3$Type == "Collision")),
           rep("grey",sum(events3$Type == "Running over"))))

aux <- brautocoll
coordinates(aux) <- ~Lat+Long
proj4string(aux) <- crs

#----------------------------------------------------------
### Arbeiten mit Polygonen
#----------------------------------------------------------
p1 <- rbind(c(2,0), c(6,0), c(6,4), c(2,4), c(2,0)) # region 1, mainland
p1i <- rbind(c(0,0), c(1,1), c(1,4), c(0,2), c(0,0)) # region 1, island
p2 <- rbind(p1[2,], c(10,3), c(10,7), c(8,7), p1[3:2,]) # region 2
p3 <- rbind(p1[4:3,], p2[4,], c(4,10), c(0,10), p1[4,]) # region 3
plot(rbind(p1, p2, p3)); polygon(p1); polygon(p1i); polygon(p2); polygon(p3)

plot(rbind(p1, p2, p3))
polygon(p1,density=20,angle=30)
polygon(p1i,density=20,angle=30,col="grey")
polygon(p2,density=10,angle=-30)
polygon(p3,density=15,angle=-60,col="grey")

pl1 <- Polygon(p1); pl1i <- Polygon(p1i); pl2 <- Polygon(p2); pl3 <- Polygon(p3)
str(pl1)

## Zugriff auf Attribute
pl1@labpt
pl1@area

## Vergabe von Identifikatoren
t1 <- Polygons(list(pl1,pl1i), "town1")
t2 <- Polygons(list(pl2), "town2")
t3 <- Polygons(list(pl3), "town3")

map3 <- SpatialPolygons(list(t1, t2, t3))
plot(map3)
plot(map3,col=grey(c(.7,.9,.5)))
cents <- coordinates(map3)
points(cents, pch=20)
text(cents[,1], cents[,2]+0.5, c("town1","town2","town3"))


### ein größeres Beispiel
p1 <- rbind(c(2,0), c(6,0), c(6,4), c(2,4), c(2,0)) # region 1, mainland
p1i <- rbind(c(0,0), c(1,1), c(1,4), c(0,2), c(0,0)) # region 1, island
p2 <- rbind(p1[2,], c(10,3), c(10,7), c(8,7), p1[3:2,]) # region 2
p2l <- rbind(c(8,2), c(9,3), c(7,4), c(7,3), c(8,2)) # region 2, lake
p3 <- rbind(p1[4:3,], p2[4,], c(4,10), c(0,10), p1[4,]) # region 3
p4 <- rbind(c(4,7), c(5,8), c(3,9), c(2,7), c(4,7)) # region 4, inside region 3
p5 <- rbind(p3[4:3,], c(10,8), c(9,10), p3[4,]) # region 5
pls5 <- list()
pls5[[1]] <- Polygons(list(Polygon(p1, hole=FALSE),
                           Polygon(p1i, hole=FALSE)), "town1")
pls5[[2]] <- Polygons(list(Polygon(p2, hole=FALSE),
                           Polygon(p2l, hole=TRUE)), "town2")
pls5[[3]] <- Polygons(list(Polygon(p3, hole=FALSE),
                           Polygon(p4, hole=TRUE)), "town3")
pls5[[4]] <- Polygons(list(Polygon(p4, hole=FALSE)), "town4")
pls5[[5]] <- Polygons(list(Polygon(p5, hole=FALSE)), "town5")
map5 <- SpatialPolygons(pls5)
plot(map5)
plot(map5, col=gray(c(.1,.3,.5,.7,.9)))
legend("bottomright", c("town1", "town2", "town3", "town4", "town5"),
       fill=gray(c(.1,.3,.5,.7,.9)))
plot(map5, col=c("red", "green", "blue", "black", "yellow"))
legend("bottomright", c("town1", "town2", "town3", "town4", "town5"),
       fill=c("red", "green", "blue", "black", "yellow"))

x <- data.frame(x1 = c("F", "F", "T", "T", "T"), x2=1:5,
                row.names = c("town4", "town5", "town1", "town2", "town3"))
map5x <- SpatialPolygonsDataFrame(map5, x, match.ID = TRUE)
map5x@data
map5x <- SpatialPolygonsDataFrame(map5, x, match.ID = F)
map5x@data

x <- data.frame(x1 = c("F", "F", "T", "T", "T"), x2=1:5,
                x3 = c("town4", "town5", "town1", "town2", "town3"))
map5x <- SpatialPolygonsDataFrame(map5, x, match.ID = "x3")

#### Karten

maps::map("world", col = grey(0.8), fill=TRUE)
maps::map.cities(country = "Brazil", capitals = 1, cex=0.7)

maps::map("world", "canada", proj="conic", param=45, fill=TRUE, col=grey(.9))
maps::map("world", "canada", proj="bonne", param=45, fill=TRUE, col=grey(.9))
maps::map("world", "canada", proj="albers", par=c(30,40), fill=TRUE, col=grey(.9))
maps::map("world", "canada", proj="lagrange", fill=TRUE, col=grey(.9))

data(brgeomunicins)
names(brgeomunicins)
table(brgeomunicins$State)

cols <- rev(gray(seq(0.1, 0.9, length = 5)))
cols
spplot(brgeomunicins, "HDIcity00", col.regions = cols, cuts = length(cols) - 1)
spplot(brgeomunicins, "PopClaimFire", col.regions = cols, cuts = length(cols) - 1)
spplot(brgeomunicins, "PopClaimColl", col.regions = cols, cuts = length(cols) - 1)
spplot(brgeomunicins, "PopClaimRob", col.regions = cols, cuts = length(cols) - 1)

brks <- quantile(brgeomunicins$"HDIcity00", prob = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)

brgeomunicins$col_var <- cut(brgeomunicins$HDIcity00, brks)
spplot(brgeomunicins, "col_var", col.regions = cols, main = "Levels are intervals")
levels(brgeomunicins$col_var) <- c("Very Low", "Low", "Middle", "High", "Very High")
spplot(brgeomunicins, "col_var", col.regions = cols, main = "User defined levels")

####################################################
#Farben
####################################################

par(mfrow=c(2,2))
pie(rep(1,10), col=heat.colors(10), main = "heat.colors()")
pie(rep(1,10), col=topo.colors(10), main = "topo.colors()")
pie(rep(1,10), col=terrain.colors(10), main = "terrain.colors()")
pie(rep(1,10), col=cm.colors(10), main = "cm.colors()")

library(RColorBrewer)
cols <- brewer.pal(5, "Reds")


spplot(brgeomunicins, "col_var", col.regions = cols,
       main = "HDI by municipalities in South Brazil")

require(RColorBrewer)
plotvar <- brgeomunicins$HDIcity00
ncls <- 5
colpal <- brewer.pal(ncls,"Greens")


library(classInt)
classes <- classIntervals(plotvar, ncls, style = "equal")
cols2 <- findColours(classes, colpal)

cols2[is.na(brgeomunicins$HDIM_00)] <- "red"

plot(brgeomunicins, col = cols2)
legend(-47.85126, -29.96805, legend=c(names(attr(cols2, "table")), "NA"),
       fill=c(attr(cols2, "palette"), "red"))

############################################################
# Verbindung zu Google Maps
############################################################

library(maptools)
library(PBSmapping)
map.susep <- SpatialPolygons2PolySet(brgeomunicins)
class(map.susep)

head(map.susep)
bb <- bbox(brgeomunicins) # getting the map bounding box
bb

library(RgoogleMaps)
MyMap <- GetMap.bbox(bb[1, ], bb[2, ], # fetching the Google Maps image
                     maptype = "satellite",
                     destfile = "myMap.png",
                     GRAYSCALE = FALSE)
str(MyMap) # inspecting the MyMap object
PlotOnStaticMap(MyMap) # plotting the image

PlotPolysOnStaticMap(MyMap, map.susep, col = cols2, lwd = 0.15,
                     border = NA, add = FALSE)

legend("topleft", fill=attr(cols2, "palette"),
       legend=leglabs( round(classes$brks, digits=2) ),
       cex=1.0, ncol=1, bg="white", bty="o")


############################################################
# Adressen in Google Maps finden
############################################################
## braucht Google API Key
### https://developers.google.com/maps/documentation/geocoding/get-api-key

require(dismo)
adress <- paste("Avenida Otacilio Negrao de Lima, ",
                seq(1, 30000, by = 200),
                " , Belo Horizonte - Minas Gerais",
                sep = "")
geo.pt <- geocode(adress) ## Google will hier einen API-key (hier nicht)
geo.pt <- rbind(geo.pt, geo.pt[1,])

require(RgoogleMaps)
center <- c(mean(geo.pt$lat), mean(geo.pt$lon))
mymap <- GetMap(center=center, zoom=14, GRAYSCALE = TRUE)
map <- PlotOnStaticMap(mymap, lat = geo.pt$latitude, lon = geo.pt$longitude,
                       lwd = 2.5, lty = 2, col="black", FUN = lines)


############################################################
# Erzeugen KML Files (können in Google Maps dynamisch gezoomt werden)
############################################################

require(RgoogleMaps)
brgeomunicins$color <- cols2
brgeomunicins$description <- paste("HDI:", brgeomunicins$HDIM_00)

KML.create <- function(shp, color, namepoly, description, file.name){
  out <- sapply(slot(shp, "polygons"),
                function(x) {
                  kmlPolygon(x,
                             name = as(shp, "data.frame")[slot(x, "ID"), namepoly],
                             col = as(shp, "data.frame")[slot(x, "ID"), color],
                             lwd = 1,
                             border = "#C0C0C0",
                             description = as(shp, "data.frame")[slot(x, "ID"),
                                                                 description]
                  )
                }
  )
  kmlFile <- file(file.name,"w")
  cat(kmlPolygon(kmlname="KML", kmldescription="KML")$header, file=kmlFile,
      sep="\n")
  cat(unlist(out["style",]), file=kmlFile, sep="\n")
  cat(unlist(out["content",]), file=kmlFile, sep="\n")
  cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
  close(kmlFile)
}

KML.create(brgeomunicins, color="color", namepoly="NAME_MUN",
           description="description", file.name="maps.kml")


############################################################
# Nachbarschaftsstrukturen
############################################################


library(maptools)
library(spdep)
pos <- which(brgeomunicins$State == "Parana") # indices of selected rows
prshape <- brgeomunicins[pos,] # new SpatialPolygonsDataFrame Parana regions
plot(prshape) # plotting the map

text(coordinates(prshape), label=prshape$City, cex=0.5) # adding areas names
pr.nb <- poly2nb(prshape) # Adjacency ngb list from SpatialPolygonsDataFrame
is.list(pr.nb) # output is TRUE
pr.nb[[1]] # neighbors of "ABATIA", the first data.frame region

plot(prshape)
plot(pr.nb, coordinates(prshape), add=TRUE, col="blue")

pr.listw <- nb2listw(pr.nb, style="W") # weighted ngb list
length(pr.listw); names(pr.listw);
pr.listw$weights[[1]] # weights of the 1st region neighbors

pr.listw$weights[[2]] # weights of the 2nd region neighbors

pos <- c(1, pr.nb[[1]])

map4 <- prshape[pos,]

plot(map4)
map4.nb <- poly2nb(map4)


sapply(map4.nb, length)

x <- rep(1/3,3) # auxiliary vector
lweights <- list((1:6)/21, runif(4), 1/(1:4), x, x, x, x)
map4.listw <- nb2listw(map4.nb, glist=lweights, style="W")
map4.listw$weights

coords <- coordinates(prshape)
pr.knn <- knearneigh(coords, k=3, longlat = TRUE)
pr.nbknn <- knn2nb(pr.knn) # ngb list
plot(prshape, border="grey") # map of the Parana regions
plot(pr.nbknn, coords, add=TRUE)

title("K nearest neighbours, k = 3")

############################################################
# Moran-Index
############################################################
require(spdep)
imoran <- moran.mc(prshape$HDIcity00, pr.listw, nsim=999)
par(mar=c(4,4,2,2))
hist(imoran$res, xlab='Index', main='', col=gray(.5), border=gray(.7))
arrows(imoran$stat,-2,imoran$stat,10,lwd=2,col=2,leng=.1,code=1)
segments(imoran$stat, 3, 0.4, 120, lty=2)
text(.4, 150, paste("Moran's I =", format(imoran$stat,dig=4)))
text(.4, 130, paste("p-value =", format(imoran$p.val, dig=4)))

############################################################
# Modellierung mit INLA
############################################################

NbMean <- function(shp, vari){
  library(spdep)
  shpnb <- poly2nb(shp)
  shpnb.mat <- nb2mat(shpnb, style="B",zero.policy=TRUE) #adjacency matrix
  selNA <- which(is.na(shp@data[, vari]))
  NAnb <- shpnb.mat[selNA, ]
  shp@data[selNA, vari] <- apply(NAnb, 1, FUN = function(x)
    mean(shp@data[which(x == 1), vari], na.rm = TRUE))
  return(shp)
}

library(maptools)
library(INLA)
shape <- prshape
### Zahl der Unfaelle in Luxus und Standard/Popular Autos
shape@data$SIN_LUX <- rowSums(shape@data[,12:15], na.rm=FALSE)
shape@data$SIN_POP <- rowSums(shape@data[,6:9], na.rm=FALSE)
### Exposure in Luxus und Standard/Popular Autos
shape@data$EXPO_LUX <- shape@data[,10]
shape@data$EXPO_POP <- shape@data[,4]
shape@data$POPDENSE <- log(shape@data[,17])

## Herausfiltern von NAs -- zunächst für LUX und POP

#ina <- is.na(shape@data$LUX)|is.na(shape@data$POPDENSE)
#shape@data <- shape@data[!ina,]

## Herausfiltern von NAs --  bei den Exposures imputieren wir

pos <- which(!is.na(shape@data$EXPO_POP)) #non missing positions
pos.ms <- which(is.na(shape@data$EXPO_POP)) #missing positions
expo <- log(shape@data$EXPO_POP[pos]+1)
pop <- shape@data$POPDENSE[pos]
reg <- lm(expo ~ pop)
pred <- as.vector(predict(reg,data.frame(pop=shape@data$POPDENSE[pos.ms])))
shape@data$EXPO_POP[pos.ms] <- sapply((exp(pred) - 1),
                                      function(x) max(x,0))

pos <- which(!is.na(shape@data$EXPO_LUX)) #non missing positions
pos.ms <- which(is.na(shape@data$EXPO_LUX)) #missing positions
expo <- log(shape@data$EXPO_LUX[pos]+1)
pop <- shape@data$POPDENSE[pos]
reg <- lm(expo ~ pop)
pred <- as.vector(predict(reg,data.frame(pop=shape@data$POPDENSE[pos.ms])))
shape@data$EXPO_LUX[pos.ms] <- sapply((exp(pred) - 1),
                                      function(x) max(x,0))

shape <- NbMean(shape,"HDIcity00")

## Mittlere Exposition pro Areal

shape@data$E_POP <- shape@data$EXPO_POP *
  sum(shape@data$SIN_POP,na.rm=TRUE)/sum(shape@data$EXPO_POP)
shape@data$E_LUX <- shape@data$EXPO_LUX *
  sum(shape@data$SIN_LUX,na.rm=TRUE)/sum(shape@data$EXPO_LUX)

shape@data$struct <- rep(1:dim(shape@data)[1])
shape@data$unstruct <- rep(1:dim(shape@data)[1])

require(INLA)
## inla(formula, family = "gaussian", data = data.frame(),...)

### schreibt die Geometrie in ein File 
nb2INLA("ngbINLA.graph",poly2nb(shape))

## Modellierung Schadeninzidenz durch POP, räumlich und HDIcity00
## separat für Luxus und Populäre Autos

f.pop <- SIN_POP ~ HDIcity00 + POPDENSE + f(unstruct,model="iid") +
  f(struct,model="besag",graph="ngbINLA.graph")

f.lux <- SIN_LUX ~ HDIcity00 + POPDENSE + f(unstruct,model="iid") + 
  f(struct,model="besag",graph="ngbINLA.graph")

## "besag": einfache Abhängigkeitsstruktur ``Intrinsic CAR'' ICAR
##          alle Autokorrelationen sind in der Priorverteilung gleich 
## "besag2": erlaubt gewichtete ICAR-Modelle
## "bym" Kombination aus ICAR und iid ...
names(inla.models()$latent)
## Pendant zu Kovarianz-Modellen in lme

m.pop <- inla(f.pop, family="poisson", data=shape@data, E=E_POP,
              control.compute=list(dic=TRUE,cpo=TRUE),
              control.predictor= list(compute=TRUE,link=1))
m.lux <- inla(f.lux, family="poisson", data=shape@data, E=E_LUX,
              control.compute=list(dic=TRUE,cpo=TRUE),
              control.predictor= list(compute=TRUE,link=1))



summary(m.pop)

## Räumlicher Effekt in POP_SPAT, 
## Schadeninzidenzvorhersage POP_PRED
## separat für Luxus und Populäre Autos

shape@data$POP_SPAT <- m.pop$summary.random$struct$mean
shape@data$POP_PRED <- m.pop$summary.fitted.values$mean * shape@data$E_POP
shape@data$POP_LPRED <- log(m.pop$summary.fitted.values$mean * shape@data$E_POP)
shape@data$POP_RR <- m.pop$summary.linear.predictor$mean

shape@data$LUX_SPAT <- m.lux$summary.random$struct$mean
shape@data$LUX_PRED <- m.lux$summary.fitted.values$mean * shape@data$E_LUX
shape@data$LUX_LPRED <- log(m.lux$summary.fitted.values$mean * shape@data$E_LUX)
shape@data$LUX_RR <- m.lux$summary.linear.predictor$mean

spplot(shape,c("POP_SPAT","LUX_SPAT"), layout = c(2,1),
       main = "Spatial Dependence", cuts=5, col.regions=grey.colors(50,1,0))

spplot(shape,c("POP_LPRED","LUX_LPRED"), layout = c(2,1),
       main = "log-Schadeninzidenzvorhersage", cuts=5, col.regions=grey.colors(50,1,0))

spplot(shape,c("POP_RR","LUX_RR"), layout = c(2,1),
       main = "Logarithmus der Relativen Risiken", cuts=5, col.regions=grey.colors(50,1,0))


##-----------------------------------------
## Modellgüte eine Arg logarithmierter QQ-Plot 
##-----------------------------------------

par(mfrow=c(1,2))
plot(log(abs(shape@data$POP_PRED)+1),log(abs(shape@data$SIN_POP)+1),
     xlab="log pred #s of popular cars accidents",
     ylab="log original #s of popular cars accidents",
     main="log-predicted x log-accidents")
abline(a=0,b=1)
plot(log(shape@data$LUX_PRED+1),log(shape@data$SIN_LUX+1),
     xlab="log pred #s of luxury cars accidents",
     ylab="log original #s of luxury cars accidents",
     main="log-predicted x log-accidents")
abline(a=0,b=1)


##-----------------------------------------
## gemeinsame Analyse 
##-----------------------------------------

k <- 2
n <- dim(shape@data)[1]
Y <- matrix(NA, n, k)
Y[1:n, 1] <- shape@data$SIN_POP
Y[1:n, 2] <- shape@data$SIN_LUX

share.dat <- list(Y=matrix(NA, nrow=n*2, ncol=2))
share.dat$Y[1:n, 1] <- Y[,1]
share.dat$Y[n+(1:n), 2] <- Y[,2]

share.dat$E <- c(shape@data$E_POP,shape@data$E_LUX)

share.dat$shared <- c(1:n, rep(NA,n))
share.dat$shared.copy <- c(rep(NA,n), 1:n)
share.dat$spat.pop <- c(1:n, rep(NA,n))
share.dat$spat.lux <- c(rep(NA,n), 1:n)
share.dat$random.pop <- c(1:n, rep(NA,n))
share.dat$random.lux <- c(rep(NA,n), 1:n)


share.dat$alpha_POP <- rep(1:0, each=n)
share.dat$alpha_LUX <- rep(0:1, each=n)
share.dat$POP_POP <- c(shape@data$POPDENSE,rep(0,n))
share.dat$POP_LUX <- c(rep(0,n),shape@data$POPDENSE)
share.dat$HDI_POP <- c(shape@data$HDIcity00 ,rep(0,n))
share.dat$HDI_LUX <- c(rep(0,n),shape@data$HDIcity00)

f.shared <- Y ~ 0 + alpha_POP + POP_POP + HDI_POP +
  alpha_LUX + POP_LUX + HDI_LUX +
  f(shared, model="besag", graph="ngbINLA.graph") +
  f(shared.copy, copy="shared", hyper=list(theta=list(fixed=FALSE,
                                                      param=c(1,1), range=c(0,Inf)))) +
  f(spat.pop, model="besag", graph="ngbINLA.graph",
    hyper=list(theta=list(initial=log(6.30),
                          param=c(0.5,0.0005)))) +
  f(random.pop, model="iid", hyper=list(theta=list(initial=log(28.22),
                                                   param=c(0.5,0.0005)))) +
  f(spat.lux, model="besag", graph="ngbINLA.graph",
    hyper=list(theta=list(initial=log(4.83),
                          param=c(0.5,0.0005)))) +
  f(random.lux, model="iid", hyper=list(theta=list(initial=log(11.65),
                                                   param=c(0.5,0.0005))))

m.shared <- inla(f.shared, family = rep("poisson", 2), data = share.dat, E = E,
                 control.inla = list(h = 0.005),
                 control.compute =list(dic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE, link = c(rep(1,n),rep(2,n))) )

summary(m.shared)

shape@data$SHARED_SPAT <- m.shared$summary.random$shared$mean

spplot(shape,"SHARED_SPAT", main = "Shared Spatial Dependence", cuts=5,
       col.regions=grey.colors(50,1,0))

sf_shape <- st_as_sf(shape[!ina,])
(mymapshape <- tm_shape(sf_shape) + 
    tm_fill(col = "SHARED_SPAT", style = "cont", palette = "Blues", colorNA = "white")+
    tm_borders(col = "black")     )

## -----------------------------------------------------------------------------------
## leaflet ... toller interaktiver Effekt...
tmap_leaflet(mymapshape)
