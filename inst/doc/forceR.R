## ----setup,  include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE, eval=F-------------------------------------
#  install.packages('forceR')

## ----warning=FALSE, message=FALSE, eval=F-------------------------------------
#  require(devtools)
#  devtools::install_github("https://github.com/Peter-T-Ruehr/forceR")

## ----warning=FALSE, message=FALSE---------------------------------------------
library(magrittr)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(readr)

library(forceR)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  data.folder <- "./example_data"

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
data.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data"

## ----eval=FALSE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6----
#  file <- file.path(data.folder, "0982.csv")
#  plot_measurement(file,
#                   columns = c(1:2))

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
cropped.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped"

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  cropped.folder <- "./example_data/cropped"

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  file.cropped <- crop_measurement(file,
#                                   path.data = cropped.folder)

## ----eval=FALSE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6----
#  cropped.folder <- "./example_data/cropped"
#  
#  file <- file.path(cropped.folder, "0982_cropped.csv")
#  plot_measurement(file)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
cropped.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped"
ampdriftcorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr"

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  # create file list with all cropped files that need amplifier drift corrections.
#  #   Her we use the cropped.folder with the cropped measurements.
#  file.list <- list.files(cropped.folder,
#                          pattern = "csv$",
#                          full.names = TRUE)
#  
#  # define folder where to save the amplifier drift corrected file.
#  #   If this folder exists, it will be created.
#  ampdriftcorr.folder <- "./cropped/ampdriftcorr"
#  
#  for(filename in file.list){
#    print(filename)
#    amp_drift_corr(filename = filename,
#                   tau = 9400,
#                   res.reduction = 10,
#                   plot.to.screen = FALSE,
#                   write.data = TRUE,
#                   write.PDFs = TRUE,
#                   write.logs = TRUE,
#                   output.folder = ampdriftcorr.folder,
#                   show.progress = FALSE)
#    print("***********")
#  }

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
cropped.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped"
ampdriftcorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr"
baselinecorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr/baselinecorr"

## ----eval=FALSE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6----
#  
#  filename = file.path(ampdriftcorr.folder, "1068_ampdriftcorr.csv")
#  
#  plot_measurement(filename)
#  
#  baselinecorr.folder <- "./cropped/ampdriftcorr/baselinecorr"
#  
#  file.baselinecorr <- baseline_corr(filename = filename,
#                                     corr.type = "auto",
#                                     window.size.mins = 2000,
#                                     window.size.means = NULL,
#                                     quantile.size = 0.05,
#                                     y.scale = 0.5,
#                                     res.reduction = 10,
#                                     Hz = 100,
#                                     plot.to.screen = TRUE,
#                                     write.data = TRUE,
#                                     write.PDFs = TRUE,
#                                     write.logs = TRUE,
#                                     output.folder = baselinecorr.folder,
#                                     show.progress = FALSE)

## ----eval=FALSE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6----
#  filename = file.path(ampdriftcorr.folder, "1174_ampdriftcorr.csv")
#  
#  plot_measurement(file)
#  
#  file.baselinecorr <- baseline_corr(filename = filename,
#                                     corr.type = "manual",
#                                     plot.to.screen = TRUE,
#                                     write.data = TRUE,
#                                     write.PDFs = TRUE,
#                                     write.logs = TRUE,
#                                     output.folder = baselinecorr.folder,
#                                     show.progress = FALSE)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=FALSE-------------------
data.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data"
cropped.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped"
ampdriftcorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr"
baselinecorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr/baselinecorr"
data.folders <- c(data.folder,
                  cropped.folder,
                  ampdriftcorr.folder,
                  baselinecorr.folder)
results.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/corrected"

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  data.folders <- c(data.folder,
#                    file.path(data.folder, "/cropped"),
#                    file.path(data.folder, "/cropped/ampdriftcorr"),
#                    file.path(data.folder, "/cropped/ampdriftcorr/baselinecorr"))
#  
#  results.folder <- file.path(data.folder, "/corrected/")
#  
#  sort_files(data.folders = data.folders,
#             results.folder = results.folder,
#             move = FALSE)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  file.list <- list.files(results.folder, pattern = "csv", full.names = TRUE)
#  df.1 <- load_single(file = file.list[1],
#                      columns = c(1:2))

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  df.all <- load_mult(folder = results.folder,
#                      columns = c(1:2),
#                      show.progress = TRUE)

## ----eval=TRUE, warning=FALSE, message=FALSE----------------------------------
# create/replace df.all
df.all <- forceR::df.all
head(df.all)

## ----eval=TRUE, warning=FALSE, message=TRUE, include=TRUE, fig.width = 7, fig.height=6----
# plot simulated measurements
ggplot(df.all,
       aes(x = t ,
           y = y,
           colour=measurement)) +
  geom_line()

## ----eval=TRUE, warning=FALSE, message=FALSE----------------------------------
# reduce frequency to 200 Hz
df.all.200 <- reduce_frq(df = df.all, 
                         Hz = 200,  
                         measurement.col = "measurement")

head(df.all.200)

## ----eval=TRUE, warning=FALSE, message=FALSE----------------------------------
# create a classifier
number_of_species <- 4
number_of_specimens_per_species <- 3
number_of_measurements_per_specimen <- 2
number_of_rows <- number_of_species *
  number_of_specimens_per_species *
  number_of_measurements_per_specimen

species <- sort(rep(paste0("species_", LETTERS[1:number_of_species]),
                    length=number_of_rows))

specimens <- sort(rep(paste0("speciemen_", letters[1:(number_of_species*number_of_specimens_per_species)]),
                      length=number_of_rows))

classifier <- tibble(species = species,
                     specimen = specimens,
                     measurement = paste0("m_",  str_pad(string= 1:number_of_rows, width = 2, pad = "0")),
                     amp = c(rep(0.5, number_of_rows/2), rep(2, number_of_rows/2)),
                     lever.ratio = rep(0.5, number_of_rows))
head(classifier)

## ----eval=TRUE, warning=FALSE, message=FALSE----------------------------------
df.all.200.tax <- y_to_force(df = df.all.200, 
                             classifier = classifier, 
                             measurement.col = "measurement")
head(df.all.200.tax)

## ----eval=TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6-----
var1 = "measurement"
var2 = "specimen"
df.summary.specimen <- summarize_measurements(df.all.200.tax, 
                                              var1, 
                                              var2)
head(df.summary.specimen)

# boxplot of maximum force in specimens
ggplot(data = df.summary.specimen, mapping = aes(x=specimen,y=max.F.measurement)) +
  geom_jitter(aes(color='blue'),alpha=0.7, width = 0.2, height = 0.0) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  # scale_y_log10() +
  labs(y="max(F)/specimen") +
  guides(color="none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


## ----eval=TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6-----
df.summary.species <- df.summary.specimen %>%
  # find max Fs of species
  group_by(species) %>%
  # calculate force values for each species
  mutate(max.F.species = max(max.F.specimen),
         mean.F.species = round(mean(max.F.specimen),6),
         sdv.max.F.species = sd(max.F.specimen)) %>% 
  ungroup() %>% 
  # count specimens / species
  group_by(species) %>% 
  mutate(n.specimens.in.species = length(unique(specimen))) %>% 
  ungroup()
df.summary.species

# boxplot of maximum force in species
ggplot(data = df.summary.species, mapping = aes(x=species,y=max.F.specimen)) +
  geom_jitter(aes(color='blue'),alpha=0.7, width = 0.2, height = 0.0) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  # scale_y_log10() +
  labs(x='species', y="max(F)/specimen") +
  guides(color="none") +
  theme_minimal()


## ----eval=TRUE, warning=FALSE, message=FALSE, include=FALSE-------------------
path.data <- file.path(data.folder, "/data")
path.plots <- file.path(data.folder, "/plots/")


## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  # create folders to save df and results
#  path.plots <- paste0(data.folder, "/plots/")
#  ifelse(!dir.exists(path.plots), dir.create(path.plots), "./plots already exists")
#  
#  path.plots.initial_peak_finding <- paste0(data.folder, "/plots/initial_peak_finding/")
#  ifelse(!dir.exists(path.plots.initial_peak_finding), dir.create(path.plots), "./plots/initial_peak_finding already exists")
#  
#  path.data <- paste0(data.folder, "/data/")
#  ifelse(!dir.exists(path.data), dir.create(path.data), "./data already exists")
#  

## ----eval=TRUE, warning=FALSE, message=FALSE, include=FALSE-------------------
peaks.df <- find_strongest_peaks(df = df.all.200.tax, 
                                 no.of.peaks = 5, 
                                 plot.to.screen = FALSE,
                                 path.data = NULL,
                                 path.plots = NULL,
                                 show.progress = FALSE)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  peaks.df <- find_strongest_peaks(df = df.all.200.tax,
#                                   no.of.peaks = 5,
#                                   plot.to.screen = TRUE,
#                                   path.data = path.data,
#                                   path.plots = path.plots,
#                                   show.progress = TRUE)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=TRUE--------------------
head(peaks.df)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  plot_peaks(df.peaks = peaks.df,
#             df.data = df.all.200.tax,
#             additional.msecs = 2000,
#             path.plots = path.plots)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  peaks.df <- correct_peak(df.peaks = peaks.df,
#                           df.data = df.all.200.tax,
#                           measurement = "m_01",
#                           peak = 1,
#                           additional.msecs = 100,
#                           path.data = path.data)

## ---- eval=TRUE, warning=FALSE, message=FALSE, include=FALSE------------------
# this prints too long to let it stay
peaks.df.norm <- rescale_peaks(df.peaks = peaks.df,
                               df.data = df.all.200.tax,
                               plot.to.screen = FALSE,
                               path.data = NULL,
                               show.progress = FALSE)

## ---- eval=FALSE, warning=FALSE, message=FALSE, include=TRUE------------------
#  peaks.df.norm <- rescale_peaks(df.peaks = peaks.df,
#                                 df.data = df.all.200.tax,
#                                 plot.to.screen = TRUE,
#                                 path.data = path.data,
#                                 show.progress = TRUE)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=TRUE--------------------
head(peaks.df.norm)

## ----eval=TRUE, warning=FALSE, message=TRUE, include=TRUE, fig.width = 7, fig.height=6----
# plot all normalized peaks
ggplot(peaks.df.norm %>%
         mutate(color.column = paste0(measurement, "__bite_", peak)),
       aes(x = t.norm , 
           y = force.norm, 
           colour=color.column)) +
  geom_line()

## ----eval=FALSE, warning=FALSE, message=FALSE, include=TRUE-------------------
#  peaks.df.norm.100 <- red_peaks_100(df = peaks.df.norm,
#                                     plot.to.screen = TRUE,
#                                     path.data = path.data,
#                                     path.plots = path.plots,
#                                     show.progress = TRUE)
#  head(peaks.df.norm.100)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=FALSE-------------------
peaks.df.norm.100 <- red_peaks_100(df = peaks.df.norm, 
                                   plot.to.screen = FALSE,
                                   path.data = NULL,
                                   path.plots = NULL,
                                   show.progress = FALSE)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=TRUE--------------------
head(peaks.df.norm.100)

## ----eval=TRUE, warning=FALSE, message=TRUE, include=TRUE, fig.width = 7, fig.height=6----
head(peaks.df.norm.100)

# plot normalized peaks: 5 bites per measurement
ggplot(peaks.df.norm.100 %>%
         mutate(color.column = paste0(measurement, "-bite", peak)),
       aes(x = index ,
           y = force.norm.100,
           colour=color.column)) +
  geom_line()

## ----eval=FALSE, warning=FALSE, message=FALSE, include=TRUE-------------------
#  peaks.df.100.avg <- avg_peaks(df = peaks.df.norm.100,
#                                path.data = path.data)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=FALSE-------------------
peaks.df.100.avg <- avg_peaks(df = peaks.df.norm.100,
                              path.data = NULL)
head(peaks.df.100.avg)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=TRUE--------------------
head(peaks.df.100.avg)

## ----eval=TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6-----
# plot averaged normalized curves per species
ggplot(peaks.df.100.avg, aes(x = index , 
                             y = force.norm.100.avg, 
                             colour=species)) +
  geom_line()

## ----eval=TRUE, warning=FALSE, message=FALSE, include=FALSE-------------------
best.fit.poly <- find_best_fits(df = peaks.df.100.avg)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  best.fit.poly <- find_best_fits(df = peaks.df.100.avg,
#                                  plot.to.screen = TRUE,
#                                  path.data = path.data,
#                                  path.plots = path.plots)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=TRUE--------------------
best.fit.poly

## ----eval=TRUE, warning=FALSE, message=FALSE, include=FALSE-------------------
models <- peak_to_poly(df = peaks.df.100.avg, 
                       coeff = best.fit.poly,
                       path.data = NULL,
                       show.progress = FALSE)

## ----eval=FALSE, warning=FALSE, message=FALSE, include=TRUE-------------------
#  models <- peak_to_poly(df = peaks.df.100.avg,
#                         coeff = best.fit.poly,
#                         path.data = path.data,
#                         show.progress = TRUE)

## ----eval=TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6-----
# create tibble with model data
models.df <- NULL
for(i in 1:length(models)){
  model.df <- tibble(species = rep(names(models)[i], 100),
                     index = 1:100,
                     y = predict(models[[i]]))
  models.df <- rbind(models.df, model.df)
}

# plot all polynomial models
ggplot(models.df,
       aes(x = index ,
           y = y,
           colour=species)) +
  geom_line()

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  ## ---------------------------
#  ##
#  ## PURPOSE OF SCRIPT:
#  ##      Simulate data and test various functions of the forceR package.
#  ##
#  ##
#  ## AUTHOR:
#  ##      Peter T. Rühr
#  ##
#  ## DATE CREATED:
#  ##      2022-04-07
#  ##
#  ## Copyright (c) Peter T. Rühr, 2022
#  ## Email: peter.ruehr@gmail.com
#  ##
#  ## ---------------------------
#  ##
#  ## NOTES:
#  ##
#  ##
#  ##
#  ##  -------------------------------------------------------------------------
#  ##  DEPENDENCIES
#  
#  # to use viewport()
#  require(grid)
#  
#  # forceR
#  require(forceR)
#  
#  # various plotting functions
#  require(ggplot2)
#  
#  # load tidyverse for its various conveniences
#  require(tidyverse)
#  
#  
#  ##  -------------------------------------------------------------------------
#  ##  FUNCTIONS
#  # get data as string for saving files
#  today <- function(){
#    date.string <- gsub("-", "_", substring(as.character(as.POSIXct(Sys.time())), 1, 10))
#    return(date.string)
#  }
#  
#  # plot linear regression and return relevant values
#  plot.linear.regression <- function(x, y,
#                                     logarithmic = F,
#                                     x.axis.label = "x",
#                                     title = NULL,
#                                     x.lim = NULL, y.lim = NULL){
#    if(logarithmic == "10"){x <- log10(x); y <- log10(y)}
#    if(logarithmic == "e"){x <- log(x); y <- log(y)}
#    lin.mod <- lm(y ~ x)
#    lin.mod.sum <- summary(lin.mod)
#    lin.mod.r2 <- lin.mod.sum$adj.r.squared
#    lin.mod.p <- lin.mod.sum$coefficients[2,4]
#    lin.mod.intercept <- lin.mod$coefficients[1]
#    lin.mod.slope <- lin.mod$coefficients[2]
#    lin.mod.label.r2 <- bquote(italic(R)^2 == .(format(lin.mod.r2, digits = 3)))
#    if(lin.mod.p > 0.05) {lin.mod.p.ast <- lin.mod.p}
#    if(lin.mod.p <= 0.05 & lin.mod.p > 0.01) {lin.mod.p.ast <- "*"}
#    if(lin.mod.p <= 0.01 & lin.mod.p > 0.001) {lin.mod.p.ast <- "**"}
#    if(lin.mod.p <= 0.001) {lin.mod.p.ast <- "***"}
#  
#    lin.mod.label.p <- bquote(italic(p) == .(lin.mod.p.ast))
#  
#    if(is.null(xlim)){
#      x.lim = c(min(x), max(x))
#    }
#    if(is.null(ylim)){
#      y.lim = c(min(y), max(y))
#    }
#  
#    if(lin.mod.p >= 0.001) p.print <- paste0("p = ", round(lin.mod.p, 3))
#    if(lin.mod.p < 0.001) p.print <- "p < 0.001"
#    plot(x, y, pch = 16, xlab = paste0(x.axis.label, ": ", p.print,
#                                       "; R2 = ", round(lin.mod.r2,3),
#                                       "; m = ",  round(lin.mod.slope,3),
#                                       "; y = ",  round(lin.mod.intercept,3)),
#         xlim = x.lim, ylim = y.lim)
#  
#    abline(lin.mod, col = "red")
#  
#    if(!is.null(title)){
#      title(main = title)
#    }
#    print(x.axis.label)
#    print(paste0(p.print))
#    print(paste0("R2 = ", round(lin.mod.r2,3)))
#    print(paste0("m = ", round(lin.mod.slope,3)))
#    print(paste0("y = ", round(lin.mod.intercept,3)))
#  
#    res <- tibble(p = lin.mod.p,
#                  r2 = lin.mod.r2,
#                  intercept = lin.mod.intercept,
#                  slope = lin.mod.slope)
#    return(res)
#  }
#  
#  # add leading zeros to number and return as string
#  add_eading_zeros <- function(number, length){
#    require(stringr)
#    str_pad(number, length, pad = "0")
#  }
#  
#  
#  ## -------------------------------------------------------------------------
#  ## Simulate bite force data
#  # set seed for randomization so results are reproducible
#  set.seed(1)
#  
#  
#  ## -------------------------------------------------------------------------
#  ## Create classifier to store data (1 row per measurement)
#  classifier <- tibble(bite.type = c(rep("sinosoidal", 5), rep("plateau", 3), rep("inter", 4)),
#                                              peak.position = c("early","early","center","late","late","center","center","center","center","center","early","late"),
#                                              species = NA,
#                                              specimen = NA,
#                                              measurement = NA,
#                                              body_mass = NA,
#                                              force_in = NA,
#                                              length.of.bite = 4000,
#                                              peak.pos = c(20, 25, 50, 65, 70, rep(NA, 7)),
#                                              slope.perc.starts = c(rep (NA, 5), 10,15,20,30,40,20,50),
#                                              slope.perc.ends = c(rep (NA, 5), 10,15,20,30,40,50,20),
#                                              type = c(rep("sin", 5), rep("plat", 7)),
#                                              no.of.bites = 7,
#                                              amp = 1,
#                                              lever.ratio = 1,
#                                              length.of.series = 35000)
#  
#  
#  # amend classifier
#  classifier <- classifier %>%
#    mutate(no = row_number())
#  classifier
#  
#  # define maximum forces per bite simulation type and add additional rows to classifier
#  forces <- c(1, 5, 15)
#  for(i in 1:nrow(classifier)){
#    classifier$force_in[i] <- forces[1]
#    classifier <- classifier %>%
#      add_row(classifier[i, ])
#    classifier$force_in[nrow(classifier)] <- forces[1]
#    for(f in 2:3){
#      classifier <- classifier %>%
#        add_row(classifier[i, ])
#      classifier$force_in[nrow(classifier)] <- forces[f]
#      classifier <- classifier %>%
#        add_row(classifier[i, ])
#      classifier$force_in[nrow(classifier)] <- forces[f]
#    }
#  }
#  
#  # arrange classifier by original bite.type and remove bite.typeing column
#  classifier <- classifier %>%
#    arrange(no) %>%
#    select(-no)
#  
#  # create vector of species names and add to classifier
#  species.names <- NULL
#  for(i in 1:(nrow(classifier)/2)){
#    species.names <- c(species.names,
#                       rep(paste0("S", add_eading_zeros(i, 2)), 2))
#  }
#  classifier$species <- species.names
#  
#  classifier$specimen <- paste0("s", add_eading_zeros(1:nrow(classifier), 3))
#  classifier$measurement <- paste0("m", add_eading_zeros(1:nrow(classifier), 3))
#  
#  # assign body mass according to the maximum force
#  classifier$body_mass[classifier$force_in == forces[1]] <- 1
#  classifier$body_mass[classifier$force_in == forces[2]] <- 6
#  classifier$body_mass[classifier$force_in == forces[3]] <- 25
#  
#  # add jitter to force and body mass to replicate biological variation
#  for(i in 1:nrow(classifier)){
#    classifier$force_in[i] <- round(classifier$force_in[i] +
#                                      ((rnorm(1, -0.2, 0.2)) * classifier$force_in[i]), 2)
#    classifier$body_mass[i] <- round(classifier$body_mass[i] +
#                                       ((rnorm(1, -0.2, 0.2)) * classifier$body_mass[i]), 2)
#  }
#  
#  
#  # -------------------------------------------------------------------------
#  # data processing
#  
#  # get overview of input data before simulating bite series
#  BFQ.regression_in <- plot.linear.regression(x = classifier$body_mass,
#                                              y = classifier$force_in,
#                                              logarithmic = "10",
#                                              x.axis.label = "body mass")
#  
#  # jitter for variation in maximum bite force within a bite series
#  # this was set to 0 when checking if the package finds the correct max. force values
#  # and to 15 to increase bite shape diversity when checking if the package can tell the different
#  # bite shapes apart
#  max.y.jit = 0 # 0 15
#  
#  # jitter to make the bite curve more unstable
#  # this was set to 0 when checking if the package finds the correct max. force values
#  # and to 1 to increase bite shape diversity when checking if the package can tell the different
#  # bite shapes apart
#  jit = 0 # 0 1
#  
#  # create tibble with simulated time series with different
#  # bite characteristics for each measurement, specimen and species
#  
#  # path.plots <- "Z:/PAPERS/PTR_Bite force METHODS/R/package_tests/plots/"
#  # print(paste0("Saving plots at ", path.plots, "/", today(),"_bite_series.pdf..."))
#  # pdf(paste0(path.plots, "/", today(),"_bite_series.pdf..."), onefile = TRUE, paper = "a4", height = 14)
#  par(mfrow = c(3,2))
#  df.all <- NULL
#  for(i in 1:nrow(classifier)){
#    df.curr <- simulate_bites(no.of.bites = 5,
#                              length.of.bite = classifier$length.of.bite[i],
#                              length.of.series = 5*classifier$length.of.bite[i] + 5*1000,
#                              max.y = classifier$force_in[i],
#                              max.y.jit = max.y.jit,
#                              jit = jit,
#                              peak.pos = classifier$peak.pos[i],
#                              slope.perc.start <- classifier$slope.perc.starts[i],
#                              slope.perc.end <- classifier$slope.perc.ends[i],
#                              bite.type = classifier$type[i],
#                              plot = TRUE)
#  
#    # add measurement number to df.curr
#    df.curr <- df.curr %>%
#      mutate(measurement = classifier$measurement[i])
#  
#    # add current sumulated bite series to df.all
#    df.all <- rbind(df.all, df.curr)
#  }
#  # dev.off()
#  
#  # remove columns from classifier that were only used during bite series simulation
#  classifier <- classifier %>%
#    select(-c(jit, length.of.bite, peak.pos,
#              slope.perc.starts, slope.perc.ends,
#              type, no.of.bites))
#  
#  
#  # here starts a forceR WORKFLOW for AFTER FILE LOADING ####
#  # please see the package vignette for details on how to load files.
#  
#  # reduce sampling frequency to 200 Hz
#  df.all.200 <- reduce_frq(df = df.all,
#                           Hz = 200,
#                           measurement.col = "measurement")
#  
#  # convert y values to force and add measurement columns from classifier info (df.all)
#  df.all.200.tax <- y_to_force(df = df.all.200,
#                               classifier = classifier,
#                               measurement.col = "measurement")
#  
#  # summarize force data per specimen
#  df.summary.specimen <- summarize_measurements(df = df.all.200.tax,
#                                                var1 = "measurement",
#                                                var2 = "specimen")
#  
#  # add body mass from classifier:
#  df.summary.specimen <- df.summary.specimen %>%
#    left_join(classifier %>%
#                select(measurement, body_mass),
#              by = "measurement")
#  
#  # boxplot of maximum force of all specimens
#  ggplot(data = df.summary.specimen, mapping = aes(x=specimen,y=max.F.measurement)) +
#    geom_jitter(color='blue',alpha=0.5, width = 0.2) +
#    geom_boxplot(fill="blue",color="black",alpha=0.1) +
#    # scale_y_log10() +
#    labs(x='specimen', y="max(F)/specimen") +
#    guides(color="none") +
#    theme_minimal() +
#    ggtitle("max. bite force per measurement") +
#    xlab("specimen") +
#    ylab("max. force (N)") +
#    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 5))
#  
#  # Summarize to species-wise info
#  # We are not using the summarize_measurements() functions because this would ignore
#  # the fact the some measurements may come from the same specimen, but we only want
#  # to consider on maximum force value per specimen and not on per measurement.
#  df.summary.species <- df.summary.specimen %>%
#    # find max Fs of species
#    group_by(species) %>%
#    # calculate force values for each species
#    mutate(max.F.species = max(max.F.specimen),
#           mean.F.species = round(mean(max.F.specimen),6),
#           sdv.max.F.species = sd(max.F.specimen)) %>%
#    ungroup() %>%
#    # count specimens / species
#    group_by(species) %>%
#    mutate(n.specimens.in.species = length(unique(specimen))) %>%
#    # add body mass from classifier
#    left_join(classifier %>%
#                select(measurement),
#              by = c("measurement")) %>%
#    # calculate mean body mass per species
#    group_by(species) %>%
#    mutate(body_mass.species = mean(body_mass)) %>%
#    ungroup()
#  
#  # boxplot of maximum force in species
#  ggplot(data = df.summary.species, mapping = aes(x=species,y=mean.F.species)) +
#    geom_jitter(color='blue',alpha=0.5, width = 0.2) +
#    geom_boxplot(fill="blue",color="black",alpha=0.1) +
#    # scale_y_log10() +
#    labs(x='species', y="mean(F)/species") +
#    guides(color="none") +
#    theme_minimal() +
#    ggtitle("max. bite force per species") +
#    xlab("species") +
#    ylab("max. force (N)") +
#    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 5))
#  
#  # calculate and plot the regressions of known (simulation inputs) and extracted forces over body length
#  # pdf(file = paste0(path.plots, today(), "_regressions.pdf"),
#  #     paper = "special", width = 5, height = 12)
#  par(mfrow=c(3,1))
#  # Specimen-wise regression of known maximum force values over body mass
#  BFQ.regression_in <- plot.linear.regression(x = classifier$body_mass,
#                                              y = classifier$force_in,
#                                              logarithmic = "10",
#                                              x.axis.label = "body mass")
#  
#  # Specimen-wise regression of extracted maximum force data over body mass
#  BFQ.regression.specimen <- plot.linear.regression(x = df.summary.specimen$body_mass,
#                                                    y = df.summary.specimen$max.F.specimen,
#                                                    logarithmic = "10",
#                                                    x.axis.label = "body mass")
#  
#  # Species-wise regression of extracted maximum force data over body mass
#  BFQ.regression.species <- plot.linear.regression(x = df.summary.species$body_mass.species,
#                                                   y = df.summary.species$mean.F.species,
#                                                   logarithmic = "10",
#                                                   x.axis.label = "body mass")
#  
#  # dev.off()
#  par(mfrow=c(1,1))
#  
#  # calculate differences between known and extracted maximum forces
#  force.comp <- classifier %>%
#    select(measurement, force_in) %>%
#    left_join(df.summary.specimen %>%
#                select(measurement, max.F.measurement),
#              by = "measurement") %>%
#    mutate(diff.abs = max.F.measurement - force_in,
#           diff.perc = diff.abs*100/force_in) %>%
#    arrange(diff.perc)
#  
#  # violin plot of differences between known and extracted maximum for per specimen [in %]
#  ggplot(data = force.comp, mapping = aes(x= 1, y=diff.perc)) +
#    geom_jitter(color='blue',alpha=0.7, width = 0.1) +
#    geom_violin(fill="blue",color="black",alpha=0.1) +
#    # scale_y_log10() +
#    labs(x='', y="diff. pred/actual [%]") +
#    guides(color="none") +
#    theme_minimal()
#  
#  # extract coefficients of species-wise regression
#  # to calculate bite force quotient (BFQ; Wroe et al. 2005, Christiansen & Wroe 2007)
#  regression.m <- BFQ.regression.species$slope
#  regression.y <- BFQ.regression.species$intercept
#  
#  # calculate BFQ per species
#  df.summary.species$BFQ.body_mass <- 100*(df.summary.species$mean.F.species/
#                                             10^(regression.m * log10(df.summary.species$body_mass) + regression.y))
#  
#  # plot species-wise BFQ as histogram
#  hist(df.summary.species$BFQ.body_mass, breaks = 25)
#  
#  # INDIVIDUAL BITE CURVE FINDING
#  # find five strongest peaks per species
#  peaks.df <- find_strongest_peaks(
#    df = df.all.200.tax,
#    no.of.peaks = 5,
#    plot.to.screen = FALSE,
#    path.data = NULL,
#    path.plots = NULL,
#    show.progress = TRUE)
#  
#  # save plots of every peak in a PDF
#  plot_peaks(df.peaks = peaks.df,
#             df.data = df.all.200.tax,
#             additional.msecs = 2000,
#             path.plots = NULL,
#             show.progress = TRUE)
#  
#  # rescale bites
#  peaks.df.norm <- rescale_peaks(df.peaks = peaks.df,
#                                 df.data = df.all.200.tax,
#                                 plot.to.screen = FALSE,
#                                 path.data = NULL,
#                                 show.progress = TRUE)
#  
#  # check if rescaling worked: both following lines should print 1
#  max(peaks.df.norm$t.norm)
#  max(peaks.df.norm$force.norm)
#  
#  # reduce to 100 observations per bite
#  peaks.df.norm.100 <- red_peaks_100(df = peaks.df.norm,
#                                     plot.to.screen = TRUE,
#                                     path.plots = NULL,
#                                     path.data = NULL,
#                                     show.progress = TRUE)
#  
#  # get average bite curve per species
#  peaks.df.100.avg <- avg_peaks(df = peaks.df.norm.100,
#                                path.data = NULL)
#  
#  # find best polynomial degree to describe all average curves
#  best.fit.poly <- find_best_fits(df = peaks.df.100.avg,
#                                  plot.to.screen = FALSE,
#                                  path.data = NULL,
#                                  path.plots = NULL,
#                                  show.progress = TRUE)
#  
#  
#  # convert species-wise average curves to polynomial models
#  models <- peak_to_poly(df = peaks.df.100.avg,
#                         coeff = best.fit.poly,
#                         path.data = NULL,
#                         show.progress = TRUE)
#  
#  # convert models to PCA input data
#  pca.data <- sapply(models, function(x){
#    cbind(x[[1]]) # extract the common coefficients
#  })
#  
#  # transpose PCA data (coefficients of polynomial models)
#  pca.data <- t(pca.data)
#  
#  # perform PCA
#  PCA.bite.shape <- prcomp(pca.data)
#  summary(PCA.bite.shape)
#  
#  # store and principal component scores in a tibble
#  PCA.res <- as_tibble(PCA.bite.shape$x[,1:3]) %>%
#    mutate(species = rownames(PCA.bite.shape$x)) %>%
#    left_join(classifier %>%
#                select(bite.type, peak.position, species),
#              by = "species") %>%
#    select(bite.type, peak.position, species, PC1, PC2) %>%
#    distinct(species, .keep_all = TRUE) %>%
#    dplyr::rename(peak.position = peak.position,
#                  bite.type = bite.type)
#  
#  # plot PC1 against PC2
#  ggplot(data = PCA.res, aes(x = PC1, y = PC2, col = bite.type)) +
#    geom_point() +
#    theme_minimal()
#  
#  # pdf(file = paste0(path.plots, today(), "_PCA_bite_shape.pdf"),
#  #     paper = "a4r", width = 29, height = 21) # , height = 14
#  ggplot(data = PCA.res, aes(x = PC1, y = PC2, col = peak.position)) +
#    geom_point() +
#    theme_minimal()
#  # dev.off()
#  
#  # plot PC1 against PC1 with bite shapes as insets
#  # pdf(file = paste0(path.plots, today(), "_PCA_bite_shape_w_curves.pdf"),
#  #     paper = "a4r", width = 20, height = 21) # , height = 14
#  # create main PCA plot
#  main_plot <- ggplot(data = PCA.res, aes(x = PC1, y = PC2, col = peak.position)) +
#    geom_point() +
#    theme(axis.title.x=element_blank(),
#          axis.text.x=element_blank(),
#          axis.ticks.x=element_blank(),
#          axis.title.y=element_blank(),
#          axis.text.y=element_blank(),
#          axis.ticks.y=element_blank(),
#          legend.position = "none")
#  print(main_plot)
#  
#  
#  # create an and plot an inlet with the bite shape
#  # of the first bite of the first
#  # simulation settings of each specimen
#  species_to_plot <- paste0("S", seq(1, nrow(PCA.res), 3))
#  for(i in seq(1, nrow(PCA.res), 3)){
#    curr.PC1 <- PCA.res$PC1[i]
#    curr.PC2 <- PCA.res$PC2[i]
#    curr.species <- PCA.res$species[i]
#    curr.bite.data <- peaks.df.100.avg %>%
#      filter(species == curr.species)
#  
#    inset_plot <- ggplot(curr.bite.data, aes(index, force.norm.100.avg)) +
#      geom_line() +
#      theme(axis.title.x=element_blank(),
#            axis.text.x=element_blank(),
#            axis.ticks.x=element_blank(),
#            axis.title.y=element_blank(),
#            axis.text.y=element_blank(),
#            axis.ticks.y=element_blank(),
#            plot.margin=grid::unit(c(0,0,0,0), "null"),
#            panel.background = element_rect(fill = 'white', colour = 'white')) +
#      theme(aspect.ratio=1)
#  
#    #A viewport taking up a fraction of the plot area
#    vp <- viewport(width = 0.1, height = 0.1,
#                   x = (curr.PC1-min(PCA.res$PC1))/diff(range(PCA.res$PC1)),
#                   y =(curr.PC2-min(PCA.res$PC2))/diff(range(PCA.res$PC2)))
#  
#    print(inset_plot, vp = vp)
#  }
#  # dev.off()
#  

