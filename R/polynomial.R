#' Find Best Polynomial Fits for Curves
#'
#' Calculates best model fits for all curves based on AIC criterion. The function fits polynomial functions with 1 to 20 coefficients and uses the Akaike Information
#'   Criterion (AIC) to evaluate the goodness of the fits. A model is considered a good fit, when the percentage of change from one model to the next (e.g. a model with
#'   6 coefficients to a model with 7 coefficients) is `< 5%`. The first for models meeting this criterion are plotted as colored graphs and the AICs of these models
#'   are visualized in a second plot for each curve. All first four coefficients per curve that fulfill the criterion are stored and in the end, a histogram of how
#'   often which coefficients were good fits is plotted as well. The function returns the numerical value of the coefficient that fulfilled the criterion of a good fit
#'   in most curves.
#'
#' @param df The resulting tibble of the function `avg_peaks()`. See `?avg_peaks` for more details.
#'
#' @param plot.to.screen A logical value indicating if results should be
#' plotted in the current R plot device. Default: `FALSE`.
#'
#' @param path.data A string character defining where to save the results. If `NULL`,
#' data is not stored in a file. Default: `NULL`.
#'
#' @param path.plots A string character defining where to save the plots. If `NULL`,
#' plots will not be saved to PDF files. Default: `NULL`.
#'
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#'
#' @return Returns the a numerical value representing the number of coefficient that was most often under the first 4 models that were followed by an
#'   AIC-change `<= 5%` by the next model. Additionally, plots showing the model fits and a histogram of the coefficients that met the 5%-criterion can be
#'   plotted to the plot device or saved as PDFs in `path.plots`.
#'
#' @export
#'
#' @examples
#' # Using the forceR::peaks.df.100.avg dataset:
#'
#' # find smallest polynomial degree that best describes all curves
#' best.fit.poly <- find_best_fits(df = forceR::peaks.df.100.avg)
#'
#' best.fit.poly
#'
find_best_fits <- function(df,
                           plot.to.screen = FALSE,
                           path.data = NULL,
                           path.plots = NULL,
                           show.progress = FALSE){

  # # testing
  # find_best_fits (df = forceR::peaks.df.100.avg,
  #                 plot.to.screen = FALSE,
  #                 path.data = NULL,
  #                 path.plots = NULL,
  #                 show.progress = FALSE)
  #
  # find_best_fits (df = forceR::peaks.df.100.avg,
  #                 plot.to.screen = TRUE,
  #                 path.data = "./test_folder",
  #                 path.plots = "./test_folder",
  #                 show.progress = TRUE)

  oldpar <- par(no.readonly = TRUE)    # code line i
  on.exit(par(oldpar))

  if(!is.null(path.data)){
    if(!dir.exists(path.data)) stop ("Folder to store plots does not exist: ", path.data, ".")
  }

  if(!is.null(path.plots)){
    if(!dir.exists(path.plots)) stop ("Folder to store plots does not exist: ", path.plots, ".")
  }

  # if(!is.logical(plot.to.pdf)) stop ("'plot.to.pdf' must be logical.")

  # dplyr NULLs
  species <- NULL

  # if(plot.to.pdf == TRUE){
  if(!is.null(path.plots)){
    # print(paste0("Saving plots at ", path.plots, "/", today(),"_mean_normalized_peaks_100_fits.pdf..."))
    pdf(file.path(path.plots, paste0("mean_normalized_peaks_100_fits_", today(), ".pdf")),
        onefile = TRUE, paper = "a4", height = 14)
  }
  par(mfrow=c(3,2))
  taxa <- unique(df$species)
  coeffs.df <- NULL
  for(b in 1:length(taxa)){ # length(taxa)
    curr.species <- taxa[b]

    curr.peak.100 <- df %>%
      filter(species == curr.species)

    sp.models <- list()
    for (i in 1:20) { # test 1st to n-th polynomial function
      curr.coeff <- stats::poly(curr.peak.100$index, degree = i)
      sp.models[[i]] <- curr.fit <- lm(curr.peak.100$force.norm.100.avg ~ curr.coeff)
      # print(summary(curr.fit)) # optional
    }

    ### AIC criterion to decide which model fit is appropriate
    aics <- lapply(sp.models, function(x){
      invisible(AIC(x))
    })
    ### BIC criterion (since more conservative)
    # bics <- lapply(sp.models, function(x){
    #   invisible(BIC(x))
    # })

    aic.diffs <- c()
    aic.diffs.perc <- c()
    for(i in 20:2){
      curr.diff <- aics[[i-1]]-aics[[i]]
      aic.diffs <- c(aic.diffs, curr.diff)
      aic.diffs.perc <- c(aic.diffs.perc, abs(curr.diff*100/aics[[i]])) # aics[[i-1]]
    }

    aic.results <- tibble(coeff.1=1:19,
                          coeff.2 = 2:20,
                          aic.diffs = rev(aic.diffs),
                          aic.diffs.perc = rev(aic.diffs.perc))

    how.many.coeffs.to.check <- 4
    coeff.colors <- c("orange", "cyan", "red", "magenta")


    # check if starts and ends of coeffs are near 0
    potential.coeffs.starts <- c()
    for(m in 1:20){
      if(predict(sp.models[[m]])[1] <= 0.2 & predict(sp.models[[m]])[1] >= -0.2 &
         predict(sp.models[[m]])[100] <= 0.2 & predict(sp.models[[m]])[100] >= -0.2){
        potential.coeffs.starts <- c(potential.coeffs.starts, m)
      }
    }

    if(!is.null(potential.coeffs.starts)){
      potential.coeffs.perc <- aic.results$coeff.1[which(aic.results$aic.diffs.perc <= 5)]

      coeffs <- intersect(potential.coeffs.starts, potential.coeffs.perc)[1:how.many.coeffs.to.check]
      coeffs <- coeffs[complete.cases(coeffs)]

      if(length(coeffs) > 0){

        ### have a look at the fitted models visually
        if(plot.to.screen == TRUE){
          plot(curr.peak.100$index, curr.peak.100$force.norm.100.avg, type="n", lwd=3)
          for (i in 2:length(sp.models)) {
            lines(predict(sp.models[[i]]), lwd=0.5, col="grey50") # rainbow(length(sp.models))[i]
          }
          lines(curr.peak.100$index, curr.peak.100$force.norm.100.avg, lwd=1)
          for(p in 1:length(coeffs)){
            curr.coeff <- coeffs[p]
            lines(predict(sp.models[[curr.coeff]]), lwd=1, col=coeff.colors[p])
          }
          title(main = paste0(curr.species), cex.main = 0.95)

          boxplot(aics)
          for(p in 1:length(coeffs)){
            curr.coeff <- coeffs[p]
            points(x = curr.coeff, y = aics[[curr.coeff]], col = coeff.colors[p], cex = 2, lwd = 2)
          }
          title(main = paste0("coeffs = ", paste(coeffs, collapse = ", ")), cex.main = 0.95)

        }
        coeffs.df <- rbind(coeffs.df,
                           tibble(species = curr.species, coeffs = paste(coeffs, collapse = "; ")))
      } else{
        warning(paste0(curr.species, " does not fit coeff-finder criteria..."))
      }
    } else{
      warning(paste0(curr.species, " does not fit coeff-finder criteria..."))
    }

    if(show.progress == TRUE){
      print_progress(b, length(taxa))
    }
  }
  # if(plot.to.pdf == TRUE){
  if(!is.null(path.plots)){
    invisible(dev.off())
  }
  par(mfrow=c(1,1))

  if(!is.null(path.data)){
    # print(paste0("Saving coeffs.df at ", path.data, "/", today(), "_mean_normalized_peaks_100_coeffs.csv..."))
    write_csv(coeffs.df, file.path(path.data, paste0("mean_normalized_peaks_100_coeffs_", today(), ".csv")))
  }

  # find most-often well-fitting coeffs
  all.coeffs <- unlist(str_split(coeffs.df$coeffs, pattern = "; "))
  best.fit.coeff <- as.numeric(names(sort(table(all.coeffs), decreasing = TRUE))[1])
  # if(plot.to.pdf == TRUE){
  if(!is.null(path.plots)){
    # print(paste0("Saving plots at ", path.plots, "/", today(),"_normalized_peaks_100_coeff_histo.pdf..."))
    pdf(file.path(path.plots,
                  paste0("normalized_peaks_100_coeff_histo_", today(), ".pdf")),
        onefile = TRUE, paper = "a4", height = 14)

    invisible(dev.off())
  }

  if(plot.to.screen == TRUE){
    plot(table(all.coeffs),
         main = paste0("best-fitting coeff = ", best.fit.coeff, "; n = ", length(all.coeffs)))
  }
  # print(paste0("Polynomial model with most best fits contains ", best.fit.coeff, " coefficients."))
  # print("Done!")
  return(best.fit.coeff)
}




#' Convert Time Series to Polynomial
#'
#' @param df The resulting tibble of the function `avg_peaks()`. See `?avg_peaks` for more details.
#'
#' @param coeff A numerical value indicating the number of coefficients the model used to fit on the time series data should have.
#'
#' @param path.data A string character defining where to save the results as
#' `*.csv` and `*.R`. If `NULL`, data is not stored in files. Default: `NULL`.
#'
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#'
#' @return A list with the length equal to the number of unique species within `df` containing the fitted models.
#' @export
#' @examples
#' # Using the forceR::peaks.df.100.avg dataset:
#'
#' # define the number of coefficients the polynomial models should have
#' number_of_coeffs = 4
#'
#' # convert curves to polynomial models
#' models <- peak_to_poly(df = forceR::peaks.df.100.avg,
#'                         coeff = number_of_coeffs)
#'
#' models

peak_to_poly <- function(df,
                         coeff,
                         path.data = NULL,
                         show.progress = FALSE){

  # # testing
  # number_of_coeffs = 4
  # models <- peak_to_poly(df = forceR::peaks.df.100.avg,
  #                        coeff = number_of_coeffs)
  #
  # number_of_coeffs = 4
  # models <- peak_to_poly(df = forceR::peaks.df.100.avg,
  #                        coeff = number_of_coeffs,
  #                        path.data = "./test_folder",
  #                        show.progress = TRUE)


  if(sum(colnames(df) %in% c("species", "index", "force.norm.100.avg")) != 3){
    stop ("column names of 'df' must contain 'species', 'index', 'force.norm.100.avg'")
  }
  if(!is.numeric(coeff)) stop ("'coeff' must be numeric.")

  species <- NULL

  models <- NULL
  taxa <- unique(df$species)
  for(b in 1:length(taxa)){ # length(taxa)
    curr.species <- taxa[b]

    curr.peak.100 <- df %>%
      filter(species == curr.species)

    # fit an n-th polynomial function on the data for each species
    models[[(length(models)+1)]] <- curr.fit <- lm(curr.peak.100$force.norm.100.avg ~ stats::poly(curr.peak.100$index, degree = coeff))

    names(models)[length(models)] <- curr.species

    if(show.progress == TRUE){
      print_progress(b, length(taxa))
    }
  }

  if(!is.null(path.data)){
    # print(paste0("Saving models at ", path.data, today(),"_normalized_peaks_100_poly_models.txt..."))
    sink(file.path(path.data, paste0("normalized_peaks_100_poly_models_", today(), ".txt")))
    print(models)
    sink()
    save(models, file = file.path(path.data, paste0("normalized_peaks_100_poly_models_", today(), ".R")))
  }
  # print("Done!")
  return(models)
}
