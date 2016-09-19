#' Plot the variance distribution of transformed metabolite abundances
#'
#' @param cometsdata output of readCOMETSinput function
#' @param title main title for the plot (default is "Distribution of Variance for Each Metabolite")
#' @param xlabel x-axis label (default is "Variance of transformed metabolite abundances")
#' @param ylabel y-axis label (default is "Frequency")
#'
#' @return a distribution plot
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' plotVar(exmetabdata)
#'
#' @export

plotVar <- function(cometsdata,
                    title = "Distribution of Variance for Each Metabolite",
                    xlabel = "Variance of transformed metabolite abundances",
                    ylabel = "Frequency") {
   if(is.null(cometsdata$metab$var)) {
	stop("The input data is not in the correct format.  Make sure it is the output of the 
            readCOMETSinput function")
   }

   toplot <- cometsdata$metab$var
   p <- plot_ly(
     x=toplot,
     type="histogram") %>%
   layout(title=title,
     xaxis = list(title=xlabel),yaxis = list(title=ylabel),
     bargap=0.3)
   return(p)
} 

#' Plot the distribution of the number of missing values for each metabolite
#'
#' @param cometsdata output of readCOMETSinput function
#' @param title main title for the plot (default is "Distribution of the Number/Missing Values for each Metabolite")
#' @param xlabel x-axis label (default is "Number of minimum/missing values")
#' @param ylabel y-axis label (default is "Frequency")
#'
#' @return a distribution plot
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' plotMinvalues(exmetabdata)
#'
#' @export

plotMinvalues <- function(cometsdata,
                    title = "Distribution of the Number/Missing Values for each Metabolite",
                    xlabel = "Number of minimum/missing values",
                    ylabel = "Frequency") {

   if(is.null(cometsdata$metab$num.min)) {
        stop("The input data is not in the correct format.  Make sure it is the output of the
            readCOMETSinput function")
   }

   toplot <- cometsdata$metab$num.min
   p <- plot_ly(
     x=toplot,
     type="histogram") %>%
   layout(title=title,
     xaxis = list(title=xlabel),yaxis = list(title=ylabel),
     bargap=0.3)
   return(p)
}


