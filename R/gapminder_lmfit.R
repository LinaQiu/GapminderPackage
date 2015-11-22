#' @title Model pop/lifeExp/gdpPercap VS year for countries in gapminder dataset.
#'
#' @description gapminder_lmfit is used to fit linear models to pop/lifeExp/gdpPercap and year for a specific country in gapminder dataset.
#' It can be used to carry out regression between pop/lifeExp/gdpPercap and year, then it will return the coefficients, residuals
#' and fitError(=sum of residuals^2) of fitted linear model back at the same time.
#'
#' @param countryName character this should be the name of country that you want to explore in gapminder dataset.
#' @param x character this should be variable "year" here.
#' @param y character this could be one of the three variables(pop/lifeExp/gdpPercap) in gapminder dataset. Default value of y is "pop".
#' @param offset this can be used to specify an a priori known component to be included in the linear predictor
#' during fitting. In this function, the default value of offset is 1952, because we think it makes more sense for
#' the intercept to correspond to pop/lifeExp/gdpPercap in 1952, the earliest date in gapminder dataset.
#'
#' @return list
#' @export
#' @examples
#' library(gapminder)
#' gapminder_lmfit("Canada")
#' gapminder_lmfit("Canada","year","lifeExp")
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @import gapminder

gapminder_lmfit <- function(countryName,x="year",y="pop",offset=1952)
{
  if(countryName == ""){
    return("Please provide a country name that you would like to explore.")
  }

  ##Get names for all countries in gapminder dataset, which are pre-stored at a file named "countryNames"
  countryNames <- (gapminder %>% arrange(year) %>% head(142))$country    ##because we know there are 142 countries in gapminder dataset in total
  countryNames <- as.character(countryNames)

  if(!(countryName %in% countryNames)){
    return("The country name you provided not found in gapminder. Please try an existing country name.")
  }

  if(!(y %in% c("pop","lifeExp","gdpPercap"))){
    return("Invalid parameter y. You can leave y equal to its default value 'pop' or set it as pop/lifeExp/gdpPercap.")
  }
  countryData <- gapminder %>% filter(country==countryName)
  results <- vector("list",4)
  lmfit <- lm(countryData[[y]]~I(countryData[[x]]-offset),countryData)
  intersect <- setNames(coef(lmfit)[1],"intercept")
  slope <- setNames(coef(lmfit)[2],"slope")
  res <- lmfit$residuals
  fitError <- setNames(sum(res^2),"fitError(sumOFres^2)")
  res <- setNames(res,c("residuals",c(2:length(res))))

  results[[1]] <- intersect
  results[[2]] <- slope
  results[[3]] <- fitError
  results[[4]] <- res

  return(results)
}
