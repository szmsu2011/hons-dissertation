loadR = function(x) {
  x = deparse(substitute(x))
  if (!eval(parse(text = paste("require(", x, ")")))) {
    install.packages(x)
    eval(parse(text = paste("library(", x, ")")))
  }
}

loadR(magrittr)
loadR(ggplot2)
loadR(tsibble)
loadR(tidyr)
