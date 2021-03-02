loadR = function(x) {
  if (!eval(parse(text = paste("require(", x, ")")))) {
    install.packages(x); eval(parse(text = paste("require(", x, ")")))
  }
}

loadR("magrittr")
loadR("ggplot2")
loadR("tsibble")
loadR("tidyr")
