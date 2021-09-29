source(textConnection(readLines("../akl-air-quality/data/prep.R")[seq_len(20)]))

## Break full data into locations
data_raw <- map(unique(data[["location"]]), function(loc) {
  data %>%
    filter(location == loc) %>%
    select(datetime, rh, temp, ws, wind_dir, aqi)
}) %>%
  set_names(unique(data[["location"]]))

## Remove starting NAs (row)
data <- map(data_raw, function(x) {
  starting_na <- max(map_dbl(array_branch(x, 2), function(y) {
    which.min(is.na(y)) - 1
  }))
  if (starting_na > 0) {
    x[-seq_len(starting_na), ]
  } else {
    x
  }
})

## Remove >20% NA columns and locations with >20% NA AQI
data <- map(data, function(x) {
  select_if(x, ~ mean(is.na(.)) < .2)
})
data <- data[map_lgl(data, function(x) {
  "aqi" %in% names(x)
})]

## Fill NA with mean
data <- map(data, function(x) {
  mutate(x, across(-datetime, function(z) {
    replace_na(z, mean(z, na.rm = TRUE))
  }))
})

## Aggregate to daily observation
vector_mean <- function(theta_deg, r = 1, offset_deg = 0, out) {
  theta_rad <- (theta_deg - offset_deg) * pi / 180
  xy <- cbind(x = r * cos(theta_rad), y = r * sin(theta_rad)) %>%
    colMeans(na.rm = TRUE)
  x <- xy[1]
  y <- xy[2]
  if (out == "theta") {
    (atan(y / x) / pi + (x < 0) + 2 * (x > 0 & y < 0)) * 180
  } else if (out == "r") {
    sqrt(x^2 + y^2)
  }
}
data <- map(data, function(x) {
  if ("ws" %in% names(x)) class(x[["ws"]]) <- "ws"
  if ("wind_dir" %in% names(x)) class(x[["wind_dir"]]) <- "wd"
  x %>%
    as_tibble() %>%
    group_by(date = as_date(datetime)) %>%
    select(-datetime) %>%
    summarise(
      across(!contains("date"), function(y) {
        if (class(y) == "wd") {
          vector_mean(y, ws, out = "theta")
        } else {
          mean(y, na.rm = TRUE)
        }
      })
    )
})

## Categorise wind direction, numerise date and initialise lags
data <- map(data, function(x) {
  (if ("wind_dir" %in% names(x)) {
    mutate(x, wind_dir = cut(wind_dir, seq(0, 360, 30), include.lowest = TRUE))
  } else {
    x
  }) %>%
    mutate(
      t = as.numeric(date),
      lag_1 = lag(aqi),
      lag_2 = lag(aqi, 2L),
      lag_3 = lag(aqi, 3L)
    ) %>%
    drop_na() %>%
    select(-date)
})

## Full fit
full_fit <- map(data, function(x) {
  lm(aqi ~ ., data = x, na.action = na.fail)
})
op <- par(mfrow = c(1, 2))
invisible(map(full_fit, function(x) {
  plot.ts(residuals(x))
  acf(residuals(x))
}))
par(op)

## Search for Information-Theoretically best fit
library(MuMIn)
all_fits <- map(data, function(x) {
  dredge(lm(aqi ~ ., data = x, na.action = na.fail))
})
map(all_fits, function(x) {
  anova(get.models(x, 1)[[1]])
})
map(all_fits, function(x) {
  summary(get.models(x, 1)[[1]])
})

## Interaction with wind direction
all_fits <- map(data, function(x) {
  formula <- "aqi ~ (. - lag_1 - lag_2 - lag_3)" %>%
    paste0(ifelse("wind_dir" %in% names(x), " * wind_dir", "")) %>%
    paste0(" + .") %>%
    as.formula()
  dredge(lm(formula, data = x, na.action = na.fail))
})
map(all_fits, function(x) {
  anova(get.models(x, 1)[[1]])
})

## Convert 2019-2020 data to cross-sectional
data <- map(data_raw, function(x) {
  (if ("wind_dir" %in% names(x)) {
    mutate(x, wind_dir = cut(wind_dir, seq(0, 360, 30), include.lowest = TRUE))
  } else {
    x
  }) %>%
    filter(between(year(datetime), 2019, 2020)) %>%
    mutate(
      tod = factor(hour(datetime), seq_len(24) - 1),
      aqi = aqi > 50
    ) %>%
    as_tibble() %>%
    select_if(function(x) !all(is.na(x))) %>%
    select(-datetime) %>%
    drop_na()
})
data <- data[map_lgl(data, function(x) {
  all(nrow(x) > 0, "aqi" %in% names(x))
})]

## Search for Information-Theoretically best fit
all_fits <- map(data, function(x) {
  dredge(glm(aqi ~ ., data = x, family = binomial, na.action = na.fail))
})
map(all_fits, function(x) {
  anova(get.models(x, 1)[[1]], test = "Chisq")
})

## Cross-validate top models
library(crossval)
library(pROC)
model_auc <- function(train.x, train.y, test.x, test.y, all_fit) {
  pred <- get.models(all_fit, 1)[[1]][["formula"]] %>%
    update(train.y ~ .) %>%
    glm(binomial, cbind(train.x, train.y)) %>%
    predict(test.x, type = "response")
  roc(
    response = test.y, predictor = pred,
    levels = levels(test.y), direction = "<"
  )[["auc"]]
}
map2(all_fits, data, function(x, data) {
  set.seed(2021)
  cv <- crossval(
    model_auc, select(data, -aqi), factor(data[["aqi"]]),
    K = 10, B = 1, verbose = FALSE, all_fit = x
  )
  c(auc_stat = cv[["stat"]], auc_se = cv[["stat.se"]])
})

## Full cross-sectional fit with interaction
source(textConnection(readLines("../akl-air-quality/data/prep.R")[seq_len(20)]))

data <- data %>%
  select(datetime, rh, temp, ws, wind_dir, aqi) %>%
  filter(between(year(datetime), 2019, 2020)) %>%
  mutate(
    location = fct_inorder(location),
    wind_dir = cut(wind_dir, seq(0, 360, 90), include.lowest = TRUE),
    tod = factor(hour(datetime), seq_len(24) - 1),
    aqi = aqi > 50
  ) %>%
  as_tibble() %>%
  select_if(function(x) !all(is.na(x))) %>%
  select(-datetime) %>%
  drop_na()

full_fit <- glm(aqi ~ . + ws * wind_dir * location, binomial, data)
anova(full_fit, test = "Chisq")
model_auc <- function(train.x, train.y, test.x, test.y, full_fit) {
  pred <- "train.y ~ . + ws * wind_dir * location" %>%
    as.formula() %>%
    glm(binomial, cbind(train.x, train.y)) %>%
    predict(test.x, type = "response")
  roc(
    response = test.y, predictor = pred,
    levels = levels(test.y), direction = "<"
  )[["auc"]]
}
set.seed(2021)
cv <- crossval(
  model_auc, select(data, -aqi), factor(data[["aqi"]]),
  K = 2, B = 1, verbose = FALSE, full_fit = full_fit
)
c(auc_stat = cv[["stat"]], auc_se = cv[["stat.se"]])
