library(tibble)
library(dplyr)
library(tidyverse)

# Create a date range vector
date_range <- seq(as.Date("2025-03-03"), as.Date("2026-08-30"), by = "day")

# Create a tibble with the date range
tibble_data <- tibble(Date = date_range)

# Add the Period column using mutate and case_match
tibble_data <- tibble_data %>%
  mutate(Period = case_when(
    Date >= as.Date("2025-03-03") & Date <= as.Date("2025-08-31") ~ "Pre-intervention period",
    Date >= as.Date("2025-09-01") & Date <= as.Date("2026-03-01") ~ "Intervention 1) Reading Program",
    Date >= as.Date("2026-03-02") & Date <= as.Date("2026-08-30") ~ "Intervention 2) Peer Tutoring Sessions"
  ))

## test

# Create a date range vector
date_range <- seq(as.Date("2025-03-03"), as.Date("2026-08-30"), by = "day")

# Create a tibble with the date range
tibble_data <- tibble(Date = date_range)

# Add the Period column using mutate and case_match
tibble_data <- tibble_data %>%
  mutate(Period = case_when(
    Date >= as.Date("2025-03-03") & Date <= as.Date("2025-08-31") ~ "Pre-intervention period",
    Date >= as.Date("2025-09-01") & Date <= as.Date("2026-03-28") ~ "Intervention 1) Reading Program",
    Date >= as.Date("2026-03-02") & Date <= as.Date("2026-08-30") ~ "Intervention 2) Peer Tutoring Sessions"
  ))

##


box::use(../../phei_functions/phei)

phei_calendar <- function(df,
                          date_column = NULL,
                          factor_column = NULL,
                          colours = NULL,
                          title = "Placeholder: Please supply title or 'element_blank()' to `title` argument",
                          subtitle = "Placeholder: Please supply subtitle or 'element_blank()' to `subtitle` argument",
                          caption = "PH.Intelligence@hertfordshire.gov.uk",
                          ...) {
  box::use(lubridate[month, mday])
  box::use(magrittr[`%>%`])
  box::use(forcats[fct_relevel])
  box::use(ggplot2[aes, element_text, element_rect, element_blank])
  box::use(stringi[stri_datetime_fields])
  box::use(dplyr[mutate])
  box::use(rlang[sym])

  date_column <- rlang::sym(date_column)
  factor_column <- rlang::sym(factor_column)

  df <- tibble_data |> dplyr::mutate(
    mon = lubridate::month(Date, label = T, abbr = F),
    wkdy = weekdays(Date,
                    abbreviate =
                      T
    ) |> forcats::fct_relevel("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    day = lubridate::mday(Date),
    week = stringi::stri_datetime_fields(Date)$WeekOfMonth,
    year = lubridate::year(Date),
    year_mon = zoo::as.yearmon(Date, "%Y %m")
  ) |>
    dplyr::mutate(across(week, ~ case_when(wkdy == "Sun" ~ week - 1,
                                           .default = as.numeric(week)
    )))

  df %>%
    ggplot2::ggplot(., ggplot2::aes(wkdy, week)) +
    # custom theme stuff below
    # geom_tile and facet_wrap will do all the heavy lifting
    ggplot2::geom_tile(
      alpha = 0.8,
      ggplot2::aes(fill = !!factor_column),
      color = "black", ...
    ) +
    ggplot2::facet_wrap(~mon, scales = "free_x", ncol = 2) +
    ggplot2::geom_text(ggplot2::aes(label = day)) +
    # put your y-axis down, flip it, and reverse it
    ggplot2::scale_y_reverse(breaks = NULL) +
    # manually fill scale colors to something you like...
    ggplot2::scale_fill_manual(
      values = colours,
      na.value = "white",
      na.translate = FALSE
    ) +
    ggpubr::theme_pubclean() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      fill = "",
      x = "",
      y = "",
      title = element_blank(),
      caption = "PH.Intelligence@hertfordshire.gov.uk"
    )
}

df <- tibble_data |> dplyr::mutate(
  mon = lubridate::month(Date, label = T, abbr = F),
  wkdy = weekdays(Date,
                  abbreviate =
                    T
  ) |> forcats::fct_relevel("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
  day = lubridate::mday(Date),
  week = stringi::stri_datetime_fields(Date)$WeekOfMonth,
  year = lubridate::year(Date),
  year_mon = zoo::as.yearmon(Date, "%Y %m")
) |>
  dplyr::mutate(across(week, ~ case_when(wkdy == "Sun" ~ week - 1,
                                         .default = as.numeric(week)
                                         )))

df %>%
  ggplot2::ggplot(., ggplot2::aes(wkdy, week)) +
  # custom theme stuff below
  # geom_tile and facet_wrap will do all the heavy lifting
  ggplot2::geom_tile(
    alpha = 0.8,
    ggplot2::aes(fill = Period),
    color = "black"
  ) +
  ggplot2::facet_wrap(~year_mon, scales = "free_x", ncol = 3) +
  ggplot2::geom_text(ggplot2::aes(label = day), size = 3) +
  # put your y-axis down, flip it, and reverse it
  ggplot2::scale_y_reverse(breaks = NULL) +
  # manually fill scale colors to something you like...
  # ggplot2::scale_fill_manual(
  #   values = colours,
  #   na.value = "white",
  #   na.translate = FALSE
  # ) +
  ggpubr::theme_pubclean() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(
    fill = "",
    x = "",
    y = "",
    title = element_blank(),
    caption = "PH.Intelligence@hertfordshire.gov.uk"
  )

phei_calendar(tibble_data, date_column = "Date", "Period", colours = phei_palettes$cool_colours)




