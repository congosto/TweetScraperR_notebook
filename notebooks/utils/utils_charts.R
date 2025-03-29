# Functions shared by visualization notebooks
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# my_theme
#
# Plantilla para las gráficas
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
my_theme <- function(
    base_size = 13,
    base_color = "#5a5856",
    base_family = "sans"
)
{
  # Establecemos la proporción del tamaño de la letra de cada uno de los elementos
  template <- 
    theme_bw(base_size=base_size, base_family = base_family) +
    theme(
      panel.border = element_rect(colour = base_color),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(
        t = 2,  # Margen superior
        r = 6,  # Margen derecho
        b = 0.5,  # Margen inferior
        l = 6
      ),
      plot.title = element_text(
        size = base_size+4,
        face = "bold",
        color = base_color,
        vjust = 1.25, 
        hjust = 0),
      plot.subtitle = element_text(size=base_size + 2, color = base_color, hjust = 0),
      plot.caption = element_text(hjust = 1, size=base_size - 1, color = base_color),
      legend.position ="top",
      legend.text = element_text(size=base_size + 1),
      text = element_text(size=base_size, color = base_color),
      axis.title.x = element_text(
        size=base_size + 1,
        vjust=0,
        color = base_color,
        margin = unit(c(3, 0, 0, 0), "mm")
      ),
      axis.title.y = element_text(
        size=base_size + 1, 
        vjust=1.25,
        color = base_color,
        margin = unit(c(0, 3, 0, 0), "mm")
      ),
      axis.title.y.right=element_text(
        size=base_size + 1,
        color = base_color,
        margin = unit(c(0, 0, 0, 3), "mm")
      ),
      axis.text.x = element_text(size=base_size - 2, color = base_color),
      axis.text.y = element_text(size=base_size-1,color = base_color),
      strip.text = element_text(size=base_size + 2, color = base_color),
      strip.text.x = element_text(size=base_size+1, color = base_color),
      strip.text.y = element_text(size=base_size+1, color = base_color),
      strip.background = element_rect(color = NA, fill = NA)
    )
  return(template)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# time_scale
#
# Escala de tiempo para las fechas del eje x
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
time_scale <- function(ini_date,end_date) {
  num_days <- as.numeric(difftime(end_date ,ini_date , units = c("days")))
  num_years <- num_days / 365
  num_months <- num_days / 30
  num_weeks <- num_days / 7
  num_hours <- num_days * 24
  spaced_dates <- case_when(
    num_years >= 10 ~ "years",
    num_months >= 12 ~ paste(as.integer((num_months+12)/12), "months"),
    num_weeks >= 25 ~ paste(as.integer((num_weeks+7)/7), "weeks"),
    num_days >= 4 ~ paste(as.integer((num_days+10)/10), "days"),
    num_days >= 2 ~ paste(as.integer((num_hours+10)/10), "hours"),
    num_hours >= 0 ~ paste(as.integer((num_hours+14)/14), "hours") 
  ) 
  return(spaced_dates)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# format_time
#
# Formato para las fechas del eje x
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
format_time <- function(ini_date,end_date) {
  num_days <- as.numeric(difftime(end_date ,ini_date , units = c("days")))
  num_years <- num_days / 365
  num_months <- num_days / 30
  num_weeks <- num_days / 7
  num_hours <- num_days * 24
  format_dates <- case_when(
    num_years >= 10 ~ "%Y",
    num_months >= 12 ~ "%b\n%Y",
    num_weeks >= 25 ~ "%d-%b\n%Y",
    num_days >= 4 ~ "%d-%b\n%Y",
    num_hours >= 0 ~ "%H:00\n%d-%b" 
  )
  return(format_dates)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# format_time_2
#
# Formato para las fechas del eje x
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
format_time_plain <- function(ini_date,end_date) {
  num_days <- as.numeric(difftime(end_date ,ini_date , units = c("days")))
  num_years <- num_days / 365
  num_months <- num_days / 30
  num_weeks <- num_days / 7
  num_hours <- num_days * 24
  format_dates <- case_when(
    num_years >= 10 ~ "%Y",
    num_months >= 12 ~ "%b-%Y",
    num_weeks >= 25 ~ "%d-%b-%Y",
    num_days >= 4 ~ "%d-%b-%Y",
    num_hours >= 0 ~ "%H:00-%d-%b" 
  )
  return(format_dates)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# expand_time
#
# Aumentar el tamaño del eje X
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
expand_time <- function(ini_date,end_date, percentage) {
  num_days <- as.numeric(difftime(end_date ,ini_date , units = c("days")))
  num_seconds <- num_days * 24 * 60 * 60
  expand <- (num_seconds * percentage)/100
  return(expand)
}
