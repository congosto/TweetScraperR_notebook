# Functions context
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# put_context_search
#
# Guarda el contexto los usuarios que va bajando
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
put_context_search <- function(dataset, prefix, date){
  context_file <- file.path(dataset,paste0(prefix,"_search_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
    return (NULL)
  }
  context <- tribble(
    ~last_date,
    date
  )
  write_csv (context, context_file)
  return ()
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# get_context_search
#
# Obtienen el contexto de la última descarga de búsqueda, si la hubiera
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
get_context_search <- function(dataset, prefix){
  context_file <- file.path(dataset,paste0(prefix,"_search_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
    return (NULL)
  }
  if(!file.exists(context_file)) {
    return (NULL)
  }
  context <- read_csv (context_file, show_col_types = FALSE) 
  last_date <- context$last_date[nrow(context)]
  return (last_date)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# put_context_user
#
# Guarda el contexto los usuarios que va bajando
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
put_context_user <- function(dataset, date, order, username){
  context_file <- file.path(dataset,paste0(prefix,"_users_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
    return (NULL)
  }
  flag_append <- FALSE
  flag_head <- TRUE
  if(file.exists(context_file)) {
    flag_append <- TRUE
    flag_head <- FALSE
  }
  context <- tribble(
    ~last_date, ~order, ~username,
    date,order, username
  )
  write_csv (context, context_file, append = flag_append, col_names = flag_head)
  return ()
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# get_context_user
#
# Obtienen el contexto de la última descarga de de un usuario
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
get_context_user <- function(dataset, prefix, username){
  context_file <- file.path(dataset,paste0(prefix,"_users_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
    return (NULL)
  }
  if(!file.exists(context_file)) {
    return (NULL)
  }
  context <- read_csv (context_file, show_col_types = FALSE) %>%
    group_by(username) %>%
    slice_tail(n = 1) %>%
    ungroup () %>%
    arrange(order)

  return (context)
}




