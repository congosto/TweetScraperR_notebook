# Functions context
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# put_context_search
#
# Guarda el contexto la última descarga de búsqueda
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
put_context_search <- function(dataset, prefix, date){
  context_file <- file.path(dataset,paste0(prefix,"_search_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# put_context_replies
#
# Guarda el contexto la última descarga de replies
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
put_context_replies <- function(dataset, prefix, last_tweet_id){
  context_file <- file.path(dataset,paste0(prefix,"_replies_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
  }
  context <- tribble(
    ~last_tweet_id,
    last_tweet_id
  )
  write_csv (context, context_file)
  return ()
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# get_context_replies
#
# Obtienen el contexto de la última descarga de replies, si la hubiera
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
get_context_replies <- function(dataset, prefix){
  context_file <- file.path(dataset,paste0(prefix,"_replies_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
    return (NULL)
  }
  if(!file.exists(context_file)) {
    return (NULL)
  }
  context <- read_csv (
    context_file,
    col_names = TRUE,
    cols_only(
      last_tweet_id = col_character()
    )
  ) 
  last_id_tweet <- context$last_tweet_id[nrow(context)]
  return (last_id_tweet)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# put_context_cites
#
# Guarda el contexto la última descarga de replies
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
put_context_cites <- function(dataset, prefix, last_tweet_id){
  context_file <- file.path(dataset,paste0(prefix,"_cites_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
  }
  context <- tribble(
    ~last_tweet_id,
    last_tweet_id
  )
  write_csv (context, context_file)
  return ()
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# get_context_repliescites
#
# Obtienen el contexto de la última descarga de replies, si la hubiera
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
get_context_cites <- function(dataset, prefix){
  context_file <- file.path(dataset,paste0(prefix,"_cites_context.csv"))
  if(!file.exists(dataset)) {
    dir.create(dataset)
    return (NULL)
  }
  if(!file.exists(context_file)) {
    return (NULL)
  }
  context <- read_csv (
    context_file,
    col_names = TRUE,
    cols_only(
      last_tweet_id = col_character()
    )
  ) 
  last_id_tweet <- context$last_tweet_id[nrow(context)]
  return (last_id_tweet)
}


