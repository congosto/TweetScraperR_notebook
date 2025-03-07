# Functions format

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# get_grupos
#
# divide una lista en grupos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
get_grupos <- function(lista, size_grupo) {
  split(lista, ceiling(seq_along(lista)/size_grupo))
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# unify_col
#
# selecciona de una lista el primer elemento
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
unify_col <- function(column) {
  sapply(column, function(x) {
    if (is.vector(x)) {
      return(x[[1]])
    } else {
      return(x)
    }
  })
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# format_meta_tweets
#
# formata los meta tweets
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
format_meta_tweets <- function(meta_tweets){
  meta_tweets <- meta_tweets %>% 
    mutate (texto = str_replace_all (texto, '[\n\r]+',' ')) %>%
    mutate (tweet_citado = str_replace_all (tweet_citado, '[\n\r]+',' ')) %>%
    mutate (emoticones = ifelse(emoticones == "character(0)",NA,emoticones)) %>%
    mutate(reproducciones =  str_extract(metricas, "\\d+ (?=reproducciones)")) %>%
    mutate (links_img_user = ifelse(links_img_user == "character(0)",NA,links_img_user)) %>%
    mutate (links_img_post = ifelse(links_img_post == "character(0)",NA,links_img_post)) %>%
    mutate (urls = ifelse(urls == "character(0)",NA,urls)) %>%
    select (fecha,username,texto,tweet_citado,user_citado,emoticones,
            links_img_user, links_img_post,links_externos,
            respuestas,reposteos,megustas,reproducciones,metricas,
            urls,hilo,url,fecha_captura)
  meta_tweets$emoticones <- unify_col(meta_tweets$emoticones)
  meta_tweets$links_img_user <- unify_col(meta_tweets$links_img_user)
  meta_tweets$links_img_post <- unify_col(meta_tweets$links_img_post)
  meta_tweets$urls <- unify_col(meta_tweets$urls)
  return (meta_tweets)
}