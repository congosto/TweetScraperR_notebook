# Functions charts
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_tweets_vs_reach_influencers
#
# Chart line de doble escala del total tweets vs alcance de los mismos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
draw_tweets_vs_reach_influencers <- function(df, periodo,  ini_date, end_date, nin_reproducciones, max_overlaps,events) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(fecha_slot >= ini_date & fecha_slot <= end_date)
  }
  # Agrupamos los tweets por slot de tiempo y calculamos el alcance
  tweets_vs_reach_df <- df %>% 
    group_by(fecha_slot) %>%
    summarise(
      num_tweets = n(), 
      reach = sum(reproducciones,na.rm=T),
      .groups = 'drop'
    ) %>% 
    ungroup() 
  # Buscamos los influencers de cada una de los slots de tiempo
  tweets_vs_influencer_df <- df %>% 
    group_by(fecha_slot, username) %>%
    reframe(
      influencer = ifelse(reproducciones >= nin_reproducciones, username, NA),
      reach = reproducciones,
      .groups = 'drop'
    ) %>%
    ungroup() %>%
    filter (!is.na(influencer)) %>%
    group_by (fecha_slot,influencer) %>%
    slice(1) %>%
    ungroup()
  # Calculamos las dos escalas
  max_tweets <- max(tweets_vs_reach_df$num_tweets,na.rm = TRUE)
  max_reach <- max(tweets_vs_influencer_df$reach,na.rm = TRUE)
  ajuste_escala <- max_reach/max_tweets
  limit_y = max_tweets
  #definimos la paleta de color
  p <- ggplot(data = tweets_vs_reach_df) + 
    # Pintamos la evolución de los tweets
    geom_step(
      aes( x = fecha_slot, y = num_tweets),
      color = color_tweets,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    geom_point(
      data = tweets_vs_influencer_df,
      aes( x=fecha_slot,  y= reach/ajuste_escala, size = reach/ajuste_escala),
      color= color_reach,
      alpha = 0.8,
      shape = 19,
      show.legend = FALSE
    ) +
    geom_text_repel(
      data = tweets_vs_influencer_df,
      aes(
        x = fecha_slot,
        y = reach/ajuste_escala,
        label = influencer
      ),
      color = COLOR_TEXTO,
      ylim = c(0, limit_y*1.1),
      force = 10,
      max.overlaps = max_overlaps,
      max.time = 10,
      size = 3.5,
      vjust = .5,
      segment.size = 0.5,
      segment.linetype = 2,
      min.segment.length = 0, 
      show.legend = FALSE
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    #Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. Original tweets per",slot_time), 
      labels = label_number(scale_cut = cut_si('')),
      limits= c(0,limit_y*1.3 ),
      expand= c(0,0),
      sec.axis = dup_axis(
        trans=(~ . * ajuste_escala), 
        name = "Reach influencers",
        labels = label_number(scale_cut = cut_si('')) 
      )
    ) +
    scale_size(range = c(1, 2)) +
    # Ponemos los títulos
    labs(
      title = paste(
        params$base_title, ":",
        "<span style='color:",
        color_tweets,
        "'>",
        "Tweets</span>",
        "per",slot_time,
        "vs.",
        "<span style='color:",
        color_reach,
        "'>",
        "Reach influencers</span>"
      ),
      subtitle = paste0("Reach influencers  >= ",  label_number(scale_cut = cut_si(""))(nin_reproducciones)),
      x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position = "top",
      plot.title = element_markdown(),
      axis.title.y = element_text(color = color_tweets),
      axis.title.y.right = element_text(color = color_reach),
      axis.text.y = element_text(color = color_tweets),
      axis.text.y.right = element_text(color = color_reach)
    )
  if (!is.null(events)) {  
    if (periodo == "zoom"){
    events <-events %>% 
        filter(date >= ini_date & date <= end_date)
    }
    p <- p +
      geom_vline(
        data = events,
        aes(xintercept=date),
        linetype="dashed",
        color = COLOR_TEXTO
      ) +
      geom_label (
        data = events,
        aes (x = date, y = max_tweets * 1.2, label = paste(format(date,"%d-%m-%Y"),"\n",event)),
        color = COLOR_TEXTO,
        size = 3.5,
        vjust = .5
     ) 
} 
  return(p)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_tweets_vs_reach
#
# Chart line de doble escala del total tweets vs alcance de los mismos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
draw_tweets_vs_reach <- function(df, periodo,  ini_date, end_date, events) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Agrupamos los tweets por hora y calculamos el alcance
  tweets_vs_reach_df <- df %>% 
    group_by(fecha_slot) %>%
    summarise(
      num_tweets = n(), 
      reach = sum(reproducciones,na.rm=T),
      .groups = 'drop'
    ) %>% 
    ungroup() 
  # Calculamos las dos escalas
  max_tweets <- max(tweets_vs_reach_df$num_tweets,na.rm = TRUE)
  max_reach <- max(tweets_vs_reach_df$reach,na.rm = TRUE)
  ajuste_escala <- max_reach/max_tweets
  limit_y = max_tweets
  #definimos la paleta de color
  p <- ggplot(data = tweets_vs_reach_df) + 
    # Pintamos la evolución de los tweets
    geom_step(
      aes( x = fecha_slot, y = num_tweets),
      color = color_tweets,
      show.legend = FALSE
    ) +
    geom_point(
      aes( x=fecha_slot,  y= reach/ajuste_escala, size = reach/ajuste_escala),
      color= color_reach,
      alpha = 0.8,
      shape = 19,
      show.legend = FALSE
    ) +
    geom_text_repel(
      data = tweets_vs_reach_df %>% top_n(1, num_tweets),
      aes(
        x = fecha_slot, y = num_tweets, 
        label = paste0(
          fecha_slot,
          "\n",
          "Max. tweets = ",scales::comma(num_tweets))
      ),
      color = COLOR_TEXTO,
      force = 1,
      size = 3.5,
      nudge_y =  max_tweets * 0.2,
      segment.size = 0.5,
      segment.linetype = 2,
      min.segment.length = 0, 
      show.legend = FALSE
    ) +
    # Anotamos el máximo de reach/hora
    geom_text_repel(
      data = tweets_vs_reach_df %>%  top_n(1, reach),
      aes(
        x = fecha_slot, y = reach/ajuste_escala, 
        label = paste0(
          fecha_slot,
          "\n",
          "Max.reach = ",scales::comma(reach))
      ),
      color = COLOR_TEXTO,
      force = 1,
      size = 3.5,
      nudge_y = max_reach/ajuste_escala * 0.35,
      direction = "y",
      segment.size = 0.5,
      segment.linetype = 2,
      min.segment.length = 0, 
      #arrow = arrow(type = 'open', length = unit(.2, 'cm')),
      show.legend = FALSE
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    #Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. Original tweets per",slot_time), 
      labels = label_number(scale_cut = cut_si('')),
      limits= c(0,limit_y*1.5 ),
      expand= c(0,0),
      sec.axis = dup_axis(
        trans=(~ . * ajuste_escala), 
        name = paste("Reach per", slot_time),
        labels = label_number(scale_cut = cut_si('')) 
      )
    ) +
    scale_size(range = c(1, 2)) +
    # Ponemos los títulos
    labs(
      title = paste(
        params$base_title, ":",
        "<span style='color:",
        color_tweets,
        "'>",
        "Tweets</span>",
        "per",slot_time,
        "vs.",
        "<span style='color:",
        color_reach,
        "'>",
        "Reach</span>"
      ),
       x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position = "top",
      plot.title = element_markdown(),
      axis.title.y = element_text(color = color_tweets),
      axis.title.y.right = element_text(color = color_reach),
      axis.text.y = element_text(color = color_tweets),
      axis.text.y.right = element_text(color = color_reach)
    )
  if (!is.null(events)) {  
    if (periodo == "zoom"){
      events <-events %>% 
        filter(date >= ini_date & date <= end_date)
    }
    p <- p +
      geom_vline(
        data = events,
        aes(xintercept=date),
        linetype="dashed",
        color = COLOR_TEXTO
      ) +
      geom_label (
        data = events,
        aes (x = date, y = max_tweets * 1.2, label = paste(format(date,"%d-%m-%Y"),"\n",event)),
        color = COLOR_TEXTO,
        size = 3.5,
        vjust = .5
      ) 
  } 
  return(p)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_tweets_vs_RTs
#
# Chart line de doble escala del total tweets vs RTs recibidos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
draw_tweets_vs_RTs <- function(df, periodo,  ini_date, end_date, min_RTs, max_overlaps) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Agrupamos los tweets por hora y calculamos los RT/hora
  tweets_vs_rt_df <- df %>% 
    group_by(fecha_slot) %>%
    summarise(
      num_tweets = n(),
      num_RTs = sum(reposteos,na.rm=T),
      .groups = 'drop'
    ) %>% 
    ungroup() 
  # Buscamos los influencers de cada una de los slots de tiempo
  tweets_vs_influencer_df <- df %>% 
    group_by(fecha_slot, username) %>%
    reframe(
      influencer = ifelse(reposteos >= min_RTs, username, NA),
      num_RTs = sum(reposteos),
      .groups = 'drop'
    ) %>%
    ungroup() %>%
    filter (!is.na(influencer)) %>%
    group_by (fecha_slot,influencer) %>%
    slice(1) %>%
    ungroup()
  print(tweets_vs_influencer_df)
  # Calculamos las dos escalas
  max_tweets <- max(tweets_vs_rt_df$num_tweets,na.rm = TRUE)
  max_RT <- max(tweets_vs_rt_df$num_RTs,na.rm = TRUE)
  ajuste_escala <- max_RT/max_tweets
  limit_y = max_tweets
  p <- ggplot() + 
    # Pintamos la evolución de los tweets originales
    geom_step(
      data = tweets_vs_rt_df,
      aes(x = fecha_slot, y = num_tweets),
      color = color_tweets,
      show.legend = FALSE
    )+
    # Pintamos los la evolución de los RTs
    geom_point(
      data = tweets_vs_rt_df,
      aes(x = fecha_slot, y = num_RTs/ajuste_escala, size = num_RTs/ajuste_escala),
      shape = 19,
      color = color_RT,
      show.legend = FALSE
    )+
    geom_text_repel(
      data = tweets_vs_influencer_df,
      aes(
        x = fecha_slot,
        y = num_RTs/ajuste_escala,
        label = influencer
      ),
      color = COLOR_TEXTO,
      ylim = c(0, limit_y*1.1),
      force = 10,
      max.overlaps = max_overlaps,
      max.time = 10,
      size = 3.5,
      vjust = .5,
      segment.size = 0.5,
      segment.linetype = 2,
      min.segment.length = 0, 
      show.legend = FALSE
    ) +
    # Anotamos el máximo de tweets originales/hora
    geom_text_repel(
      data = tweets_vs_rt_df %>% top_n(1, num_tweets),
      aes(
        x = fecha_slot, y = num_tweets, 
        label = paste0(
          fecha_slot,
          "\n",
          "Max. tweets = ",scales::comma(num_tweets)
        )
      ),
      color = COLOR_TEXTO,
      force = 1,
      size = 3.5,
      nudge_y =  max_tweets * 0.2,
      segment.size = 0.5,
      segment.linetype = 2,
      min.segment.length = 0, 
      arrow = arrow(type = 'open', length = unit(.2, 'cm')),
      show.legend = FALSE
    ) +
    # Anotamos el máximo de RTs/hora
    geom_text_repel(
      data = tweets_vs_rt_df %>%  top_n(1, num_RTs),
      aes(
        x = fecha_slot, y = num_RTs/ajuste_escala, 
        label = paste0(
          fecha_slot,
          "\n",
          "Max.RTs = ",scales::comma(num_RTs)
        )
      ),
      color = COLOR_TEXTO,
      force = 1,
      size = 3.5,
      nudge_y = max_RT/ajuste_escala * 0.35,
      direction = "y",
      segment.size = 0.5,
      segment.linetype = 2,
      min.segment.length = 0, 
      arrow = arrow(type = 'open', length = unit(.2, 'cm')),
      show.legend = FALSE
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. Original tweets per",slot_time), 
      labels = label_number(scale_cut = cut_si('')),
      limits= c(0,limit_y*1.4),
      expand= c(0,0),
      sec.axis = dup_axis(
        trans=(~ . * ajuste_escala), 
        name = paste("RTs per",slot_time),
        labels = label_number(scale_cut = cut_si(''))
      )
    ) +
    scale_size(range = c(1, 2)) +
    # Ponemos los títulos
    labs(
      title = paste(
        params$base_title, ":",
        "<span style='color:",
        color_tweets,
        "'>",
        "Tweets</span>",
        "per",slot_time,
        "vs.",
        "<span style='color:",
        color_reach,
        "'>",
        "RTs</span>"
      ),
      x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="top",
      plot.title = element_markdown(),
      axis.title.y = element_text(color = color_tweets),
      axis.title.y.right = element_text(color = color_RT),
      axis.text.y = element_text(color = color_tweets),
      axis.text.y.right = element_text(color = color_RT)
    )
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_word_frequency
#
# tagcloud
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Obtener las stop words en Inglés, español y catalán
draw_word_frequency <- function(df, periodo,  ini_date, end_date, RTs) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  custom_stop_words <- bind_rows(
    stop_words,
    data_frame(word = tm::stopwords("spanish"),lexicon = "custom"),
    data_frame(word = tm::stopwords("catalan"),lexicon = "custom")
  )
  # Limpiar y tokenizar texto
  word_frequency <- tweets_meta[, -1] %>% # la primera columna se queda pegada
    mutate(text_plain = gsub('http\\S+\\s*',"",texto)) %>% # Quitamos las URLs
    mutate(text_plain = gsub("RT @\\w+:","",text_plain)) %>% # Quitamos los RTs
    mutate(text_plain = gsub("&amp;","&",text_plain)) %>% # Rectificamos el &
    mutate(text_plain = gsub("@\\w+","",text_plain)) %>% # Quitamos las menciones
    select(reposteos, text_plain) %>%
    unnest_tokens(word, text_plain) %>% # Convertimos las frases en un conjunto de palabras
    anti_join(custom_stop_words) %>% # Eliminamos las stop words
    group_by(word) %>%     # Agrupamos por palabras   
    summarise(
      freq = ifelse(RTs == TRUE, n() + sum(reposteos), n()),
      .groups = "drop"
    ) %>%  # Calculamos la frecuencia de cada palabra
    ungroup() %>%
    filter (!is.na(word)) %>% # Quitamos textos vacíos
    arrange(desc(freq))  %>% # Ordenamos de mayor a menor frecuencia de aparición
    head (1000)
  write_csv (
    word_frequency,
    file.path(data_path,paste0(params$prefix, "_frequency_word.csv"))
  )
  
  # Generamos los colores para las palabras según frecuencia
  paleta <- brewer.pal(8, "Dark2")
  if (RTs){title = "most frequent words"}
    else {title = "most frequently amplified words"}
  # Pintamos la nube de palabras
  p <- ggplot() +
    # Dibujamos la nube de palabras
    geom_text_wordcloud_area(
      data = word_frequency %>% head(100),
      aes(label = word, size = freq, color = freq),
      angle = 0.35
    ) +
    # Definimos la proporción del tamaño de las letras según frecuencia
    geom_text_wordcloud_area(rm_outside = TRUE) +
    geom_text_wordcloud_area(area_corr_power = 1) +
    scale_radius(range = c(4, 20), limits = c(0, NA)) +
    # Aplicamos una paleta de color
    scale_color_gradientn(colors = paleta) +
    # Definimos el título principal y el de los ejes
    labs(
      title = paste(params$base_title,": most frequent words"),
      subtitle = ifelse(RTs == TRUE,"(Adding retweet amplification)",""),
      x = "", y = "",
      color=""
    ) +
    my_theme() 
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_element_frequency
#
# tagcloud
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
draw_col_frequency <- function(df, column, periodo, ini_date, end_date, element) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  col_frequency <- mutate (df,col=df[[column]]) %>%
    filter(!is.na(col)) %>%
    group_by(col) %>%     # Agrupamos por palabras   
    summarise(
      freq = n(),
      .groups = "drop"
    ) %>%  # Calculamos la frecuencia de cada palabra
    ungroup() %>%
    arrange(desc(freq))  %>% # Ordenamos de mayor a menor frecuencia de aparición
    head (1000)
  
  write_csv (
    col_frequency,
    file.path(data_path,paste0(params$prefix, "_frequency_",column,".csv"))
  )
  
  # Generamos los colores para las palabras según frecuencia
  paleta <- brewer.pal(8, "Dark2")
  # Pintamos la nube de palabras
  p <- ggplot() +
    # Dibujamos la nube de palabras
    geom_text_wordcloud_area(
      data = col_frequency %>% head(100),
      aes(label = col, size = freq, color = freq),
      angle = 0.35
    ) +
    # Definimos la proporción del tamaño de las letras según frecuencia
    geom_text_wordcloud_area(rm_outside = TRUE) +
    geom_text_wordcloud_area(area_corr_power = 1) +
    scale_radius(range = c(4, 30), limits = c(0, NA)) +
    # Aplicamos una paleta de color
    scale_color_gradientn(colors = paleta) +
    # Definimos el título principal y el de los ejes
    labs(
      title = paste(params$base_title, ":", element," frequency"),
      x = "", y = "",
      color=""
    ) +
    my_theme() 
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_emoji_frequency
#
# treemap
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
draw_emoji_frequency <- function(df, periodo, ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  
  frecuency_emoji <- df %>%
   filter(!is.na(emoticones)) %>%
  group_by(emoticones) %>%     # Agrupamos por emoticones 
  summarise(
    freq = n(),
    .groups = "drop"
  ) %>%  # Calculamos la frecuencia de cada palabra
  ungroup() %>%
  arrange(desc(freq))  %>% # Ordenamos de mayor a menor frecuencia de aparición
  head (15) 


  # Añadir emoticonos al treemap
  # Generamos los colores para las palabras según frecuencia
  paleta <- brewer.pal(8, "Dark2")
  # Pintamos el treemap
  p<- ggplot(
    data =  frecuency_emoji, 
      aes(
      area = freq,
      label = emoticones,
      fill = freq
    )) +
    geom_treemap() +
    geom_treemap_text(
      place = "centre",
      #family = "Segoe UI Emoji",
      family = "notoemoji",
      grow = TRUE, reflow = TRUE,
      padding.x = grid::unit(1, "mm"), padding.y = grid::unit(3, "mm")) +
    scale_fill_gradient(low = "#cfecf1", high = "#35c8e1") +
    labs(
      title = paste(params$base_title,": emojis"),
      x = "", y = "",
     fill="Frequency"
    ) +
    my_theme() +
    theme(legend.position ="right")
return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_site_acumulated
#
# chart line
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
draw_site_acumulate <- function(df, periodo, ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  df <- df %>%
    mutate(link = str_extract(texto, "https?://\\S+")) %>% # Extraer enlaces
    mutate(site = str_extract(link, "(?<=://)[^/]+")) %>% # Extraer dominios
    filter(
      site != "x.com" &
      site != "ift.tt" &
      site != "t.co" &
      site != "bit.ly") %>% 
    select (fecha_slot, site, reposteos) 
  # Verificar si cada site tiene la fecha máxima, y si no, añadirla
  df_full <- df %>%
    group_by(site) %>%
      mutate(necesita_max = !any(fecha_slot == as.Date(end_date))) %>%
    ungroup()
  # Añadir filas con la fecha máxima para los sites que lo necesitan
  new_rows <- df_full %>%
    filter(necesita_max) %>%
    group_by(site) %>%
      slice(1) %>%
    ungroup() %>%
    select(site) %>%
    mutate(
      fecha_slot = as.Date(end_date),
    reposteos = 0) %>%
    select (fecha_slot, site, reposteos)
  df <- bind_rows(df, new_rows) %>%
    group_by(site,fecha_slot) %>%
    summarise(
      tweets_count = n()+ sum(reposteos)
    ) %>%
    mutate(cumulative_sum = cumsum(tweets_count)) 
  top_sites <- df %>%
    group_by(site) %>%
    summarise(
      total = sum(tweets_count),
      .groups = "drop"
    ) %>%
    arrange (desc(total)) %>%
    head (15)
  visible_dates <- as.POSIXct(seq(ini_date, end_date, by = time_scale(ini_date, end_date + ( expand_time(ini_date, end_date, 50)))))
  limit_x = as.POSIXct(c(ini_date, end_date + ( expand_time(ini_date, end_date, 50))))
  limit_y =  max(df$cumulative_sum, na.rm = TRUE) 
  p <- ggplot() + 
    geom_line(
      data = df %>% filter (site %in% top_sites$site),  
      aes(x = fecha_slot, y = cumulative_sum, color = site),
      linewidth = 1,
     show.legend =FALSE
    ) +
    geom_text_repel(
     data = df  %>%
       top_n(1, cumulative_sum) %>%
       filter (site %in% top_sites$site),
     aes(
       x = fecha_slot, y = cumulative_sum, color = site,
       label = paste0(
         site,
         "(",
         format(cumulative_sum, big.mark=".",decimal.mark=","),
         " ref.)")
     ), 
     ylim = c(0, limit_y*1.3),
     vjust = 1,
     size = 4,
     nudge_x =  expand_time(ini_date,end_date, 40), # Ajuste eje x
     nudge_y = 0.005,  # Ajuste eje y
     direction = "y",
     max.overlaps = 20,
     segment.size = 0.5,
     segment.linetype = 2,
     show.legend =FALSE
   )+
  scale_x_datetime(
    limits=limit_x,
    breaks = visible_dates,
    date_labels = format_time(ini_date, end_date)
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_si('')),
    limits= c(0,limit_y*1.2 ),
    expand= c(0,0)
  ) +
  labs(
    title = paste0(params$base_title, ": Accumulated sites"),
    subtitle = "(Adding retweet amplification)",
    x = "", 
    y = paste0(" accumulated sites per ", slot_time),
    color=""
  ) +
  guides(color=guide_legend(ncol=2)) +
  my_theme()
return(p)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_topics_acumulated
#
# chart line
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
draw_topics_acumulate <- function(df, topics, periodo, ini_date, end_date, RTs) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  first_time <- TRUE
  df <- df %>%
    select(fecha_slot,texto,reposteos) %>%
    mutate (num_tweets = 1)
  for (topic in topics$topics) {
    aux_df <- df %>%
      filter(str_detect (tolower(texto), tolower(topic))) %>%
      mutate (topics = topic)  %>%
      mutate(reposteos = ifelse(is.na(reposteos), 0, reposteos)) %>%
      mutate(num_tweets = ifelse(is.na(num_tweets), 0, num_tweets)) 
    if (first_time == TRUE){
      topics_df <- aux_df
      first_time <- FALSE
    }else{
      topics_df <- rbind (topics_df,aux_df)
    }
  }
  # Verificar si cada site tiene la fecha máxima, y si no, añadirla
  df_full <- topics_df %>%
    group_by(topics) %>%
    mutate(necesita_max = !any(fecha_slot == end_date)) %>%
    ungroup()
  # Añadir filas con la fecha máxima para los sites que lo necesitan
  new_rows <- df_full %>%
    filter(necesita_max) %>%
    group_by(topics) %>%
    slice(1) %>%
    ungroup() %>%
    select(topics) %>%
    mutate(
      fecha_slot = end_date,
      reposteos = 0,
      num_tweets = 0) 
  topics_df <- bind_rows(topics_df, new_rows) %>%
    group_by(topics,fecha_slot) %>%
    summarise(
      num_topics = ifelse(RTs == TRUE, sum(num_tweets) + sum(reposteos),sum(num_tweets))
    ) %>%
    mutate(cumulative_sum = cumsum(num_topics)) %>%
    arrange (desc(fecha_slot))
 
  visible_dates <- as.POSIXct(seq(ini_date, end_date, by = time_scale(ini_date, end_date + ( expand_time(ini_date, end_date, 50)))))
  limit_x = as.POSIXct(c(ini_date, end_date + ( expand_time(ini_date, end_date, 50))))
  limit_y =  max(topics_df$cumulative_sum, na.rm = TRUE)  
  topics_color <- setNames( topics$color,topics$topics)
  topics_color <-gsub(",", "", topics_color)

  p <- ggplot() + 
    geom_line(
      data = topics_df, 
      aes( 
        x = fecha_slot,
        y = cumulative_sum,
        color = topics,
      ),
      show.legend =FALSE,
      linewidth =1, alpha  = 0.7)+
    geom_text_repel(
      data = topics_df %>% 
        filter(fecha_slot == end_date) %>%
        arrange(desc(cumulative_sum)),
      aes(
        x = fecha_slot,
        y = cumulative_sum,
        color = topics,
        label = paste0(
       topics,
        "(",
        format(cumulative_sum, big.mark=".",decimal.mark=","),
        " ref.)")
      ), 
      force = 10,
      ylim = c(0, limit_y*1.6),
      vjust = 0,
      size = 4,
      nudge_x =  expand_time(ini_date,end_date, 40), # Ajuste eje x
      nudge_y = 0.005,  # Ajuste eje y
      direction = "y",
      max.overlaps = 20,
      segment.size = 0.5,
      segment.linetype = 2,
      show.legend =FALSE
    )+
  scale_x_datetime(
    limits=limit_x,
    breaks = visible_dates,
    date_labels = format_time(ini_date, end_date)
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_si('')),
    limits= c(0,limit_y*1.6 ),
    expand= c(0,0)
  ) +
  scale_color_manual(values = topics_color) +
  labs(
    title = paste0(params$base_title,": Accumulated topics"),
    subtitle = ifelse(RTs == TRUE,"(Adding retweet amplification)",""),
      x = "",
      y =  paste0(" accumulated topics per ", slot_time),
      color = ""
  ) +
  my_theme()
return(p)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# draw_emoji_acumulated
#
# chart line
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
draw_emojis_acumulate <- function(df, periodo, ini_date, end_date, RTs) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  
  frecuency_emoji <- df %>%
    filter(!is.na(emoticones)) %>%
    group_by(emoticones) %>%     # Agrupamos por emoticones 
    summarise(
      freq = n(),
      .groups = "drop"
    ) %>%  # Calculamos la frecuencia de cada palabra
    ungroup() %>%
    arrange(desc(freq))  %>% # Ordenamos de mayor a menor frecuencia de aparición
    head (15)
  first_time <- TRUE
  df <- df %>%
    select(fecha_slot,texto,reposteos) %>%
    mutate (num_tweets = 1)
  for (emoji in frecuency_emoji$emoticones) {
    aux_df <- df %>%
      filter(str_detect (texto, emoji)) %>%
      mutate (emojis = emoji)  %>%
      mutate(reposteos = ifelse(is.na(reposteos), 0, reposteos)) %>%
      mutate(num_tweets = ifelse(is.na(num_tweets), 0, num_tweets)) 
    if (first_time == TRUE){
      emojis_df <- aux_df
      first_time <- FALSE
    }else{
      emojis_df <- rbind (emojis_df,aux_df)
    }
  }
  # Verificar si cada site tiene la fecha máxima, y si no, añadirla
  df_full <- emojis_df %>%
    group_by(emojis) %>%
    mutate(necesita_max = !any(fecha_slot == end_date)) %>%
    ungroup()
  # Añadir filas con la fecha máxima para los sites que lo necesitan
  new_rows <- df_full %>%
    filter(necesita_max) %>%
    group_by(emojis) %>%
    slice(1) %>%
    ungroup() %>%
    select(emojis) %>%
    mutate(
      fecha_slot = end_date,
      reposteos = 0,
      num_tweets = 0) 
  emojis_df <- bind_rows(emojis_df, new_rows) %>%
    group_by(emojis,fecha_slot) %>%
    summarise(
      num_emojis = ifelse(RTs == TRUE, sum(num_tweets) + sum(reposteos),sum(num_tweets))
    ) %>%
    mutate(cumulative_sum = cumsum(num_emojis)) %>%
    arrange (desc(fecha_slot))
  
  visible_dates <- as.POSIXct(seq(ini_date, end_date, by = time_scale(ini_date, end_date + ( expand_time(ini_date, end_date, 50)))))
  limit_x = as.POSIXct(c(ini_date, end_date + ( expand_time(ini_date, end_date, 50))))
  limit_y =  max(emojis_df$cumulative_sum, na.rm = TRUE) 

  p <- ggplot() + 
    geom_line(
      data = emojis_df, 
      aes( 
        x = fecha_slot,
        y = cumulative_sum,
        color = emojis
      ),
      show.legend =FALSE,
      linewidth =1, alpha  = 0.7)+
    geom_text_repel(
      data = emojis_df %>% 
        filter(fecha_slot == end_date) %>%
        arrange(desc(cumulative_sum)),
      aes(
        x = fecha_slot,
        y = cumulative_sum,
        label = paste0(
          emojis,
          "(",
          format(cumulative_sum, big.mark=".",decimal.mark=","),
          " ref.)"),
        color = emojis
      ), 
      family = "notoemoji",
      force = 10,
      ylim = c(0, limit_y*1.5),
      vjust = 0,
      size = 3.5,
      nudge_x =  expand_time(ini_date,end_date, 30), # Ajuste eje x
      nudge_y = 0.005,  # Ajuste eje y
      direction = "y",
      max.overlaps = 30,
      segment.size = 0.5,
      segment.linetype = 2,
      show.legend =FALSE
    )+
    scale_x_datetime(
      limits=limit_x,
      breaks = visible_dates,
      date_labels = format_time(ini_date, end_date)
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_si('')),
      limits= c(0,limit_y*1.7 ),
      expand= c(0,0)
    ) +
    labs(
      title = paste0(params$base_title,": Accumulated emojis"),
      subtitle = ifelse(RTs == TRUE,"(Adding retweet amplification)",""),
      x = "",
      y =  paste0(" accumulated emojis per ", slot_time),
      color = ""
    ) +
    my_theme()
  return(p)
}