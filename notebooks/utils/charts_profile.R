
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# daily_routine
#
# Scatterplot chart de la rutina de publicación by relation
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
daily_routine <- function(df, date_ini, date_end, time_zone, show_events) {
  df <- df %>%
    filter(fecha >= date_ini & fecha <= date_end)  %>%
    mutate(
      day = as.POSIXct(floor_date(fecha_slot,"day")),
      hour_tweet= hour(lubridate::with_tz(fecha, time_zone))
    )
  tweets_horario_df <- df %>% 
    group_by(day,hour_tweet,username) %>% 
    summarise(
      num_tweets = n(),
      .groups = "drop"
    ) %>%
    ungroup()
  p <- ggplot() + 
    geom_point(
      data = tweets_horario_df,
      aes(
        x = hour_tweet,
        y= day,
        size = num_tweets
      ),
      color = color_tweets,
      alpha =0.5) +
    scale_y_datetime(
      date_labels = format_time_plain(date_ini, date_end),
      date_breaks = ifelse (str_detect (time_scale(date_ini, date_end),"hours"),"1 day", time_scale(date_ini, date_end))
    ) +
    scale_x_continuous(
      breaks = seq(0,23,1),
      limits= c(0,23),
      sec.axis = dup_axis()
    ) +
    labs(
      title = glue("{params$base_title}: daily routine"),
      subtitle = glue("Time zone: {time_zone}"),
      x = "",
      y = "",
      color = "",
      size = "N. tweets") +
    my_theme() +
    theme(
      panel.grid.major.x = element_line(),
      panel.grid.major.y = element_line(),
      legend.position="top",
      legend.text=element_markdown(size=12)
    )
  if (show_events) {
    # Anotamos sucesos
    p <- p + 
      scale_x_continuous(
        breaks = seq(0,23,1),
        limits= c(-5,23),
        sec.axis = dup_axis()
      ) +
      geom_hline(
        data = events,
        aes(yintercept=date),
        linetype="dashed",
        color = "grey50"
      ) +
      geom_label_repel (
        data = events,
        aes (x = -5, y = date, label =event),
        size = 3,
        color = "grey50"
      )
  }
  return(p)
}

# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# rhythm_week 
#
# heatmap de la rutina de publicación por dos unidades de tiempos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
rhythm_week <- function(df, date_ini, date_end, time_zone) {
  df <- df %>%
    filter(fecha >= date_ini & fecha <= date_end)
  tweets_horario_df <- df %>% 
    mutate( slot_time_Y = lubridate::wday (fecha, label  = TRUE)) %>%
    mutate( slot_time_X = as.character (lubridate::hour (fecha))) %>%
    group_by(slot_time_Y, slot_time_X, username) %>% 
    summarise(
      num_tweets = n(),
      .group = "drop"
    ) %>%
    ungroup()
  tweets_horario_df$slot_time_Y <- factor (
    tweets_horario_df$slot_time_Y,
    levels = c("Mon","Tue","Wed","Thu","Fri","Sat", "Sun"))
  tweets_horario_df$slot_time_Y <- fct_rev(tweets_horario_df$slot_time_Y)
  tweets_horario_df$slot_time_X <- factor (
    tweets_horario_df$slot_time_X,
    levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12",
               "13","14","15","16","17","18","19","20","21","22","23"))
  p <- ggplot() + 
    geom_tile(
      data = tweets_horario_df,
      aes(x = slot_time_X, y = slot_time_Y, fill=num_tweets),
      color = "white")+
    scale_fill_gradient(
      low = "#DDEAFA",
      high = "#036DFA",
      guide = guide_legend()) +
    scale_x_discrete(
      expand =  c(0,0)
    ) +
    scale_y_discrete(
      expand =  c(0,0)
    ) +
    guides(color = guide_legend(nrow=2,override.aes = list(size = 4) ) ) +
    labs(
      title = glue("{params$base_title} : weekly rhythm"),
      subtitle = glue("Time zone: {time_zone}"),
      x = "Hour",
      y = "",
      fill = "N. tweets") +
    coord_fixed() +
    my_theme() +
    theme(
      legend.position="right",
      axis.title.y=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  return(p)
}
# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# rhythm_month 
#
# heatmap de la rutina de publicación por dos unidades de tiempos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
rhythm_month <- function(df, date_ini, date_end, time_zone) {
  df <- df %>%
    filter(fecha >= date_ini & fecha <= date_end)
  tweets_horario_df <- df %>% 
    mutate( slot_time_Y = lubridate::month (fecha, label  = TRUE)) %>%
    mutate( slot_time_X = as.character (format (fecha,format ="%y"))) %>%
    group_by(slot_time_Y,slot_time_X, username) %>% 
    summarise(
      num_tweets = n(),
      .group = "drop"
    ) %>%
    ungroup()
  tweets_horario_df$slot_time_Y <- factor (
    tweets_horario_df$slot_time_Y,
    levels = c("Jan","Feb","Mar","Apr","May","Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))
  tweets_horario_df$slot_time_Y <- fct_rev(tweets_horario_df$slot_time_Y)
  p <- ggplot() + 
    geom_tile(
      data = tweets_horario_df,
      aes(x = slot_time_X, y = slot_time_Y, fill=num_tweets),
      color = "white")+
    scale_fill_gradient(low = "#DDEAFA", high = "#036DFA",
                        guide = guide_legend()) +
    scale_x_discrete(
      expand =  c(0,0)) +
    scale_y_discrete(
      expand =  c(0,0)
    ) +
    guides(color = guide_legend(nrow=2,override.aes = list(size = 4) ) ) +
    labs(
      title = glue("{params$base_title}: monthly rhythm"),
      subtitle = glue("Time zone: {time_zone}"),
      x = "Year",
      y = "",
      fill = "N. tweets") +
    my_theme() +
    theme(
      legend.position="right",
      axis.title.y=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# impact_tweets
#
# line chart de doble escala con los tweet originales vs impacto
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
impact_tweets <- function(df, date_ini, date_end, indicator, impact_color){
  tweets_impacto_df <- df %>% 
    filter(fecha >= date_ini & fecha <= date_end) %>%
    group_by(fecha_slot, username) %>%
    summarise(
      num_tweets = n(),
      impact = 
        ifelse(indicator == "Fav", sum(megustas),
               ifelse(indicator == "RTs", sum(reposteos),
                      ifelse(indicator == "Replies", sum(respuestas),
                              ifelse(indicator == "Impresions",sum(reproducciones), NA)))
        ),
      .groups = "drop"
    ) %>% 
    ungroup()
  # Calculamos la media de mensajes e impacto
  mean_impact = round(mean(tweets_impacto_df$impact, na.rm = TRUE),1)
  mean_tweets = round(mean(tweets_impacto_df$num_tweets, na.rm = TRUE),1)
  # Calculamos los límites
  max_tweets <- max(tweets_impacto_df$num_tweets,na.rm = TRUE)
  max_impact <- max(tweets_impacto_df$impact,na.rm = TRUE)
  ajuste_escala <- max_impact/max_tweets
  limit_y = max_tweets
  p <- ggplot(data = tweets_impacto_df) + 
    # Evolución de los tweets
    geom_step(
      aes( x = fecha_slot, y = num_tweets),
      color = color_tweets,
      show.legend = FALSE
    ) +
    # Evolución del impacto
    geom_point(
      aes( x= fecha_slot,  y= impact/ajuste_escala, size = impact/ajuste_escala),
      color= impact_color,
      alpha = 0.8,
      shape = 19,
      show.legend = FALSE
    ) +
    # Pintamos el máximo de tweets
    geom_text_repel(
      data = tweets_impacto_df %>% top_n(1, num_tweets),
      aes(
        x = fecha_slot, y = num_tweets, 
        label = glue("{fecha_slot}\nMax.tweets = {scales::comma(num_tweets)}")
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
    # Anotamos el máximo de impact/hora
    geom_text_repel(
      data = tweets_impacto_df %>%  top_n(1, impact),
      aes(
        x = fecha_slot, y = impact/ajuste_escala, 
        label = glue("{fecha_slot}\nMax.{indicator} = {scales::comma(impact)}")
      ),
      color = COLOR_TEXTO,
      force = 1,
      size = 3.5,
      nudge_y =  max_impact/ajuste_escala * 0.35,
      segment.size = 0.5,
      segment.linetype = 2,
      min.segment.length = 0, 
      arrow = arrow(type = 'open', length = unit(.2, 'cm')),
      show.legend = FALSE
    ) +
    scale_size(range = c(1, 2)) +
    # Anotamos la media  de tweets e impacto
    geom_label(
      aes(
        x = date_ini + (date_end - date_ini) * 0.06, # 6% desde el inicio 
        y = limit_y * 1.4,
        label = glue(
          "mean tweets = {scales::comma(mean_tweets)}
           mean {indicator} = {scales::comma(mean_impact)}")
      ),
      color = COLOR_TEXTO,
      size = 3
    )+
    scale_x_datetime(
      date_labels = format_time(date_ini, date_end),
      date_breaks = time_scale(date_ini, date_end)
    ) +
    scale_y_continuous(
      name = glue("Num. Original tweets per {slot_time}"),
      labels = label_number(scale_cut = cut_si('')),
      limits= c(0,max_tweets*1.5),
      expand= c(0,0),
      sec.axis = sec_axis(trans=(~ . * ajuste_escala), name = glue("{indicator} per day"),
                          labels = (label_number(scale_cut = cut_si(''))) )) +
    labs(
      title = glue(
        "{params$base_title}:<span style='color:{color_tweets}'>
        Tweets</span> per {slot_time} vs <span style='color:{impact_color}'>
        {indicator}</span>"
      ),
      x = "", 
      color=""
    ) +
    my_theme() +
    theme(
      legend.position="top",
      plot.title = element_markdown(),
      axis.title.y = element_text(color = color_tweets, size = 14),
      axis.title.y.right = element_text(color = impact_color, size = 14),
      axis.text.y = element_text(color = color_tweets),
      axis.text.y.right = element_text(color = impact_color)
    )
  if (!is.null(events)) {
    events <-events %>% 
    filter(date >= date_ini & date <= date_end)
    p <- p +
      geom_vline(
        data = events,
        aes(xintercept=date),
        linetype="dashed",
        color = COLOR_TEXTO
      ) +
      geom_label (
        data = events,
        aes (x = date, y = max_tweets * 1.45, label = glue("{event}")),
        color = COLOR_TEXTO,
        size = 3.5,
        vjust = .5
      ) 
  }
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# engagement_tweets
#
# line chart de doble escala con los tweet originales vs impacto
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
engagement_tweets <- function(df, date_ini, date_end, my_color){
  tweets_engagement_df <- df %>% 
    filter(fecha >= date_ini & fecha <= date_end) %>%
    group_by(fecha_slot, username) %>%
    summarise(
      engagement = ifelse(
        sum(reproducciones) > 0,
        sum(reposteos)*100/sum(reproducciones),
        0
      ),
      .groups = "drop"
    ) %>% 
    ungroup()
  max_engagement <- max(tweets_engagement_df$engagement,na.rm = TRUE)
  p <- ggplot(data = tweets_engagement_df) + 
    geom_line(
      aes(
        x = fecha_slot,
        y = engagement
      ),
      color = my_color,
      size = 1,
      alpha = 0.8,
      show.legend = FALSE)+
    geom_text(
      data = tweets_engagement_df %>%   
        top_n(1, engagement),
      aes(
        x = fecha_slot,
        y = engagement * 1.1, 
        label = glue("{fecha_slot}\n{scales::comma(engagement,accuracy = 0.01)} engagement}")
      ),
      color = COLOR_TEXTO,
      nudge_x = 800, 
      nudge_y = 0.0001,
      size = 3.5,
    ) +
    scale_x_datetime(
      date_labels = format_time(date_ini, date_end),
      date_breaks = time_scale(date_ini, date_end)
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_si('')),
      limits= c(0,max_engagement * 1.3),
      expand= c(0,0)
    ) +
    labs(
      title = glue("{params$base_title}: engagement per {slot_time}"),
      subtitle = "engagement = (Sum(RTs) * 100) / impresions", 
      x = "",
      y = "engagement per day",
      color=""
    ) +
    my_theme() +
    theme(
      legend.position="top"
    )
  if (!is.null(events)) {
    events <-events %>% 
      filter(date >= date_ini & date <= date_end)
    p <- p +
      geom_vline(
        data = events,
        aes(xintercept=date),
        linetype="dashed",
        color = COLOR_TEXTO
      ) +
      geom_label (
        data = events,
        aes (x = date, y = max_engagement * 1.25, label = glue("{event}")),
        color = COLOR_TEXTO,
        size = 3.5,
        vjust = .5
      ) 
  }
  return(p)
}