# Constantes

DATA_URL  = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTK0P84XEWt-l9bfDn3lztEjl1noD3cqr9TwXYPASVBxR7_HDAgbriM3Zqp2dl6034dTE1QaADRIlVI/pub?gid=0&single=true&output=csv"


data <- read.csv(url(DATA_URL), stringsAsFactors = FALSE)

casos <- data %>%
  filter(municipio %in% c("Monclova","Saltillo","Torreón")) %>%
  mutate(fecha_identificacion = as.Date(fecha_identificacion,"%d/%m/%Y")) %>%
  group_by(municipio, fecha_identificacion) %>%
  tally(name = "casos") %>%
  mutate(acumulados = cumsum(casos) ) %>%
  mutate(dif = c(0, diff(acumulados))) %>%
  select(-c(casos:acumulados)) %>%
  # tidyr::spread(municipio,fecha_identificacion)
  reshape2::dcast(fecha_identificacion ~ municipio )
  # tidyr::replace_na(0)
  # replace(., is.na(.), 0)
  # data.frame() %>% head()


ggplot(data = casos, aes(x = fecha_identificacion, y = value, color = variable )  ) +
            ylab('Diferencia de casos acumulados') +
            scale_y_log10() +
            geom_line(aes(y = Monclova , col='Monclova'), size=1, alpha=.5) +
            geom_line(aes(y = Saltillo, col='Saltillo'),  size=1, alpha=.5) +
            geom_line(aes(y = Torreón, col='Torreón'),  size=1, alpha=.5) +
            theme(legend.position=c(.1,.85))
