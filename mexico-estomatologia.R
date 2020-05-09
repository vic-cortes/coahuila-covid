# Visualización y análsis de los datos para determinar como

library(datosmx)
library(dplyr)

cases <- datosmx::get_covid_cases()


# Seleccionar a Puebla(21)
puebla <- cases %>%
    janitor::clean_names() %>%
    filter(entidad_um  == 21, resultado == 1) %>%
    select(fecha_ingreso, municipio_res, sexo, edad) %>%
    mutate(sexo = ifelse(sexo == 1,"Femenino","Masculino"))

# Casos por municipio
by_mun <- function(data){
    data %>% 
        group_by(municipio_res) %>% 
        tally(name = "n_casos")
}

older_than <- function(data, age = 40){
    data %>%
        filter(edad > age) %>%
        group_by(municipio_res) %>%
        tally(name = "n_casos")
}


# histograma población total En puebla
age = 40

hist(puebla$edad, col = "blue", 
    border = "white", 
    main = "Casos en Puebla")

abline(v = age, col = "red", lwd = 3, lty=2)

p <- ggplot(puebla, aes(x = edad)) + 
    geom_histogram(#aes(y = ..density..), 
        color = "black", fill="#48dbfb") +
    # geom_density(alpha = .2, fill = "#48dbfb") +
    theme_bw()

p + geom_vline(aes(xintercept = mean(puebla$edad)),
    color = "red", linetype = "dashed", size = 1)


# Histograma por edad y sexo
density( puebla %>% filter(sexo == 2) %>% .$edad )

plot(density(subset(puebla, sexo == 2)$edad), col = "blue")
lines(density(subset(puebla, sexo == 1)$edad), col = "pink")


ggplot(puebla, aes(x = edad, color = sexo, fill =  sexo)) +
    geom_histogram(alpha=0.3, position="identity")

p <- ggplot(puebla, aes(x = edad))+
    geom_histogram(color="black", fill="#48dbfb")+
    facet_grid(sexo ~ .)
p

mu <- puebla %>%
    group_by(sexo) %>%
    summarize(grp.mean = mean(edad))

# Add mean lines
p + geom_vline(data = mu, aes(xintercept=grp.mean, color="red"),
            linetype="dashed", size=1)

# Mapa de Puebla
library(rgdal)
library(tmap)

setwd("/Users/vic/Downloads")

state_geo <- function(STATE_ID="21"){
    folder = "mun2005kgw"; file = "mun2005kgw.shp"
    full_path = paste(folder, file, sep="/")

    st_read(full_path) %>%
        filter(CVE_ENT == STATE_ID)
}

to_character <- function(vector){
    as.character(as.numeric(as.character(vector)))
}

puebla_map <- state_geo("21") %>%
    clean_names() %>%
    mutate_each(funs(to_character), cve_mun) %>% # Cambiar a character 
    left_join(( # join con el mapa
        by_mun(puebla) %>%
        # older_than(puebla, age = 60) %>%
            mutate_each(funs(to_character), municipio_res)), # Cambiar a character 
        by = c("cve_mun" = "municipio_res")
    )
    


tm_shape(puebla_map) +
    tm_borders(col = '#2c3e50', lwd = 0.3) +
    tm_fill(
        col = 'n_casos',
        title = 'casos',
        legend.hist = FALSE,
        colorNA = "white",
        showNA = FALSE,
        legend.format = list(text.separator= "-")
    )

# Plot the boundary of estado
plot(st_geometry(puebla_map))


# Pyramid chart

# Hacer el agrupamiento por sexo y edad
etiquetar <- function(vector){
    secuencia <- seq(0,100, by = 10)
    rango <- cut(vector, breaks = secuencia, include.lowest = TRUE) 

    rango %>%
        gsub("\\,"," a ", .) %>%
        gsub("[[:punct:]]","",.)
}


x_lab = seq(-100,100,10)

pyramid <- puebla %>% 
    select(sexo, edad) %>%
    mutate(rango = etiquetar(edad))
    # group_by(sexo, rango) %>%
    # tally(name = "n_casos_rango")


lp <- ggplot(data = pyramid, aes(x = as.factor(rango), fill = sexo)) + 
    geom_bar(data = subset(pyramid, sexo == "Femenino")) + 
    geom_bar(data = subset(pyramid, sexo == "Masculino"), 
        aes(y = ..count..*(-1))) + 
    scale_y_continuous(breaks = x_lab, labels = abs(x_lab)) + 
    coord_flip() +
    scale_fill_brewer(palette = "Set1") + 
    theme_bw() +
    labs(x = "Rango de edad", y = "Casos Positivos")

# Change the legend
lp + scale_shape_discrete(name  = "Sexo",
    breaks =c("1", "2"),
    labels =c("Femenino", "Masculino"))


# Analisis por padecimiento

vars <- c(fecha_actualizacion = "fecha_actualizacion",)

padecimientos <- cases %>%
    janitor::clean_names() %>%
    filter(entidad_um  == 21, resultado == 1)  %>%
    mutate(rango = etiquetar(edad)) %>%
    select(fecha_actualizacion, sexo, rango, municipio_res, intubado:otro_caso,uci) %>%
    mutate_each(funs(replace(. , . == 2, 0)), intubado:uci) %>%
    mutate_each(funs(replace(. , . %in% c(97:99), NA)), intubado:uci) %>%
    mutate(sexo = ifelse(sexo == 1, "Mujer", "Hombre"))


data_graph <- padecimientos %>%
    group_by(sexo, rango) %>%
    summarize(#n_casos = n(),
            intubado = sum(intubado, na.rm = TRUE),
            neumonia = sum(neumonia, na.rm = TRUE),
            embarazo = sum(embarazo, na.rm = TRUE),
            diabetes = sum(diabetes, na.rm = TRUE),
            epoc = sum(epoc, na.rm = TRUE),
            asma = sum(asma, na.rm = TRUE),
            inmusupr = sum(inmusupr, na.rm = TRUE),
            hipertension = sum(hipertension, na.rm = TRUE),
            otra_com = sum(otra_com, na.rm = TRUE),
            cardiovascular = sum(cardiovascular, na.rm = TRUE),
            obesidad = sum(obesidad, na.rm = TRUE),
            renal_cronica = sum(renal_cronica, na.rm = TRUE),
            tabaquismo = sum(tabaquismo, na.rm = TRUE),
            otro_caso = sum(otro_caso, na.rm = TRUE),
            uci = sum(uci, na.rm = TRUE)
    ) %>%
    reshape2::melt(id.vars = c("sexo","rango"))

ggpubr::ggballoonplot(data_graph, x = "variable", y = "rango", size = "value",
    fill = "value", facet.by = "sexo",
    ggtheme = theme_bw()
)