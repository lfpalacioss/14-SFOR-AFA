source("C:/Users/lfpal/Google Drive/4) R/1) Scripts/1) OEFA/14) SFOR-AFA/PROPUESTA_AFA/global.R")



function(input, output, session) {
        
### ############################################ ###
###     II) BD_AFA: TRABAJAR DATA Y GRAFICAR     ####
### ############################################ ###


# ------------- -
# GRAFICOS AFA
# ------------- -

# Generar los resumenes
output$GRAF_AFA_1 <- renderFormattable({
    
    
    TABLA_AFA_RESUM = BD_AFA %>%
        subset(select = c(NombreFinalGrupo,
                          entidadCapacitada,
                          Entrada,
                          Salida,
                          periodo)) %>% 
        mutate(entidadCapacitada = if_else(entidadCapacitada=="Entidad Capacitada", 1,0, missing = 0)) %>% 
        
        subset(periodo %in% input$PERIODO_1) %>% 
        
        group_by(NombreFinalGrupo) %>% 
        summarise(Matriculados = n(),
                  Capacitados = sum(entidadCapacitada),
                  Entrada = as.numeric(round(mean(Entrada, na.rm = TRUE),1)),
                  Salida = round(mean(Salida, na.rm = TRUE),1)) %>% 
        mutate(Mejoria = round(100*(Salida/Entrada-1),1)) %>% 
        arrange(desc(Matriculados)) %>%
        
        summarise(Matriculados = sum(Matriculados),
                  Capacitados = sum(Capacitados),
                  Entrada = round(mean(Entrada, na.rm = TRUE),1),
                  Salida = round(mean(Salida, na.rm = TRUE),1)) %>%
        
        rename("Total de matriculados" = "Matriculados",
               "Total de capacitados" = "Capacitados",
               "Evaluación de entrada" = "Entrada",
               "Evaluación de salida" = "Salida") %>%
        
        #Tabla
        formattable(align=c("c","c","c","c"),
                    list(`Indicator Name` = formatter("span",
                                                      style = ~ style(color = "grey",
                                                                      font.weight = "bold")),
                         "Total de matriculados" = color_text("white", PALETA.PRINCIPAL[1]),
                         "Total de capacitados" = color_text("white", PALETA.PRINCIPAL[2]),
                         "Evaluación de entrada" = color_text("white", PALETA.PRINCIPAL[3]),
                         "Evaluación de salida" = color_text("white", PALETA.PRINCIPAL[4])
                    )
        )
    
    TABLA_AFA_RESUM
    
    
})



# Generar tabla general
output$GRAF_AFA_2 <- renderFormattable({
    
    
    
    TABLA_AFA = BD_AFA %>%
        subset(select = c(NombreFinalGrupo,
                          entidadCapacitada,
                          Entrada,
                          Salida,
                          periodo)) %>% 
        mutate(entidadCapacitada = if_else(entidadCapacitada=="Entidad Capacitada", 1,0, missing = 0)) %>% 
        
        subset(periodo %in% input$PERIODO_1) %>% 
        
        group_by(NombreFinalGrupo) %>% 
        summarise(Matriculados = n(),
                  Capacitados = sum(entidadCapacitada),
                  Entrada = as.numeric(round(mean(Entrada, na.rm = TRUE),1)),
                  Salida = round(mean(Salida, na.rm = TRUE),1)) %>% 
        
        mutate(Mejoria = round(100*(Salida/Entrada-1),1)) %>% 
        arrange(desc(Matriculados)) %>% 
        
        mutate(Entrada = ifelse(is.nan(Entrada) == T, NA, Entrada),
               Salida = ifelse(is.nan(Salida) == T, NA, Salida),
               Mejoria = ifelse(is.nan(Mejoria) == T, NA, Mejoria),
               Mejoria = ifelse(is.infinite(Mejoria) == T, NA, Mejoria)) %>% 
        
        rename("Evaluación de entrada" = "Entrada",
               "Evaluación de salida" = "Salida",
               "Mejoría (%)" = "Mejoria") %>% 
        
        # Tabla
        formattable(align=c("l","l","l","l","l","c"),
                    list(`Indicator Name` = formatter("span", 
                                                      style = ~ style(color = "grey",
                                                                      font.weight = "bold")),
                         Matriculados = color_bar(PALETA.PRINCIPAL[4]),
                         Capacitados = color_bar(PALETA.PRINCIPAL[3]),
                         `Evaluación de entrada` = color_bar(na.rm = TRUE, PALETA.PRINCIPAL[5]),
                         `Evaluación de salida` = color_bar(na.rm = TRUE, PALETA.SECUNDARIA[2]),
                         `Mejoría (%)` = color_bar(na.rm = TRUE, PALETA.SECUNDARIA[4])
                    )
        )
    TABLA_AFA

})



# Grafico donut
output$GRAF_AFA_3 <- renderPlot({
    
    
    TABLA_AFA_PIE = BD_AFA %>%
        subset(select = c(sexo,
                          codMatricula,
                          periodo)) %>%
        
        subset(periodo %in% input$PERIODO_1) %>% 
        
        group_by(sexo) %>%
        summarise(Matriculados = n_distinct(codMatricula)) %>% 
        mutate(Porcentaje = Matriculados/sum(Matriculados)) %>% 
        arrange(desc(Matriculados))%>% 
        
        #Gráfico
        ggplot(aes(x = 10*(1+0.6)/2, y = Matriculados, fill = reorder(sexo,Matriculados)))+
        geom_col(width = (1-0.6)*10,color="white",lwd=2)+ #,
        coord_polar("y", start = 0) +
        xlim(0, 10)+
        geom_shadowtext(aes(x=10,label = paste0(round(Porcentaje*100), "%")),
                        color="white", fontface="bold", size=5,
                        position = position_stack(vjust = 0.5), check_overlap = T)+
        labs(x="", y="",
             # title = "Resumen de las medidas por imponer, por estado",
             # subtitle = paste0("(Del ",format(Inicio, format = "%d/%m/%Y")," al ",format(Fin, format = "%d/%m/%Y"),")"),
             caption = "")+
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(face="bold",hjust = 0.5, size = 12),
              plot.subtitle = element_text(hjust = 0.5, size = 11),
              plot.caption = element_text(hjust = 0,size=10)) +
        scale_fill_manual(values = PALETA.PRINCIPAL)+
        ggeasy::easy_add_legend_title("")+
        ggeasy::easy_legend_at("bottom")+
        ggeasy::easy_plot_legend_size(size=10)
    
    TABLA_AFA_PIE
    
})



# Gráfico barras
output$GRAF_AFA_4 <- renderPlot({

    
    TABLA_AFA_BARRAS = BD_AFA %>%
        subset(select = c(filtroArea,
                          entidadCapacitada,
                          periodo)) %>%
        
        subset(periodo %in% input$PERIODO_1) %>% 
        
        mutate(entidadCapacitada = if_else(entidadCapacitada=="Entidad Capacitada", 1,0, missing = 0),
               filtroArea = ifelse(filtroArea == "Otras áreas del OEFA", "Otras áreas", filtroArea)) %>%
        group_by(filtroArea) %>%
        summarise(entidadCapacitada = sum(entidadCapacitada)) %>%
        mutate(Porcentaje = round(100*entidadCapacitada/sum(entidadCapacitada),1)) %>% 
        arrange(desc(entidadCapacitada)) %>% 
        
        #Gráfico
        ggplot(aes(x = reorder(filtroArea,desc(entidadCapacitada)), y = entidadCapacitada)) + 
        geom_bar(stat = "identity", fill = PALETA.PRINCIPAL[1]) +
        geom_shadowtext(aes(x=filtroArea,label = entidadCapacitada),
                        color="white", fontface="bold", size=5,
                        nudge_y = 10, check_overlap = T)+
        theme_minimal()+
        labs(x="Áreas", y="Cantidad de capacitados",
             title = "", 
             caption = "")+
        theme(legend.position = "bottom",
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
        scale_y_continuous(expand = expand_scale(mult = c(0, .15)))
    
    
    TABLA_AFA_BARRAS
    
 

})


####################################################











### ############################################# ###
###     III) BD_AFA: TRABAJAR DATA Y GRAFICAR     ####
### ############################################# ###


# ------------- -
# GRAFICOS EFA
# ------------- -

# Generar los resumenes
output$GRAF_EFA_1 <- renderFormattable({
    
    
    TABLA_EFA_RESUM = BD_EFA %>%
        subset(select = c(entidadConRegion,
                          Entrada,
                          Salida,
                          periodo,
                          ambitoEntidadMatricula,
                          NombreFinalGrupo)) %>%
        
        subset(periodo %in% input$PERIODO_2 &
                   ambitoEntidadMatricula %in% input$AMBITO_2 &
               NombreFinalGrupo %in% input$NOMBRE_2
               ) %>%        
        
        group_by(entidadConRegion) %>%
        summarise(Matriculados = n(),
                  Entrada = as.numeric(round(mean(Entrada, na.rm = TRUE),1)),
                  Salida = round(mean(Salida, na.rm = TRUE),1)) %>%
        mutate(Mejoria = round(100*(Salida/Entrada-1),1)) %>%
        arrange(desc(Matriculados)) %>% 
        
        summarise(Matriculados = sum(Matriculados),
                  Entrada = round(mean(Entrada, na.rm = TRUE),1),
                  Salida = round(mean(Salida, na.rm = TRUE),1)) %>% 
    
        
        formattable(align=c("c","c","c"),
                    list(`Indicator Name` = formatter("span",
                                                      style = ~ style(color = "grey",
                                                                      font.weight = "bold")),
                         "Matriculados" = color_text("white", PALETA.PRINCIPAL[1]),
                         "Entrada" = color_text("white", PALETA.PRINCIPAL[3]),
                         "Salida" = color_text("white", PALETA.PRINCIPAL[4])
                    )
        )

    TABLA_EFA_RESUM

})



# Generar tabla general
output$GRAF_EFA_2 <- renderFormattable({

    
    TABLA_EFA = BD_EFA %>%
        subset(select = c(entidadConRegion,
                          Entrada,
                          Salida,
                          periodo,
                          ambitoEntidadMatricula,
                          NombreFinalGrupo)) %>%
        
        subset(periodo %in% input$PERIODO_2 &
               ambitoEntidadMatricula %in% input$AMBITO_2 &
               NombreFinalGrupo %in% input$NOMBRE_2
               ) %>%        
        
        group_by(entidadConRegion) %>%
        summarise(Matriculados = n(),
                  Entrada = as.numeric(round(mean(Entrada, na.rm = TRUE),1)),
                  Salida = round(mean(Salida, na.rm = TRUE),1)) %>%
        mutate(Mejoria = round(100*(Salida/Entrada-1),1)) %>%
        arrange(desc(Matriculados)) %>%
        
        
        
        mutate(Entrada = ifelse(is.nan(Entrada) == T, NA, Entrada),
               Salida = ifelse(is.nan(Salida) == T, NA, Salida),
               Mejoria = ifelse(is.nan(Mejoria) == T, NA, Mejoria),
               Mejoria = ifelse(is.infinite(Mejoria) == T, NA, Mejoria)) %>%
        rename("Evaluación de entrada" = "Entrada",
               "Evaluación de salida" = "Salida",
               "Mejoría (%)" = "Mejoria") %>%

        formattable(align=c("l","l","l","l","c"),
                    list(`Indicator Name` = formatter("span",
                                                      style = ~ style(color = "grey",
                                                                      font.weight = "bold")),
                         Matriculados = color_bar(PALETA.PRINCIPAL[4]),
                         `Evaluación de entrada` = color_bar(na.rm = TRUE, PALETA.PRINCIPAL[5]),
                         `Evaluación de salida` = color_bar(na.rm = TRUE, PALETA.SECUNDARIA[2]),
                         `Mejoría (%)` = color_bar(na.rm = TRUE, PALETA.SECUNDARIA[4])
                    )
        )
    TABLA_EFA

})




# Grafico donut
output$GRAF_EFA_3 <- renderPlot({

    
    GRAF_EFA_PIE = BD_EFA %>%
        subset(select = c(sexo,
                          periodo,
                          ambitoEntidadMatricula,
                          NombreFinalGrupo)) %>%
        
        subset(periodo %in% input$PERIODO_2 &
               ambitoEntidadMatricula %in% input$AMBITO_2 &
               NombreFinalGrupo %in% input$NOMBRE_2
               ) %>%
        
        group_by(sexo) %>%
        summarise(Matriculados = n()) %>%
        mutate(Porcentaje = round(100*Matriculados/sum(Matriculados),1)) %>%
        arrange(desc(Matriculados)) %>% 
    
        ggplot(aes(x = 10*(1+0.6)/2, y = Matriculados, fill = reorder(sexo,Matriculados)))+
        geom_col(width = (1-0.6)*10,color="white",lwd=2)+ #,
        coord_polar("y", start = 0) +
        xlim(0, 10)+
        geom_shadowtext(aes(x=10,label = paste0(Porcentaje, "%")),
                        color="white", fontface="bold", size=5,
                        position = position_stack(vjust = 0.5), check_overlap = T)+
        labs(x="", y="",
             # title = "Resumen de las medidas por imponer, por estado",
             # subtitle = paste0("(Del ",format(Inicio, format = "%d/%m/%Y")," al ",format(Fin, format = "%d/%m/%Y"),")"),
             caption = "")+
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(face="bold",hjust = 0.5, size = 12),
              plot.subtitle = element_text(hjust = 0.5, size = 11),
              plot.caption = element_text(hjust = 0,size=10)) +
        scale_fill_manual(values = PALETA.PRINCIPAL)+
        ggeasy::easy_add_legend_title("")+
        ggeasy::easy_legend_at("bottom")+
        ggeasy::easy_plot_legend_size(size=10)

    GRAF_EFA_PIE

})




# Gráfico mapa
output$GRAF_EFA_4 <- renderPlot({
    
    TABLA_EFA_MAPA = BD_EFA %>%
        subset(select = c(dptoEntidadMatricula,
                          periodo,
                          ambitoEntidadMatricula,
                          NombreFinalGrupo)) %>%
        
        # subset(periodo %in% input$PERIODO_2 &
        #        ambitoEntidadMatricula %in% input$AMBITO_2 &
        #        NombreFinalGrupo %in% input$NOMBRE_2
        #        ) %>%
        
        group_by(dptoEntidadMatricula) %>%
        summarise(Cantidad = n()) %>% 
        mutate(PAIS="PERU")
    
    
    
    GRAF_MAPA = merge(BD_MAPA, TABLA_EFA_MAPA,
                      by.x = c("NOMBDEP"),
                      by.y = c("dptoEntidadMatricula"),
                      all.x = T) %>% 
    
    ggplot() +
        
        geom_sf(
                color = "black", size = 1.25,
                data = . %>% group_by(PAIS) %>% summarise()) +
        
        geom_sf( aes(fill = Cantidad),
                 color = "white", size = 0.25) +
        
        theme_void()+
        coord_sf(crs = "+proj=robin")+
        ggeasy::easy_move_legend(to="right")+
        ggeasy::easy_add_legend_title("Matriculados")+
        scale_fill_continuous(high = PALETA.PRINCIPAL[1], low = PALETA.PRINCIPAL[3], na.value = "white")


    GRAF_MAPA

})

#####################################################






}

