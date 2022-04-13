#1-Charles Syndie Fritznie
#2-Fleuridor Christie Yelva
#3-Content Stella Maria Shovana
#4-Gelin Caroline


# Chargement des libraries 

library(tidyverse)
library(readxl)


# Ingestion de données 

## Location the Data Source 
xls_data <- "data/WB_HT_DATA.xlsx"

## Lecture du fichier et coup d'oeil
rawData <- read_excel(xls_data, sheet = "Data")
head(rawData)

# Nettoyage de données

## Recolter les données necessaires 

entete <- rawData[3,]
entete2 <- as.vector(entete [5:length(entete)])
entete2 <- gsub(" ", "", entete2)

## Enlever les lignes et les colonnes non nécessaires
stagingData <- rawData[-(1:4), 5:length(entete)]

## Ajouter une nouvelle entête à la table
colnames(stagingData) <- entete2
head(stagingData)

##Convertir en un dataframe
brh_data_df <- as.data.frame(stagingData)

## Convertir en numérique et remplacer les NA par 0
brh_data_df[, 3:length(entete2)] <- sapply(brh_data_df[, (3:length(entete2))], as.numeric)
brh_data_df[,3:length(entete2)][is.na(brh_data_df[,3:length(entete2)])] <- 0
head(brh_data_df)

# Exploration de données


## Conversion de wide a long. Methode nous permettant d'allonger le dataframe 
brh_data_df_long <-gather(brh_data_df,year,value,'1960':'2020')
brh_data_df_long[, "year"] <- sapply(brh_data_df_long[, "year"], as.numeric)

head(brh_data_df_long)

glimpse(brh_data_df_long)

## Considerer uniquement les valeurs supérieurs à zero pour tracer le graphe 2.4
brh_data_df_long %>% 
        filter(value > 0) %>%
        group_by(IndicatorName) %>%
        summarise (max = max(value),
                min = min(value),
                mean= mean(value),
                sd = sd(value))


brh_data_df_long_liquidities <- brh_data_df_long %>% 
                                       filter(IndicatorCode == "FD.RES.LIQU.AS.ZS" & value > 0)



# Graphe 2.4
colors <- c("Liquidite" = "green", "Tendance" = "red")

ggplot(brh_data_df_long_liquidities, aes(year)) +
geom_line(aes(y = value, color = "Liquidite")) +
geom_smooth( aes(year,value, color = "Tendance"), se = FALSE) + 
geom_vline(xintercept = 2008, linetype = "dashed", color = "red") +
geom_text(aes(x=2008, label="Crise Financière", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
geom_vline(xintercept = 2010, linetype = "dashed", color = "red") +
geom_text(aes(x=2010, label="Tremblement de Terre", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
geom_vline(xintercept = 2015, linetype = "dashed", color = "red") +
geom_text(aes(x=2015, label="BIC Operationnel", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
geom_vline(xintercept = 2016, linetype = "dashed", color = "red") +
geom_text(aes(x=2016, label="Cyclone Matthieu", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
geom_vline(xintercept = 2019, linetype = "dashed", color = "red") +
geom_text(aes(x=2019, label="Covid-19", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
labs(title=paste("Ratio des réserves liquides des banques sur leurs actifs (%)"), color = "Legend") + 
scale_color_manual(values = colors)




# Graphe 3.1

brh_data_df_long_transferts <- brh_data_df_long %>% 
        filter(IndicatorCode == "NY.TRF.NCTR.CD" & value > 0 & year >=2000)


colors <- c("Liquidite" = "green")

ggplot(brh_data_df_long_transferts, aes(year)) +
        geom_line(aes(y = value, color = "Transferts")) +
        scale_y_log10() + 
        geom_vline(xintercept = 2008, linetype = "dashed", color = "red") +
        geom_text(aes(x=2008, label="Crise Financière", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
        geom_vline(xintercept = 2010, linetype = "dashed", color = "red") +
        geom_text(aes(x=2010, label="Tremblement de Terre", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
        geom_vline(xintercept = 2016, linetype = "dashed", color = "red") +
        geom_text(aes(x=2016, label="Cyclone Matthieu", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
        geom_vline(xintercept = 2019, linetype = "dashed", color = "red") +
        geom_text(aes(x=2019, label="Covid-19", y=85), colour="black", angle=90,vjust = -1.2, text=element_text(size=11))+
        labs(title=paste("Evolution des transferts annuelles (en % du PIB)"), color = "Legend") + 
        scale_color_manual(values = colors)
        




