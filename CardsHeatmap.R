
library(data.table)
library(RPostgreSQL)
library(ggplot2)
library(scales)
library(gridExtra)
library(stringr)

labs <- paste('[', seq(1, 100, by = 5), '-', seq(5, 100, by = 5), ']', sep = '')
plotHM <- function(data, cardType, maxCol){

data[, cMinuta := cut(as.numeric(Minutes), breaks = seq(1,105, by = 5), 
                            right = F, labels = labs)]

data[GamePart == 1 & Minutes > 45, cMinuta := '45+']
data[Minutes > 90, cMinuta := '90+']
data[, cMinuta := ordered(cMinuta, levels = c(labs[1:9], '45+', labs[10:20], '90+'))]


t <- data[, .(ctCards = sum(ctCards)), .(idTeam, Rodzaj, cMinuta)]
tt <- merge(t, Imp.Teams, by = 'idTeam', all.x = T)


ggplot(data[Rodzaj == cardType]) +  
    geom_tile(aes(x = cMinuta, y = Klub, fill = ctCards)) +
    geom_vline(aes(xintercept = 10.5), size = 1.5)+
    scale_fill_gradient(low = "#f2f0f7",high = maxCol, limits = c(0, 5)) + 
    labs(title = paste("W którym momencie meczu drużyny były karane żółtymi kartkami?", 
                       ' | Nice 1 Liga | ', "Sezon 2017/18 | Kolejki 1-20", sep =''), 
         x = 'Minuta meczu', fill = 'Suma liczby żółtych kartek')+
    theme_minimal() + 
    theme(axis.title.y = element_blank(), 
          legend.position = 'bottom', legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          axis.text = element_text(size = 22), text = element_text(size = 22), 
          plot.title = element_text(size = 26, margin = margin(b = 20)),
          panel.grid = element_blank(),
          plot.subtitle = element_text(colour = '#636363', vjust = -2.5),
          axis.text.y  = element_text(margin = margin(r = 20))) 


}


# png(paste('.\\img\\Kartki.ESA.png', sep =''),  width = 1920, height = 1080)
# Plot.ESA
# dev.off()
# 
# png(paste('.\\img\\Kartki.1Liga.png', sep =''),  width = 1920, height = 1080)
# Plot.1Liga
# dev.off()
