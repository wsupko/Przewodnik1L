library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(stringr)
library(dplyr)
library(stringr)
library(xlsx)

options(java.parameters = "- Xmx1024m")

#### Custom functions ####

source('CardsHeatmap.R', encoding = 'UTF-8')

wsTheme <- theme(axis.title = element_text(size = 22, margin = margin(r = 20)), 
                 legend.position = 'bottom', legend.text = element_text(size = 22),
                 legend.title = element_text(size = 22),
                 axis.text = element_text(size = 22), text = element_text(size = 22), 
                 plot.title = element_text(size = 26, margin = margin(b = 20)),
                 panel.grid = element_blank(),
                 plot.subtitle = element_text(colour = '#636363', vjust = -2.5),
                 axis.text.y  = element_text(margin = margin(r = 20)))

#### Database connection #### 

#### 1. Data Input ####

Imp.Games <- data.table(xlsx::read.xlsx('./data/in/1liga_1718_po_1.kolejce_.xlsx', sheetName = 'Mecze',
                                        colIndex = 1:15, rowIndex = 1:171, stringsAsFactors = F, encoding = 'UTF-8'))

Imp.Players <- data.table(xlsx::read.xlsx('./data/in/1liga_1718_po_1.kolejce_.xlsx', sheetName = 'Kadra',
                                          colIndex = 17:23, rowIndex = 1:1379, stringsAsFactors = F, encoding = 'UTF-8'))

Imp.Teams <- data.table(xlsx::read.xlsx('./data/in/1liga_1718_po_1.kolejce_.xlsx', sheetName = 'Kadra',
                                        colIndex = 30:31, rowIndex = 1:19, stringsAsFactors = F, encoding = 'UTF-8'))

Imp.Lineup <- fread('./data/in/1L_Lineup.txt')
Imp.Goals <- fread('./data/in/1L_Goals.txt')
Imp.Cards <- fread('./data/in/1L_Kartki.txt')


#### 1.1 Working data ####

Proc.GamesCards <- merge(Imp.Games[, ctGamesRef := .N, Sedzia], Imp.Cards[, .(ctCards = .N), .(ID, Rodzaj)], by = 'ID', all.x = T)

Proc.GamesGoals <- rbind(Imp.Games[, .(ID, Team = Gosp, Typ = 'Scored', Count = BramkiGosp)],
                    Imp.Games[, .(ID, Team = Gosc, Typ = 'Scored', Count = BramkiGosc)],
                    Imp.Games[, .(ID, Team = Gosp, Typ = 'Conceded', Count = BramkiGosc)],
                    Imp.Games[, .(ID, Team = Gosc, Typ = 'Conceded', Count = BramkiGosp)])

Proc.Games <-  rbind(Imp.Games[, .(ID, Team = Gosp)],
                     Imp.Games[, .(ID, Team = Gosc)])
Imp.Players[str_detect(Nationality, 'Polska'), Nat := 'Polska']
Imp.Players[is.na(Nat), Nat := 'Zagranica']

Imp.Players[year(BirthDate) >= 1997 & Nat == 'Polska', U21 := 'Młodzież']
Imp.Players[is.na(U21), U21 := 'Pozostali']

Proc.Lineup <- merge(Imp.Lineup, Imp.Players, by = 'idPlayer', all.x = T)

Imp.Goals[nchar(Rodzaj) == 0, Rodzaj := 'gol']


Proc.Goals <- merge(Imp.Goals,  Imp.Games[, .(ID, Gosp, Gosc)], by = 'ID', all.x = T)
Proc.Goals[Rodzaj == 'samobój', concTeam := Klub]
Proc.Goals[is.na(concTeam), concTeam := ifelse(Klub == Gosp, Gosc, Gosp)]

#### 2. Data Wrangling ####

#### Team: Goals ####


GPG <- Imp.Games[, sum(BramkiGosp) + sum(BramkiGosc)] / Imp.Games[,.N] ; names(GPG) <- 'Srednia goli'

topGoalsGames <- Imp.Games[, .(ID, Gosp, Gosc, BramkiGosp, BramkiGosc, 
                               Bramki = BramkiGosp + BramkiGosc)][order(-Bramki)][Bramki >= 5]
Team.Goals <- Proc.GamesGoals[, .(totalGoals = sum(Count), GoalsPerGame = sum(Count) / .N), .(Typ, Team)] %>%
    melt(id.vars = c('Typ', 'Team')) %>%
    mutate(Variable = paste0(variable, ': ', Typ)) %>%
    dcast(Team ~ Variable)


#### Team: Cards ####

CPG <- Imp.Cards[, .(CardsPerGame = .N / Imp.Games[, .N]), Rodzaj]

topCardsGames <- Proc.GamesCards[, .(ttlCards = sum(ctCards, na.rm  = T)), .(ID, Gosp, Gosc)][ttlCards >= 10]

Team.Cards <-  rbind(Proc.GamesCards[, .(ID, Team = Gosp, Rodzaj, ctCards)],
                     Proc.GamesCards[, .(ID, Team = Gosc, Rodzaj, ctCards)])
Team.Cards <- merge(Team.Cards, Proc.Games[, .(ctGames = .N), Team], by = 'Team', all.x = T)

Team.Cards <- Team.Cards[, .(totalCards = sum(ctCards), CardsPerGame = sum(ctCards) / mean(ctGames)), .(Rodzaj, Team)] %>%
    melt(id.vars = c('Rodzaj', 'Team'), measure.vars = c('totalCards', 'CardsPerGame')) %>%
    mutate(Variable = paste0(Rodzaj, ': ', variable)) %>%
    dcast(Team ~ Variable)

Proc.Cards <- Imp.Cards[, .(ctCards = .N), .(idTeam, Rodzaj, GamePart, Minutes)]
Proc.Cards <- merge(Proc.Cards, Imp.Teams, by = 'idTeam', all.x = T)




####  Referees #### 


Out.Referees <- Proc.GamesCards[,.(ctCards = sum(ctCards, na.rm = T), CardsPerGame = sum(ctCards, na.rm = T) / ctGamesRef ), 
                                .(Sedzia, Rodzaj, ctGamesRef)][order(Rodzaj, -CardsPerGame)]
Out.Referees <- Out.Referees %>% melt(id.vars = c('Sedzia', 'Rodzaj', 'ctGamesRef')) %>%
    mutate(Variable = paste0(Rodzaj, ': ', variable)) %>%
    dcast(Sedzia + ctGamesRef ~ Variable) 


####  Cross #### 

onpitch.Players.Count <- rbind(Proc.Lineup[Minutes > 0, .N, .(Klub, idPlayer)][,.(liczbaZawodnikow = .N, Klub = 'Nice 1 Liga')],
                               Proc.Lineup[Minutes > 0, .N, .(Klub, idPlayer)][,.(liczbaZawodnikow = .N), Klub])


Proc.Age.Team.Lineup <- merge(Proc.Lineup[, .(totalMinutes = sum(Minutes)), .(Rocznik = year(BirthDate), Klub)], 
      Proc.Lineup[, .(clubMinutes = sum(Minutes)), .(Klub)], by = 'Klub', all.x = T)

Proc.Age.Team.Lineup[, ageShare := totalMinutes / clubMinutes]
setorder(Proc.Age.Team.Lineup, Klub, Rocznik)

Proc.Age.Lineup <- Proc.Lineup[, .(totalAgeMinutes = sum(Minutes)), 
                               .(Rocznik = (year(BirthDate)))][, ':='(totalMinutes = sum(totalAgeMinutes,
                                                                                         na.rm = T))]
Proc.Age.Lineup[, ageShare := totalAgeMinutes / totalMinutes]

Proc.Age.Team.Lineup <- rbind(Proc.Age.Team.Lineup, 
                              Proc.Age.Lineup[, .(Klub = 'Nice 1 Liga', 
                                                  Rocznik, 
                                                  totalMinutes = totalAgeMinutes, 
                                                  clubMinutes = totalMinutes, ageShare)])
setorder(Proc.Age.Lineup, Rocznik)

Proc.Nationality.Team.Lineup <- merge(Proc.Lineup[, .(totalNatMinutes = sum(Minutes)), .(Nat, Klub)], 
                              Proc.Lineup[, .(clubMinutes = sum(Minutes)), .(Klub)], by = 'Klub', all.x = T)
Proc.Nationality.Team.Lineup[, natShare := totalNatMinutes / clubMinutes]

Proc.Nationality.Team.Lineup <- rbind(Proc.Nationality.Team.Lineup, 
    Proc.Nationality.Team.Lineup[Nat == 'Polska', 
                                 .(Klub = 'Nice 1 Liga', Nat = 'Polska', 
                                   totalNatMinutes = sum(totalNatMinutes), 
                                   clubMinutes = Proc.Lineup[, sum(Minutes)],
                                   natShare = sum(totalNatMinutes) / Proc.Lineup[, sum(Minutes)])],
    Proc.Nationality.Team.Lineup[Nat == 'Zagranica', 
                                 .(Klub = 'Nice 1 Liga', Nat = 'Zagranica', 
                                   totalNatMinutes = sum(totalNatMinutes), 
                                   clubMinutes = Proc.Lineup[, sum(Minutes)],
                                   natShare = sum(totalNatMinutes) / Proc.Lineup[, sum(Minutes)])]
    )

ctNatTeamLineup <- Proc.Lineup[Minutes > 0, 
            .(totalNatMinutes = sum(Minutes)), 
            .(FullName, Klub, Nat)][, .(liczbaZawodnikow = .N), 
                                    .(Klub, Nat)][order(-liczbaZawodnikow)] %>% 
    dcast(Klub ~ Nat, value.var = 'liczbaZawodnikow')  
    
Proc.Nationality.Team.Lineup[ , Nat := ordered(Nat, levels =  c( 'Zagranica', 'Polska'))]

colNat <- c('#bdbdbd', '#fe9929'); names(colNat) <- c( 'Zagranica', 'Polska')

temp <- Proc.Nationality.Team.Lineup[Nat == 'Polska' & Klub != 'Nice 1 Liga'][order(-natShare)]; temp[, Pos := 1:.N]
Proc.Nationality.Team.Lineup <- merge(Proc.Nationality.Team.Lineup, temp[, .(Klub, Pos)], by = 'Klub', all.x = T); Proc.U21.Team.Lineup[Klub == 'Nice 1 Liga', Pos := 0]




Proc.U21.Team.Lineup <- merge(Proc.Lineup[, .(totalU21Minutes = sum(Minutes)), .(U21, Klub)], 
                                      Proc.Lineup[, .(clubMinutes = sum(Minutes)), .(Klub)], by = 'Klub', all.x = T)
Proc.U21.Team.Lineup[, u21Share := totalU21Minutes / clubMinutes]

Proc.U21.Team.Lineup <- rbind(Proc.U21.Team.Lineup, 
                              Proc.U21.Team.Lineup[U21 == 'Młodzież', 
                                                           .(Klub = 'Nice 1 Liga', U21 = 'Młodzież', 
                                                             totalU21Minutes = sum(totalU21Minutes), 
                                                             clubMinutes = Proc.Lineup[, sum(Minutes)],
                                                             u21Share = sum(totalU21Minutes) / Proc.Lineup[, sum(Minutes)])],
                              Proc.U21.Team.Lineup[U21 == 'Pozostali', 
                                                                   .(Klub = 'Nice 1 Liga', U21 = 'Pozostali', 
                                                                     totalU21Minutes = sum(totalU21Minutes), 
                                                                     clubMinutes = Proc.Lineup[, sum(Minutes)],
                                                                     u21Share = sum(totalU21Minutes) / Proc.Lineup[, sum(Minutes)])])

ctU21TeamLineup <- Proc.Lineup[Minutes > 0, 
                               .(totalNatMinutes = sum(Minutes)), 
                               .(FullName, Klub, U21)][, .(liczbaZawodnikow = .N), 
                                                       .(Klub, U21)][order(-liczbaZawodnikow)] %>% 
    dcast(Klub ~ U21, value.var = 'liczbaZawodnikow')
Proc.U21.Team.Lineup[, U21 := ordered(U21, levels = c( 'Pozostali', 'Młodzież'))]

colU21 <- c('#bdbdbd', '#fe9929'); names(colU21) <- c( 'Pozostali', 'Młodzież')
temp <- Proc.U21.Team.Lineup[U21 == 'Młodzież' & Klub != 'Nice 1 Liga'][order(-u21Share)]; temp[, Pos := 1:.N]
Proc.U21.Team.Lineup <- merge(Proc.U21.Team.Lineup, temp[, .(Klub, Pos)], by = 'Klub', all.x = T); Proc.U21.Team.Lineup[Klub == 'Nice 1 Liga', Pos := 0]


#### Players ####

Players.Data <- Proc.Lineup[Minutes > 0,.(totalMin = sum(Minutes),
                                               liczbaWystepow = .N), .(Klub, FullName, idPlayer)]
Players.Data <- merge(Players.Data, Proc.Lineup[p11 == 'T', .(liczba11 = .N), idPlayer],
                           by = 'idPlayer', all.x = T)

Players.Data <- merge(Players.Data, 
                      Imp.Goals[, .(liczbaBramek = .N), .(Rodzaj, idPlayer)] %>% 
                          dcast(idPlayer ~ Rodzaj, value.var = 'liczbaBramek'),
                      by = 'idPlayer', all.x = T)

Players.Data <- merge(Players.Data,
                      Imp.Cards[, .(liczbaKartek = .N), .(idPlayer, Rodzaj)] %>% 
                          dcast(idPlayer ~ Rodzaj, value.var = 'liczbaKartek'),
                      by = 'idPlayer', all.x = T)

Players.Data <- merge(Players.Data, Imp.Players[, .(idPlayer, PositionName, BirthDate)], 
                      by = 'idPlayer', all.x = T)

Players.Data[is.na(liczba11), liczba11 := 0]; Players.Data[is.na(gol), gol := 0]
Players.Data[is.na(karny), karny := 0]; Players.Data[is.na(samobój), samobój := 0]
Players.Data[is.na(cz), cz := 0]; Players.Data[is.na(ż), ż := 0]

Players.Data[, goal90 := ((gol + karny) / totalMin) * 90]

t <- merge(Imp.Lineup, 
           Imp.Players[, .(idPlayer, PositionName)], 
           by = 'idPlayer', all.x = T)[PositionName == 'Bramkarz' & Minutes  == 90]



tt <- merge(t, Proc.Goals[, .(concGoals = .N), .(ID, concTeam)], 
            by.x = c('ID', 'Klub'), by.y = c('ID', 'concTeam'), all.x = T)


Players.Data <- merge(Players.Data, tt[is.na(concGoals), .(liczbaCzysteKonta = .N), idPlayer],
                      by = 'idPlayer', all.x = T)                    
####  Goalkeepers #### 

Imp.Lineup[, idPlayer := as.numeric(idPlayer)]
tt <- merge(Imp.Lineup, Imp.Players[, .(idPlayer = pk_Player, FullName, PositionName)], by = 'idPlayer')
temp <- tt[PositionName == 'Bramkarz', .(ttlMin = sum(Minutes, na.rm = T)), .(FullName)][order(-ttlMin)]

Proc.Games <- rbind(Imp.Games[, .(ID, Klub = Gosp, Typ = 'Scored', Count = BramkiGosp)],
      Imp.Games[, .(ID, Klub = Gosc, Typ = 'Scored', Count = BramkiGosc)],
      Imp.Games[, .(ID, Klub = Gosp, Typ = 'Conceded', Count = BramkiGosc)],
      Imp.Games[, .(ID, Klub = Gosc, Typ = 'Conceded', Count = BramkiGosp)])

merge(tt, Proc.Games[Typ == 'Conceded'], by = c('ID', 'Klub'))[PositionName == 'Bramkarz' & Minutes > 0, 
                                                               .(ttlMin = sum(Minutes, na.rm = T),
                                                                 ttlGames = .N,
                                                                 cp90 = (sum(Count)/sum(Minutes, na.rm = T))*90 ), FullName] [order(-ttlMin)]
Imp.Lineup[, unique(Imię.i.nazwisko)]      

#### Vizualization ####

Plot.GPG <- ggplot(Imp.Games[, .(gpg = (BramkiGosp + BramkiGosc))]) + 
    geom_histogram(aes(gpg), binwidth =  1) + 
    scale_x_continuous(breaks = 0:7)+
    labs(y = 'Liczba meczów', x = 'Liczba bramek w meczu')+
    theme_minimal() + wsTheme



plotAgeTeamLineup <- function(club){
    
    colVector <- c('#bdbdbd', '#fe9929')
    names(colVector) <- c( 'Nice 1 Liga', club)
    
    ggplot(Proc.Age.Team.Lineup[Klub == club & totalMinutes > 0]) + 
        geom_line(data = Proc.Age.Lineup, aes(x = Rocznik, y = ageShare, col = 'Nice 1 Liga'), size = 2)+
        geom_point(data = Proc.Age.Lineup, aes(x = Rocznik, y = ageShare, col = 'Nice 1 Liga'), size = 6)+
        geom_line(aes(x = Rocznik, y = ageShare, col = club), stat = 'identity', size = 4)+  
        geom_point(aes(x = Rocznik, y = ageShare, col = club), size = 10)+      
        theme_minimal() + labs(y = 'Udział w minutach na boisku [%]') +
        coord_cartesian(y = c(0, .25))+
        scale_x_continuous(breaks = 1979:2000)+
        scale_color_manual(values = colVector)+
        ggtitle(club)+
        scale_y_continuous(breaks = seq(0, .25, .05), labels = percent) +
        wsTheme + 
        theme(legend.title =  element_blank())
}    



plotNatTeamLineup <- ggplot(Proc.Nationality.Team.Lineup, aes(x = reorder(Klub, -Pos), y = natShare, fill = Nat)) + 
    geom_bar( stat = 'identity') +
    geom_text(aes(label = percent(natShare)), color = 'white', position = position_stack(vjust = 0.5), size = 8)+
        coord_flip(ylim = c(0, 1)) +
    scale_y_continuous(labels = percent)+
    scale_fill_manual(values = colNat)+
    theme_minimal() + 
    labs(y = 'Udział w minutach na boisku [%]') +
    wsTheme +
    theme(panel.grid.minor = element_blank(), legend.position =  'bottom', 
          legend.title =  element_blank(), axis.title.y = element_blank())

plotU21TeamLineup <-  ggplot(Proc.U21.Team.Lineup, aes(x = reorder(Klub, -Pos), y = u21Share, fill = U21)) + 
    geom_bar( stat = 'identity') +
    geom_text(aes(label = percent(u21Share)), color = 'white', position = position_stack(vjust = 0.5), size = 8)+
    coord_flip(ylim = c(0, 1)) +
    scale_y_continuous(labels = percent)+
    scale_fill_manual(values = colU21)+
    theme_minimal() + 
    labs(y = 'Udział w minutach na boisku [%]') +
    wsTheme +
    theme(panel.grid.minor = element_blank(), legend.position =  'bottom', 
          legend.title =  element_blank(), axis.title.y = element_blank())

#### Viz: Saving to files ####

png(paste('.\\img\\GoalsPerGame.1Liga.png', sep =''),  width = 1920, height = 1080)
Plot.GPG
dev.off()

png(paste('.\\img\\Kartki.1Liga.png', sep =''),  width = 1920, height = 1080)
plotHM(Proc.Cards, 'ż', '#fe9929')
dev.off()


for(t in Imp.Teams[, Klub]){
    
    png(paste('.\\img\\Age_',t,'.png', sep =''),  width = 1920, height = 1080)
    plot(plotAgeTeamLineup(t))
    dev.off()
}

png(paste('.\\img\\Narodowosc_1Liga.png', sep =''),  width = 1920, height = 1080)
plot(plotNatTeamLineup)
dev.off()

png(paste('.\\img\\Mlodziez_1Liga.png', sep =''),  width = 1920, height = 1080)
plot(plotU21TeamLineup)
dev.off()

#### Saving to Excel ####

wb <- createWorkbook(type="xlsx")
sheetL <- createSheet(wb, sheetName = "liga")
sheetR <- createSheet(wb, sheetName = "sedziowie")
sheetP <- createSheet(wb, sheetName = "przekroj")
sheetPlayers <- createSheet(wb, sheetName = "pilkarze")

xlsx::addDataFrame(GPG, sheetL, col.names = F, row.names = T
                   , startRow = 1, startColumn = 1)

xlsx::addDataFrame(topGoalsGames, sheetL, col.names = T, row.names = F
                   , startRow = 5, startColumn = 1)

xlsx::addDataFrame(CPG, sheetL, col.names = T, row.names = F
                   , startRow = 1, startColumn = 8)

xlsx::addDataFrame(topCardsGames, sheetL, col.names = T, row.names = F
                   , startRow = 5, startColumn = 8)

xlsx::addDataFrame(Out.Referees, sheetR, col.names = T, row.names = F
                   , startRow = 1, startColumn = 1)


xlsx::addDataFrame(onpitch.Players.Count, sheetL, col.names = T, row.names = F
                   , startRow = 1, startColumn = 13)

xlsx::addDataFrame(Proc.Age.Team.Lineup, sheetP, col.names = T, row.names = F
                   , startRow = 1, startColumn = 1)

xlsx::addDataFrame(Proc.Nationality.Team.Lineup[, .(Klub, Nat, totalNatMinutes, clubMinutes, natShare)], 
                   sheetP, col.names = T, row.names = F
                   , startRow = 1, startColumn = 7)

xlsx::addDataFrame(ctNatTeamLineup[, .(Klub, liczbaZawodnikowPolska = Polska, liczbaZawodnikowZagr = Zagranica)], 
                   sheetP, col.names = T, row.names = F
                   , startRow = 1, startColumn = 13)

xlsx::addDataFrame(Proc.U21.Team.Lineup[, .(Klub, U21, totalU21Minutes, clubMinutes, u21Share)], 
                   sheetP, col.names = T, row.names = F
                   , startRow = 1, startColumn = 17)

xlsx::addDataFrame(ctU21TeamLineup[, .(Klub, liczbaZawodnikowMlodzie = Młodzież, liczbaZawodnikowPozostali = Pozostali)], 
                   sheetP, col.names = T, row.names = F
                   , startRow = 1, startColumn = 23)

xlsx::addDataFrame(Team.Goals, 
                   sheetP, col.names = T, row.names = F
                   , startRow = 1, startColumn = 27)

xlsx::addDataFrame(Players.Data, 
                   sheetPlayers, col.names = T, row.names = F
                   , startRow = 1, startColumn = 1)

saveWorkbook(wb, './data/out/PK.1L.Output.xlsx')

