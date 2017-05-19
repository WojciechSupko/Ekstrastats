rm(list=ls())
########################################
## 0. SETTINGS
########################################
start <- Sys.time()  

# Biblioteki do zainstalowania

# install.packages(c('data.table', 'ggplot2'))

# Libraries

library(data.table)
library(ggplot2)
library(gridExtra)
library(ggforce)
# Database connection

# Working directory (change it accordingly)

# setwd('F:\\EkstraStats\\Ekstraklasa\\Prognoza')
setwd('D:\\prv\\Ekstraklasa\\Forecast')
# setwd('C:\\Users\\User\\Dropbox\\sezon 2016-17\\Prognoza')

# Colors palette


# Sourcing



# Custom functions

# cross-join 

CJ.table <- function(X,Y){
  setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]}

########################################
## 1. DATA IMPORT
########################################

# Dane odnosnie strzalow z ExtraStats

Imp.Shots <- fread('./in/Imp.Shots.txt', stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = '')

# Terminarz

Imp.Terminarz <- fread('./in/Imp.Terminarz.txt', stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = '')

# Tabela

Imp.Table <- fread('./in/Imp.Table.txt', stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = '')

########################################
## 2. DATA CLEAN-UP
########################################

# Trzeba jakos sobie radzic z polskimi znakami...

setnames(Imp.Shots, iconv(names(Imp.Shots), 'UTF-8', 'UTF-8'))

Imp.Shots[, ':='(ForPlace = ifelse(StrzałDla == Gospodarze, 'H', 'A'),
                 AgainstPlace = ifelse(StrzałPrzeciw == Gospodarze, 'H', 'A'))]

Proc.ShotsFor <- Imp.Shots[, .(ctShotsF = .N, sumXGF = sum(expG)), .(StrzałDla, Place = ForPlace)]
Proc.ShotsAgainst <- Imp.Shots[, .(ctShotsA = .N, sumXGA = sum(expG)), .(StrzałPrzeciw, Place = AgainstPlace)]

Imp.Schedule <- rbind(Imp.Terminarz[!is.na(goalHome), .(id, Team = HomeTeam, Place = 'H')], 
                      Imp.Terminarz[!is.na(goalAway), .(id, Team = AwayTeam, Place = 'A')])

Proc.Schedule <- Imp.Schedule[, .(ctGames = .N), .(Team, Place)]

Proc.Schots <- merge(Proc.Schedule, Proc.ShotsFor, by.x = c('Team', 'Place'), by.y = c('StrzałDla', 'Place'), all.x = T)
Proc.Schots <- merge(Proc.Schots, Proc.ShotsAgainst, by.x = c('Team', 'Place'), by.y = c('StrzałPrzeciw', 'Place'), all.x = T)

Proc.Schots[, ':='(g_ctShotsF =  ctShotsF/ctGames,  
                     g_sumXGF = sumXGF/ctShotsF,
                     g_ctShotsA = ctShotsA/ctGames,
                     g_sumXGA = sumXGA/ctShotsA
                     )]

Proc.NextGames <- Imp.Terminarz[is.na(goalHome), .(id, HomeTeam, AwayTeam)]

Proc.All <- merge(Proc.NextGames, 
                  Proc.Schots[Place == 'H', .(Team, hg_ctShotsF = g_ctShotsF,  hg_sumXGF = g_sumXGF, 
                                              hg_ctShotsA = g_ctShotsA,  hg_sumXGA = g_sumXGA)], 
                  by.x = 'HomeTeam', by.y = 'Team')

Proc.All <- merge(Proc.All, 
                  Proc.Schots[Place == 'A', .(Team, ag_ctShotsF = g_ctShotsF,  ag_sumXGF = g_sumXGF, 
                                              ag_ctShotsA = g_ctShotsA,  ag_sumXGA = g_sumXGA)], 
                  by.x = 'AwayTeam', by.y = 'Team')


Proc.All <- CJ.table(Proc.All, data.table(ctShots = 0:40))

Proc.All[, ':='(hShot = ppois(ctShots, mean(c(hg_ctShotsF, ag_ctShotsA))),
                aShot = ppois(ctShots, mean(c(ag_ctShotsF, hg_ctShotsA))),
                hXG = (hg_sumXGF + ag_sumXGA)/2,
                aXG = (ag_sumXGF + hg_sumXGA)/2                
                )]

nSim <- 10000
Scenarios <- data.table(Scen = paste('S', 1:nSim, sep = ''))

Proc.All <- CJ.table(Proc.All, Scenarios)  

set.seed(12)

Proc.All[, ':='(hLosShot = runif(.N, 0, 1),
                aLosShot = runif(.N, 0, 1),
                hLosXG = runif(.N, 0, 1),
                aLosXG = runif(.N, 0, 1))
             ]
Proc.All[, ':='(hSht01 = ifelse(hShot < hLosShot,1,0),
                aSht01 = ifelse(aShot < aLosShot,1,0))]   
Proc.All[, ':='(hGoal01 = ifelse(hXG > hLosXG, hSht01, 0),
                aGoal01 = ifelse(aXG > aLosXG, aSht01, 0))]   

Proc.Res <- Proc.All[, .(hScore = sum(hGoal01), aScore = sum(aGoal01)), .(id, HomeTeam, AwayTeam, Scen)]

Proc.Res[, Res := ifelse(hScore > aScore, 'D', ifelse(hScore == aScore, 'R', 'W'))]

lProc.Res <- Proc.Res[, .(Prob = .N/nSim), .(id, HomeTeam, AwayTeam, Res)]

rm(Proc.All, Proc.Res)

wProc.Res <- dcast(lProc.Res, id + HomeTeam + AwayTeam ~Res, value.var = 'Prob')

wProc.Res <- CJ.table(wProc.Res, Scenarios)

set.seed(100)

wProc.Res[, losRes := runif(.N, 0, 1)]

wProc.Res[, PointsH := ifelse (losRes < D, 3,ifelse(losRes < D + R, 1, 0))]
wProc.Res[, PointsA := ifelse (PointsH == 3, 0,ifelse(PointsH == 1, 1, 3))]

Fin.All <- rbind(wProc.Res[, .(id, Team = HomeTeam, Scen, Points = PointsH)],
                 wProc.Res[, .(id, Team = AwayTeam, Scen, Points = PointsA)])

Fin.All <- merge(Fin.All, Imp.Table, by.x = 'Team', by.y = 'Team')

Fin.All[, Kol := round(id, -2)/100]

setorder(Fin.All, Team, Kol)

Fin.All[, cumPoints := cumsum(Points), .(Team, Scen)]

Fin.All[, TotalPoints := TotalPoints + cumPoints]

Fin.All[Scen %in% c('S3', 'S2', 'S1') & Kol == 30 & Team == 'Jagiellonia']

setorder(Fin.All, -TotalPoints)



# Fin.All[, Pos := factor(1:.N), .(Kol, Scen)]
Fin.All[, Pos := rank(-TotalPoints, ties.method = 'min'), .(Kol, Scen)]
tmp <- Fin.All[ , .N, .(Scen,Pos)][N>1]

Fin.All <- merge(Fin.All, tmp, by = c('Scen', 'Pos'), all.x = T)

Fin.All[N > 1, correction := sample(seq(0, N-1),N, rep = F),.(Scen, N)]

Fin.All[!is.na(correction),Pos := Pos + correction]


#### Dirty Fix ####
scenLeg <- Fin.All[Team == 'Legia' & Pos == 4, Scen]
scenLech <- Fin.All[Team == 'Lech' & Pos %in% c(2), Scen]
scenGor <- Fin.All[Team == 'Górnik' & Pos %in% c(13), Scen]

Fin.All[Scen %in% scenGor & Pos == 14, Pos:=13]
Fin.All[Scen %in% scenGor & Team == 'Górnik', Pos:=14]

Fin.All[Scen %in% scenLech & Pos == 3, Pos:=2]
Fin.All[Scen %in% scenLech & Team == 'Lech', Pos:=3]

Fin.All[Scen %in% scenLeg & Pos == 3, Pos:=4]
Fin.All[Scen %in% scenLeg & Team == 'Legia', Pos:=3]

Out <- Fin.All[Kol == 30, .(Prob = .N/10000), .(Team, Pos)]
Out <- merge(Out, Imp.Table, by = 'Team') 

Out[, Team := ordered(Team, levels = Imp.Table[16:1,Team])]


########################################
#### 3. VISUALIZATION ####
########################################

Plot.Forecast <- ggplot(Out, aes(x = Pos, y = Team)) + 
  geom_tile(aes(fill= Prob)) + 
  geom_text(aes(label = sprintf("%1.1f%%", 100*Prob)), size = 9) + 
  scale_fill_gradient(low = "#FFEF9C", high = "#FF7128") + 
  theme_minimal() + 
  scale_x_discrete(position = 'top')+
  ggtitle('Prawdopodobieństwo zajęcia pozycji w tabeli po rundzie zasadniczej | Sezon 2016/17') +
  theme(legend.position =  'none', panel.border = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 24),
        plot.title = element_text(size = 24, margin=margin(b = 20, unit = "pt"))
        ) 


png(paste('.\\img\\', Sys.Date(), '_PrognozaTabela.png', sep =''),  width = 1920, height = 1080)
Plot.Forecast
dev.off()

# write.csv(Out, paste('.\\out\\',Sys.Date(), '_Prognoza.Tabela.csv', sep =''), row.names = F, quote = T)
