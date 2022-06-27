if(country == 'Nigeria'){
  if(FCY == 2){
    ds <- FR_NG_FCY2_plm
    
  }else if(FCY == 1){
    ds <- FR_NG_FCY1_plm
  }else if(FCY == 3){
    ds <- FR_NG_FCY3_plm
  }
} else if (country == 'Tanzania'){
  if(FCY == 2){
    ds=FR_TZ_FCY2_plm
  } else if(FCY == 1){
    ds=FR_TZ_FCY1_plm
  }else if(FCY == 3){
    ds=FR_TZ_FCY3_plm
  }
}


Oyo <- droplevels(ds[ds$STATE == "Oyo", ])
Oyolabel <- data.frame(state= c("Oyo"), lon=c(3.3), lat=c(9))

Ogun <- droplevels(ds[ds$STATE == "Ogun", ])
Ogunlabel <- data.frame(state= c("Ogun"), lon=c(3.4), lat=c(7.65))

Kogi <- droplevels(ds[ds$STATE == "Kogi", ])
Kogilabel <- data.frame(state= c("Kogi"), lon=c(6.63), lat=c(8.56))

Kwara <- droplevels(ds[ds$STATE %in% c("Kwara"), ])
Kwaralabel <- data.frame(state= c( "Kwara"), lon=c(4.9), lat=c(9.5))

Taraba <- droplevels(ds[ds$STATE %in% c("Taraba"), ])
Tarabalabel <- data.frame(state= c( "Taraba"), lon=c(10.2), lat=c(9.05))

Benue_CR <- droplevels(ds[ds$STATE %in% c("Benue","Cross River"), ])
Benue_CRlabel <- data.frame(state= c("Benue", "Cross River"), lon=c(8, 9.5), lat=c(8.2, 5.8))

Edo <- droplevels(ds[ds$STATE %in% c("Edo"), ])
Edolabel <- data.frame(state= c("Edo"), lon=c(5.3), lat=c(7))

Delta_Edo <- droplevels(ds[ds$STATE %in% c("Delta","Edo"), ])
Delta_Edolabel <- data.frame(state= c( "Delta","Edo"), lon=c(6, 5.3), lat=c(5,7))

Akwa_Ibom <- droplevels(ds[ds$STATE %in% c("Akwa Ibom"), ])
Akwa_Ibomlabel <- data.frame(state= c( "Akwa Ibom"), lon=c(8), lat=c(5.45))

Imo <- droplevels(ds[ds$STATE %in% c("Imo"), ])
Imolabel <- data.frame(state= c( "Imo"), lon=c(6.9), lat=c(5.9))

Abia <- droplevels(ds[ds$STATE %in% c("Abia"), ])
Abialabel <- data.frame(state= c( "Abia"), lon=c(7.7), lat=c(5.9))

AkwaIbom_Imo_Abia <- droplevels(ds[ds$STATE %in% c("Akwa Ibom", "Imo", "Abia"), ])
AkwaIbom_Imo_AbiAlabel <- data.frame(state= c( "Akwa Ibom", "Imo", "Abia"), lon=c(7.3, 7, 7.8), lat=c(4.4, 6, 6))

Ondo <- droplevels(ds[ds$STATE %in% c("Ondo"), ])
Ondolabel <- data.frame(state= c( "Ondo"), lon=c(5.3), lat=c(6.6))

Ekiti <- droplevels(ds[ds$STATE %in% c("Ekiti"), ])
Ekitilabel <- data.frame(state= c( "Ekiti"), lon=c(5.3), lat=c(8.1))

Osun <- droplevels(ds[ds$STATE == "Osun", ])
Osunlabel <- data.frame(state= c("Osun"), lon=c(4.2), lat=c(8.05))

Ekiti_Ondo_Osun <- droplevels(ds[ds$STATE %in% c("Ekiti", "Ondo", "Osun"), ])
Ekitil_Ondo_Osunabel <- data.frame(state= c( "Ekiti", "Ondo", "Osun"), lon=c(5.3, 5.3, 4.2), lat=c(8.1, 6.6, 8.05))

Anambra <- droplevels(ds[ds$STATE %in% c("Anambra"), ])
Anambralabel <- data.frame(state= c( "Anambra"), lon=c(7.15), lat=c(6.45))

Ebonyi <- droplevels(ds[ds$STATE %in% c("Ebonyi"), ])
Ebonyilabel <- data.frame(state= c( "Ebonyi"), lon=c(7.83), lat=c(6.67))

Enugu <- droplevels(ds[ds$STATE %in% c("Enugu"), ])
Enugulabel <- data.frame(state= c( "Enugu"), lon=c(7.1), lat=c(6.95))

Anambra_Enugu_Ebonyi <- droplevels(ds[ds$STATE %in% c("Anambra", "Enugu", "Ebonyi"), ])
Anambra_Enugu_Ebonyilabel <- data.frame(state= c( "Anambra", "Enugu", "Ebonyi"), lon=c(6.7, 7, 8.25), lat=c(5.9, 7.1, 6.9))