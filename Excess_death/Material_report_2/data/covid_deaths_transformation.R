################# ENGLAND #################
england_deaths <- read.csv("coronavirus-deaths_latest.csv")
england <- england_deaths[england_deaths$Area.name == 'England',]
wales <- england_deaths[england_deaths$Area.name == 'Wales',]
england <- england[,-c(1,2,3,6)]
wales <- wales[,-c(1,2,3,6)]
zeros <- rep(0, 11)
zeros2 <- cbind(zeros,zeros)
colnames(zeros2) <- colnames(wales)
wales <- rbind(wales, zeros2)

england$Daily.change.in.deaths <- rev(england$Daily.change.in.deaths)
england <- england[-1,]
sum_eng <- c()
for (i in 0:10){
  somma <- sum(england[(1+7*i):(7+1+7*i),2])
  sum_eng <- rbind(sum_eng, somma)
}

wales$Daily.change.in.deaths <- rev(wales$Daily.change.in.deaths)
wales <- wales[-12,]

sum_wales <- c()
for (i in 0:10){
  somma <- sum(wales[(1+7*i):(7+1+7*i),2])
  sum_wales<- rbind(sum_wales, somma)
}

total_sum <- sum_eng + sum_wales
row.names(total_sum) <- c()
total_sum <- cbind(c(1:11), total_sum)
colnames(total_sum) <- c("weeks", "England_Wales_deaths")

write.csv(total_sum,"england_wales_weekly_covid_deaths.csv")

#deaths_global <- read.csv("time_series_covid19_deaths_global.csv", sep = ',')
#france <- deaths_global[deaths_global$Country.Region=='France',]

################# FRANCE #################

france <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,4,4,6,9,11,19,
            19,33,48,48,79,91,91,148,148,148,243,450,562,674,
            860,1100,1331,1696,1995,2314,2606,3024,3523,4403,
            5387,6507,7560,8078,8911,10328,10869,12210,13197,
            13832,14393,14967,15712,17148,17901,18661,19303,19694,
            20240,20765,21309,21825,22214,22583,22825,23262,23629,
            24056,24345,24563,24729,24864,25168,25498,25772,25949,
            26192,26271,26341,26604,26951,27032,27381,27485,27483,
            28062,28193,27976,28084,28167,28167,28167,28317,28407)
france_daily <- c()
for (i in 1:(length(france)-1)){
  france_daily[i] = france[i+1]-france[i]
  if (france_daily[i] < 0)
    france_daily[i] = 0;
}
france_daily

france_daily <- c(france_daily, c(0,0))

sum_france <- c()
for (i in 0:(length(belgium_daily)/7-1)){
  somma <- sum(france_daily[(1+7*i):(7*i+7)])
  sum_france <- c(sum_france, somma)
}
sum_france

################# NORWAY #################

norway <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            3,3,3,3,6,7,7,7,7,10,12,14,14,19,23,25,32,39,44,50,59,
            62,71,76,89,101,108,113,119,128,134,139,150,152,161,164,
            165,181,182,187,194,199,201,201,205,206,207,210,210,211,
            211,214,215,216,217,218,219,219,224,228,229,232,232,232,
            232,233,233,234,235,235,235,235,235)
norway_daily <- c()
for (i in 1:(length(norway)-1)){
  norway_daily[i] = norway[i+1]-norway[i]
}
norway_daily
norway_daily <- c(norway_daily, c(0,0))

sum_norway <- c()
for (i in 0:(length(belgium_daily)/7-1)){
  somma <- sum(norway_daily[(1+7*i):(7*i+7)])
  sum_norway <- c(sum_norway, somma)
}
sum_norway
################# BELGIUM #################

belgium <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,4,4,5,10,
            14,21,37,67,75,88,122,178,220,289,353,431,513,705,828,1011,
            1143,1283,1447,1632,2035,2240,2523,3019,3346,3600,3903,4157,
            4440,4857,5163,5453,5683,5828,5998,6262,6490,6679,6917,7094,
            7207,7331,7501,7594,7703,7765,7844,7924,8016,8339,8415,8521,
            8581,8656,8707,8761,8843,8903,8959,9005,9052,9080,9108,9150,
            9186,9212,9237,9280,9312)
belgium_daily <- c()
for (i in 1:(length(belgium)-1)){
  belgium_daily[i] = belgium[i+1]-belgium[i]
}
belgium_daily <- c(belgium_daily,0)

sum_belgium <- c()
for (i in 0:(length(belgium_daily)/7-1)){
  somma <- sum(belgium_daily[(1+7*i):(7*i+7)])
  sum_belgium <- c(sum_belgium, somma)
}
sum_belgium

################# JOINING THEM TOGETHER #################
france_belgium_norway <- cbind(c(1:18), sum_france, sum_belgium, sum_norway)
row.names(france_belgium_norway) <- c()
colnames(france_belgium_norway) <- c("weeks", "france", "belgium", "norway")
france_belgium_norway

write_csv(data.frame(france_belgium_norway), "france_belgium_norway.csv")
