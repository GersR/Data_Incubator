all_del <- readRDS("all_del.rds")

#Examinar duplicados
#Andreu
aux <- base::duplicated(all_del$direccion)
duplicados <- all_del[aux,] %>% arrange(calle)

calles_duplicadas <- as.character(unique(duplicados$calle))

base_duplicados<-data.frame()

for(i in 1:length(calles_duplicadas)){
  x<-calles_duplicadas[i]
  filtro <- all_del %>% filter(calle==x)
  base_duplicados <-rbind(base_duplicados,filtro)
  base_duplicados
}

seleccionadas <- c(2,11,15,29,44,73,458,497,16,21,60,80,86,101,154,579,102,403,560,103,104,106,240,423,585,
                   113,121,123,165,189,205,352,119,172,304,124,132,379,404,125,126,129,127,216,287,406,
                   128,228,130,222,307,350,131,330,382,400,401,476,606,134,155,243,136,202,137,214,218,
                   139,149,158,317,374,147,385,148,153,293,544,607,193,446,192,219,360,224,226,289,380,
                   227,229,239,241,271,581,286,395,329,343,461,387,468,410,570,441,450,444,460,452,604,
                   474,610,482:488,509,541,559,586,587,589,591,623,682,683,766,687,842,691,697,709,745,
                   720,737,883,721,728,817,851,729,756,816,822,734,735,741,744,797,767,826,776,811,815,
                   782,852,891,900,901,914,956,1097,902,923,903,904,913,919,933,978,937,953,947,971,1063,
                   987,992,1004,1061,1064,1092,979,1117,1001,1016,1098,1005,1144,1006,1142,1010,1026,1106,
                   1031,1130,1132,1048,1049,1094,1050,1062,1088,1055,1057,1058,1110,1163)

base_duplicados_seleccionadas <- base_duplicados %>% filter(idg %in% seleccionadas)

previa <- anti_join(all_del,base_duplicados,by="idg")
previa <- rbind(previa,base_duplicados_seleccionadas)
