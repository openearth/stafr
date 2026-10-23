labstafFiles <-
  function(fileDir,
           fileList,
           gebied,
           schipOfLab,
           labstaf,
           laptop,
           experiment,
           conn) {
    
    
    
    if (schipOfLab == 'schip') {
      schipOfLabShort = 'S'
    } else if (schipOfLab == 'lab') {
      schipOfLabShort = 'L'
    }
    
    # print(gebied)
    if (gebied == 'Markermeer') {
      short = 'MM'
    } else if (gebied == 'Grevelingen') {
      short = 'GRV'
    } else if (gebied == 'VeerseMeer') {
      short = 'VM'
    } else if (gebied == 'Oosterschelde') {
      short = 'OS'
    } else if (gebied == 'Westerschelde') {
      short = 'WS'
    } else if (gebied == 'IJsselmeer') {
      short = 'IJ'
    } else if (gebied == 'Kiel-Oslo'){
      short = 'KO'
    } else if (gebied == 'Randmeren'){
      short = 'RM'
    } else if (gebied == 'MarkerWadden'){
      short = 'MW'
    }else if (gebied == 'Oostvaardersdiep'){
      short = 'OVD'
    }
    
    
    
    gebied <-
      paste(gebied, schipOfLab, sep = "_") #'Markermeer_schip' # _schip of _lab of _boei _experiment
    
    gebiedShort <- paste(short, schipOfLabShort, sep = "_")
    
    timezoneLaptopLabstaf <-
      'Europe/Amsterdam'   # Utc of Europe/Amsterdam
    secondsPerIrradiance <- 90
    
    labstaf_Protocol <- "Markermeer.flc"
    
    #list of directories
    dirs <- list.dirs(fileDir)
    
    firstFile <- TRUE
    # different versions Act2 Software
    # software <-
    #   NA   #if not set below, defaults to this value. Nakijken voor labstaf!
    # 
    
    
    for (dirr in dirs) {
      print(dirr)
      
      files <- list.files(dirr, pattern = ".csv")
      
      for(f in files){
        # serial number labstaf
        sn <- extractSerialNumber(paste(dirr, f, sep = '/'))
        
        # colnames
        newColnames <- extractNewColumnNames(paste(dirr, f, sep = '/'))
        
        #read the file
        combinedAll <- readWithNewColnames(paste(dirr, f, sep = '/'), newColnames)
        
        
        combinedAll <- combinedAll %>% 
          mutate(gebied = gebied) %>% 
          mutate(DateTimeStartAms = ymd_hm(Autoname, tz = 'Europe/Amsterdam')) %>% 
          mutate(uniqueId = paste(as.numeric(DateTimeStartAms), E, gebiedShort, sep = "_")) %>% 
          mutate(DateTimeStartAms = as.character(DateTimeStartAms)) %>% 
          mutate(LabstafSerialNumber = sn) %>% 
          mutate(LabstafProtocol = labstaf_Protocol) %>% 
          mutate(experiment = experiment) %>% 
          mutate(Time = as.character(Time)) %>% 
          mutate(timezoneLaptopLabstaf = timezoneLaptopLabstaf)
        
        
        if(schipOfLab == 'schip'){
          combinedAll <- combinedAll %>% 
            mutate(DateTimeSamplingAms = DateTimeStartAms)
        }
        
        
        # filter files not already in db
        sqlCheckAlreadyInDB <- "select uniqueId from a_labstafRaw;" 
        
        dCheck <- dbGetQuery(conn, sqlCheckAlreadyInDB)
        
        combinedAll <- combinedAll %>% 
          filter(!uniqueId %in% dCheck$uniqueId)
  
      }
    }
    
    if (firstFile) {
      firstFile <- FALSE
      csvAll <- combinedAll
    } else{
      csvAll <- bind_rows(csvAll, combinedAll)
    }
    
    return(csvAll)
    
  }





extractSerialNumber <- function(f){
  
  # get the serial number
  d <- read_delim(f, delim= 'FFF',  col_names = FALSE, skip = 0)
  # extract the serial number
  SN <- d %>% filter(str_detect(X1, 'SN'))
  sn <- str_remove(SN$X1[[1]], ",")
  print(sn)
  return(sn)
  
}

extractNewColumnNames <- function(f){
  print(f)
  # get the new colnames
  d <- read_csv(f, col_names = FALSE, skip =6, n_max = 2)
  d1 <- d
  # View(d1)
  dd <-as_tibble(cbind(nms = names(d), t(d)))
  
  vv <- NA
  for(n in dd$nms){
    
    ddd <- dd %>% filter(nms == n)
    v2 <- ddd$V2[[1]]
    if(!is.na(v2)){
      vv <- v2
    }
    
    dd[which(dd$nms == n), ]$V2 <- vv
  }
  
  dd <- dd %>% 
    mutate(newColnames = str_remove(paste(V2, V3, sep = "_"), "NA_"))
  # View(dd)
  newColnames <- dd$newColnames
  
  print(newColnames)
  return(newColnames)
}



readWithNewColnames <- function(f, newColnames){
  # read in with the new colnames
  
  d <- read_csv(f, col_names = newColnames, skip =8)
  # View(d)
  dZonderSlotregels <- d %>% 
    filter(!is.na(`Step E_Step 5`)) #  aangenomen dat deze altijd is gevuld
  # View(dZonderSlotregels)
  
  first <- TRUE
  for(a in dZonderSlotregels$`Auto name`){
    
    d <- dZonderSlotregels %>% 
      filter(`Auto name` == a)
    
    
    Autoname <- d$`Auto name`[[1]]
    Date <- d$Date[[1]]
    Time <- d$Time[[1]]
    Seconds <- d$Seconds[[1]]
    Hours <- d$Hours[[1]]
    Step_max <- d$`Step max`[[1]]
    Quench_Fb <- d$`Quench Fb`[[1]]
    FvFm <- d$`Fv/Fmc`[[1]]
    Ka <- d$Ka[[1]]
    PEC <- d$PEC[[1]]
    cPEC <- d$cPEC[[1]]
    cPEP <- d$cPEP[[1]]
    aLHII <- d$aLHII[[1]]
    highE <- d$`High E`[[1]]
    
    dd <-as_tibble(cbind(nms = names(d), t(d)))
    
    dE <- dd %>% filter(str_detect(nms, "Step E")) %>% rename(E = V2) %>% select(-nms)
    
    combined <- dE %>% 
      mutate(Autoname = Autoname) %>% 
      mutate(Date = Date) %>% 
      mutate(Time = Time) %>% 
      mutate(Seconds = Seconds) %>% 
      mutate(Hours = Hours) %>% 
      mutate(Step_max = Step_max) %>% 
      mutate(Quench_Fb = Quench_Fb) %>% 
      mutate(FvFm = FvFm) %>% 
      mutate(Ka = Ka) %>% 
      mutate(PEC = PEC) %>% 
      mutate(cPEC = cPEC) %>% 
      mutate(cPEP = cPEP) %>% 
      mutate(aLHII = aLHII) %>% 
      mutate(aLHII = aLHII)
    
    rP <- dd %>% filter(str_detect(nms, "rP_Step")) %>% rename(rP = V2) %>% select(-nms)
    combined <- bind_cols(combined, rP)
    JVPII <- dd %>% filter(str_detect(nms, "JVPII_Step")) %>% rename(JVPII = V2) %>% select(-nms)
    combined <- bind_cols(combined, JVPII)
    JPII <- dd %>% filter(str_detect(nms, "JPII_Step")) %>% rename(JPII = V2)
    JPII <- JPII %>% filter(!str_detect(nms, 'dJPII_Step')) %>% select(-nms)
    combined <- bind_cols(combined, JPII)
    FoAndFPrime <- dd %>% filter(str_detect(nms, "Fo and F'_Step")) %>% rename(FoAndF = V2) %>% select(-nms)
    combined <- bind_cols(combined, FoAndFPrime)
    FmAndFmPrime <- dd %>% filter(str_detect(nms, "Fm and Fm'_Step")) %>% rename(FmAndFmPrime = V2) %>% select(-nms)
    combined <- bind_cols(combined, FmAndFmPrime)
    FvAndFq <- dd %>% filter(str_detect(nms, "Fv and Fq'_Step")) %>% rename(FvAndFq = V2) %>% select(-nms)
    combined <- bind_cols(combined, FvAndFq)
    FvFmAndFqFm <- dd %>% filter(str_detect(nms, "Fv/Fm and Fq'/Fm'_Step")) %>% rename(FvFmAndFqFm = V2) %>% select(-nms)
    combined <- bind_cols(combined, FvFmAndFqFm)
    FvFmcAndFqFmc <- dd %>% filter(str_detect(nms, "Fv/Fmc and Fq'/Fmc'_Step")) %>% rename(FvFmcAndFqFmc = V2) %>% select(-nms)
    combined <- bind_cols(combined, FvFmcAndFqFmc)
    FvFmc <- dd %>% filter(str_detect(nms, "Fv'/Fmc'_Step")) %>% rename(FvFmc = V2) %>% select(-nms)
    combined <- bind_cols(combined, FvFmc)
    FqFv <- dd %>% filter(str_detect(nms, "Fq'/Fv'_Step")) %>% rename(FqFv = V2) %>% select(-nms)
    combined <- bind_cols(combined, FqFv)
    Ek <- dd %>% filter(str_detect(nms, "Ek_Step"))
    Ek <- Ek %>% filter(!str_detect(nms, "TauEk_Step")) %>% rename(Ek = V2) %>% select(-nms)
    combined <- bind_cols(combined, Ek)
    Ekt <- dd %>% filter(str_detect(nms, "Ekt'_Step")) %>% rename(Ekt = V2) %>% select(-nms)
    combined <- bind_cols(combined, Ekt)
    Ektu <- dd %>% filter(str_detect(nms, "Ektu'_Step")) %>% rename(Ektu = V2) %>% select(-nms)
    combined <- bind_cols(combined, Ektu)
    RhoAndRhoPrime <- dd %>% filter(str_detect(nms, "Rho and Rho'_Step")) %>% rename(RhoAndRhoPrime = V2) %>% select(-nms)
    combined <- bind_cols(combined, RhoAndRhoPrime)
    
    hRho <- dd %>% filter(str_detect(nms, "\\(h\\)Rho'_Step")) %>% rename(hRho = V2) %>% select(-nms)
    combined <- bind_cols(combined, hRho)
    
    dRho <- dd %>% filter(str_detect(nms, "\\(d\\)Rho'_Step")) %>% rename(dRho = V2) %>% select(-nms)
    combined <- bind_cols(combined, dRho)
    AlphaPIIAndAlphaPIIPrime <- dd %>% filter(str_detect(nms, "AlphaPII and AlphaPII'_Step")) %>% rename(AlphaPIIAndAlphaPIIPrime = V2) %>% select(-nms)
    combined <- bind_cols(combined, AlphaPIIAndAlphaPIIPrime)
    SigmaPIIAndSigmaPIIPrime <- dd %>% filter(str_detect(nms, "SigmaPII and SigmaPII'_Step")) %>% rename(SigmaPIIAndSigmaPIIPrime = V2) %>% select(-nms)
    combined <- bind_cols(combined, SigmaPIIAndSigmaPIIPrime)
    oo <- dd %>% filter(str_detect(nms, "\\[oo\\]_Step")) %>% rename(oo = V2) %>% select(-nms)
    combined <- bind_cols(combined, oo)
    oc <- dd %>% filter(str_detect(nms, "\\[oc\\]_Step")) %>% rename(oc = V2) %>% select(-nms)
    combined <- bind_cols(combined, oc)
    cc <- dd %>% filter(str_detect(nms, "\\[cc\\]_Step")) %>% rename(cc = V2) %>% select(-nms)
    combined <- bind_cols(combined, cc)
    dRhoAnddRhoPrime <- dd %>% filter(str_detect(nms, "dRho and dRho'_Step")) %>% rename(dRhoAnddRhoPrime = V2) %>% select(-nms)
    combined <- bind_cols(combined, dRhoAnddRhoPrime)
    dSigmaAnddSigmaPrime <- dd %>% filter(str_detect(nms, "dSigma and dSigma'_Step")) %>% rename(dSigmaAnddSigmaPrime = V2) %>% select(-nms)
    combined <- bind_cols(combined, dSigmaAnddSigmaPrime)
    docSigmaAnddocSigmaPrime <- dd %>% filter(str_detect(nms, "docSigma and docSigma'_Step")) %>% rename(docSigmaAnddocSigmaPrime = V2) %>% select(-nms)
    combined <- bind_cols(combined, docSigmaAnddocSigmaPrime)
    dJPII <- dd %>% filter(str_detect(nms, "dJPII_Step")) %>% rename(dJPII  = V2) %>% select(-nms)
    combined <- bind_cols(combined, dJPII)
    qP <- dd %>% filter(str_detect(nms, "qP_Step ")) %>% rename(qP = V2) %>% select(-nms)
    combined <- bind_cols(combined, qP)
    qL <- dd %>% filter(str_detect(nms, "qL_Step ")) %>% rename(qL = V2) %>% select(-nms)
    combined <- bind_cols(combined, qL)
    qJ <- dd %>% filter(str_detect(nms, "qJ_Step ")) %>% rename(qJ = V2) %>% select(-nms)
    combined <- bind_cols(combined, qJ)
    qd <- dd %>% filter(str_detect(nms, "qd_Step ")) %>% rename(qd = V2) %>% select(-nms)
    combined <- bind_cols(combined, qd)
    TauEk <- dd %>% filter(str_detect(nms, "TauEk_Step")) %>% rename(TauEk = V2) %>% select(-nms)
    combined <- bind_cols(combined, TauEk)
    Taut <- dd %>% filter(str_detect(nms, "Taut'_Step")) %>% rename(Taut = V2) %>% select(-nms)
    combined <- bind_cols(combined, Taut)
    FbAndFbPrime <- dd %>% filter(str_detect(nms, "Fb and Fb'_Step")) %>% rename(FbAndFbPrime = V2) %>% select(-nms)
    combined <- bind_cols(combined, FbAndFbPrime)
    
    if(first){
      first <-FALSE
      combinedAll <- combined
    }else{
      combinedAll <- bind_rows(combinedAll, combined)
    }
  }
  # View(combinedAll)
  return(combinedAll)
}
