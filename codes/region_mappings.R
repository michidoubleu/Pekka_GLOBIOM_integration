# Create the full mapping table
mapping_key <- tibble(
  region = c(
    # --- Direct Country Matches ---
    "Argentina", "Australia", "Austria", "Belgium", "Brazil", "Bulgaria", "Canada", 
    "China", "Croatia", "Cyprus", "CzechRep", "Denmark", "Estonia", "Finland", 
    "France", "Germany", "Greece", "Hungary", "India", "Indonesia", "Ireland", 
    "Italy", "Japan", "Latvia", "Lithuania", "Luxembourg", "Malaysia", "Malta", 
    "Mexico", "Netherlands", "NewZealand", "Poland", "Portugal", "Romania", 
    "RussianFed", "Slovakia", "Slovenia", "SouthAfrica", "KoreaRep", "Spain", 
    "Sweden", "Turkey", "Ukraine", "USA",
    
    # --- CongoBasin ---
    "Cameroon", "CentAfrRep", "CongoDemR", "CongoRep", "EqGuinea", "Gabon",
    
    # --- EasternAf ---
    "Burundi", "Ethiopia", "Kenya", "Madagascar", "Rwanda", "Sudan", "Tanzania", "Uganda",
    
    # --- NorthernAf ---
    "Algeria", "Libya", "Morocco", "Tunisia", "Egypt",
    
    # --- SouthernAf ---
    "Botswana", "Lesotho", "Namibia", "Swaziland", "Zimbabwe", "Mozambique", "Malawi",
    
    # --- WesternAf ---
    "Benin", "BurkinaFaso", "CotedIvoire", "Gambia", "Ghana", "Guinea", "GuineaBissau", 
    "Liberia", "Mali", "Nigeria", "Senegal", "SierraLeone", "Togo", "Mauritius",
    
    # --- Former_USSR (Central Asia & Caucasus) ---
    "Armenia", "Azerbaijan", "Belarus", "Georgia", "Kazakhstan", "Kyrgyzstan", 
    "MoldovaRep", "Tajikistan", "Turkmenistan", "Uzbekistan", "FormerSovietUnion",
    
    # --- MiddleEast ---
    "Iran", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Oman", "SaudiArabia", 
    "Syria", "UntdArabEm", "Yemen",
    
    # --- RCAM (Rest of Central America & Caribbean) ---
    "Belize", "CostaRica", "Cuba", "DominicanRp", "ElSalvador", "Guatemala", "Haiti", 
    "Honduras", "Jamaica", "Nicaragua", "Panama", "PuertoRico", "TrinidadTob", 
    "Bahamas", "Guadeloupe", "LatinAmericaCarib",
    
    # --- RSAM (Rest of South America) ---
    "Bolivia", "Chile", "Colombia", "Ecuador", "FalklandIs", "FrGuiana", "Guyana", 
    "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela",
    
    # --- RSAS (Rest of South Asia) ---
    "Bangladesh", "Bhutan", "Nepal", "Pakistan", "SriLanka",
    
    # --- RSEA_PAC & RSEA_OPA (SE Asia & Pacific) ---
    "BruneiDarsm", "Cambodia", "Laos", "Myanmar", "Philippines", "Singapore", 
    "Thailand", "TimorLeste", "VietNam",
    
    # --- Pacific_Islands ---
    "FijiIslands", "NewCaledonia", "PapuaNGuin", "SolomonIs", "Vanuatu",
    
    # --- RCEU (Rest of Central/Eastern Europe) ---
    "Albania", "BosniaHerzg", "Macedonia", "Serbia-Monte",
    
    # --- ROWE (Rest of World Europe / Others) ---
    "Norway", "Switzerland", "UK", "Iceland"
  ),
  target_region = c(
    # Matches the order of the 'input_name' vector above
    rep("ArgentinaReg", 1), rep("AustraliaReg", 1), rep("AustriaReg", 1), rep("BelgiumReg", 1), 
    rep("BrazilReg", 1), rep("BulgariaReg", 1), rep("CanadaReg", 1), rep("ChinaReg", 1), 
    rep("CroatiaReg", 1), rep("CyprusReg", 1), rep("CzechRepReg", 1), rep("DenmarkReg", 1), 
    rep("EstoniaReg", 1), rep("FinlandReg", 1), rep("FranceReg", 1), rep("GermanyReg", 1), 
    rep("GreeceReg", 1), rep("HungaryReg", 1), rep("IndiaReg", 1), rep("IndonesiaReg", 1), 
    rep("IrelandReg", 1), rep("ItalyReg", 1), rep("JapanReg", 1), rep("LatviaReg", 1), 
    rep("LithuaniaReg", 1), rep("LuxembourgReg", 1), rep("MalaysiaReg", 1), rep("MaltaReg", 1), 
    rep("MexicoReg", 1), rep("NetherlandsReg", 1), rep("NewZealandReg", 1), rep("PolandReg", 1), 
    rep("PortugalReg", 1), rep("RomaniaReg", 1), rep("RussiaReg", 1), rep("SlovakiaReg", 1), 
    rep("SloveniaReg", 1), rep("SouthAfrReg", 1), rep("SouthKorea", 1), rep("SpainReg", 1), 
    rep("SwedenReg", 1), rep("TurkeyReg", 1), rep("UkraineReg", 1), rep("USAReg", 1),
    
    rep("CongoBasin", 6), rep("EasternAf", 8), rep("NorthernAf", 5), rep("SouthernAf", 7), 
    rep("WesternAf", 14), rep("Former_USSR", 11), rep("MiddleEast", 11), rep("RCAM", 16), 
    rep("RSAM", 12), rep("RSAS", 5), rep("RSEA_PAC", 9), rep("Pacific_Islands", 5), 
    rep("RCEU", 4), rep("ROWE", 4)
  )
)

eu27_regions <- c(
  "AustriaReg", "BelgiumReg", "BulgariaReg", "CroatiaReg", 
  "CzechRepReg", "DenmarkReg", "EstoniaReg", "FinlandReg", 
  "FranceReg", "GermanyReg", "GreeceReg", "HungaryReg", 
  "IrelandReg", "ItalyReg", "LatviaReg", "LithuaniaReg", 
  "LuxembourgReg", "NetherlandsReg", "PolandReg", "PortugalReg", 
  "RomaniaReg", "SlovakiaReg", "SloveniaReg", "SpainReg", 
  "SwedenReg"
)