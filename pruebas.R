coordenadas <- read.csv("coordenadasCentrales.csv", header = TRUE, sep = ",")


# Modificación de Y_i (checar foto)
poblacionNeigh <- c(19261,55297,19890,126909,18306,47263,33489,38924,40867,5351,26659,17386,8749,30021,54161,36772,4403,32571,6619,33526)
neigNamesVcetor <- c("Allston", "Brighton", "Charlestown", "Dorchester", "Downtown", "East Boston", "Fenway", "Hyde Park", "Jamaica Plain", "Longwood", "Mattapan", "Mission Hill", "North End", "Roslindale", "Roxbury", "South Boston", "South Boston Waterfront", "South End", "West End", "West Roxbury")

mVector <- data.frame(cbind(neigVcetor, poblacionNeigh))

mVector$mVectorInicial <- as.numeric(mVector$poblacionNeigh)

