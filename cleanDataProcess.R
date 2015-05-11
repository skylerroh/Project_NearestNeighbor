uniqueMAC = unique(offline$mac)
macHits = sapply(uniqueMAC, function(x) length(offline$mac[offline$mac == x]))
macCandidates = names(macHits)[1:7]
hist(as.numeric(offline$signal[offline$mac == macCandidates[3]]))
hist(as.numeric(offline$signal[offline$mac == macCandidates[7]]))


keepMacs = c("00:14:bf:b1:97:8a", 
             "00:14:bf:b1:97:90", 
             "00:14:bf:b1:97:8d", 
             "00:14:bf:b1:97:81", 
             "00:14:bf:3b:c7:c6", 
             macCandidates[3])
findAccessLocation = function (macAddress, macList, signal, posX, posY) {
  x = posX[macList == macAddress][signal[macList == macAddress] == max(signal[macList == macAddress])]
  y = posY[macList == macAddress][signal[macList == macAddress] == max(signal[macList == macAddress])]
  return (list(x = unique(x), y = unique(y)))
}

macToAccess = lapply(keepMacs, findAccessLocation, macList = offline$mac, 
                     signal = as.numeric(offline$signal), posX = as.numeric(offline$posX),
                     posY = as.numeric(offline$posY))
names(macToAccess) = keepMacs

#only keepMacs and type 3
#offline3 = offline[offline$type == "3",] ::made redundant by following subset
offline3 = offline[offline$mac %in% keepMacs, ]

#to numeric
offline3[, c(3, 4, 5, 6, 8)] = sapply(offline3[, c(3, 4, 5, 6, 8)], as.numeric)

#round orientation to nearest 45
offline3$orientationRounded = round(offline3$orientation / 45, digits = 0) * 45

#drop time, scanMac, posZ, channel, type
offline3 = offline3[, -c(1, 2, 5, 9, 10)]

#median of each unique combination x,y,angle,mac
medianSS = aggregate(x = offline3$signal, 
                    by = list(offline3$posX, offline3$posY, offline3$orientationRounded, offline3$mac), 
                    FUN = median)
                           
#create new df, drop redundant MAC
cleanDF = data.frame(posX = medianSS[[1]][1:1331], 
                     posY = medianSS[[2]][1:1331], 
                     orientation = medianSS[[3]][1:1331], 
                     SS1 = medianSS[[5]][1:1331],
                     SS2 = medianSS[[5]][1332:2662],
                     SS3 = medianSS[[5]][2663:3993],
                     SS4 = medianSS[[5]][3994:5324],
                     SS5 = medianSS[[5]][5325:6655],
                     SS6 = medianSS[[5]][6656:7986])
#ordered by x,y
cleanorder = cleanDF[order(cleanDF$posX, cleanDF$posY),]





