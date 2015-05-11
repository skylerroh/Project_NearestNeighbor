#Part 1 - Data Processing
#task1
processLine = function (x) {
  splitX = strsplit(x, split = ";")[[1]]
  processed = matrix(0, ncol = 10, nrow = length(splitX) - 4)
  processed[, 1] = gsub("t=", "", splitX[1])
  processed[, 2] = gsub("id=", "", splitX[2])
  pos = strsplit(gsub("pos=", "", splitX[3]), ",")[[1]]
  processed[, 3] = pos[1]
  processed[, 4] = pos[2]
  processed[, 5] = pos[3]
  processed[, 6] = gsub("degree=", "", splitX[4])
  processed[, 7] = gsub("=.*", "", splitX[-(1:4)])
  processed[, 8] = gsub("(.*=)|(,.*)", "", splitX[-(1:4)])
  processed[, 9] = gsub("(^[^,]*,)|(,[1-9]$)", "", splitX[-(1:4)])
  processed[, 10] = gsub("(^.*,)", "", splitX[-(1:4)])
  return(processed)
}

txt = readLines("offline.final.trace.txt")
txt = txt[-grep("#", txt)]
tmp = lapply(txt, processLine)
offline = as.data.frame(do.call("rbind", tmp), stringsAsFactors = FALSE)
names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                   "orientation", "mac", "signal", "channel", "type")

#find which MAC Addresses to keep
  #find all unique MAC Addresses, eliminate any below 12,000 records 
uniqueMAC = unique(offline$mac)
macHits = sapply(uniqueMAC, function(x) length(offline$mac[offline$mac == x]))
macCandidates = names(macHits)[1:7]

  #histograms to describe signal strength distributions for remaining non-Cisco devices
hist(as.numeric(offline$signal[offline$mac == "00:0f:a3:39:e1:c0"]))
hist(as.numeric(offline$signal[offline$mac == "00:0f:a3:39:dd:cd"]))

  #decide to keep "00:0f:a3:39:e1:c0"
keepMacs = c("00:14:bf:b1:97:8a", 
             "00:14:bf:b1:97:90", 
             "00:14:bf:b1:97:8d", 
             "00:14:bf:b1:97:81", 
             "00:14:bf:3b:c7:c6", 
             "00:0f:a3:39:e1:c0")
  
#created function to find pos with strongest signal strengths for each access point,
#results in approximate access point locations for each address
findAccessLocation = function (macAddress, macList, signal, posX, posY) {
  x = posX[macList == macAddress][signal[macList == macAddress] == max(signal[macList == macAddress])]
  y = posY[macList == macAddress][signal[macList == macAddress] == max(signal[macList == macAddress])]
  return (list(x = unique(x), y = unique(y)))
}

macToAccess = lapply(keepMacs, findAccessLocation, macList = offline$mac, 
                     signal = as.numeric(offline$signal), posX = as.numeric(offline$posX),
                     posY = as.numeric(offline$posY))
names(macToAccess) = keepMacs


#task2
cleanData = function(data, keepMacs = c("00:14:bf:b1:97:8a", 
                                        "00:14:bf:b1:97:90", 
                                        "00:14:bf:b1:97:8d", 
                                        "00:14:bf:b1:97:81", 
                                        "00:14:bf:3b:c7:c6", 
                                        "00:0f:a3:39:e1:c0"),
                     type = 3) {
  #offline3 = offline[offline$type == "3",] ::made redundant by following subset
  data = data[data$mac %in% keepMacs, ]
  #to numeric
  data[, c(3, 4, 5, 6, 8)] = sapply(data[, c(3, 4, 5, 6, 8)], as.numeric)
  #round orientation to nearest 45
  data$orientationRounded = round(data$orientation / 45, digits = 0) * 45
  #drop time, scanMac, posZ, channel, type
  data = data[, -c(1, 2, 5, 9, 10)]
  #median of each unique combination x,y,angle,mac
  medianSS = aggregate(x = data$signal, 
                       by = list(data$posX, data$posY, data$orientationRounded, data$mac), 
                       FUN = median)
  #create new df, drop redundant MAC
  cleanDF = data.frame(posX = medianSS[[1]][1:(length(medianSS[[1]]) / 6)], 
                       posY = medianSS[[2]][1:(length(medianSS[[1]]) / 6)], 
                       orientation = medianSS[[3]][1:(length(medianSS[[1]]) / 6)], 
                       SS1 = medianSS[[5]][medianSS$Group.4 == "00:14:bf:b1:97:90"],
                       SS2 = medianSS[[5]][medianSS$Group.4 == "00:14:bf:b1:97:8d"],
                       SS3 = medianSS[[5]][medianSS$Group.4 == "00:14:bf:b1:97:81"],
                       SS4 = medianSS[[5]][medianSS$Group.4 == "00:14:bf:3b:c7:c6"],
                       SS5 = medianSS[[5]][medianSS$Group.4 == "00:14:bf:b1:97:8a"],
                       SS6 = medianSS[[5]][medianSS$Group.4 == "00:0f:a3:39:e1:c0"])
  #ordered by x,y
  return (cleanorder = cleanDF[order(cleanDF$posX, cleanDF$posY), ])
}

#clean offline data into new data frame
offline2 = cleanData(offline)

#created function to map signal strengths corresponding to an access point
#as a heat map (green = strong signal, red = poor signal)
library("fields")
mapSignal = function (SS, orientation, title) {
  SS = SS[offline2$orientation == orientation]
  colors = colorRampPalette(colors = c("red", "yellow", "green"))(max(SS) - min(SS))[SS - min(SS)]
 
  plot(offline2$posY[offline2$orientation == orientation] ~ 
         offline2$posX[offline2$orientation == orientation], 
       main = title,
       xlim = c(-2, 35), xlab = "x position",
       ylim = c(-2, 15), ylab = "y position",
       pch = 15, cex = 1,
       col = colors)
  #Access1
  points(x = 1, y = 14)
  text(x = 3, y = 15, labels = "SS1 - 00:14:bf:b1:97:8a", cex = .7)
  #Access
  points(x = 34, y = 8)
  text(x = 30, y = 9, labels = "SS2 - 00:14:bf:b1:97:8d", cex = .7)
  #Access3
  points(x = 34, y = 3)
  text(x = 30, y = 2, labels = "SS3 - 00:14:bf:b1:97:81", cex = .7)
  #Access4
  points(x = 13, y = -2)
  text(x = 13, y = -1, labels = "SS4 - 00:14:bf:3b:c7:c6", cex = .7)
  #Access5
  points(x = 3, y = -1)
  text(x = 3, y = -2, labels = "SS5 - 00:14:bf:b1:97:8a", cex = .7)
  #Access6
  points(x = 7.5, y = 5.5)
  text(x = 7.5, y = 4.5, labels = "SS6 - 00:0f:a3:39:e1:c0", cex = .7)
  
  #legend
  colorbar.plot(x = 30, y = 13, strip = 1:(max(SS) - min(SS)), 
                col = colorRampPalette(colors = c("red", "yellow", "green"))(max(SS) - min(SS)))
  text(x = 30, y = 14.5, labels = "weak <--> strong", cex = .8)
}

#mapping building
mapSignal(offline2$SS3, orientation = 225.0, 
          title = "Heat Map of Signal Strength for Access 3:
          \n Orientation = 225.0")
mapSignal(offline2$SS3, orientation = 315.0, 
          title = "Heat Map of Signal Strength for Access 3: 
          \n Orientation = 315.0")
mapSignal(offline2$SS5, orientation = 45.0, 
          title = "Heat Map of Signal Strength for Access 5:
          \n Orientation = 45.0")
mapSignal(offline2$SS5, orientation = 180.0, 
          title = "Heat Map of Signal Strength for Access 5: 
          \n Orientation = 180.0")

#Part 3
#Nearest neighbor function
predNeighbor = function(k, offline, online) {
  #online and offline inputs are both data frames with 6 SS and corresponding position data
  #finds the Euclidean distances between the signal strengths of the observation point 
  #and thoseeach the data points with same orientation
  #transpose online data frame into matrix to apply temporary function 
  #for every position x,y,orientation (each row data frame --> column of matrix,
  #apply across columns)
  transpose = t(online)
  d = apply(transpose, 2, FUN = function(onlinePoint) {
                              dist = sqrt((onlinePoint["SS1"] - offline$SS1) ** 2 
                                         + (onlinePoint["SS2"] - offline$SS2) ** 2 
                                         + (onlinePoint["SS3"] - offline$SS3) ** 2
                                         + (onlinePoint["SS4"] - offline$SS4) ** 2
                                         + (onlinePoint["SS5"] - offline$SS5) ** 2
                                         + (onlinePoint["SS6"] - offline$SS6) ** 2)
                              #sorts the data frame by increasing distance, keeping the first k
                              #must have the same orientation as online point
                              sortedbyDist = offline[order(dist), c("posX", "posY", "orientation")]
                              sameAngle = sortedbyDist[sortedbyDist$orientation == 
                                                         onlinePoint["orientation"],][1:k, ] 
                              #Estimate the x and y positions by finding the average of
                              #the first k closest data point positions
                              return (c(x.estimated = sum(sameAngle$posX) / k,
                                        y.estimated = sum(sameAngle$posY) / k)) 
                          })
  return (d)
}

#cross validation process
crossValidate = function (offline, k, vfold = 5) {
  n = nrow(offline)
  permRows = (sample(1:n, floor(n / vfold) * vfold))
  folds = matrix(permRows, byrow = TRUE, nrow = vfold)

  distanceError = matrix(0, nrow = n, ncol = length(k))
  for (i in 1:length(k)) {
    for (j in 1:vfold) {
      predXY = predNeighbor(k = k[i], 
                            online = offline[folds[j, ], ],
                            offline = offline[folds[-j, ],])
      #create vector of all predicted X's
      predX = predXY[1, ]
      #create vector of all predicted Y's
      predY = predXY[2, ]
      #calculate L1 error by taking the sum of absolute error in x and y
      L1error = abs(predX - offline$posX[folds[j, ]]) + abs(predY - offline$posY[folds[j, ]])
      distanceError[folds[j, ], i] = L1error
    }
  }
  #return the matrix with error values for each position and each value of k
  return(distanceError)
}

#finding best k
findBestK = function(orientation, offline, k, vfold = 5) {
  #cross validate using all offline data of a specific orientation
  test = crossValidate(offline = offline[offline$orientation == orientation, ], k = k, vfold = vfold)
  #return k with minimum sum of squares
  return(which(apply(test^2, 2, sum) == min(apply(test^2, 2, sum))))
}

#set seed to control random samples from cross validation
set.seed(94720)
#replicate cross validation over all orientations 10 times
#(80 cross validations)
bestK = (replicate(10, sapply(seq(0.0, 315.0, by = 45.0), 
                              FUN = findBestK, 
                              offline = offline2, k = 1:20, vfold = 5)))
#histogram of P(k = Best K)
hist(bestK, main = "Best K Value: \nMinimum Error in \nRepeated Cross Validation", xlab = "k", )
#plot of errors from one cross validation 
cv180 = apply(crossValidate(offline = offline2[offline2$orientation == 180.0, ], 
              k = 1:20, vfold = 5)^2, 2, sum)
plot(cv180, main = "L1 Error for \nCross Validation, Orientation = 180",
     xlab = "k", ylab = "sum of L1 errors (meters)")

#using k = 5, predict positions of online data
predictedPos = predNeighbor(offline = offline2, online = online2, k = 5)
#extract actual positions of online data
actualPos = t(online2)[c("posX", "posY"),]
#actual L1 error of our k = 5 nearest neighbors prediction 
L1error = abs(actualPos - predictedPos)[1,] + abs(actualPos - predictedPos)[2,]

#histogram showing distribution of L1 errors
hist(L1error, freq = FALSE, main = "L1 Error Values", xlab = "L1 Error (meters)")
# ~ 85% of predictions were within 4 meters 
quantile(abs(actualPos - predictedPos)[1,] + abs(actualPos - predictedPos)[2,], probs = .85)

#plot actual and predicted positions on map
plot(x = offline2$posX, y = offline2$posY, pch = 15, cex = .8,
     main = "Actual and Predicted Positions of Online Data",
     xlab = "x position", ylab = "y position")
points(x = predictedPos[1,], y = predictedPos[2,], col = "green")
points(x = actualPos[1,], y = actualPos[2,], col = "red")

save(offline, offline2, online2, 
     mapSignal, bestK, cv180,
     actualPos, predictedPos, L1error,
     file = "ProjectObjects.rda")

