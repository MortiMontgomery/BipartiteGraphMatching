InputIncomings <- read.csv("~/Faranto/IncomingTest.csv")
InputBuddys <- read.csv("~/Faranto/BuddyTest.csv", header = F)
mergedMatrix=data.frame(IName=character(), ILand=character(),IeMail=character(), BName=character(), BeMail=character(), Bland1=character(),Bland2=character(),Bland3=character(),stringsAsFactors = F)

test1 = makeAdj(InputIncomings, InputBuddys, mergedMatrix)
#erstellt adjazenzmatrix
makeAdj=function(inc, bud, mergedMatrix)
{
  
  #leere Matrix mit 0en
  adjMatrix= matrix(0, nrow = nrow(inc), ncol = nrow(bud))
  incoming=1
  buddy=1
  
  while(incoming<=nrow(inc))
  {

    while(buddy<=nrow(bud))
    {
      #prüfen der Länderspalten der Buddys mit Länderspalte Incoming
      if(as.character(inc[incoming,2])==as.character(bud[buddy,3]) | as.character(bud[buddy,3])=="keins" | as.character(inc[incoming,2])==as.character(bud[buddy,4]) | as.character(bud[buddy,4])=="keins" | as.character(inc[incoming,2])==as.character(bud[buddy,5]) | as.character(bud[buddy,5])=="keins" )
         {
          adjMatrix[incoming,buddy]=1 
      }
      buddy=buddy+1
    }
    incoming=incoming+1
    buddy=1
  }
  #calcMaxMatch(adjMatrix, inc, bud)
  print("Calculated Adjacency ;Ataa")
  print(adjMatrix)
  calcMaxMatch(adjMatrix, inc, bud, mergedMatrix)
  
  #return(adjMatrix)
  
}
#IncomingTest <- read.csv("~/Faranto/IncomingTest.csv")
#BuddyTest <- read.csv("~/Faranto/BuddyTest.csv")
#makeAdj(IncomingTest, BuddyTest)
#calcMaxMatch(makeAdj(IncomingTest, BuddyTest))
#which(sol !=0, arr.ind = T)
calcMaxMatch = function(adjMatrix, inc ,bud, mergedMatrix)
{
  dummyColumns=c()
  dummyRows=c()
  dummyVector=integer(max(dim(adjMatrix)))
  while(dim(adjMatrix)[1]<dim(adjMatrix)[2])
  {
    dummyRows=c(dummyRows,c(dim(adjMatrix)[1]+1))
    adjMatrix=rbind(adjMatrix, dummyVector)
  }
  while(dim(adjMatrix)[2]<dim(adjMatrix)[1])
  {
    #dummyColumns=c(dummyRows,c(dim(adjMatrix)[2]+1))
    dummyColumns=c(dummyColumns,dim(adjMatrix)[2]+1)
    adjMatrix=cbind(adjMatrix, dummyVector)
  }
  res <- lp.assign(adjMatrix, "max")
  solution <- res$solution
  print("Zuordnung berechnet:")
  print(solution)
  if(length(dummyRows)>0)
  {
    solution <- solution[-dummyRows,]
  }
  if(length(dummyColumns)>0)
  {
    solution <- solution[,-dummyColumns]
  }
  print("Dummy Variablen geloescht:")
  print(solution)
  mergingTable(solution, inc, bud, mergedMatrix)
  #return(solution)
}
mergingTable = function(solution, inc ,bud, mergedMatrix)
{
  #apply(solution, 1, function(x) inc[which(solution!=0, arr.ind=T)[x],] bud[which(solution!=0, arr.ind=T)[x],])
  #print(which(solution!=0, arr.ind=T))
  print(solution)
  matchIndices=which(solution!=0, arr.ind=T)
  print(matchIndices)
  #print(inc[which(sol!=0, arr.ind=T)[1],])
  i=1
  #"Name", "Land","eMail", "BName", "BeMail", "land1","land2","land3",
  #mergedMatrix=data.frame("Name", "Land","eMail", "BName", "BeMail", "land1","land2","land3",stringsAsFactors = F)

  for(i in 1:dim(matchIndices)[1])
  {
    #print(inc[matchIndices[i,1],])
    print(matchIndices)
    temporaryMatch=cbind(inc[matchIndices[i,1],], bud[matchIndices[i,2],])
    #temporaryMatch=inc[matchIndices[i,1],]
    tempvector=unlist(temporaryMatch[1,])
    print(tempvector)
    #mergedMatrix[i,]=c(temporaryMatch[1],temporaryMatch[2],temporaryMatch[3], temporaryMatch[4], temporaryMatch[5], temporaryMatch[6],temporaryMatch[7],temporaryMatch[8])
    mergedMatrix[nrow(mergedMatrix)+1,]=tempvector
    print("MERGED////////////////////////////////////////////")
    print(mergedMatrix)
   # mergedMatrix[nrow(mergedMatrix)+1,] <- temporaryMatch
    #print(bud[which(solution!=0, arr.ind=T)[i,2],])
  }
  print(mergedMatrix)
  ########################TODO
  print("Uebrig gebliebene Incomings:")
  #solution=solution[-matchIndices[,1],]
  inc=inc[-matchIndices[,1],]
  print(inc)
  if(nrow(inc)==0) 
  {
    print("Zuordnung beendet")
    print(mergedMatrix)
    write.csv(mergedMatrix, file="BuddyIncomingZuordnung.csv")
    return(mergedMatrix)
  } else {
    makeAdj(inc, bud, mergedMatrix)
  }
  return(mergedMatrix)
  
}

