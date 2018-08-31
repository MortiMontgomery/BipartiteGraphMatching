InputIncomings <- read.csv("~/****.csv")
InputBuddys <- read.csv("~/****.csv", header = F)
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
      #testing if match
      if(as.character(inc[incoming,2])==as.character(bud[buddy,3]) | as.character(bud[buddy,3])=="keins" | as.character(inc[incoming,2])==as.character(bud[buddy,4]) | as.character(bud[buddy,4])=="keins" | as.character(inc[incoming,2])==as.character(bud[buddy,5]) | as.character(bud[buddy,5])=="keins" )
         {
          adjMatrix[incoming,buddy]=1 
      }
      buddy=buddy+1
    }
    incoming=incoming+1
    buddy=1
  }

  calcMaxMatch(adjMatrix, inc, bud, mergedMatrix)
  
  
}

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
}
mergingTable = function(solution, inc ,bud, mergedMatrix)
{
  matchIndices=which(solution!=0, arr.ind=T)
  print(matchIndices)
  i=1
 
  for(i in 1:dim(matchIndices)[1])
  {
    temporaryMatch=cbind(inc[matchIndices[i,1],], bud[matchIndices[i,2],])
    tempvector=unlist(temporaryMatch[1,])
    mergedMatrix[nrow(mergedMatrix)+1,]=tempvector
    print(mergedMatrix)
  }
  print("Uebrig gebliebene Incomings:")
  inc=inc[-matchIndices[,1],]
  print(inc)
  if(nrow(inc)==0) 
  {
    print("Zuordnung beendet")
    print(mergedMatrix)
    write.csv(mergedMatrix, file="*****.csv")
    return(mergedMatrix)
  } else {
    makeAdj(inc, bud, mergedMatrix)
  }
  return(mergedMatrix)
  
}

