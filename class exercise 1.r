# R1
install.packages("data.table")
install.packages("datasets")
prof <- women   #Copying the data frame women into another dataset so that the original fields are not afected
View(prof)

id <- 0
for(i in 1:nrow(prof))
{
  if
  (prof$height[i]>65.0 & prof$weight[i]<136.7)
  {
  id<- id+1
 }
} 
print("Number of women satisfying the criteria are")
print(id)

#Rough Space
prof$height
nrow(prof)
