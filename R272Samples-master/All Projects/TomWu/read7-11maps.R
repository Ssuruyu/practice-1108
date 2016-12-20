AreaN = 12
stores = data.frame()
for(fid in 1:AreaN)
{
  filename = paste(fid, '.csv', sep='')
  source = paste('./7-11maps/', filename, sep='')
  storesTemp = read.csv(source)
  storesTemp = merge(storesTemp[,1:2], rep(fid, length(storesTemp[,1])))
  names(storesTemp) = c("lan", "lat", "dno")
  stores = rbind(stores, storesTemp)
}