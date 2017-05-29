# clear files
#file.remove(list.files("temp/", full.names = T))
foreach(i = 1:buckets) %do% {
  # take 20 vid each time
  index = 1:20 + 20*(i-1)
  v = vid[index]
  query = paste0("SELECT sale_week, sale_price, vin_decode_id FROM transaction
                 WHERE vin_decode_id IN (", paste(paste0("'", v, "'"),collapse = ","),")")
  trans_data = dbGetQuery(con, query)
  write.table(trans_data, paste0("temp/task_", i, ".txt"), sep="\t", quote = F)
}

# combine them all together
task_files = list.files("temp")
table = NULL
for(t in task_files){
  tab = read.table(paste0("temp/", t), stringsAsFactors = F)
  table = rbind(table, tab)
}

