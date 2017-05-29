for(a in annual_files){
  # read file
  df = read.table(paste0("Auction Data/Data Files/", a), sep = "\t", header = T, stringsAsFactors = F, fill = T, quote = "")
  # write into table
  # Note: function dbWriteTable() does not refer to default values for column ID, use dbWriteTable2, a wrapper around dbWriteTable from package "caroline"
  dbWriteTable2(con, table.name = "transaction", df = df,  append = T, row.names = F, add.id = T,  pg.update.seq = T)
  print(paste0("File ", a ," done."))
}

for(w in week_files){
  # read file
  df = read.table(paste0("Auction Data/Data Files/", w), sep = "\t", header = T, stringsAsFactors = F, fill = T, quote = "")
  # write into table
  # Note: function dbWriteTable() does not refer to default values for column ID, use dbWriteTable2, a wrapper around dbWriteTable from package "caroline"
  dbWriteTable2(con, table.name = "transaction", df = df,  append = T, row.names = F, add.id = T,  pg.update.seq = T)
  print(paste0("File ", w ," done."))
}
