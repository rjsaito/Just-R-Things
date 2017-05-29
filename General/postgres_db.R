

https://sourceforge.net/projects/postgresqlportable/files/v9.6.1/
  
  
  unzip somewhere

you will find PostgreSQLPortable

launch database

POSTGRESQL Tutorial

## First, to fire-up the server, open the executable "PostgreSQLPortable",
## and create a database and user.

## In the command line, you can use the following commands to:

https://www.kaggle.com/currie32/crimes-in-chicago

dataset is 6 million rows

# Create your Database [testdb]
CREATE DATABASE crime;

# Create your Username [guest]
CREATE USER yourname;



Then go to R

if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(dplyr, devtools, RPostgreSQL, caroline)

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "crime",
                 host = "localhost", port = 5432,
                 user = "yourname", password = "")

files = list.files("C:/Users/rjsai/Downloads/crimes-in-chicago", full.names = T)

dbSendQuery(con, "DROP TABLE chicago_crime")

dbSendQuery(con, "
  CREATE TABLE chicago_crime (
    X integer,
    ID integer, 
    Case_Number varchar(10),
    Date date,
    Block varchar(50),
    IUCR integer,
    Primary_Type varchar(32), 
    Description varchar(100),
    Location_Description varchar(100),
    Arrest varchar(30),
    Domestic varchar(30),
    Beat integer,
    District float,
    Ward float,
    Community_Area float,
    FBI_Code integer,
    X_Coordinate float,
    Y_Coordinate float,
    Year float,
    Updated_On date,
    Latitude float,
    Longitude float,
    Location varchar(29)
  );
")  


for(f in files){
  
  # Read file
  temp = read.csv(f, stringsAsFactors = F)
  
  if(f == "C:/Users/rjsai/Downloads/crimes-in-chicago/Chicago_Crimes_2001_to_2004.csv") temp = temp[-1602848,]

  ## Clean Data
  names(temp) = gsub("\\.", "_", names(temp))

  # Change to Date
  temp$Date = as.POSIXct(temp$Date, format='%m/%d/%Y %H:%M:%S %p')
  temp$Updated_On = as.POSIXct(temp$Updated_On, format='%m/%d/%Y %H:%M:%S %p')
  
  # Change to numeric
  tonumeric = c("IUCR", "Beat", "District", "Ward", "Community_Area", "FBI_Code", "X_Coordinate", "Y_Coordinate", "Year", "Latitude", "Longitude")
  temp[,tonumeric] = apply(temp[,tonumeric], 2, function(x) as.numeric(x))

  # Write to table in DB
  dbWriteTable2(con, table.name = "chicago_crime", df = temp,  append = T, row.names = F)
  
}

s




dbWriteTable2(con, table.name = "transaction", df = df,  append = T, row.names = F, add.id = T,  pg.update.seq = T)
print(paste0("File ", a ," done."))

## Other useful functions within command line
# Change directory to "testdb"
\c crime

# View all tables in current directory (i.e. "testdb")
\dt


## Other commands can be found in these resources:
https://gist.github.com/Kartones/dd3ff5ec5ea238d4c546
