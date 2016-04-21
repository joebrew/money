library(readr)
library(dplyr)

# Read in BLS data
files <- dir('data')
for (i in 1:length(files)){
  assign(gsub('.', '_', gsub('ap.|.txt', '', files[i]),
              fixed = TRUE),
         read_delim(paste0('data/', 
                           files[i]),
                    delim= '\t'))
}

# Join together
df <- rbind(data_0_Current,
            data_1_HouseholdFuels,
            data_2_Gasoline,
            data_3_Food)
  
# Get item names
df$item_code <- 
  as.character(as.numeric(substr(df$series_id, 4, nchar(df$series_id))))

# Join together
df <- left_join(x = df,
               y = item, 
               by = 'item_code')

# Get rid of non-matchable data
df <- df[!is.na(df$item_name),]

# Get rid of extra columns
df <- df %>%
  dplyr::select(item_name, year, period, value)

# Extract quantity
df$quantity <-
  regmatches(df$item_name, 
             gregexpr("(?<=\\().*?(?=\\))", 
                      df$item_name, 
                      perl=T))[[1]]

# Get everything before "per lb"
df$name <-
  unlist(lapply(strsplit(x = df$item_name,
           split = ', per '),
         function(x){
           x[[1]]
         }))

# Get a dataframe just by items for the most recent year
recent <- df %>%
  filter(year == 2016) %>%
  group_by(name) %>%
  summarise(value = mean(value, na.rm = TRUE))

# Remove the non applicable stuff
recent <- recent[!recent$name %in% 
                   c('Electricity per KWH',
                     'Fuel oil #2 per gallon (3.785 liters)'),]

# Remove weird things from names
recent$name <- 
  gsub(', all sizes|, 33-80 oz. pkg|, any origin|, dry pint|, all sizes',
       '',
       recent$name)

