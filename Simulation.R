# Simulate ma(1) model
#
require( tidyverse )
require( here )


file_path <-
  file.path( here(),
             "data",
             "diabetes.csv" )

Readings <- 
  read_csv( file_path )

Readings %>% 
   summarise( mean_glucose = mean(glucose),
              std_glucose  = sd( glucose ))

theta = -0.5
mu    = 128 /1.5
mu

glucose <- rep(NA, 206 )
glucose[1] <- 128
error <- rnorm( 206, 0, 15 )

 for(i in 2:206 )
 {
   glucose[i] <- 128 + theta * error[i-1] + error[1]
 }
glucose[1] <- 128

glucose <- round(glucose, 0 )

i <- 1:206

Readings <-
  Readings %>% 
  select( date_time,
          description ) %>% 
  add_column( i ) %>%
  add_column( glucose )

Readings

file_path <- 
  file.path( here(),
             "data",
             "Readings.csv")

write_csv( Readings, file_path )

ggplot( data=Readings, aes( x=i, y=glucose )) +
  geom_line()
