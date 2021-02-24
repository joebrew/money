library(RColorBrewer)
library(tidyverse)
library(gsheet)
library(yaml)

creds <- yaml::yaml.load_file('credentials.yaml')
google_url <- creds$google_url

df <- gsheet::gsheet2tbl(google_url)

# Get currency
Quandl::Quandl.auth(creds$quandl_api_key)
fx <- Quandl::Quandl('CURRFX/USDEUR')
bx <- Quandl::Quandl('BITSTAMP/USD')
ex <- Quandl::Quandl('BITFINEX/ETHUSD')

fx <- fx %>% dplyr::select(Date, Rate)
names(fx) <- c('date', 'eur')
fx <- fx %>% arrange(date)
left <- data_frame(date = seq(min(fx$date),
                              Sys.Date(),
                              by = 1))
fx <- left_join(left, fx, by = 'date')
fx <- fx %>% arrange(date)
fx <- fx %>% tidyr::fill(eur, .direction = 'down')

bx <- bx %>% dplyr::select(date = Date, btc = Last)
left <- data_frame(date = seq(min(bx$date),
                              Sys.Date(),
                              by = 1))
bx <- left_join(left, bx, by = 'date')
bx <- bx %>% arrange(date)
bx <- bx %>% tidyr::fill(btc, .direction = 'down')

ex <- ex %>% dplyr::select(date = Date, eth = Last)
left <- data_frame(date = seq(min(ex$date),
                              Sys.Date(),
                              by = 1))
ex <- left_join(left, ex, by = 'date')
ex <- ex %>% arrange(date)
ex <- ex %>% tidyr::fill(eth, .direction = 'down')

# Convert everything to usd
df <- left_join(df, bx)
df <- left_join(df, fx)
df <- left_join(df, ex)
eur_to_usd_columns <- c('caixa',
                        'transferwise_business_eur',
                        'transferwise_personal_eur',
                        'cash_eur',
                        'debt_eur',
                        'coinbase_eur')
for(this_column in eur_to_usd_columns){
  df[,this_column] <- df[,this_column] / df$eur
}
btc_to_usd_columns <- c('btc_trezor', 'btc_coinbase')
for(this_column in btc_to_usd_columns){
  df[,this_column] <- df[,this_column] * df$btc
}
eth_to_usd_columns <- c('eth_trezor', 'eth_coinbase')
for(this_column in eth_to_usd_columns){
  df[,this_column] <- df[,this_column] * df$eth
}
# Now everything is in US

# Make data long
long <- df %>%
  dplyr::select(date:debt_eur) %>%
  tidyr::gather(key, 
                value,
                campus_checking:debt_eur) %>%
  mutate(grp = unlist(lapply(strsplit(key, '_'), function(x){x[1]}))) %>%
  group_by(date, grp) %>%
  summarise(usd = sum(value)) 

the_title <- long %>% ungroup %>% dplyr::filter(date == max(date)) %>%
  summarise(assets = sum(usd[usd > 0]),
            debt = sum(usd[usd < 0])) %>%
  mutate(net = assets + debt) %>%
  mutate(x = paste0(scales::comma(round(net)), ' USD net (',
                    scales::comma(round(assets)), ' USD in assets, ',
                    scales::comma(round(debt)), ' USD in debt)')) %>%
  .$x

long$grp <- factor(long$grp, 
                   levels = rev(unique(c('btc', 'eth',
                                     'stocks', 'campus', long$grp))))

ggplot(data = long,
       aes(x = date,
           y = usd,
           group = grp,
           fill = grp)) +
  geom_area(position = 'stack') +
  # geom_bar(position = position_stack(),
  #          stat = 'identity',
  #          width = 1) +
  scale_fill_manual(name = '',
                    values = RColorBrewer::brewer.pal(n = length(unique(long$grp)), 'Set1')) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'Date',
       y = 'USD',
       title = the_title)

total <- long %>%
  group_by(date) %>%
  summarise(usd = sum(usd))
ggplot(data = total,
       aes(x = date,
           y = usd)) +
  geom_point(size = 15, alpha = 0.5) +
  geom_line() +
  geom_text(aes(label = round(usd)),
             color = 'white',
            angle = 90) +
  theme_bw()
total

# Crypto only
crypto <- long %>%
  filter(grp %in% c('btc', 'eth'))

crypto_title <- crypto %>% ungroup %>% dplyr::filter(date == max(date)) %>%
  summarise(assets = sum(usd[usd > 0])) %>%
  mutate(x =  paste0(scales::comma(round(assets)), ' USD in crypto')) %>%
  .$x
# crypto_min <- crypto %>% ungroup %>% group_by(grp) %>%
#   dplyr::filter(date == min(date[usd > 0])) %>%
#   summarise(assets = sum(usd[usd > 0])) %>%
#   mutate(x =  paste0('(', scales::comma(round(assets)), ' USD purchased)')) %>%
#   .$x
# crypto_title <- paste0(crypto_title, ' ', crypto_min)
ggplot(data = crypto,
       aes(x = date,
           y = usd,
           group = grp,
           fill = grp)) +
  geom_area(position = 'stack') +
  scale_fill_manual(name = '',
                    values = RColorBrewer::brewer.pal(n = length(unique(crypto$grp)), 'Set1')) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'Date',
       y = 'USD',
       title = crypto_title)

crypto_agg <- crypto %>%
  group_by(date) %>%
  summarise(usd = sum(usd))
tail(crypto_agg)
