library(RColorBrewer)
library(tidyverse)
library(gsheet)
library(yaml)
today <- Sys.Date() +1
creds <- yaml::yaml.load_file('credentials.yaml')
google_url <- creds$google_url

original_df <- df <- gsheet::gsheet2tbl(google_url)

# Get currency
Quandl::Quandl.auth(creds$quandl_api_key)
fx <- Quandl::Quandl('ECB/EURUSD')
original_bx <- bx <- Quandl::Quandl('BITSTAMP/USD')
ex <- Quandl::Quandl('BITFINEX/ETHUSD')



fx <- fx %>% dplyr::select(Date, Rate = Value)
names(fx) <- c('date', 'eur')
fx <- fx %>% arrange(date)
left <- data_frame(date = seq(min(fx$date),
                              today,
                              by = 1))
fx <- left_join(left, fx, by = 'date')
fx <- fx %>% arrange(date)
fx <- fx %>% tidyr::fill(eur, .direction = 'down')

bx <- bx %>% dplyr::select(date = Date, btc = Last)
left <- data_frame(date = seq(min(bx$date),
                              today,
                              by = 1))
bx <- left_join(left, bx, by = 'date')
bx <- bx %>% arrange(date)
bx <- bx %>% tidyr::fill(btc, .direction = 'down')

ex <- ex %>% dplyr::select(date = Date, eth = Last)
left <- data_frame(date = seq(min(ex$date),
                              today,
                              by = 1))
ex <- left_join(left, ex, by = 'date')
ex <- ex %>% arrange(date)
ex <- ex %>% tidyr::fill(eth, .direction = 'down')

# Convert everything to usd
df <- left_join(df, bx)
df <- left_join(df, fx)
df <- left_join(df, ex)
eur_to_usd_columns <- c('caixa',
                        'wise_business_eur',
                        'wise_personal_eur',
                        'cash_eur',
                        'debt_eur',
                        'coinbase_eur')
for(this_column in eur_to_usd_columns){
  df[,this_column] <- df[,this_column] * df$eur
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
cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, 'Set1'))(length(unique(long$grp)))
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
                    values = cols) +
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
  geom_point(size = 1, alpha = 0.5) +
  geom_line() +
  geom_text(aes(label = round(usd)),
             color = 'black',
            angle = 90,
            nudge_y = 400,
            size = 3) +
  theme_bw() +
  labs(title = the_title)
total

# Crypto purchases
crypto_purchases <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1uQQqM9odKqLf_o6oJsK_-JNSOu34yebTfZ1fYzp1Es8/edit#gid=1674910204')
crypto_purchases_pro <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1uQQqM9odKqLf_o6oJsK_-JNSOu34yebTfZ1fYzp1Es8/edit#gid=1497580685')

# buys only
crypto_purchases <- crypto_purchases %>% filter(`Transaction Type` == 'Buy')
x <- crypto_purchases_pro %>%
  filter(type %in% c('match', 'fee')) %>%
  group_by(date = as.Date(time), 
           `trade id`) %>%
  summarise(euros_spent = abs(sum(amount[`amount/balance unit` == 'EUR' & type == 'match'])),
            btc = sum(amount[`amount/balance unit` == 'BTC' & type == 'match']),
            eth = sum(amount[`amount/balance unit` == 'ETH' & type == 'match']),
            fee = abs(sum(amount[type == 'fee']))) %>%
  left_join(fx) %>%
  mutate(usd_spent = euros_spent * eur,
         fees_spent = fee * eur)

# Get total amount spent
crypto_purchases_summary <- crypto_purchases %>%
  summarise(usd = sum(`USD Subtotal`),
            usd_with_fees = sum(`USD Total (inclusive of fees)`,
                                fees = sum(`USD Fees`)),
            usd_spent_on_btc = sum(`USD Total (inclusive of fees)`[Asset == 'BTC']),
            usd_spent_on_eth = sum(`USD Total (inclusive of fees)`[Asset == 'ETH'])) %>%
  bind_rows(
    x %>% ungroup %>%
      summarise(usd = sum(usd_spent),
                usd_with_fees = sum(fees_spent + usd_spent),
                usd_spent_on_btc = sum(usd_spent[btc > 0]),
                usd_spent_on_eth = sum(usd_spent[eth > 0]))
  ) %>%
  summarise(usd = sum(usd),
            usd_with_fees = sum(usd_with_fees),
            usd_spent_on_btc = sum(usd_spent_on_btc),
            usd_spent_on_eth = sum(usd_spent_on_eth),)



# Crypto only
crypto <- long %>%
  filter(grp %in% c('btc', 'eth'))

crypto_title <- crypto %>% ungroup %>% dplyr::filter(date == max(date)) %>%
  summarise(assets = sum(usd[usd > 0])) %>%
  mutate(x =  paste0(scales::comma(round(assets)), ' USD in crypto')) %>%
  .$x



crypto_title <- paste0(crypto_title, ' (originally purchased:',
                       round(crypto_purchases_summary$usd),
                       ', ',
                       round(crypto_purchases_summary$usd_with_fees), ' with fees)')
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

ggplot(data = crypto,
       aes(x = date,
           y = usd,
           group = grp,
           color = grp)) +
  geom_line() +
  facet_wrap(~grp) +
  scale_color_manual(name = '',
                    values = RColorBrewer::brewer.pal(n = length(unique(crypto$grp)), 'Set1')) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = 'Date',
       y = 'USD',
       title = crypto_title)

crypto %>% tidyr::spread(key = grp, value = usd)

crypto_agg <- crypto %>%
  group_by(date) %>%
  summarise(usd = sum(usd))
tail(crypto_agg)
  

# Get total amount hodled
hodl <- original_df %>% 
  filter(date == max(date)) %>%
  summarise(
    date = date,
    btc_hodl = btc_coinbase + btc_trezor,
    eth_hodl = eth_coinbase + eth_trezor
  )
right <- df %>%
  filter(date == hodl$date) %>%
  dplyr::select(date, eth, btc)
hodl <- left_join(hodl, right) %>%
  mutate(btc_value = btc_hodl * btc,
         eth_value = eth_hodl * eth) %>%
  mutate(btc_spent = crypto_purchases_summary$usd_spent_on_btc) %>%
  mutate(eth_spent = crypto_purchases_summary$usd_spent_on_eth)
hodl_text <- paste0(
  'Spent ', round(hodl$eth_spent + hodl$btc_spent, digits = 2) , ' USD on crypto. Now worth: ',
  round(hodl$eth_value + hodl$btc_value, digits = 2), ' USD (',
  round(((hodl$eth_value + hodl$btc_value) / (hodl$eth_spent + hodl$btc_spent) * 100) - 100, digits = 2),
  '%)\n',
  '*Total gain/loss of: ',
  round(hodl$eth_value + hodl$btc_value - hodl$eth_spent - hodl$btc_spent), ' USD\n',
  
  '---Spent ', round(hodl$btc_spent, digits = 2), ' USD on BTC. Now worth: ', round(hodl$btc_value, digits = 2), ' (',
  round((hodl$btc_value / hodl$btc_spent * 100) - 100, digits = 2), '%).\n',
  '------Bought at an average price of ', round(hodl$btc_spent / hodl$btc_hodl, digits = 2), ' USD per BTC. Now trading at ',  hodl$btc, '.\n',
  '---Spent ', round(hodl$eth_spent, digits = 2), ' USD on ETH Now worth: ', round(hodl$eth_value, digits = 2), ' (',
  round((hodl$eth_value / hodl$eth_spent * 100) - 100, digits = 2), '%).\n',
  '------Bought at an average price of ', round(hodl$eth_spent / hodl$eth_hodl, digits = 2), ' USD per ETH Now trading at ',  hodl$eth, '.\n'
)
cat(hodl_text)

