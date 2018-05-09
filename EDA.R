
#####################
# EDA


aggregates <- df_all_years %>%
  group_by(lost_money, prosper_rating) %>%
  summarise(count = n()
            ,median_amt_borrowed = median(amount_borrowed)
            ,total_paid = round(median(total_paid))
            ,total_fees_paid = round(median(total_fees_paid))
            ,median_principal_paid = round(median(principal_paid))
            ,median_service_fees = round(median(service_fees_paid)))

aggregates %>% arrange(prosper_rating, lost_money) %>% kable(align=c(rep('c', ncol(.))))


## FOLLOW-UP: why do we notice the below (those row counts should be zero)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Notice we have some "COMPLETED" with payments due....
unique(filter(df_all_years_raw, next_payment_due_amount != 0)$loan_status_description)
have_payment_due <- filter(df_all_years_raw, next_payment_due_amount != 0)
glimpse(filter(have_payment_due, loan_status_description == 'COMPLETED'))
arrange(count(have_payment_due, loan_status_description), desc(n))


# And we have some "CURRENT" with no payments due...
unique(filter(df_all_years_raw, next_payment_due_amount == 0)$loan_status_description)
have_NO_payment_due <- filter(df_all_years_raw, next_payment_due_amount == 0)
glimpse(filter(have_NO_payment_due, loan_status_description == 'CURRENT'))
arrange(count(have_NO_payment_due, loan_status_description), desc(n))


# Counts by YEAR
df_all_years %>% 
  ggplot() +
  geom_bar(aes(year, fill = lost_money))

df_all_years %>% 
  group_by(year, lost_money) %>% 
  summarise(count = n()) %>%
  spread(key = year, value = count) %>% 
  kable(align=c(rep('c', ncol(.))))


# Counts by MONTH
df_all_years %>% 
  ggplot() +
  geom_bar(aes(month_of_origination, fill = lost_money))

df_all_years %>% 
  group_by(month_of_origination, lost_money) %>% 
  summarise(count = n()) %>% 
  spread(key = month_of_origination, value = count) %>% 
  kable(align=c(rep('c', ncol(.))))


# Counts by TERM
df_all_years %>% 
  ggplot() +
  geom_bar(aes(term, fill = lost_money))

df_all_years %>% 
  group_by(term, lost_money) %>% 
  summarise(count = n()) %>% 
  spread(key = term, value = count) %>% 
  kable(align=c(rep('c', ncol(.))))


# Plots AMOUNT BORROWED by rating
# Clearly the more you lend, the more likely you'll lose money
ggplot(df_all_years) +
  geom_histogram(aes(amount_borrowed, binwidth = 20000), fill = "black") +
  facet_grid(lost_money ~ prosper_rating) +
  scale_x_continuous(labels = c())

# ggplot(df_all_years, aes(lost_money, amount_borrowed)) +
#   geom_boxplot() +
#   facet_wrap(~ prosper_rating)

ggplot(df_all_years, aes(prosper_rating, amount_borrowed)) +
  geom_boxplot() +
  facet_wrap(~ lost_money)


# Plots BORROWER RATE by rating
# Unfortunately, the higher the rate, the higher the chances you'll lose money 
ggplot(df_all_years) +
  geom_histogram(aes(borrower_rate), fill = "black") +
  facet_grid(lost_money ~ prosper_rating)

# ggplot(df_all_years, aes(lost_money, borrower_rate)) +
#   geom_boxplot() +
#   facet_wrap(~ prosper_rating)

ggplot(df_all_years, aes(prosper_rating, borrower_rate)) +
  geom_boxplot() +
  facet_wrap(~ lost_money)


# Plots TOTAL FEES PAID by rating
# Loans which lose money also have higher fees (perhaps driven by higher loan amounts?)...
ggplot(df_all_years) +
  geom_histogram(aes(total_fees_paid), fill = "black") +
  facet_grid(lost_money ~ prosper_rating) +
  scale_x_continuous(labels = c())

# ggplot(df_all_years, aes(lost_money, total_fees_paid)) +
#   geom_boxplot() +
#   facet_wrap(~ prosper_rating)

ggplot(df_all_years, aes(prosper_rating, total_fees_paid)) +
  geom_boxplot() +
  facet_wrap(~ lost_money)