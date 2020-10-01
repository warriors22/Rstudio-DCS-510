setwd('~/DCS510')
getwd()
library(tidyverse)
library(modelr)
sim2 %>%ggplot(aes(x,y))+
  geom_point(alpha = 0.2, color = "blue")
model_2 <- lm(y~x, data=sim2)
summary(model_2)
#NHST = Null Hypothesis Significance Testing
(sim2_1 <- sim2%>%add_predictions(model_2)%>%add_residuals(model_2))

#Tsting for normality using residuals
sim2_1%>%ggplot(aes(x))+
  geom_point(aes(y = y), color = 'blue', alpha = 0.2)+
  geom_point(aes(y = pred), color = 'red', size = 4)
sim2_1%>%ggplot(aes(resid))+
  geom_freqpoly(binwidth = 0.5)
sim2_1%>%ggplot(aes(resid))+
  geom_histogram(bins = 13)
sim3
ggplot(sim3, aes(x1,y))+
  geom_point(aes(color=x2), alpha = 0.6)
model_3 <- lm(y~x1 + x2, data=sim3)
model_4 <- lm(y~x1 * x2, data=sim3)
# y~ x1 + x2 means y = a0 + a1x1 + a2x2
summary(model_3)
#y~x1*x2 means y = a0 +a1x1 + a2x2 +a3 x1x2
summary(model_4)
#y~sqrt(x1) + x2 means y = a0 + a1x1raised to the 1/2 + a2x2
(grid_points <- sim3%>%
    data_grid(x1, x2)%>%
    gather_predictions(model_3, model_4))
(sim3_1 <- sim3%>%
  gather_predictions(model_3, model_4)%>%
  gather_residuals(model_3, model_4))
sim3_1%>%ggplot(aes(x1, y, color = x2))+
  geom_point()+
  geom_line(data=grid_points, aes(y = pred))+
  facet_wrap(~model)
sim3_1%>%ggplot(aes(x = pred, y = resid, color = model))+
  geom_point()+
  facet_grid(~model)
sim3_1%>%ggplot(aes(x1, resid, color=x2))+
  geom_point()+
  facet_grid(model~x2)
AIC(model_3, model_4)
BIC(model_3, model_4)
sim4[40:45,]
library(GGally)
model_5 <- lm(y~x1+x2, data = sim4)
model_6 <- lm(y~x1*x2, data=sim4)
summary(model_5)
summary(model_6)
(sim4_1 <- sim4%>%gather_predictions(model_5, model_6)%>%gather_residuals(model_5, model_6))
sim4_1%>%ggplot(aes(x = pred, y=resid, color=model))+geom_point()+facet_grid(~model)
AIC(model_5, model_6)
BIC(model_5, model_6)
ggplot(diamonds, aes(cut, price))+geom_boxplot()
ggplot(diamonds, aes(color, price))+geom_boxplot()
?diamonds
ggplot(diamonds, aes(clarity, price))+geom_boxplot()
ggplot(diamonds, aes(carat, price))+geom_hex(bins=50)
ggplot(diamonds, aes(carat))+
  geom_histogram(bins=50)
diamonds%>%summarise(prop_lt_25 = mean(carat<=2.5, na.rm=TRUE))
diamonds2 <- diamonds%>%
  filter(carat<= 2.5)%>%
  mutate(lprice = log2(price), lcarat=log2(carat))
ggplot(diamonds2, aes(lcarat, lprice))+geom_hex(bins=50)
mod_diamond1 <- lm(lprice~lcarat, data=diamonds2)
mod_diamond2 <- lm(lprice~lcarat+color+cut+clarity, data=diamonds2)
summary(mod_diamond1)
summary(mod_diamond2)
AIC(mod_diamond1, mod_diamond2)
BIC(mod_diamond1, mod_diamond2)
grid <- diamonds2%>%
  data_grid(cut, model = mod_diamond2)%>%
  add_predictions(mod_diamond2)
(diamonds2 <- diamonds2%>%
  gather_predictions(mod_diamond1, mod_diamond2)%>%
  gather_residuals(mod_diamond1, mod_diamond2))
diamonds2%>%ggplot(aes(x = pred, y = resid, color = model))+
  geom_point(alpha = 0.3)+
  facet_grid(~model)
diamonds2%>%
  filter(model=='mod_diamond2', abs(resid)>1)%>%
  mutate(pred=round(2^pred))%>%
  select(price, pred, carat:table, x:z)%>%
  arrange(price)
library(lubridate)
(daily <- flights%>%mutate(date=make_date(year, month, day))%>%
    group_by(date)%>%
    summarise(n = n()))
install.packages('blogdown')
library(blogdown)
library(nycflights13)
ggplot(daily, aes(date, n))+
  geom_line()
(daily <- daily%>%mutate(wday = wday(date, label = TRUE)))
ggplot(daily, aes(wday, n))+
  geom_boxplot()
mod <- lm(n~wday, data=daily)
(grid <- daily%>%
    data_grid(wday)%>%
    add_predictions(mod, 'n'))
ggplot(daily, aes(wday, n))+
  geom_boxplot()+
  geom_point(data = grid, color = 'red', size = 4)
(daily <- daily%>%add_residuals(mod))
daily%>%ggplot(aes(date, resid))+geom_ref_line(h=0)+
  geom_line()
ggplot(daily, aes(date, resid, color = wday))+
  geom_ref_line(h = 0)+
  geom_line()
daily%>%
  filter(resid < -100)
daily%>%
  ggplot(aes(date, resid))+
  geom_ref_line(h=0)+
  geom_line(color = 'grey50')+
  geom_smooth(se = FALSE, span = 0.20)
daily%>%filter(wday=='Sat')%>%
  ggplot(aes(date, n))+geom_point()+geom_line()+
  scale_x_date(NULL, date_breaks = '1 month', date_labels ='%b' )
term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") ) }
(daily <- daily%>%
    mutate(term = term(date)))
daily%>%filter(wday=='Sat')%>%
  ggplot(aes(date, n, color = term))+
  geom_point(alpha = 1/3)+
  geom_line()+
  scale_x_date(NULL, date_breaks = '1 month', date_labels = '%b')
ggplot(aes(wday, n, color = term))+geom_boxplot()
mod1 <- lm(n~wday, data=daily)
mod2 <- lm(n~wday*term, data = daily)
daily%>%
  gather_residuals(without_term = mod1, with_term = mod2)%>%
  ggplot(aes(date, resid, color = model))+
  geom_line(alpha = 0.75)
(daily <- daily%>%
    gather_predictions(mod1, mod2)%>%
    gather_residuals(mod1, mod2))
daily%>%ggplot(aes(x = pred, y = resid, color=model))+geom_point(alpha=0.3)+facet_grid(~model)
AIC(mod1, mod2)
BIC(mod1, mod2)
(grid_points <- daily%>%filter(model=='mod2')%>%data_grid(wday, term)%>%add_predictions(mod2, 'n'))
ggplot(daily, aes(wday, n))+geom_boxplot()+geom_point(data=grid_points, color = 'red')+facet_wrap(~term)
summary(mod1)

