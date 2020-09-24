getwd()
library(tidyverse)
library(nycflights13)
ggplot(data=faithful, mapping=aes(x=eruptions))+geom_freqpoly(binwidth = 0.25)
ggplot(faithful, aes(eruptions))+geom_freqpoly(binwidth = 0.25)
diamonds%>%
  count(cut, clarity)%>%
  ggplot(aes(clarity, cut, fill=n))+
  geom_tile()
install.packages("GGally")
library(GGally)
ggpairs(iris)
install.packages("rmarkdown")
library(rmarkdown)
class(iris)
as_tibble(iris)
tibble(x = 1:5, y = 1, z = x^2 +y)
(tb <- tibble(':)' = "smile", ' ' = "space", '2000' = "number"))
tribble(~x, ~y, ~z, #column headings!
        "a", 2, 3.6,
        "b", 1, 8.5)

df <- tibble(
  x = runif(5), 
  y = rnorm(5))
df
df$x
df[["x"]]
df[[1]]
df%>%.$x
df%>%.[["x"]]
df$y
df[["y"]]
df[[2]]
df%>%.$y
df%>%.[["y"]]
class(as.data.frame(df))
read_csv("a,b,c
         1,2,3
         4,5,6")
read_csv("The first line of metadata
         The second line of metadata
         x,y,z
         1,2,3, skip = 2")
read_csv("#A comment I want to skip
         x,y,z
         1,2,3", comment = "#")
read_csv("1,2,3\n4,5,6", col_names=FALSE)
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
read_csv("a,b,c\n1,2,.", na = ".")
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))
parse_integer(c("1", "231", ".", "456"), na = ".")
x <- parse_integer(c("123", "345", "abc", "123.45"))
problems(x)
parse_double("1.23")
parse_double("1.23", locale = (decimal_mark = ","))
parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45 and $45")
parse_number("$123,456,789")
parse_number("123.456.789", locale = locale(grouping_mark = "."))
parse_number("123'456'789", locale = locale(grouping_mark = "'"))
charToRaw("Carolina University")
charToRaw("Josh")
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = )
guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))
day_of_week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
parse_factor(c("Monday", "Labor Day"), levels=day_of_week)
parse_datetime("2020-09-21T18:30")
parse_datetime("20200921")
parse_date("2020-09-21");library(hms)
parse_date("09/12/20", "%d/%m/%y")
parse_date("21 février 2022", "%d %B %Y", locale = locale("fr"))
date_names_langs()
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(x = col_double(), y = col_date()))
tail(challenge)
challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
tail(challenge2)
(df <- tribble(
  ~x, ~y, 
  "1", "1.21", 
  "2", "2.32", 
  "3", "4.56"))
))
type_convert(df)
challenge2 <- read_csv(readr_example("challenge.csv"), col_types = cols(.default = col_character()))
tail(type_convert(challenge2))
tail(challenge2)
library(modelr)
options(na.action = na.warn)
ggplot(sim1, aes(x,y))+geom_point()
as_tibble(sim1)
ggplot(sim1, aes(x,y))+
  geom_point()+
  geom_smooth(method=lm, color="red", se=FALSE)
model_1 <- lm(y~x, data=sim1)
summary(model_1)
ggplot(sim1, aes(x,y))+
  geom_point()+
  geom_abline(intercept=model_1$coefficients[1], slope=model_1$coefficients[2], color="red")
(sim1_1 <- sim1%>% add_predictions(model_1)%>% add_residuals(model_1))
sim1_1%>%ggplot(aes(resid))+geom_freqpoly((binwidth= 0.5))
View(sim1)
sim1_1%>%ggplot(aes(x = pred, y = resid))+ geom_point()
sim2%>%ggplot(aes(x,y))+ geom_point(alpha=0.2, color="blue")
