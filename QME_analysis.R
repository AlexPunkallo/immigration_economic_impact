### QME FINAL PROJECT ###


#### PART 1 - Introductive Analysis ####

# DATA #

data_it = read.csv('DATASET_IT.csv', sep=';')
data_it = data_it[-19,]

data_uk = read.csv('DATASET_UK.csv', sep=';')

# PLOT #

# Immigrants IT vs UK 
imm = read.csv('migr_imm1ctz_1_Data.csv', header=T)

imm_tot = subset(imm, SEX == "Total" & AGE == 'Total' & CITIZEN == 'Total')
imm_tot$Value = as.numeric(gsub(",","",imm_tot$Value,fixed=TRUE)) 
imm_tot$Value[is.na(imm_tot$Value)] = 523552

imm_it = subset(imm_tot, GEO == 'Italy')
imm_uk = subset(imm_tot, GEO == 'United Kingdom')

imm_it = as.data.frame(imm_it)
imm_uk = as.data.frame(imm_uk)
df = rbind.data.frame(imm_it, imm_uk)

ggplot(df,aes(TIME,Value,fill=GEO))+
  geom_bar(stat="identity",position='dodge') + ggtitle('Immigrants in Italy vs UK')+labs(x='Years', y= 'Number of immigrants') + theme(plot.title = element_text(hjust = 0.5))

# Comparison between population of IT/UK
blue = rgb(0, 0, 1, alpha=0.5)
red = rgb(1, 0, 0, alpha=0.5)

ggplot(data = data_it, aes(x = Year, y = PopCitIT)) + 
  geom_bar(stat = "identity", fill = red, colour = red)+
  geom_bar(data = data_uk, aes(x = Year, y = PopCitUK),
           stat = "identity", fill = blue, colour = blue)

# Foreign population IT & UK
ggplot(data = data_it, aes(x = Year, y = PopCitForeign, fill='red')) + 
  geom_bar(stat = "identity", fill = red, colour = red)+
  geom_bar(data = data_uk, aes(x = Year, y = PopCitForeign),
           stat = "identity", fill = blue, colour = blue)+ ggtitle('Foreign population in Italy and UK')+labs(x='Years', y= 'Foreign population') + theme(plot.title = element_text(hjust = 0.5)) # light blue = UK & purple = IT

# Foreign population wrt total population
ggplot(data = data_it, aes(x = Year, y = PopCitIT)) + 
  geom_bar(stat = "identity", fill = red, colour = red)+
  geom_bar(data = data_it, aes(x = Year, y = PopCitForeign),
           stat = "identity", fill = blue, colour = blue)

ggplot(data = data_uk, aes(x = Year, y = PopCitUK)) + 
  geom_bar(stat = "identity", fill = red, colour = red)+
  geom_bar(data = data_uk, aes(x = Year, y = PopCitForeign),
           stat = "identity", fill = blue, colour = blue)

# Employment Rate Italy VS UK
year = as.numeric(data_it$Year)
emp_it = as.numeric(gsub(",",".",data_it$EmpRate,fixed=TRUE))
emp_uk = as.numeric(gsub(",",".",data_uk$EmpRate,fixed=TRUE))

par(mfrow=c(1,2))
plot(year, emp_it, type = 'l', col='brown1', main= 'Employment rate IT', xlab='Year', ylab='Employment Rate', lwd=2)
plot(year, emp_uk, type = 'l', col='cyan3', main= 'Employment rate UK', xlab='Year', ylab='Employment Rate', lwd=2)

# Annual net earnings for single person
earn_it = as.numeric(gsub(",",".",data_it$AnnNetEarn,fixed=TRUE))
earn_uk = as.numeric(gsub(",",".",data_uk$AnnNetEarn,fixed=TRUE))

par(mfrow=c(1,2))
plot(year, earn_it, type = 'l', col='brown1', main= 'Annual net earnings IT', xlab='Year', ylab='Net earnings', lwd=2)
plot(year, earn_uk, type = 'l', col='cyan3', main= 'Annual net earnings UK', xlab='Year', ylab='Net earnings', lwd=2)

# GDP 
gdp_it = data_it$GDPpers[-c(17,18)]
gdp_uk = data_uk$GDPpers[-c(17,18)]
data_it$GDPpers[c(17,18)] = mean(gdp_it)
data_uk$GDPpers[c(17,18)] = mean(gdp_uk)
year = year[-c(17,18)]
par(mfrow=c(1,2))
plot(year, gdp_it, type = 'l', col='brown1', main= 'GDP per capita Italy', xlab='Year', ylab='GDP per capita', lwd=2)
plot(year, gdp_uk, type = 'l', col='cyan3', main= 'GDP per capita UK', xlab='Year', ylab='GDP per capita', lwd=2)


#### PART 2 - Analysis ####

library(urca)
library(timeSeries)

# UPSAMPLING TRY: transform annual series to quaterly series
#quarter = seq(as.Date("1998-01-01"), as.Date("2015-01-01"), by="quarter")
#create the linear interploated values
#estshare = approx(data_it$AnnNetEarn, n=length(quarter))
#newdf = data.frame(quaters<-quarter, AnnNetEarn<-estshare$y)

#f = approxfun(year, earn_it)
#f(seq(from=1,to=50,by=15))


# Italy part
emp_it = ts(emp_it) # time series transformation
gdp_it = ts(gdp_it)
earn_it = ts(earn_it)
im_it = ts(data_it$Imm)

lemp_it = log(emp_it) # log transformation
lgdp_it = log(gdp_it)
learn_it = log(earn_it)
lim_it = log(im_it)

adf.test(lemp_it)  # test stationarity with ADF test
adf.test(lgdp_it)
adf.test(learn_it)
adf.test(lim_it)

dlemp_it = diff(lemp_it, differences = 3) # differentiation
dlgdp_it = diff(lgdp_it, differences = 3)
dlearn_it = diff(learn_it, differences = 3)
dlim_it = diff(lim_it, differences = 3)

adf.test(dlemp_it) # test ADF on differentiated series
adf.test(dlgdp_it)
adf.test(dlearn_it)
adf.test(dlim_it)

jotest = ca.jo(data.frame(dlemp_it,dlearn_it,dlim_it), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)  # cointegration test

vecm.jo = VECM(data.frame(dlemp_it,dlearn_it,dlim_it), lag=2) # VECM model
summary(vecm.jo)

# regression model
library(boost)

data_it$EmpRate = as.numeric(gsub(",",".",data_it$EmpRate,fixed=TRUE))
data_it$Imm = as.numeric(gsub(",",".",data_it$Imm,fixed=TRUE))
data_it$AnnNetEarn = as.numeric(gsub(",",".",data_it$AnnNetEarn,fixed=TRUE))
data_it$GDPpers = as.numeric(gsub(",",".",data_it$GDPpers,fixed=TRUE))
data_it$MalesImm = as.numeric(gsub(",",".",data_it$MalesImm,fixed=TRUE))
data_it$FemalesImm = as.numeric(gsub(",",".",data_it$FemalesImm,fixed=TRUE))

y = data_it$EmpRate
x1 = data_it$Imm
x2 = data_it$AnnNetEarn
x3 = data_it$GDPpers
form = data_it$EmpRate ~ (data_it$Imm + data_it$AnnNetEarn + data_it$GDPpers)
form = EmpRate ~ (Imm + AnnNetEarn + GDPpers)

#form = data_it$EmpRate ~ (data_it$Imm + data_it$AnnNetEarn + data_it$GDPpers + data_it$MalesImm + data_it$FemalesImm )

var = c('Imm', 'AnnNetEarn', 'GDPpers')
vplot = data_it[var]
plot(vplot)

mod1i = lm(form, data_it)  # outcome = Employment rate
summary(mod1i)

form = data_it$GDPpers ~ (data_it$Imm + data_it$AnnNetEarn + data_it$EmpRate)
form = GDPpers ~ (Imm + AnnNetEarn + EmpRate)

mod2i = lm(form,data = data_it)   # outcome = GDP
summary(mod2i)
plot(data_it$GDPpers~ data_it$Imm, main='Scatterplot', xlab='Immigrants', ylab='GDP per capita')
abline(lm(data_it$GDPpers~ data_it$Imm), col="red")

# specification test for the models
res = residuals(mod2i)  # mean of the error is not significant different from 0
t.test(res)

shap = shapiro.test(res) # normal distribution of the errors
shap
qqnorm(scale(res))
abline(0,1)

library(lmtest)
testbp = bptest(form, data=data_uk)
testbp  # omoschedasticity
dw = dwtest(form, data=data_uk)
dw  # absence of correlation


# UK part
emp_uk = ts(emp_uk) # time series transformation
gdp_uk = ts(gdp_uk)
earn_uk = ts(earn_uk)
im_uk = ts(data_uk$Imm)

lemp_uk = log(emp_uk) # log transformation
lgdp_uk = log(gdp_uk)
learn_uk = log(earn_uk)
lim_uk = log(im_uk)

adf.test(lemp_uk)  # test stationarity with ADF test
adf.test(lgdp_uk)
adf.test(learn_uk)
adf.test(lim_uk)

dlemp_uk = diff(lemp_uk, differences = 6) # differentiation
dlgdp_uk = diff(lgdp_uk, differences = 6)
dlearn_uk = diff(learn_uk, differences = 6)
dlim_uk = diff(lim_uk, differences = 6)

adf.test(dlemp_uk) # test ADF on differentiated series
adf.test(dlgdp_uk)
adf.test(dlearn_uk)
adf.test(dlim_uk)

jotest = ca.jo(data.frame(dlemp_uk,dlearn_uk,dlim_uk), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)  # cointegration test

vecm.jo = VECM(data.frame(dlemp_uk,dlearn_it,dlim_uk), lag=2, estim = 'ML') # VECM model
summary(vecm.jo)


# regression model

library(boost)

data_uk$EmpRate = as.numeric(gsub(",",".",data_uk$EmpRate,fixed=TRUE))
data_uk$Imm = as.numeric(gsub(",",".",data_uk$Imm,fixed=TRUE))
data_uk$AnnNetEarn = as.numeric(gsub(",",".",data_uk$AnnNetEarn,fixed=TRUE))
data_uk$GDPpers = as.numeric(gsub(",",".",data_uk$GDPpers,fixed=TRUE))

y = data_uk$EmpRate
x1 = data_uk$Imm
x2 = data_uk$AnnNetEarn
x3 = data_uk$GDPpers
form = data_uk$EmpRate ~ (data_uk$Imm + data_uk$AnnNetEarn + data_uk$GDPpers)
form = EmpRate ~ (Imm + AnnNetEarn + GDPpers)

var = c('Imm', 'AnnNetEarn', 'GDPpers')
vplot = data_uk[var]
plot(vplot)

mod1u = lm(form, data_uk)  # outcome = Employment rate
summary(mod1u)

plot(data_uk$EmpRate~ data_uk$Imm, main='Scatterplot', xlab='Immigrants', ylab='Employment rate')
abline(lm(data_uk$EmpRate~ data_uk$Imm), col="red")

form = data_uk$GDPpers ~ (data_uk$Imm + data_uk$AnnNetEarn + data_uk$EmpRate)
form = GDPpers ~ (Imm + AnnNetEarn + EmpRate)

mod2u = lm(form,data = data_uk)   # outcome = GDP
summary(mod2u)

# specification test for the models
res = residuals(mod2u)  # mean of the error is not significant different from 0
t.test(res)

shap = shapiro.test(res) # normal distribution of the errors
shap
qqnorm(scale(res))
abline(0,1)

library(lmtest)
testbp = bptest(form, data=data_uk)
testbp  # omoschedasticity
dw = dwtest(form, data=data_uk)
dw  # absence of correlation
