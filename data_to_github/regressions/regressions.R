library(stargazer)
library(matlib)
library(pracma)
df <- read.csv('../final_data_set/final_data.csv')

df <- subset(df, select = -c(Total.Rain, Total.Snow, Mean.Min.Temp, Mean.Temp))

df.all.area <- as.matrix(df[,c('Mean.Max.Temp','Total.Precip','Tourism.PC','Lightning.Perc','Fire.Area')])
df.all.perc <- as.matrix(df[,c('Mean.Max.Temp','Total.Precip','Tourism.PC','Lightning.Perc','Fire.Area.Perc')])

controls.all <- as.matrix(df[,1:46])

proj.all <- controls.all%*%inv(t(controls.all)%*%controls.all)%*%t(controls.all)

df.proj.all.area <- as.data.frame(df.all.area - proj.all%*%df.all.area)
df.proj.all.perc <- as.data.frame(df.all.perc - proj.all%*%df.all.perc)

all.model.area <- lm(Fire.Area~
                  Bulkley.Nechako...Stikine+Capital+Cariboo+Central.Kootenay+Central.Okanagan+Columbia.Shuswap+Comox...Strathacona+Cowichan.Valley+
                  East.Kootenay+Fraser.Valley+Fraser.Fort.George+Greater.Vancouver+
                  Kitimat.Stikine+Kooteny.Boundary+Mount.Waddington+Nanaimo+North.Okanagan+Okanagan.Similkameen+
                  Squamish.Lillooet+Sunshine.Coast+Thompson.Nicola+
                  X2002+X2003+X2004+X2005+X2006+X2007+X2008+X2009+X2010+X2011+X2012+X2013+X2014+X2015+X2016+X2017+X2018+X2019+X2020+X2021+
                  X5+X6+X7+X8+X9+
                  Mean.Max.Temp+Total.Precip+Tourism.PC+Lightning.Perc,df)

proj.all.model.area <- lm(Fire.Area~
                       Mean.Max.Temp+Total.Precip+Tourism.PC+Lightning.Perc,df.proj.all.area)

all.model.perc <- lm(Fire.Area.Perc~
                       Bulkley.Nechako...Stikine+Capital+Cariboo+Central.Kootenay+Central.Okanagan+Columbia.Shuswap+Comox...Strathacona+Cowichan.Valley+
                       East.Kootenay+Fraser.Valley+Fraser.Fort.George+Greater.Vancouver+
                       Kitimat.Stikine+Kooteny.Boundary+Mount.Waddington+Nanaimo+North.Okanagan+Okanagan.Similkameen+
                       Squamish.Lillooet+Sunshine.Coast+Thompson.Nicola+
                       X2002+X2003+X2004+X2005+X2006+X2007+X2008+X2009+X2010+X2011+X2012+X2013+X2014+X2015+X2016+X2017+X2018+X2019+X2020+X2021+
                       X5+X6+X7+X8+X9+
                       Mean.Max.Temp+Total.Precip+Tourism.PC+Lightning.Perc,df)

proj.all.model.perc <- lm(Fire.Area.Perc~
                       Mean.Max.Temp+Total.Precip+Tourism.PC+Lightning.Perc,df.proj.all.perc)

stargazer(all.model.area, proj.all.model.area, all.model.perc, proj.all.model.perc,
          type = 'html',
          title = 'Wildfires Regressions',
          column.labels = c('All (Fire Area)','Projection Matrix (Fire Area)','All  (Fire %)','Projection Matrix (Fire %)'),
          covariate.labels = c('Bulkley Nechako + Stikine','Capital','Cariboo','Central Kootenay','Central Okanagan','Columbia Shuswap','Comox + Strathacona',
                               'Cowichan Valley','East Kootenay','Fraser Valley','Fraser Fort George','Greater Vancouver',
                               'Kitimat Stikine','Kooteny Boundary','Mount Waddington','Nanaimo','North Okanagan','Okanagan Similkameen',
                               'Squamish Lillooet','Sunshine Coast','Thompson Nicola',
                               '2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021',
                               'May','June','July','August','September',
                               'Avg. Max Temp','Avg. Total Precipitation','Avg. Tourism Tax Per Capita', 'Lightning Percentage'),
          notes.label = 'Significance levels',
          report = 'vcp*',
          style = 'default',
          out = 'output/Wass-Little_Thesis_Regressions_All_variables_w_proj.html')

stargazer(all.model.area, proj.all.model.area, all.model.perc, proj.all.model.perc,
          type = 'html',
          title = 'Wildfires Regressions',
          column.labels = c('All (Fire Area)','Projection Matrix (Fire Area)','All (Fire %)','Projection Matrix (Fire %)'),
          omit = c('Bulkley.Nechako...Stikine','Capital','Cariboo','Central.Kootenay','Central.Okanagan','Columbia.Shuswap','Comox...Strathacona',
                   'Cowichan.Valley','East.Kootenay','Fraser.Valley','Fraser.Fort.George','Greater.Vancouver',
                   'Kitimat.Stikine','Kooteny.Boundary','Mount.Waddington','Nanaimo','North.Okanagan','Okanagan.Similkameen',
                   'Squamish.Lillooet','Sunshine.Coast','Thompson.Nicola',
                   'X2002','X2003','X2004','X2005','X2006','X2007','X2008','X2009','X2010','X2011','X2012','X2013','X2014','X2015','X2016','X2017','X2018','X2019','X2020','X2021',
                   'X5','X6','X7','X8','X9'),
          covariate.labels = c('Avg. Max Temp','Avg. Total Precipitation','Avg. Tourism Tax Per Capita', 'Lightning Percentage'),
          notes.label = 'Significance levels',
          report = 'vcp*',
          style = 'default',
          out = 'output/Wass-Little_Thesis_Regressions_w_proj.html')
