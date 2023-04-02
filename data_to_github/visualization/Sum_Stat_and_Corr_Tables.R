library(stargazer)
df <- read.csv('../final_data_set/final_data.csv')

#Summary Stats
df_interest <- df[,c('Mean.Max.Temp','Total.Precip','Tourism.PC','Lightning.Perc','Fire.Area','Fire.Area.Perc')]


stargazer(df_interest,
          title = 'All Region Variables',
          type = 'html',
          covariate.labels = c('Mean Max Temperature (\u00B0C)','Total Precipiation (mm)','Tourism Money Per Capita', 'Lightning Percentage', 'Fire Area (hectares)', 'Fire Area Percentage (%)'),
          summary.stat = c('n', 'min', 'max', 'mean', 'median', 'sd'), 
          out = 'output/tables/summary_statistics.htm')


#df_cor <- df[,c('Central','Coast','Island','Kootenays','Lower.Mainland','Okanagan','Mean.Max.Temp','Total.Precip','Tourism.Money','Fire.Area')]
df_cor <- cor(df_interest)
colnames(df_cor) <- c('Mean Max Temperature (\u00B0C)','Total Precipiation (mm)','Tourism Money Per Capita', 'Lightning Percentage', 'Fire Area (hectares)', 'Fire Area Percentage (%)')
row.names(df_cor) <- c('Mean Max Temperature (\u00B0C)','Total Precipiation (mm)','Tourism Money Per Capita', 'Lightning Percentage', 'Fire Area (hectares)', 'Fire Area Percentage (%)')

#Correlation Tables
stargazer(df_cor,
          title = 'Correlation Table',
          type = 'html',
          out = 'output/tables/correlation_table.htm')

