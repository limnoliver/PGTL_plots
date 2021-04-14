model_colors <-  c(
  # setNames(RColorBrewer::brewer.pal(4, 'Dark2'), c('PB-MTL','PG-MTL','PG-MTL9','Obs')), # Obs is pink
  setNames(RColorBrewer::brewer.pal(3, 'Dark2'), c('PB-MTL','PG-MTL','PG-MTL9')),
  'Observed'='black',
  'PB0'='darkgray')
pbmtl_colors <- c(dark='#105d46', central='#7570b3', light='#2bdba6') # https://www.colorhexa.com/1B9E77
pgmtl9_colors <- c(dark='#4f4a8c', central='#7570b3', light='#a5a2ce', neutral='#e4e3f0') # https://www.colorhexa.com/7570b3
pgmtl_colors <- c(dark='#8d3e01', central='#d95f02', light='#fd862a') # https://www.colorhexa.com/D95F02
neutral_colors <- c(dark='darkgray', light='lightgray')
example_colors <-  RColorBrewer::brewer.pal(8, 'Dark2')[5:8] #c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf")[c(4,7,1,10)]

# Jordan's map colors
map_colors <- c(sources = '#ca0020', main_targets = '#0571b0', extended_targets = '#74add1')
