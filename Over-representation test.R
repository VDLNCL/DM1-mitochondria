#Venn diagram for CI_CIV_low fibres
grid.newpage()
draw.single.venn(area = 7735)
draw.pairwise.venn(area1 = 528,                        # Create pairwise venn diagram
                   area2 = 323,
                   cross.area = 125,
                   fill = c("#e740c4","#51d489"),
                   lty = "blank")


data = "Over rep test.csv"
dat = read.delim(data, sep=",", stringsAsFactors=FALSE)
head(dat)

#CI low and CIV low
for (fn in sort(unique(dat$Sample))){
  x = dat$NCL[(dat$Sample==fn)]
  m = dat$CL[(dat$Sample==fn)]
  y = dat$NL[(dat$Sample==fn)]
  n = dat$Total[(dat$Sample==fn)]
  
  hypertest=function(x,m,y,n) {
    1-phyper(x-1, y, n-y, m)
  }
  print(fn)
  print((hypertest(x,m,y,n)))}



#Venn diagram for CI_CIV_high fibres
grid.newpage()
draw.single.venn(area = 7735)
draw.pairwise.venn(area1 = 168,                        # Create pairwise venn diagram
                   area2 = 145,
                   cross.area = 8,
                   fill = c("#e740c4","#51d489"),
                   lty = "blank")

#CI high and CIV high
for (fn in sort(unique(dat$Sample))){
  x = dat$NDUFB8&COX1_high[(dat$Sample==fn)]
  m = dat$COX1_high[(dat$Sample==fn)]
  y = dat$NDUFB8_high[(dat$Sample==fn)]
  n = dat$Total[(dat$Sample==fn)]
  
  hypertest=function(x,m,y,n) {
    1-phyper(x-1, y, n-y, m)
  }
  print(fn)
  print((hypertest(x,m,y,n)))}