#' ---
#' title: functions
#' author: Philippe Glaziou
#' date: 2019-05-22
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---
#'
#' ## SDG: e.coli and MRSA
#' 
library(here)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(openxlsx)

source(here('R/fun.R'))

load(here('data/pop.Rdata'))


#' load 2016 - 2018 SDG indicators, datasets from Barbara (april 2021)
#' 
mrsa <- fread('input/2020_SGD_3.d.2_MRSA_2016_17_18.csv')
ecoli <- fread('input/2020_SGD_3.d.2_Ecoli 3GC_2016_17_18.csv')

#' load 2019 SDG indicators
#' 
mrsa19 <- fread('input/2021_SGD_3.d.2_MRSA_2019data.csv')
ecoli19 <- fread('input/2021_SGD_3.d.2_Ecoli 3GC_2019data.csv')

cty <- fread('input/cty_2020-10-02.csv')
tb <- fread('input/est_s07_2020-08-31.csv')

mrsa <- rbind(mrsa, mrsa19)
ecoli <- rbind(ecoli, ecoli19)

vars <- c('series.code','series.name','ind.num','area.code','area.type','area.name','year',
         'value','unit','nature','disagg','time.detail','reporting','footnote','source')
setnames(mrsa, vars)
setnames(ecoli, vars)

mrsa[, sum(!is.na(value)), by=year]
ecoli[, sum(!is.na(value)), by=year]

mrsa <- mrsa[year>2015]
ecoli <- ecoli[year>2015]

mrsa[area.type=='3.0-Country', country := area.name]
ecoli[area.type=='3.0-Country', country := area.name]

mrsa2 <- merge(mrsa[!is.na(country)], cty, by='country')
mr <- merge(mrsa2, tb[year==2019, .(iso3, g.income, g.gbd)], by='iso3')

ecoli2 <- merge(ecoli[!is.na(country)], cty, by='country')
ec <- merge(ecoli2, tb[year==2019, .(iso3, g.income, g.gbd)], by='iso3')

mr[g.income=='HIC', hic := 'High income']
mr[g.income!='HIC', hic := 'Low and middle income']

fit.mr.hic <- loess(value ~ year, data=mr[g.income=='HIC'])
fit.mr.nhic <- loess(value ~ year, data=mr[g.income!='HIC'])
out.mr.hic <- data.table(year=2016:2019, country='', hic='High income', 
                         yhat=predict(fit.mr.hic, newdata=data.table(year=2016:2018)))
out.mr.nhic <- data.table(year=2016:2019, country='', hic='Low and middle income', 
                          yhat=predict(fit.mr.nhic, newdata=data.table(year=2016:2018)))
(out.mr <- rbind(out.mr.hic, out.mr.nhic))

p1 <- qplot(year, value, data=mr, group=country, geom='line', colour=I('grey40'),
            main='BSI caused by methicillin-resistant Staphylococcus aureus') +
  geom_point(size=I(.5), colour=I('grey40')) +
  geom_line(aes(year, yhat), data=out.mr, size=I(2)) +
  scale_y_continuous(name='Resistance (%)') +
  scale_x_continuous(breaks=2016:2019) +
  xlab('') +
  facet_wrap(~hic) +
  theme_bw(base_size = 18)
(p1)
ggsave(
  here('output/mrsa.pdf'),
  width = 10,
  height = 8
)


ec[g.income=='HIC', hic := 'High income']
ec[g.income!='HIC', hic := 'Low and middle income']

fit.ec.hic <- loess(value ~ year, data=ec[g.income=='HIC'])
fit.ec.nhic <- loess(value ~ year, data=ec[g.income!='HIC'])
out.ec.hic <- data.table(year=2016:2019, country='', hic='High income', 
                         yhat=predict(fit.ec.hic, newdata=data.table(year=2016:2018)))
out.ec.nhic <- data.table(year=2016:2019, country='', hic='Low and middle income', 
                          yhat=predict(fit.ec.nhic, newdata=data.table(year=2016:2018)))
(out.ec <- rbind(out.ec.hic, out.ec.nhic))

p2 <- qplot(year, value, data=ec, group=country, geom='line', colour=I('grey40'), 
            main='BSI caused by E. coli resistant to 3rd generation cephalosporins ') +
  geom_point(size=I(.5), colour=I('grey40')) +
  geom_line(aes(year, yhat), data=out.ec, size=I(2)) +
  scale_y_continuous(name='Resistance (%)') +
  scale_x_continuous(breaks=2016:2018) +
  xlab('') +
  facet_wrap(~hic) +
  theme_bw(base_size = 18)
(p2)
ggsave(p2,
  here('output/ecoli.pdf'),
  width = 10,
  height = 8
)


mp <-
  grid.arrange(p1, p2, ncol = 1)

ggsave(
  mp,
  file = here('output/amr_sdg.pdf'),
  width = 10,
  height = 10
)



#' SDG storyline 
#' 
setkey(ec, year)
setkey(mr, year)

ec[!is.na(value), table(year)]
ec[!is.na(value), median(value), by=year]
ec[!is.na(value), quantile(value, .25), by=.(year)]
ec[!is.na(value), quantile(value, .75), by=.(year)]
(out.ec)

mr[!is.na(value), table(year)]
mr[!is.na(value), median(value), by=.(year)]
mr[!is.na(value), quantile(value, .25), by=.(year)]
mr[!is.na(value), quantile(value, .75), by=.(year)]
(out.mr)

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1", gridLines = FALSE)
print(mp)
insertPlot(wb, 1, width = 10, height = 10, fileType = "png", units = "in")
saveWorkbook(wb, "output/sdg_3d2.xlsx", overwrite = TRUE)

fwrite(ec[,.(iso3,year,value,hic)], file=here('output/ec.csv'))
fwrite(mr[,.(iso3,year,value,hic)], file=here('output/mr.csv'))
fwrite(out.ec, file=here('output/out_ec.csv'))
fwrite(out.mr, file=here('output/out_mr.csv'))




library(whomap)
dta <- mr[year==2019, .(iso3, var=cut(value, 
              c(0, 20, 40, 60, 80, 100),
              c('0-19', '20-39.9', '40-59.9', '60-79', '80-100'),
              right = F,
              ordered_result = T))]
whomap(X=sg(dta),
       Z = scale_fill_brewer(
         "MRSA\nPrevalence (%)",
         palette = "YlGnBu",
         type = "seq",
       ))

dta <- ec[year==2019, .(iso3, var=cut(value, 
                            c(0, 20, 40, 60, 80, 100),
                            c('0-19', '20-39.9', '40-59.9', '60-79', '80-100'),
                            right = F,
                            ordered_result = T))]
whomap(X=sg(dta),
       Z = scale_fill_brewer(
         "E Coli\nPrevalence (%)",
         palette = "YlGnBu",
         type = "seq",
       ))



#' AMR datasets - snapshot 07/04/2021
#' 
smp <- fread('input/export_sample_20210407.csv')
ris <- fread('input/export_ris_20210407.csv')

setnames(smp, tolower(names(smp)))
setnames(smp, 'numsampledpatients','patients')
setnames(ris, tolower(names(ris)))

#' recode SUM to UNK
#' 
ris[origin=='SUM', origin := 'UNK']
ris[gender=='SUM', gender := 'UNK']
ris[agegroup=='SUM', agegroup := 'UNK']

smp[origin=='SUM', origin := 'UNK']
smp[gender=='SUM', gender := 'UNK']
smp[agegroup=='SUM', agegroup := 'UNK']


#' merge ris and smp
#' 
dim(ris)
dim(smp)
amr <- merge(ris, smp, by=c('country','year','specimen','gender','origin','agegroup','batchid'), all.x=T)
setnames(amr, 'country','iso3')
setkey(amr, iso3, year)
dim(amr)

amr[, ast := rowSums(cbind(resistant, intermediate, nonsusceptible, susceptible), na.rm=TRUE)]
amr[, res := rowSums(cbind(resistant, nonsusceptible), na.rm=TRUE)]
amr[, infect := rowSums(cbind(ast, unknown_no_ast, unknown_no_breakpoints), na.rm=TRUE)]


save(amr, file=here('data/amr.Rdata'))
fwrite(amr, file=here('csv/amr_20210407.csv'))


#' where does most of the data come from?
#' 
load(here('data/amr.Rdata'))
amr[year==2019, sums(ast)]
amr[year==2019, .(ast=sums(ast)), by=iso3][order(-ast), .(iso3, ast, cum=cumsum(ast)/sum(ast))][1:5]


#' number of infections (2019) - table 2.7 of the GLASS report 2021
#' 
inf <-
  amr[year==2019, .(inf.tot = max(infect, na.rm=T)), by = .(iso3, year, batchid, gender, agegroup, specimen, origin, pathogen)]
inf[, xtabs(inf.tot ~ specimen + pathogen + origin)]
inf[, xtabs(inf.tot ~ specimen)]

ast <-
  ast[, .(ast.tot = max(ast, na.rm = T)), by = .(iso3, year, batchid, gender, agegroup, specimen, origin, pathogen)]
ast[, xtabs(ast.tot ~ specimen + pathogen + origin)]
ast[, xtabs(ast.tot ~ specimen)]


#' number of AST
#'
ast <-
  amr[year==2019, .(ast = max(ast)), by = .(iso3, year, batchid, gender, agegroup, specimen, origin, pathogen)]
ast[, xtabs(ast ~ specimen + pathogen + origin)]

out <- amr[year == 2019, .(ast = sums(ast)), by = iso3][order(-ast)][, cum := cumsum(ast)][]

out <- merge(out, pop[year==2019, .(iso3,pop,g.whoregion)], by='iso3')
out[, ast.pc := ast/pop * 100]
(out[order(-ast)][1:5])
(out[, sums(ast)])

setkey(amr, year)
amr[, .(AST=sums(ast)), by=year]


