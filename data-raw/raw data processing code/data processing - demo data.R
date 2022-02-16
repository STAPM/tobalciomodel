###########################################################
### generate data for demo IO model - 3-sector economy


library(data.table)

code <- 1:3
Sector <- c("sector 1", "Sector 2", "Sector 3")

###########################
### User-defined inputs ###

## columns of the input output table
sec1 <- c(200, 300, 123)
sec2 <- c(234, 234, 344)
sec3 <- c(23,  345, 653)

## imports
imports <- c(100, 220, 340)

gva.wages <- c(1100, 150, 800)
gva.taxes <- c(100, 100, 100)
gva.gos   <- c(0, 0, 0)

############################
### Derived inputs

gva.total <- c(gva.wages[1] + gva.gos[1] + gva.taxes[1],
               gva.wages[2] + gva.gos[2] + gva.taxes[2],
               gva.wages[3] + gva.gos[3] + gva.taxes[3])

hhold.output <- gva.wages

### total output = total intermediate purchases plus GVA
total.output <- c(sum(sec1) + imports[1] + gva.total[1],
                  sum(sec2) + imports[2] + gva.total[2],
                  sum(sec3) + imports[3] + gva.total[3])
total.demand <- total.output  # to balance, total demand must equal total output

## total demand = intermediate demand + final demand. calculate final demand from the two knowns
final.demand <- c(total.demand[1] - sec1[1] - sec2[1] - sec3[1],
                  total.demand[2] - sec1[2] - sec2[2] - sec3[2],
                  total.demand[3] - sec1[3] - sec2[3] - sec3[3])

## household demand + govt demand + exports = final demand
## assume final demand is 90% household, 10% government, 0% exports

hhold.demand <- c(0.9*final.demand[1],
                  0.9*final.demand[2],
                  0.9*final.demand[3])

govt.demand <- c(final.demand[1] - hhold.demand[1],
                 final.demand[2] - hhold.demand[2],
                 final.demand[3] - hhold.demand[3])

demo_iotoble <- data.table(code, Sector, sec1, sec2, sec3,
                           hhold.demand, govt.demand, final.demand, hhold.output,
                           total.output, total.demand,
                           gva.taxes, gva.wages, gva.gos, gva.total)

##### demo employment data

tot_emp <- c(10000, 20000, 12000)
tot_fte <- c(10000, 20000, 12000)
year <- rep(2019,3)

demo_lfs_empl <- data.table(code, Sector, tot_emp, tot_fte)

##### demo earnings data

avg_salary <- c(12000, 25000, 60000)

demo_ashe_earn <- data.table(code, Sector, avg_salary)


demo_data <- merge(demo_iotoble,
                   merge(demo_lfs_empl, demo_ashe_earn, by = c("code", "Sector")),
                   by = c("code","Sector"))


usethis::use_data(demo_data, overwrite = T)






