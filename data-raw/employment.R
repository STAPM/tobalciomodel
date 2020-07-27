## construct 4-digit employment by industry-year from the Labour Force Survey

library(lfsclean)
library(dplyr)

vars <- c("year","quarter","pwt","age","gender","lmstatus","full_time","sic2007_4dig")

data <- combine_years(list(
lfs_clean_global(lfs_read_2010(),keep_vars = vars),
lfs_clean_global(lfs_read_2011(),keep_vars = vars),
lfs_clean_global(lfs_read_2012(),keep_vars = vars),
lfs_clean_global(lfs_read_2013(),keep_vars = vars),
lfs_clean_global(lfs_read_2014(),keep_vars = vars),
lfs_clean_global(lfs_read_2015(),keep_vars = vars),
lfs_clean_global(lfs_read_2016(),keep_vars = vars),
lfs_clean_global(lfs_read_2017(),keep_vars = vars),
lfs_clean_global(lfs_read_2018(),keep_vars = vars),
lfs_clean_global(lfs_read_2019(),keep_vars = vars),
lfs_clean_global(lfs_read_2020(),keep_vars = vars)
)
)

### create variable for full-time equivalence

data[full_time == "full_time",fte := 1]
data[full_time == "part_time",fte := 0.5]

empl_data <- data %>%
  filter(lmstatus=="employed") %>%
  filter(!is.na(full_time)) %>%
  filter(!is.na(sic2007_4dig)) %>%
  group_by(time,sic2007_4dig) %>%
  mutate(empl_ = sum(pwt)) %>%
  mutate(fte_ = sum(pwt*fte)) %>%
  select(time,year,sic2007_4dig,empl_,fte_) %>%
  distinct()

sort_empl_data <- empl_data[order(empl_data$time,empl_data$sic2007_4dig),]

# average within-industry employment out over the year for an annual figure

data_employment_sic2007 <- sort_empl_data %>%
  group_by(year,sic2007_4dig) %>%
  mutate(empl = mean(empl_)) %>%
  mutate(fte = mean(fte_)) %>%
  select(year,sic2007_4dig,empl,fte) %>%
  distinct()

# add up to obtain total annual employment - check the figures look logical

#empl_data_y <- data_employment_sic2007 %>%
#  group_by(year) %>%
#  mutate(empl_ = sum(empl)) %>%
#  mutate(fte_ = sum(fte)) %>%
#  select(year,empl_,fte_) %>%
#  distinct()



#usethis::use_data(data_employment_sic2007,overwrite=TRUE)

data <- as.data.table(data_employment_sic2007)

#data$sic2007_4dig <- as.numeric(data$sic2007_4dig)

#### Now convert employment from SIC-2007 industries to the 106 alcohol-disaggregated sectors used in the
#### FAI report


data[sic2007_4dig %in% c(111,113,116,119,121,122,123,124,125,126,127,128,129,130,141,142,143,145,146,147,149,150,161,162,163,164,170), group := "Crop And Animal Production, Hunting And Related Service Activities"]
data[sic2007_4dig %in% c(210,220,230,240), group := "Forestry And Logging"]
data[sic2007_4dig %in% c(311,312,321,322), group := "Fishing And Aquaculture"]
data[sic2007_4dig %in% c(510,520), group := "Mining Of Coal And Lignite"]
data[sic2007_4dig %in% c(610,620,710,721,729), group := "Extraction Of Crude Petroleum And Natural Gas  & Mining Of Metal Ores"]
data[sic2007_4dig %in% c(811,812,891,892,893,899), group := "Other Mining And Quarrying"]
data[sic2007_4dig %in% c(910,990), group := "Mining Support Service Activities"]
data[sic2007_4dig %in% c(1011,1012,1013), group := "Processing and preserving of meat and production of meat products"]
data[sic2007_4dig %in% c(1020,1031,1032,1039), group := "Processing and preserving of fish, crustaceans, molluscs, fruit and vegetables"]
data[sic2007_4dig %in% c(1041,1042), group := "Manufacture of vegetable and animal oils and fats"]
data[sic2007_4dig %in% c(1051,1052), group := "Manufacture of dairy products"]
data[sic2007_4dig %in% c(1061,1062), group := "Manufacture of grain mill products, starches and starch products"]
data[sic2007_4dig %in% c(1071,1072,1073), group := "Manufacture of bakery and farinaceous products"]
data[sic2007_4dig %in% c(1081,1082,1083,1084,1085,1086,1089), group := "Manufacture of other food products"]
data[sic2007_4dig %in% c(1091,1092), group := "Manufacture of prepared animal feeds"]
data[sic2007_4dig %in% c(1101,1102,1103,1104,1105,1106), group := "Manufacture of alcoholic beverages"]
data[sic2007_4dig %in% c(1107), group := "Manufacture of soft drinks; production of mineral waters and other bottled waters"]
data[sic2007_4dig %in% c(1200), group := "Manufacture Of Tobacco Products"]
data[sic2007_4dig %in% c(1310,1320,1330,1391,1392,1393,1394,1395,1396,1399), group := "Manufacture Of Textiles"]
data[sic2007_4dig %in% c(1411,1412,1413,1414,1419,1420,1431,1439), group := "Manufacture Of Wearing Apparel"]
data[sic2007_4dig %in% c(1511,1512,1520), group := "Manufacture Of Leather And Related Products"]
data[sic2007_4dig %in% c(1610,1621,1622,1623,1624,1629), group := "Manufacture Of Wood & Products Of Wood & Cork, Except Furniture; Manuf. Of Articles Of Straw"]
data[sic2007_4dig %in% c(1711,1712,1721,1722,1723,1724,1729), group := "Manufacture Of Paper And Paper Products"]
data[sic2007_4dig %in% c(1811,1812,1813,1814,1820), group := "Printing And Reproduction Of Recorded Media"]
data[sic2007_4dig %in% c(1910,1920), group := "Manufacture Of Coke And Refined Petroleum Products"]
data[sic2007_4dig %in% c(2030), group := "Manufacture of paints, varnishes and similar coatings, printing ink and mastics"]
data[sic2007_4dig %in% c(2041,2042), group := "Manufacture of soap & detergents, cleaning & polishing, perfumes & toilet preparations"]
data[sic2007_4dig %in% c(2051,2052,2053,2059), group := "Manufacture of other chemical products"]
data[sic2007_4dig %in% c(2011,2013,2015), group := "Manufacture of industrial gases, inorganics and fertilisers (inorganic chemicals) - 20.11/13/15"]
data[sic2007_4dig %in% c(2014,2016,2017,2060), group := "Manufacture of petrochemicals - 20.14/16/17/60"]
data[sic2007_4dig %in% c(2012,2020), group := "Manufacture of dyestuffs, agro-chemicals - 20.12/20"]
data[sic2007_4dig %in% c(2110,2120), group := "Manufacture Of Basic Pharmaceutical Products And Pharmaceutical Preparations"]
data[sic2007_4dig %in% c(2211,2219,2221,2222,2223,2229), group := "Manufacture Of Rubber And Plastic Products"]
data[sic2007_4dig %in% c(2351,2352,2361,2362,2363,2364,2365,2369), group := "Manufacture of cement, lime, plaster and articles of concrete, cement and plaster"]
data[sic2007_4dig %in% c(2311,2312,2313,2314,2319,2320,2331,2332,2341,2342,2343,2344,2349,2370,2391,2399), group := "Manufacture of glass, refractory, clay, porcelain, ceramic, stone products - 23.1-4/7-9"]
data[sic2007_4dig %in% c(2410,2420,2431,2432,2433,2434), group := "Manufacture of basic iron and steel"]
data[sic2007_4dig %in% c(2441,2442,2443,2444,2445,2446,2451,2452,2453,2454), group := "Manufacture of other basic metals and casting"]
data[sic2007_4dig %in% c(2540), group := "Manufacture of weapons and ammunition"]
data[sic2007_4dig %in% c(2511,2512,2521,2529,2530,2550,2561,2562,2571,2572,2573,2591,2592,2593,2594,2599), group := "Manufacture of fabricated metal products, excluding weapons & ammunition - 25.1-3/5-9"]
data[sic2007_4dig %in% c(2611,2612,2620,2630,2640,2651,2652,2660,2670,2680), group := "Manufacture Of Computer, Electronic And Optical Products"]
data[sic2007_4dig %in% c(2711,2712,2720,2731,2732,2733,2740,2751,2752,2790), group := "Manufacture Of Electrical Equipment"]
data[sic2007_4dig %in% c(2811,2812,2813,2814,2815,2821,2822,2823,2824,2825,2829,2830,2841,2849,2891,2892,2893,2894,2895,2896,2899), group := "Manufacture Of Machinery And Equipment N.E.C."]
data[sic2007_4dig %in% c(2910,2920,2931,2932), group := "Manufacture Of Motor Vehicles, Trailers And Semi-Trailers"]
data[sic2007_4dig %in% c(3011,3012), group := "Building of ships and boats"]
data[sic2007_4dig %in% c(3030), group := "Manufacture of air and spacecraft and related machinery"]
data[sic2007_4dig %in% c(3020,3040,3091,3092,3099), group := "Manufacture of other transport equipment - 30.2/4/9"]
data[sic2007_4dig %in% c(3101,3102,3103,3109), group := "Manufacture Of Furniture"]
data[sic2007_4dig %in% c(3211,3212,3213,3220,3230,3240,3250,3291,3299), group := "Other Manufacturing"]
data[sic2007_4dig %in% c(3315), group := "Repair and maintenance of ships and boats"]
data[sic2007_4dig %in% c(3316), group := "Repair and maintenance of aircraft and spacecraft"]
data[sic2007_4dig %in% c(3311,3312,3313,3314,3317,3319,3320), group := "Rest of repair; Installation - 33.11-14/17/19/20"]
data[sic2007_4dig %in% c(3511,3512,3513,3514), group := "Electric power generation, transmission and distribution"]
data[sic2007_4dig %in% c(3521,3522,3523,3530), group := "Manufacture of gas; distribution of gaseous fuels through mains; steam and aircon supply"]
data[sic2007_4dig %in% c(3600), group := "Water Collection, Treatment And Supply"]
data[sic2007_4dig %in% c(3700), group := "Sewerage"]
data[sic2007_4dig %in% c(3811,3812,3821,3822,3831,3931,3832), group := "Waste Collection, Treatment And Disposal Activities; Materials Recovery"]
data[sic2007_4dig %in% c(3900), group := "Remediation Activities And Other Waste Management Services"]
data[sic2007_4dig %in% c(4110,4120,4211,4212,4213,4221,4222,4291,4299,4311,4312,4313,4321,4322,4329,4331,4332,4333,4334,4339,4391,4399), group := "Construction"]
data[sic2007_4dig %in% c(4511,4519,4520,4531,4532,4540), group := "Wholesale And Retail Trade And Repair Of Motor Vehicles And Motorcycles"]
data[sic2007_4dig %in% c(4611,4612,4613,4614,4615,4616,4617,4618,4619,4621,4622,4623,4624,4631,4632,4633,4634,4635,4636,4637,4638,4639,4641,4642,4643,4644,4645,4646,4647,4648,4649,4651,4652,4661,4662,4663,4664,4665,4666,4669,4671,4672,4673,4674,4675,4676,4677,4690,
                         4711,4719,4721,4722,4723,4724,4725,4726,4729,4730,4741,4742,4743,4751,4752,4753,4754,4759,4761,4762,4763,4764,4765,4771,4772,4773,4774,4775,4776,4777,4778,4779,4781,4782,4789,4791,4799), group := "Wholesale Trade, and Motor Vehicles And Motorcycles (non-Alcohol)"]
data[sic2007_4dig %in% c(4910,4920), group := "Rail transport"]
data[sic2007_4dig %in% c(4931,4932,4939,4941,4942,4950), group := "Land transport services and transport services via pipelines, excluding rail transport"]
data[sic2007_4dig %in% c(5010,5020,5030,5040), group := "Water Transport"]
data[sic2007_4dig %in% c(5110,5121,5122), group := "Air Transport"]
data[sic2007_4dig %in% c(5210,5221,5222,5223,5224,5229), group := "Warehousing And Support Activities For Transportation"]
data[sic2007_4dig %in% c(5310,5320), group := "Postal And Courier Activities"]
data[sic2007_4dig %in% c(5510,5520,5530,5590), group := "Accommodation  (non-Alcohol)"]
data[sic2007_4dig %in% c(5610,5621,5629,5630), group := "Food And Beverage Service Activities  (non-Alcohol)"]
data[sic2007_4dig %in% c(5811,5812,5813,5814,5819,5821,5829), group := "Publishing Activities"]
data[sic2007_4dig %in% c(5911,5912,5913,5914,5920,6010,6020), group := "Motion Picture, Video & TV Programme Production, Sound Recording & Music Publishing Activities & Programming And Broadcasting Activities"]
data[sic2007_4dig %in% c(6110,6120,6130,6190), group := "Telecommunications"]
data[sic2007_4dig %in% c(6201,6202,6203,6209), group := "Computer Programming, Consultancy And Related Activities"]
data[sic2007_4dig %in% c(6311,6312,6391,6399), group := "Information Service Activities"]
data[sic2007_4dig %in% c(6411,6419,6420,6430,6491,6492,6499), group := "Financial Service Activities, Except Insurance And Pension Funding"]
data[sic2007_4dig %in% c(6511,6512,6520,6530), group := "Insurance and reinsurance, except compulsory social security & Pension funding"]
data[sic2007_4dig %in% c(6611,6612,6619,6621,6622,6629,6630), group := "Activities Auxiliary To Financial Services And Insurance Activities"]
data[sic2007_4dig %in% c(6810), group := "Buying and selling, renting and operating of own or leased real estate, excluding imputed rent"]
data[sic2007_4dig %in% c(6820,6831,6832), group := "Real estate activities on a fee or contract basis"]
data[sic2007_4dig %in% c(6910), group := "Legal activities"]
data[sic2007_4dig %in% c(6920), group := "Accounting, bookkeeping and auditing activities; tax consultancy"]
data[sic2007_4dig %in% c(7010,7021,7022), group := "Activities Of Head Offices; Management Consultancy Activities"]
data[sic2007_4dig %in% c(7111,7112,7120), group := "Architectural And Engineering Activities; Technical Testing And Analysis"]
data[sic2007_4dig %in% c(7211,7219,7220), group := "Scientific Research And Development"]
data[sic2007_4dig %in% c(7311,7312,7320), group := "Advertising And Market Research"]
data[sic2007_4dig %in% c(7410,7420,7430,7490), group := "Other Professional, Scientific And Technical Activities"]
data[sic2007_4dig %in% c(7500), group := "Veterinary Activities"]
data[sic2007_4dig %in% c(7711,7712,7721,7722,7729,7731,7732,7733,7734,7735,7739,7740), group := "Rental And Leasing Activities"]
data[sic2007_4dig %in% c(7810,7820,7830), group := "Employment Activities"]
data[sic2007_4dig %in% c(7990,7912,7911), group := "Travel Agency, Tour Operator And Other Reservation Service And Related Activities"]
data[sic2007_4dig %in% c(8010,8020,8030), group := "Security And Investigation Activities"]
data[sic2007_4dig %in% c(8110,8121,8122,8129,8130), group := "Services To Buildings And Landscape Activities"]
data[sic2007_4dig %in% c(8211,8219,8220,8230,8291,8292,8299), group := "Office Administrative, Office Support And Other Business Support Activities"]
data[sic2007_4dig %in% c(8411,8412,8413,8421,8422,8423,8424,8425,8430), group := "Public Administration And Defence; Compulsory Social Security"]
data[sic2007_4dig %in% c(8510,8520,8531,8532,8541,8542,8551,8552,8553,8559,8560), group := "Education"]
data[sic2007_4dig %in% c(8610,8621,8622,8623,8690), group := "Human Health Activities"]
data[sic2007_4dig %in% c(8710,8720,8730,8790,8810,8891,8899), group := "Residential Care  & Social Work Activities"]
data[sic2007_4dig %in% c(9001,9002,9003,9004), group := "Creative, Arts And Entertainment Activities"]
data[sic2007_4dig %in% c(9101,9102,9103,9104), group := "Libraries, Archives, Museums And Other Cultural Activities"]
data[sic2007_4dig %in% c(9200), group := "Gambling And Betting Activities"]
data[sic2007_4dig %in% c(9311,9312,9313,9319,9321,9329), group := "Sports Activities And Amusement And Recreation Activities"]
data[sic2007_4dig %in% c(9411,9412,9420,9491,9492,9499), group := "Activities Of Membership Organisations"]
data[sic2007_4dig %in% c(9511,9512,9521,9522,9523,9524,9525,9529), group := "Repair Of Computers And Personal And Household Goods"]
data[sic2007_4dig %in% c(9601,9602,9603,9604,9609), group := "Other Personal Service Activities"]
data[sic2007_4dig %in% c(9700,9810,9820,9900), group := "Not Applicable"]

#### re-order

data[group %in% "Crop And Animal Production, Hunting And Related Service Activities",num := 1]
data[group %in% "Forestry And Logging",num := 2]
data[group %in% "Fishing And Aquaculture",num := 3]
data[group %in% "Mining Of Coal And Lignite",num := 4]
data[group %in% "Extraction Of Crude Petroleum And Natural Gas  & Mining Of Metal Ores",num := 5]
data[group %in% "Other Mining And Quarrying",num := 6]
data[group %in% "Mining Support Service Activities",num := 7]
data[group %in% "Processing and preserving of meat and production of meat products",num := 8]
data[group %in% "Processing and preserving of fish, crustaceans, molluscs, fruit and vegetables",num := 9]
data[group %in% "Manufacture of vegetable and animal oils and fats",num := 10]
data[group %in% "Manufacture of dairy products",num := 11]
data[group %in% "Manufacture of grain mill products, starches and starch products",num := 12]
data[group %in% "Manufacture of bakery and farinaceous products",num := 13]
data[group %in% "Manufacture of other food products",num := 14]
data[group %in% "Manufacture of prepared animal feeds",num := 15]
data[group %in% "Manufacture of alcoholic beverages",num := 16]
data[group %in% "Manufacture of soft drinks; production of mineral waters and other bottled waters",num := 17]
data[group %in% "Manufacture Of Tobacco Products",num := 18]
data[group %in% "Manufacture Of Textiles",num := 19]
data[group %in% "Manufacture Of Wearing Apparel",num := 20]
data[group %in% "Manufacture Of Leather And Related Products",num := 21]
data[group %in% "Manufacture Of Wood & Products Of Wood & Cork, Except Furniture; Manuf. Of Articles Of Straw",num := 22]
data[group %in% "Manufacture Of Paper And Paper Products",num := 23]
data[group %in% "Printing And Reproduction Of Recorded Media",num := 24]
data[group %in% "Manufacture Of Coke And Refined Petroleum Products",num := 25]
data[group %in% "Manufacture of paints, varnishes and similar coatings, printing ink and mastics",num := 26]
data[group %in% "Manufacture of soap & detergents, cleaning & polishing, perfumes & toilet preparations",num := 27]
data[group %in% "Manufacture of other chemical products",num := 28]
data[group %in% "Manufacture of industrial gases, inorganics and fertilisers (inorganic chemicals) - 20.11/13/15",num := 29]
data[group %in% "Manufacture of petrochemicals - 20.14/16/17/60",num := 30]
data[group %in% "Manufacture of dyestuffs, agro-chemicals - 20.12/20",num := 31]
data[group %in% "Manufacture Of Basic Pharmaceutical Products And Pharmaceutical Preparations",num := 32]
data[group %in% "Manufacture Of Rubber And Plastic Products",num := 33]
data[group %in% "Manufacture of cement, lime, plaster and articles of concrete, cement and plaster",num := 34]
data[group %in% "Manufacture of glass, refractory, clay, porcelain, ceramic, stone products - 23.1-4/7-9",num := 35]
data[group %in% "Manufacture of basic iron and steel",num := 36]
data[group %in% "Manufacture of other basic metals and casting",num := 37]
data[group %in% "Manufacture of weapons and ammunition",num := 38]
data[group %in% "Manufacture of fabricated metal products, excluding weapons & ammunition - 25.1-3/5-9",num := 39]
data[group %in% "Manufacture Of Computer, Electronic And Optical Products",num := 40]
data[group %in% "Manufacture Of Electrical Equipment",num := 41]
data[group %in% "Manufacture Of Machinery And Equipment N.E.C.",num := 42]
data[group %in% "Manufacture Of Motor Vehicles, Trailers And Semi-Trailers",num := 43]
data[group %in% "Building of ships and boats",num := 44]
data[group %in% "Manufacture of air and spacecraft and related machinery",num := 45]
data[group %in% "Manufacture of other transport equipment - 30.2/4/9",num := 46]
data[group %in% "Manufacture Of Furniture",num := 47]
data[group %in% "Other Manufacturing",num := 48]
data[group %in% "Repair and maintenance of ships and boats",num := 49]
data[group %in% "Repair and maintenance of aircraft and spacecraft",num := 50]
data[group %in% "Rest of repair; Installation - 33.11-14/17/19/20",num := 51]
data[group %in% "Electric power generation, transmission and distribution",num := 52]
data[group %in% "Manufacture of gas; distribution of gaseous fuels through mains; steam and aircon supply",num := 53]
data[group %in% "Water Collection, Treatment And Supply",num := 54]
data[group %in% "Sewerage",num := 55]
data[group %in% "Waste Collection, Treatment And Disposal Activities; Materials Recovery",num := 56]
data[group %in% "Remediation Activities And Other Waste Management Services",num := 57]
data[group %in% "Construction",num := 58]
data[group %in% "Wholesale And Retail Trade And Repair Of Motor Vehicles And Motorcycles",num := 59]
data[group %in% "Wholesale Trade, and Motor Vehicles And Motorcycles (non-Alcohol)",num := 60]
data[group %in% "Rail transport",num := 62]
data[group %in% "Land transport services and transport services via pipelines, excluding rail transport",num := 63]
data[group %in% "Water Transport",num := 64]
data[group %in% "Air Transport",num := 65]
data[group %in% "Warehousing And Support Activities For Transportation",num := 66]
data[group %in% "Postal And Courier Activities",num := 67]
data[group %in% "Accommodation  (non-Alcohol)",num := 68]
data[group %in% "Food And Beverage Service Activities  (non-Alcohol)",num := 70]
data[group %in% "Publishing Activities",num := 72]
data[group %in% "Motion Picture, Video & TV Programme Production, Sound Recording & Music Publishing Activities & Programming And Broadcasting Activities",num := 73]
data[group %in% "Telecommunications",num := 74]
data[group %in% "Computer Programming, Consultancy And Related Activities",num := 75]
data[group %in% "Information Service Activities",num := 76]
data[group %in% "Financial Service Activities, Except Insurance And Pension Funding",num := 77]
data[group %in% "Insurance and reinsurance, except compulsory social security & Pension funding",num := 78]
data[group %in% "Activities Auxiliary To Financial Services And Insurance Activities",num := 79]
data[group %in% "Buying and selling, renting and operating of own or leased real estate, excluding imputed rent",num := 80]
data[group %in% "Real estate activities on a fee or contract basis",num := 81]
data[group %in% "Legal activities",num := 82]
data[group %in% "Accounting, bookkeeping and auditing activities; tax consultancy",num := 83]
data[group %in% "Activities Of Head Offices; Management Consultancy Activities",num := 84]
data[group %in% "Architectural And Engineering Activities; Technical Testing And Analysis",num := 85]
data[group %in% "Scientific Research And Development",num := 86]
data[group %in% "Advertising And Market Research",num := 87]
data[group %in% "Other Professional, Scientific And Technical Activities",num := 88]
data[group %in% "Veterinary Activities",num := 89]
data[group %in% "Rental And Leasing Activities",num := 90]
data[group %in% "Employment Activities",num := 91]
data[group %in% "Travel Agency, Tour Operator And Other Reservation Service And Related Activities",num := 92]
data[group %in% "Security And Investigation Activities",num := 93]
data[group %in% "Services To Buildings And Landscape Activities",num := 94]
data[group %in% "Office Administrative, Office Support And Other Business Support Activities",num := 95]
data[group %in% "Public Administration And Defence; Compulsory Social Security",num := 96]
data[group %in% "Education",num := 97]
data[group %in% "Human Health Activities",num := 98]
data[group %in% "Residential Care  & Social Work Activities",num := 99]
data[group %in% "Creative, Arts And Entertainment Activities",num := 100]
data[group %in% "Libraries, Archives, Museums And Other Cultural Activities",num := 101]
data[group %in% "Gambling And Betting Activities",num := 102]
data[group %in% "Sports Activities And Amusement And Recreation Activities",num := 103]
data[group %in% "Activities Of Membership Organisations",num := 104]
data[group %in% "Repair Of Computers And Personal And Household Goods",num := 105]
data[group %in% "Other Personal Service Activities",num := 106]
data[group %in% "Not Applicable",num := 107]

# filter out groups 97/98/99 as these are not included in the IO Table:
# (domestic / undifferentiated goods, extraterritorial organisations)

clean <- data %>%
  filter(group != "Not Applicable") %>%
  group_by(year,group) %>%
  mutate(empl_ = sum(empl)) %>%
  mutate(fte_ = sum(fte)) %>%
  select(num,year,group,empl_,fte_) %>%
  distinct()

# reshape wide and then sort into industry numerical order

library(tidyr)

clean_empl <- data.frame(clean$num,clean$group,clean$year,clean$empl_)
clean_fte <- data.frame(clean$num,clean$group,clean$year,clean$fte_)

reshape_empl <- spread(clean_empl,clean.year,clean.empl_)
reshape_fte <- spread(clean_fte,clean.year,clean.fte_)

reshaped <- data.table(reshape_fte$clean.num,
                       reshape_fte$clean.group,
                       reshape_fte[,"2010"],reshape_fte[,"2011"],reshape_fte[,"2012"],reshape_fte[,"2013"],reshape_fte[,"2014"],reshape_fte[,"2015"],
                       reshape_fte[,"2016"],reshape_fte[,"2017"],reshape_fte[,"2018"],reshape_fte[,"2019"],reshape_fte[,"2020"],
                       reshape_empl[,"2010"],reshape_empl[,"2011"],reshape_empl[,"2012"],reshape_empl[,"2013"],reshape_empl[,"2014"],reshape_empl[,"2015"],
                       reshape_empl[,"2016"],reshape_empl[,"2017"],reshape_empl[,"2018"],reshape_empl[,"2019"],reshape_empl[,"2020"])

setnames(reshaped,
         c(paste0("V",seq(1,24,1))),
         c("num","group", paste0("fte_",seq(2010,2020,1)), paste0("empl_",seq(2010,2020,1)) ) )


######## need to disaggregate the sectors for alcohol

## obtain total output for those sectors. disaggregate employment by assuming employment
## splits in the same proportion as does output

output <- tobalciomodel::iotable[c(60,61,68,69,70,71),c("sector","total.output")]

alc1.emp.prop <-  output[2,"total.output"]/(output[1,"total.output"] + output[2,"total.output"])
alc2.emp.prop <-  output[4,"total.output"]/(output[3,"total.output"] + output[4,"total.output"])
alc3.emp.prop <-  output[6,"total.output"]/(output[5,"total.output"] + output[6,"total.output"])

# empl in non-alcohol disaggregation
sec1_empl_non <- reshaped[60,c(3:24)]*(1-alc1.emp.prop)
sec2_empl_non <- reshaped[67,c(3:24)]*(1-alc2.emp.prop)
sec3_empl_non <- reshaped[68,c(3:24)]*(1-alc3.emp.prop)

reshaped[60,c(3:24)] <- sec1_empl_non
reshaped[67,c(3:24)] <- sec2_empl_non
reshaped[68,c(3:24)] <- sec3_empl_non

# empl in alcohol disaggregation
sec1_empl <- reshaped[60,c(3:24)]*(alc1.emp.prop)
sec2_empl <- reshaped[67,c(3:24)]*(alc2.emp.prop)
sec3_empl <- reshaped[68,c(3:24)]*(alc3.emp.prop)


#### Create rows for the alcohol specific sectors

sec61 <- as.vector(as.matrix(c(61,"Wholesale Trade, and Motor Vehicles And Motorcycles (Alcohol)",rep(0,22))))
sec69 <- as.vector(as.matrix(c(69,"Accommodation  (Alcohol)",rep(0,22))))
sec71 <- as.vector(as.matrix(c(71,"Food And Beverage Service Activities  (Alcohol)",rep(0,22))))

newsectors <- rbind(sec61,sec69,sec71)
newsectors <-  data.frame(newsectors)

setnames(newsectors, old = names(newsectors), new = names(reshaped))

newsectors$num <- as.numeric(c(61,69,71))
for (k in 3:24) {
newsectors[,k] <- as.numeric(newsectors[,k])
}
newsectors$group = as.character(newsectors$group)

newsectors[1,c(3:24)] <- sec1_empl
newsectors[2,c(3:24)] <- sec2_empl
newsectors[3,c(3:24)] <- sec3_empl

# merge the new sectors with the original final data object

final_data <- as.data.frame(reshaped)

empldata <- rbind(final_data,newsectors)
empldata <- empldata[order(empldata$num),]
rownames(empldata) <- 1:nrow(empldata)

data_employment_fai <- subset(empldata,select=-c(num))

setnames(data_employment_fai,
         old = names(data_employment_fai),
         new = c("sector",paste0("fte_",seq(2010,2020,1)), paste0("empl_",seq(2010,2020,1)) ) )

employment <- data_employment_fai

  usethis::use_data(employment,overwrite=TRUE)

