library(tidyverse)
library(haven)
library(ggthemes)
library(gridExtra)
library(BCDating)
library(rio)

dat <- read_csv("/Users/ShuLFO/Downloads/macro all  variables2/c17ce830-3c12-459b-91ab-443770a36e13_Data.csv")

dat2 <- dat %>%
        mutate(year = as.numeric(Time),
               country = `Country Name`,
               country_code = `Country Code`,
               gdppc = as.numeric(`GDP per capita (constant 2010 US$) [NY.GDP.PCAP.KD]`),
               gdpgrowth = as.numeric(`GDP per capita growth (annual %) [NY.GDP.PCAP.KD.ZG]`),
               inflation = as.numeric(`Inflation, consumer prices (annual %) [FP.CPI.TOTL.ZG]`),
               ca_per_gdp = as.numeric(`Current account balance (% of GDP) [BN.CAB.XOKA.GD.ZS]`),
               reserves = as.numeric(`Reserves and related items (BoP, current US$) [BN.RES.INCL.CD]`),
               gov_debt_per_gdp = as.numeric(`Central government debt, total (% of GDP) [GC.DOD.TOTL.GD.ZS]`),
               short_debt_per_reserve = as.numeric(`Short-term debt (% of total reserves) [DT.DOD.DSTC.IR.ZS]`),
               net_trade = as.numeric(`Net trade in goods and services (BoP, current US$) [BN.GSR.GNFS.CD]`),
               gov_exp = as.numeric(`General government final consumption expenditure (constant 2010 US$) [NE.CON.GOVT.KD]`),
               gov_exp_per_gdp = as.numeric(`General government final consumption expenditure (% of GDP) [NE.CON.GOVT.ZS]`),
               net_inflow = as.numeric(`Portfolio equity, net inflows (BoP, current US$) [BX.PEF.TOTL.CD.WD]`),
               fdi_inflow_per_gdp = as.numeric(`Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`)) %>% 
        select(year, country, country_code, gdppc, gdpgrowth, inflation, ca_per_gdp, reserves,
               gov_debt_per_gdp, short_debt_per_reserve, net_trade, gov_exp, gov_exp_per_gdp,
               net_inflow, fdi_inflow_per_gdp)


dat2 <- dat2 %>% 
        mutate(decade = ifelse(year %in% 1970:1989, 1,
                                ifelse(year %in% 1990:1999, 2,
                                       ifelse(year %in% 2000:2009, 3,
                                              ifelse(year %in% 2010:2018, 4, 0)))))


AE <- c("Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France",
        "Germany", "Greece", "Ireland", "Italy", "Japan", "Netherlands","New Zealand",
        "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States")

EM <- c("China", "Hong Kong SAR, China", "India", "Indonesia", "Korea, Rep.", "Malaysia",
        "Pakistan", "Philippines", "Singapore", "Sri Lanka", "Thailand","Azerbaijan", 
        "Belarus", "Kazakhstan", "Russian Federation", "Ukraine", "Albania", "Bosnia and Herzegovina",
        "Bulgaria", "Croatia", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania",
        "Macedonia, FYR", "Poland", "Romania", "Serbia", "Slovak Republic", "Slovenia", "Turkey",
        "Argentina", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador",
        "El Salvador", "Guatemala", "Jamaica", "Mexico", "Panama", "Paraguay", "Peru", 
        "Trinidad and Tobago", "Uruguay", "Venezuela, RB", "Algeria", "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Iraq", "Israel",
        "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", "Saudi Arabia", "Syrian Arab Republic",
        "Tunisia", "United Arab Emirates","Angola", "Botswana", "Namibia", "South Africa")

LIC <- c("Afghanistan", "Bangladesh", "Cambodia", "Lao PDR", "Myanmar", "Nepal", "Papua New Guinea",
         "Timor-Leste", "Vietnam", "Armenia", "Georgia", "Kyrgyz Republic", "Moldova",
         "Mongolia", "Bolivia", "Haiti", "Honduras", "Nicaragua", "Mauritania", "Sudan", "Yemen, Rep.",
         "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad",
         "Congo, Dem. Rep.", "Congo, Rep.", "Cote d'Ivoire", "Eritrea", "Ethiopia", "Ghana",
         "Guinea", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique",
         "Niger", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "Tanzania", "Togo", "Uganda",
         "Zambia", "Zimbabwe")
dat2 <- dat2 %>%
        mutate(category = ifelse(country %in% AE, "AEs",
                                 ifelse(country %in% EM, "EMs",
                                        ifelse(country %in% LIC, "LICs", "others"))),
               category2 = ifelse(country %in% AE, "AEs",
                                  ifelse(country %in% EM, "EMDEs",
                                         ifelse(country %in% LIC, "EMDEs", "others"))))

others <- dat2 %>% filter(decade == 2 & category == "others") %>% 
        select(country) %>% 
        unique()


dat2 %>% 
        group_by(year, category2) %>% 
        summarise(avg_growth = mean(gdpgrowth, na.rm = T)) %>% 
        filter(category2 %in% c("AEs", "EMDEs"),
               year > 1990) %>% 
        ggplot(aes(year, avg_growth, linetype = category2)) +
        geom_line() +
        geom_hline(yintercept = 0, alpha = .5) +
        scale_x_continuous(breaks = seq(1990, 2018, 2)) +
        labs(x = "Year",
             y = "Average Real GDP per capita growth (%)",
             caption = "Source: World Development Indicators",
             linetype = "Groups") +
        theme_few(base_family = "Times New Roman") +
        theme(axis.title = element_text(size = 11),
              legend.text = element_text(size = 13),
              legend.title = element_text(size = 13))

dat2 %>% 
        group_by(year, category2) %>% 
        summarise(avg_growth = mean(gdpgrowth, na.rm = T)) %>% 
        filter(category2 %in% c("AEs", "EMDEs"),
               year >= 1980) %>% 
        ggplot(aes(year, avg_growth, linetype = category2)) +
        geom_line() +
        geom_hline(yintercept = 0, alpha = .5) +
        scale_x_continuous(breaks = seq(1980, 2018, 5)) +
        labs(x = "Year",
             y = "Average Real GDP per capita growth (%)",
             caption = "Source: World Development Indicators",
             linetype = "Groups") +
        theme_few(base_family = "Times New Roman") +
        theme(axis.title = element_text(size = 11),
              legend.text = element_text(size = 13),
              legend.title = element_text(size = 13))

dat2 %>% filter(year == 2000, category2 %in% c("AEs", "EMDEs")) %>% 
        group_by(category2) %>% 
        count()

num_country <- c(21, 118)
        

AE_negrowth <- dat2 %>% 
        group_by(year, category2) %>% 
        filter(category2 %in% c("AEs", "EMDEs"),
               gdpgrowth < 0) %>% 
        add_count() %>% 
        mutate(neg_share = ifelse(category2 == "AEs", n / 21 * 100, n / 118 * 100)) %>% 
        filter(category2 == "AEs") %>% 
        ggplot(aes(year, neg_share)) +
        geom_col(position = "dodge") +
        scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
        scale_x_continuous(limits = c(1980, 2018), breaks = seq(1970, 2018, 5)) +
        labs(x = "",
             y = "(%)",
             title = "Advanced Economies") +
        theme_few(base_family = "Times New Roman") +
        theme(axis.title.y = element_text(size = 13))


EMDE_negrowth <- dat2 %>% 
        group_by(year, category2) %>% 
        filter(category2 %in% c("AEs", "EMDEs"),
               gdpgrowth < 0) %>% 
        add_count() %>% 
        mutate(neg_share = ifelse(category2 == "AEs", n / 21 * 100, n / 118 * 100)) %>% 
        filter(category2 == "EMDEs") %>% 
        ggplot(aes(year, neg_share)) +
        geom_col(position = "dodge") +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
        scale_x_continuous(limits = c(1980, 2018),breaks = seq(1970, 2018, 5)) +
        labs(x = "Year",
             y = "(%)",
             caption = "Source: World Development Indicators",
             title = "Emerging Markets and Developing Economies") +
        theme_few(base_family = "Times New Roman")

grid.arrange(AE_negrowth, EMDE_negrowth )

               
             

UK <- dat2 %>% filter(country == "United Kingdom") %>% 
        ggplot(aes(year, log(gdppc))) + 
        geom_line() +
        labs(x = "",
             y = "log GDP per capita",
             title = "United Kingdom") +
        theme_few(base_family = "Times New Roman")

Japan <- dat2 %>% filter(country == "Japan") %>% 
        ggplot(aes(year, log(gdppc))) +
        geom_line() +
        labs(x ="",
             y = "",
             title = "Japan") +
        theme_few(base_family = "Times New Roman")

        
Mexico <- dat2 %>% filter(country == "Mexico") %>% 
        ggplot(aes(year, log(gdppc))) +
        geom_line() +
        labs(x ="",
             y = "log GDP per capita",
             title = "Mexico",
             caption = "") +
        theme_few(base_family = "Times New Roman")

Nigeria <- dat2 %>% filter(country == "Nigeria") %>% 
        ggplot(aes(year, log(gdppc))) +
        geom_line() +
        labs(x ="",
             y = "",
             title = "Nigeria",
             caption = "Source: World Development Indicators") +
        theme_few(base_family = "Times New Roman")


grid.arrange(UK, Japan, Mexico, Nigeria)

# All volatility
all_vol <-  dat2 %>% group_by(decade) %>% 
        mutate(growth_vol = sd(gdpgrowth, na.rm = T)) %>% 
        filter(decade %in% c(1,2,3,4)) %>% 
        distinct(growth_vol) %>% 
        ggplot(aes(decade, growth_vol)) +
        geom_col() +
        scale_y_continuous(limits = c(0,8)) +
        
        
# AEs volatility

ae_vol <-dat2 %>% filter(category2 == "AEs", decade %in% c(1,2,3,4)) %>% 
        group_by(decade) %>% 
        mutate(growth_vol = sd(gdpgrowth, na.rm = T)) %>% 
        distinct(growth_vol) %>% 
        ggplot(aes(decade, growth_vol)) +
        geom_col() +
        scale_y_continuous(limits = c(0, 8))

# EMDEs vol
emde_vol <- dat2 %>% filter(category2 == "EMDEs", decade %in% c(1,2,3,4)) %>% 
        group_by(decade) %>% 
        mutate(growth_vol = sd(gdpgrowth, na.rm = T)) %>% 
        distinct(growth_vol) %>% 
        ggplot(aes(decade, growth_vol)) +
        geom_col()


# FIgure 4: volatility of growth
dat2 %>% filter(decade %in% c(1,2,3,4), category2 %in% c("AEs", "EMDEs")) %>% 
        group_by(decade, category2) %>%
        mutate(growth_vol = sd(gdpgrowth, na.rm = T)) %>% 
        distinct(growth_vol) %>% 
        ggplot(aes(decade, growth_vol, fill = category2)) +
        geom_col(position = "dodge") +
        scale_fill_manual("Groups", values = c("AEs" = "black", "EMDEs" = "gray")) +
        scale_x_discrete(breaks = c("1","2","3","4"),labels = c("1970-89", "1990-99", "2000-09", "2010-2018"), limits = c("1","2", "3", "4")) +
        labs(x = "",
             y = "(%)",
             caption = "Source: World Development Indicators",
             fill = "Groups") +
        theme_few() 

dat2 %>% filter(decade %in% c(1,2,3,4), category != "others") %>% 
        group_by(decade, category) %>%
        mutate(growth_vol = sd(gdpgrowth, na.rm = T)) %>% 
        distinct(growth_vol) %>% 
        ggplot(aes(decade, growth_vol, fill = category)) +
        geom_col(position = "dodge") +
        scale_fill_manual("", values = c("AEs" = "black", "EMs" = "gray", "LICs" = "navy")) +
        scale_x_discrete(breaks = c("1","2","3","4"),labels = c("1970-89", "1990-99", "2000-09", "2010-2018"), limits = c("1","2", "3", "4")) +
        labs(x = "",
             y = "(%)",
             caption = "Source: World Development Indicators",
             fill = "") +
        theme_few() 

# figure 5: infation rate 

dat2 %>% filter(category2 %in% c("AEs", "EMDEs")) %>% 
        group_by(category2, year) %>% 
        summarise(med_infla = median(inflation, na.rm = T)) %>% 
        ggplot(aes(year, med_infla, linetype = category2)) +
        geom_line() +
        labs(x = "",
             y = "Inflation rate (annual %)",
             caption = "Source: World Development Indicators",
             linetype = "Groups") +
        theme_few(base_family = "Times New Roman")

dat2 %>% filter(category != "others") %>% 
        group_by(category, year) %>% 
        summarise(med_infla = median(inflation, na.rm = T)) %>% 
        ggplot(aes(year, med_infla, col = category)) +
        geom_line() +
        labs(x = "",
             y = "Inflation rate (annual %)",
             caption = "Source: World Development Indicators",
             linetype = "") +
        theme_few(base_family = "Times New Roman")


# Figure 6: high inflation
dat2 %>% filter(category2 == "EMDEs") %>% 
        group_by(year) %>% 
        summarise(above50 = mean(inflation>=50, na.rm = T)*100,
                  above100 = mean(inflation >=100, na.rm = T)*100) %>% 
        ggplot(aes(year, above50, linetype = "solid")) +
        geom_line() +
        geom_line(aes(year, above100, linetype = "dashed")) +
        scale_linetype_manual(values = c("dashed", "solid"),labels = c("Above 100%", "Above 50%")) +
        labs(x = "",
             y = "(%)",
             caption = "Source: World Development Indicators",
             linetype = "") +
        theme_few(base_family = "Times New Roman")


# Current Account
# Figure 7
dat2 %>% filter(category2 %in% c("AEs", "EMDEs"), decade != 0) %>% 
        group_by(decade, category2) %>% 
        summarise(med_ca = median(ca_per_gdp, na.rm = T)) %>% 
        ggplot(aes(decade, med_ca, fill = category2)) +
        geom_col(position = "dodge") +
        scale_fill_manual("", values = c("AEs" = "black", "EMDEs" = "gray")) +
        scale_x_discrete(breaks = c("1","2","3","4"),labels = c("1970-89", "1990-99", "2000-09", "2010-2018"), limits = c("1","2", "3", "4")) +
        labs(x = "",
             y = "(%)",
             caption = "Source: World Development Indicators",
             fill = "") +
        theme_few(base_family = "Times New Roman") 

dat2 %>% filter(category != "others", decade != 0) %>% 
        group_by(decade, category) %>% 
        summarise(med_ca = median(ca_per_gdp, na.rm = T)) %>% 
        ggplot(aes(decade, med_ca, fill = category)) +
        geom_col(position = "dodge") +
        scale_fill_manual("", values = c("AEs" = "black", "EMs" = "gray", "LICs" = "navy")) +
        scale_x_discrete(breaks = c("1","2","3","4"),labels = c("1970-89", "1990-99", "2000-09", "2010-2018"), limits = c("1","2", "3", "4")) +
        labs(x = "",
             y = "(%)",
             caption = "Source: World Development Indicators",
             fill = "") +
        theme_few(base_family = "Times New Roman") 


# Reserves
# Figure 8
dat2 %>% filter(category2 != "others", decade != 0) %>% 
        group_by(decade, category2) %>% 
        summarise(med_ca = median(reserves, na.rm = T)) %>% 
        ggplot(aes(decade, log(med_ca), fill = category2)) +
        geom_col(position = "dodge") +
        scale_fill_manual("", values = c("AEs" = "black", "EMDEs" = "gray")) +
        scale_x_discrete(breaks = c("1","2","3","4"),labels = c("1970-89", "1990-99", "2000-09", "2010-2018"), limits = c("1","2", "3", "4")) +
        labs(x = "",
             y = "current US$",
             caption = "Source: World Development Indicators",
             fill = "") +
        theme_few(base_family = "Times New Roman") 

dat2 %>% filter(category != "others", decade != 0) %>% 
        group_by(year, category) %>% 
        summarise(med_ca = mean(reserves, na.rm = T)) %>% 
        ggplot(aes(year, med_ca, linetype = category)) +
        geom_line() +
        scale_linetype_manual(values = c("solid", "dashed", "dotted"))  +
        labs(x = "",
             y = "current US$",
             caption = "Source: World Development Indicators",
             linetype = "") +
        theme_few(base_family = "Times New Roman") 
        
        
  


# gov debt
dat2 %>% filter(category != "others", decade %in% c(2,3,4)) %>% 
        group_by(decade, category) %>% 
        summarise(med_debt = median(gov_debt_per_gdp, na.rm = T)) %>% 
        ggplot(aes(decade, med_debt, fill = category)) +
        geom_col(position = "dodge") +
        scale_fill_manual("", values = c("AEs" = "black", "EMs" = "gray", "LICs" = "navy")) +
        scale_x_discrete(breaks = c("1","2","3","4"),labels = c("1969-1989", "1990-99", "2000-09", "2010-2018"), limits = c("", "2", "3", "4")) +
        labs(x = "",
             y = "(%)",
             caption = "Source: World Development Indicators",
             fill = "") +
        theme_few(base_family = "Times New Roman") 

dat2 %>% filter(category != "others", decade %in% c(2,3,4)) %>% 
        group_by(year, category) %>% 
        summarise(med_debt = mean(gov_debt_per_gdp, na.rm = T)) %>% 
        ggplot(aes(year, med_debt, linetype = category)) +
        geom_line() +
        scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
        scale_x_continuous(breaks = seq(1990, 2020, 5)) +
        labs(x = "",
             y = "(%)",
             caption = "Source: World Development Indicators",
             linetype = "") +
        theme_few(base_family = "Times New Roman") 
        
        


# FDI

dat2 %>% filter(category != "others", decade != 0) %>% 
        group_by(year, category) %>% 
        summarise(med_fdi = median(fdi_inflow_per_gdp, na.rm = T)) %>% 
        ggplot(aes(year, med_fdi, linetype = category)) +
        geom_line() +
        scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
        labs(x = "",
             y = "(%)",
             caption = "Source: World Development Indicators",
             fill = "") +
        theme_few(base_family = "Times New Roman") 

# Counter-cyclicality


dat2 %>% filter(category != "others", decade != 0) %>% 
        group_by(decade, country, category2) %>%
        summarise(counter = cor(gdppc, gov_exp_per_gdp)) %>%
        filter(decade == 2, !is.na(counter)) %>% 
        ggplot(aes(reorder(country, counter), counter, fill = category2)) +
        geom_col() +
        scale_fill_manual(values = c("black", "gray")) +
        coord_flip() +
        labs(x = "",
             y = "Corr(G, GDP)",
             fill ="",
             caption = "Note: countercyclicality of fiscal policy is measured by correlation of government expenditure and real GDP, 
             Source: World Development Indicators") +
        theme_few(base_family = "Times New Roman") +
        theme(
                axis.text.y = element_text(size = 10)
        )


dat2 %>% filter(category != "others", decade != 0) %>% 
        group_by(decade, country, category2) %>%
        summarise(counter = cor(gdppc, gov_exp_per_gdp)) %>%
        filter(decade == 3, !is.na(counter)) %>% 
        ggplot(aes(reorder(country, counter), counter, fill = category2)) +
        geom_col() +
        scale_fill_manual(values = c("black", "gray")) +
        coord_flip() +
        labs(x = "",
             y = "Corr(G, GDP)",
             fill ="",
             caption = "Note: countercyclicality of fiscal policy is measured by correlation of government expenditure and real GDP, 
             Source: World Development Indicators") +
        theme_few(base_family = "Times New Roman") +
        theme(
                axis.text.y = element_text(size = 10)
        )

        

n <- max(length(AE), length(EM), length(LIC))
length(AE) <- n
length(EM) <- n
length(LIC) <- n
economy_groups <- cbind(AE, EM, LIC)
rio::export(economy_groups, "econ_groups.xlsx")








