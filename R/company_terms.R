


company_terms <- list(
  "Unifiltered Tweets" = list("Unfiltered Tweets" = "NoFilter"),
  "DAX" = list(

                   "Adidas" = "adidas",
                   "Allianz" = "Allianz",


                  "BASF" = "BASF",
                  "Bayer" = "Bayer",
                  "Beiersdorf",
                  "BMW",

                  "Continental",
                  "Covestro",
                  "Daimler",
                  "Delivery Hero",
                  "Deutsche Bank",
                  "Deutsche Börse",
                  "Deutsche Post",
                  "Deutsche Telekom",
                  "Deutsche Wohnen",

                  "EON",
                  "Fresenius",
                  "Fresenius Medical Care",
                  "HeidelbergCement",
                  "Henkel",
                  "Infineon Teechnologies",
                  "Linde",
                  "Merck (German)" = "MerckDE",
                  "MTU Aero Engines",
                  "Münchener Rück",
                  "RWE",
                  "SAP",
                  "Siements",
                  "Volkswagen",
                  "Vonovia"
                  ),


"Dow Jones Industrial" = list( "3M" = "3M",
              "American Express" = "American Express",
              "Amgen" = "Amgen",
              "Apple" = "Apple",
              "Boeing",
              "Caterpillar",
              "Chevron",
              "Cisco Systems" = "Cisco",
              "Coca-Cola",
              "Dow",
                "Goldman Sachs",
                "Home Depot",
                  "Honeywell International",
                  "IBM",
                  "Intel",
                  "Johnson & Johnson" = "JohnsonJohnson",
                  "JPMorgan",
                 "McDonald's",
                "Merck (American)" = "MerckEN",
              "Microsoft",
                 "NIKE" = "NIKE",
                  "Procter & Gamble = Procter Gamble",

                  "Salesforce" = "salesforce",
                  "Traverlers Companies",
            "UnitedHealth",
                  "Verizon",
                  "Visa",
              "Walgreens Boots Alliance",
                  "Walmart",
                  "Walt Disney"
                  )
)







us_names <- c("Dow Jones Industrial",
              "Apple", "Amgen", "American Express", "Boeing",
              "Caterpillar", "Salesforce", "Cisco Systems", "Chevron",
              "Walt Disney", "Dow", "Goldman Sachs",
              "Home Depot", "Honeywell International", "IBM",
              "Intel", "Johnson & Johnson", "JPMorgan Chase", "Coca-Cola",
              "McDonald's", "3M", "Merck (American)", "Microsoft",
              "NIKE", "Procter & Gamble", "The Travelers Companies",
              "UnitedHealth", "Visa", "Verizon",
              "Walgreens Boots Alliance", "Walmart")

us_tickers <- c("DJI", "AAPL", "AMGN", "AXP", "BA", "CAT", "CRM", "CSCO", "CVX", "DIS",
                "DOW", "GS", "HD", "HON", "IBM", "INTC", "JNJ", "JPM", "KO",
                "MCD", "MMM", "MRK", "MSFT", "NKE", "PG", "TRV", "UNH", "V",
                "VZ", "WBA", "WMT")


ger_names <- c("DAX",
               "Covestro", "Adidas", "Allianz", "BASF", "Bayer", "Beiersdorf",
               "BMW", "Continental", "Daimler", "Deutsche Börse",
               "Deutsche Bank", "Delivery Hero", "Deutsche Post", "Deutsche Telekom",
               "Deutsche Wohnen", "EON", "Fresenius Medical Care", "Fresenius",
               "HeidelbergCement", "Henkel", "Infineon Technologies", "Linde",
               "Merck (German)", "MTU Aero Engines",
               "Münchener Rück", "RWE", "SAP",
               "Siemens", "Vonovia", "Volkswagen")

ger_tickers <- c("GDAXI",
                 "1COV.DE", "ADS.DE", "ALV.DE", "BAS.DE", "BAYN.DE", "BEI.DE",
                   "BMW.DE", "CON.DE", "DAI.DE", "DB1.DE", "DBK.DE", "DHER.DE",
                   "DPW.DE", "DTE.DE", "DWNI.DE", "EOAN.DE", "FME.DE", "FRE.DE",
                   "HEI.DE", "HEN3.DE", "IFX.DE", "LIN.DE", "MRK.DE", "MTX.F",
                   "MUV2.DE", "RWE.DE", "SAP.DE", "SIE.DE", "VNA.DE", "VOW3.DE")


company_terms_stock_us <- setNames(as.list(us_tickers), us_names)
company_terms_stock_ger <- setNames(as.list(ger_tickers), ger_names)

company_terms_stock <- list(
  "Dow Jones Industrial" = company_terms_stock_us,
  "DAX" = company_terms_stock_ger
)


# us <- readr::read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Yahoo/USA/USA_Index_Components.csv")
# ger <- readr::read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Yahoo/Germany/Germany_Index_Components.csv")
#
# dput(us$`Company Name`)
# dput(us$Symbol)
#
# dput(ger$`Company Name`)
# dput(ger$Symbol)

