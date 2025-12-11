# =========================================================
# 📦 All-in-One R Shiny Dashboard App (Final Themed Version)
# Theme: Musinsa Yuva (Red-Orange, Beige, Playfair Display Font)
# =========================================================

# 1. 필수 패키지 로드
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, tidyverse, lubridate, plotly, DT, scales,
               readxl, stringr, leaflet, sf, forecast, ggrepel, ggplot2, geojsonio, readr)
library(sf)

# =========================================================
# 2. 데이터 로드 및 전처리 (수정 없음)
# =========================================================

# --- [환율 및 경로 설정] ---
EXCHANGE_RATE_INR_TO_USD <- 83 # 1 USD = 83 INR (2024년 평균치 기준)
INR_TO_USD <- function(amount) { amount / EXCHANGE_RATE_INR_TO_USD }
# --------------------------------------------------------

# ⚠️ 사용자 환경에 맞게 경로를 수정해 주세요.



PATH_AMAZON_SALES <- "C:/Users/cmdys/Downloads/eda4/Amazon Sale Report.csv"
PATH_INTL_SALES <- "C:/Users/cmdys/Downloads/eda4/International sale Report.csv"
PATH_STOCK <- "C:/Users/cmdys/Downloads/eda4/Sale Report.csv"
PATH_APP1_RAW_PINCODE <- "C:/Users/cmdys/Downloads/eda4/All-India-Pincode-list-with-latitude-and-longitude.csv"
PATH_JOINED_FLIPKT <- "C:/Users/cmdys/Downloads/eda4/joined_flipkt_data.csv"
PATH_JOINED_AMAZON <- "C:/Users/cmdys/Downloads/eda4/joined_amazon_data.csv"
PATH_CITIES_GEO <- "C:/Users/cmdys/Downloads/eda4/india_cities_geo.csv"
PATH_APP1_RAW_EXCEL <- "C:/Users/cmdys/Downloads/eda4/Amazon Sale Report.xlsx"

india_cities <- c(
  "Mumbai", "Navi Mumbai", "Thane", "Delhi", "New Delhi", "Bengaluru", "Bangalore", "Chennai", "Hyderabad", "Kolkata", "Pune", "Ahmedabad", "Noida", "Greater Noida", "Gurgaon", "Gurugram", "Faridabad", "Ghaziabad", "Meerut", "Sonipat", "Panipat", "Rohtak", "Rewari", "Palwal", "Nagpur", "Nashik", "Aurangabad", "Solapur", "Kolhapur", "Satara", "Sangli", "Jalgaon", "Akola", "Amravati", "Latur", "Nanded", "Beed", "Surat", "Vadodara", "Baroda", "Rajkot", "Bhavnagar", "Jamnagar", "Junagadh", "Morbi", "Surendranagar", "Mehsana", "Patan", "Jaipur", "Udaipur", "Ajmer", "Jodhpur", "Kota", "Bikaner", "Alwar", "Bhilwara", "Chittorgarh", "Sikar", "Sri Ganganagar", "Hanumangarh", "Barmer", "Jaisalmer", "Nagaur", "Tonk", "Churu", "Lucknow", "Kanpur", "Varanasi", "Agra", "Mathura", "Jhansi", "Etawah", "Firozabad", "Bareilly", "Moradabad", "Rampur", "Shahjahanpur", "Saharanpur", "Muzaffarnagar", "Bijnor", "Prayagraj", "Allahabad", "Gorakhpur", "Deoria", "Ayodhya", "Faizabad", "Hardoi", "Indore", "Bhopal", "Ujjain", "Ratlam", "Gwalior", "Jabalpur", "Rewa", "Satna", "Sagar", "Dewas", "Chhindwara", "Betul", "Khargone", "Raipur", "Bilaspur", "Durg", "Bhilai", "Korba", "Ambikapur", "Jagdalpur", "Mysuru", "Mysore", "Hubli", "Dharwad", "Mangalore", "Udupi", "Belagavi", "Bellary", "Davangere", "Shimoga", "Chitradurga", "Bagalkot", "Coimbatore", "Madurai", "Salem", "Erode", "Tiruchirappalli", "Trichy", "Tirunelveli", "Thoothukudi", "Tuticorin", "Vellore", "Kanchipuram", "Cuddalore", "Dindigul", "Karur", "Namakkal", "Warangal", "Karimnagar", "Khammam", "Nizamabad", "Ramagundam", "Vijayawada", "Guntur", "Visakhapatnam", "Vizag", "Rajahmundry", "Kakinada", "Eluru", "Ongole", "Tirupati", "Chittoor", "Anantapur", "Kadapa", "Machilipatnam", "Kochi", "Cochin", "Thiruvananthapuram", "Trivandrum", "Thrissur", "Palakkad", "Alappuzha", "Kottayam", "Malappuram", "Kannur", "Kasaragod", "Pathanamthitta", "Chandigarh", "Amritsar", "Ludhiana", "Jalandhar", "Patiala", "Bathinda", "Hoshiarpur", "Ambala", "Karnal", "Patna", "Gaya", "Bhagalpur", "Muzaffarpur", "Darbhanga", "Purnia", "Ranchi", "Jamshedpur", "Dhanbad", "Bokaro", "Hazaribagh", "Deoghar", "Howrah", "Hooghly", "Durgapur", "Asansol", "Siliguri", "Kharagpur", "Midnapore", "Malda", "Raiganj", "Balurghat", "Guwahati", "Shillong", "Imphal", "Agartala", "Aizawl", "Kohima", "Dimapur", "Itanagar", "Dehradun", "Haridwar", "Roorkee", "Haldwani", "Jammu", "Srinagar"
)

if (file.exists(PATH_APP1_RAW_EXCEL) && file.exists(PATH_APP1_RAW_PINCODE)) {
  # 🌟 App 1: 파일 로드
  joined_flipkt <- read_csv(PATH_JOINED_FLIPKT, show_col_types = FALSE)
  joined_amazon <- read_csv(PATH_JOINED_AMAZON, show_col_types = FALSE)
  india_cities_geo <- read_csv(PATH_CITIES_GEO, show_col_types = FALSE)
  orders_raw <- read_excel(PATH_APP1_RAW_EXCEL)
  
  orders <- orders_raw %>%
    rename(
      order_id  = `Order ID`, date = Date, status = Status, fulfilment = Fulfilment,
      qty    = Qty, amount = Amount, category = Category, ship_city = `ship-city`,
      ship_state = `ship-state`, postal_code = `ship-postal-code`,
      promo_id  = `promotion-ids`, size = Size
    ) %>%
    mutate(
      date = mdy(date),
      postal_code = as.character(postal_code),
      fulfilment_mode = case_when(str_detect(fulfilment, regex("amazon", ignore_case = TRUE)) ~ "Amazon (FBA)", TRUE ~ "Merchant / 3PL"),
      is_shipped  = str_detect(status, regex("shipped|delivered", ignore_case = TRUE)),
      is_cancelled = str_detect(status, regex("cancel|return|refund", ignore_case = TRUE)),
      is_failed  = is_cancelled,
      # ⚠️ USD로 변환
      order_value  = INR_TO_USD(amount),
      price_bucket = case_when(is.na(order_value) ~ "Unknown",
                               # USD 기준으로 버킷 조정 (기존 INR 300/700 기준을 83으로 나눔)
                               order_value < INR_TO_USD(300) ~ "Low (<$3.6)",
                               order_value < INR_TO_USD(700) ~ "Mid ($3.6–$8.4)",
                               TRUE ~ "High (≥$8.4)"),
      is_promo = !is.na(promo_id) & promo_id != "",
      order_size = if_else(qty > 1, "Bulk", "Single"),
      product_size = if_else(is.na(size) | size == "", "Unknown", as.character(size)),
      ship_city_clean = str_to_title(ship_city),
      popular_ship_city = ifelse(ship_city_clean %in% india_cities, ship_city_clean, "Other")
    )
  
  
  # Pincode → 위도/경도 매칭
  pincode_ref_raw <- read_csv(PATH_APP1_RAW_PINCODE, show_col_types = FALSE)
  pincode_ref_unique <- pincode_ref_raw %>% transmute(
    pincode = as.character(Pincode), lat = Latitude, lon = Longitude, state = State, district = District, area = `CityName/AreaName`
  ) %>% group_by(pincode) %>% summarise(lat = first(lat), lon = first(lon), state = first(state), district = first(district), area = first(area), .groups = "drop")
  
  orders <- orders %>% left_join(pincode_ref_unique, by = c("postal_code" = "pincode"))
  
  # orders + 도시 좌표 join (Map View에서 사용)
  orders_geo <- orders %>%
    mutate(city_clean = str_squish(str_to_title(popular_ship_city))) %>%
    left_join(india_cities_geo, by = c("city_clean" = "City"))
  
  # 공통 설정값 (App 1)
  has_category <- "category" %in% names(orders)
  category_choices <- if (has_category) { sort(unique(orders$category[!is.na(orders$category)])) } else { "All" }
  min_date <- min(orders$date, na.rm = TRUE)
  max_date <- max(orders$date, na.rm = TRUE)
  
  summarise_by_mode <- function(dat) {
    if (nrow(dat) == 0) return(tibble())
    dat %>% group_by(fulfilment_mode) %>% summarise(
      orders_total = n_distinct(order_id), shipped_orders = sum(is_shipped, na.rm = TRUE),
      failed_orders = sum(is_failed, na.rm = TRUE), shipped_rate = shipped_orders / orders_total,
      cancel_rate = failed_orders / orders_total, total_sales = sum(order_value, na.rm = TRUE),
      cancelled_sales = sum(order_value[is_failed], na.rm = TRUE), avg_order_value = mean(order_value, na.rm = TRUE), .groups = "drop"
    ) %>% mutate(shipped_rate = round(shipped_rate * 100, 1), cancel_rate = round(cancel_rate * 100, 1))
  }
  city_choices <- orders %>% filter(!is.na(popular_ship_city)) %>% pull(popular_ship_city) %>% unique() %>% sort()
  
  app1_available <- TRUE
  metric_labels <- c(
    orders_total = "Total Orders",
    shipped_rate = "Shipped Rate (%)",
    cancel_rate = "Cancel Rate (%)",
    # ⚠️ USD로 변경
    total_sales = "Total Sales (USD)",
    cancelled_sales = "Cancelled Sales (USD)",
    avg_order_value = "Average Order Value (USD)"
  )
  # ⚠️ USD로 변경
  CURRENCY_UNIT <- "USD"
  
} else {
  warning("[File Missing] App 1의 필수 데이터 파일이 없습니다. 경로를 확인해주세요.")
  app1_available <- FALSE
}


# 2-B. GlobalThreads Analytics (App 2) 데이터 전처리 (수정 없음)
if (all(file.exists(PATH_AMAZON_SALES), file.exists(PATH_INTL_SALES), file.exists(PATH_STOCK))) {
  file_amazon <- read.csv(PATH_AMAZON_SALES, stringsAsFactors = FALSE)
  
  # SKU Code 변환
  file_stock_names <- make.names(names(read.csv(PATH_STOCK, nrows=1, stringsAsFactors = FALSE)))
  sku_col <- file_stock_names[grep("SKU|Code", file_stock_names, ignore.case = T)[1]]
  
  file_intl  <- read.csv(PATH_INTL_SALES, stringsAsFactors = FALSE)
  file_stock <- read.csv(PATH_STOCK, stringsAsFactors = FALSE)
  
  # --- Amazon Data ---
  df_amazon <- file_amazon %>%
    filter(!Status %in% c("Cancelled", "Returned")) %>%
    mutate(Date = parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
           Date = as.Date(Date),
           # ⚠️ USD로 변환
           Amount = INR_TO_USD(as.numeric(ifelse(Amount == "", 0, Amount))),
           # [✅ 추가] App 2 상세 분석을 위한 State 명칭 통일
           ship.state = str_to_title(ship.state))
  
  # [✅ 추가] App 2 Dropdown을 위한 State 리스트
  state_choices <- df_amazon %>%
    filter(!is.na(ship.state) & ship.state != "") %>%
    pull(ship.state) %>%
    unique() %>%
    sort()
  
  # --- International (RFM) ---
  df_intl <- file_intl %>%
    mutate(DATE = parse_date_time(DATE, orders = c("mdy", "dmy", "ymd")),
           DATE = as.Date(DATE),
           # ⚠️ USD로 변환
           GROSS.AMT = INR_TO_USD(as.numeric(GROSS.AMT))) %>%
    filter(!is.na(DATE))
  
  # --- RFM Calculation ---
  ref_date <- max(df_intl$DATE, na.rm = TRUE)
  df_rfm <- df_intl %>% group_by(CUSTOMER) %>%
    summarise(Recency = as.numeric(ref_date - max(DATE)), Frequency = n(),
              Monetary = sum(GROSS.AMT, na.rm = TRUE))
  
  med_R <- median(df_rfm$Recency, na.rm = TRUE)
  med_M <- median(df_rfm$Monetary, na.rm = TRUE)
  
  df_rfm <- df_rfm %>% mutate(
    Segment = case_when(
      Recency <= med_R & Monetary >= med_M ~ "Champions",
      Recency > med_R & Monetary >= med_M ~ "At Risk VIP",
      Recency <= med_R & Monetary < med_M ~ "Recent Users",
      TRUE ~ "Hibernating"
    ))
  
  # --- Inventory Data ---
  df_stock <- file_stock %>% rename(SKU = all_of(sku_col)) %>%
    mutate(SKU = str_to_upper(SKU),
           SKU = str_trim(SKU),  
           SKU = str_replace_all(SKU, "[^A-Z0-9-]", ""))
  
  df_amazon_sales <- file_amazon %>%
    group_by(SKU) %>%
    summarise(Total_Sold = sum(Qty, na.rm = TRUE))
  
  df_amazon_sales <- df_amazon_sales %>%
    mutate(SKU = str_to_upper(SKU), SKU = str_trim(SKU), SKU = str_replace_all(SKU, "[^A-Z0-9-]", ""))
  
  df_inventory <- df_stock %>% mutate(Stock = as.numeric(Stock)) %>%
    left_join(df_amazon_sales, by = "SKU") %>%
    mutate(Total_Sold = replace_na(Total_Sold, 0))
  
  df_inventory <- df_inventory %>%
    mutate(
      Stock_Risk = case_when(Stock <= 20 ~ "Critical", Stock <= 40 ~ "Low", Stock <= 100 ~ "Medium", Stock <= 200 ~ "High", TRUE ~ "Excess"),
      Sales_Risk = case_when(Total_Sold <= 10 ~ "Very Low", Total_Sold <= 60 ~ "Low", Total_Sold <= 90 ~ "Medium", Total_Sold <= 200 ~ "High", TRUE ~ "Very High")
    ) %>%
    mutate(
      Status = case_when(
        Stock == 0 & Total_Sold > 0 ~ "Out of Stock / Lost Sales", Stock <= 20 ~ "Critical Risk",
        Sales_Risk %in% c("High","Very High") & Stock_Risk %in% c("Critical","Low") ~ "Critical Risk",
        Sales_Risk %in% c("Medium","High","Very High") & Stock_Risk == "Critical" ~ "High Risk",
        Sales_Risk %in% c("Very Low","Low") & Stock_Risk %in% c("High","Excess") ~ "Overstock",
        Sales_Risk %in% c("Very Low","Low") & Stock_Risk %in% c("Low","Medium") ~ "Low Performer",
        TRUE ~ "Healthy"
      ))
  app2_available <- TRUE
} else {
  warning("App 2 데이터 파일 중 일부가 없습니다. 해당 탭은 작동하지 않을 수 있습니다.")
  app2_available <- FALSE
}


# 2-C. Musinsa EDA & ARIMA (App 3) 데이터 전처리 (수정 없음)
if (file.exists(PATH_AMAZON_SALES)) {
  raw_data_musinsa <- read.csv(PATH_AMAZON_SALES, stringsAsFactors = FALSE)
  colnames(raw_data_musinsa) <- make.names(colnames(raw_data_musinsa))
  
  raw_data_musinsa$Date <- parse_date_time(raw_data_musinsa$Date, orders = c("mdy", "dmy", "ymd"))
  df_musinsa <- raw_data_musinsa %>%
    mutate(Date = as.Date(Date)) %>%
    filter(!is.na(Date)) %>%
    filter(Date >= as.Date("2022-04-01") & Date <= as.Date("2022-06-29"))
  
  df_musinsa$Amount[is.na(df_musinsa$Amount)] <- 0
  df_musinsa$ship.state <- toupper(df_musinsa$ship.state)
  
  categories_musinsa <- unique(df_musinsa$Category)
  statuses_musinsa <- unique(df_musinsa$Status)
  
  india_geojson_url <- "https://gist.githubusercontent.com/jbrobst/56c13bbbf9d97d187fea01ca62ea5112/raw/e388c4cae20aa53cb5090210a42ebb9b765c0a36/india_states.geojson"
  
  india_map <- tryCatch({
    map_data <- st_read(india_geojson_url, quiet = TRUE)
    map_data$ST_NM_UPPER <- str_to_upper(map_data$ST_NM)
    map_data
  }, error = function(e) { NULL })
  
  app3_available <- TRUE
} else {
  warning("App 3 데이터 파일이 없습니다. 해당 탭은 작동하지 않을 수 있습니다.")
  app3_available <- FALSE
}


# =========================================================
# 3. User Interface (UI) - Musinsa Yuva 테마 적용 (Final Version)
# =========================================================

ui_integrated <- dashboardPage(
  # 🎨 SKIN 변경: 'blue'로 설정하고, CSS로 모든 색상 오버라이드
  skin = "blue",
  
  # 🌟 헤더 제목 변경: Musinsa Yuva 컨셉 적용
  dashboardHeader(title = "Musinsa Yuva: India Market Strategy"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("1. Fulfilment (Overview - USD)", tabName = "tab_fulfilment", icon = icon("truck-loading")),
      if (app2_available) menuItem("2. RFM / Inventory (USD)", tabName = "tab_global", icon = icon("globe-asia")),
      if (app3_available) menuItem("3. EDA / ARIMA / Map (USD)", tabName = "tab_musinsa", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    # 🌟 [Musinsa Yuva Final Theme CSS]
    tags$head(
      tags$style(HTML("
    /* 폰트 스타일 (명조/세리프 계열 폰트 사용) */
    @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&family=Roboto:wght@400;500&display=swap');
    
    body, .content-wrapper, .right-side {
     font-family: 'Roboto', sans-serif;
     color: #333333; /* 짙은 텍스트 색상 */
    }
    
    /* 전체 배경색 (밝은 베이지) */
    .content-wrapper, .right-side, .main-footer, .box-body { 
     background-color: #FAF3E8 !important; 
    }
    
    /* 헤더 및 로고 (Primary Color: 짙은 붉은 주황) */
    .main-header .navbar, .main-header .logo { 
     background-color: #C4593E !important; /* 짙은 붉은 오렌지색 */
     color: #FAF3E8 !important;
     font-family: 'Playfair Display', serif; /* 명조 계열 폰트 */
     font-weight: 700;
     font-size: 20px;
    }
    
    /* 사이드바 (Accent Color: 밝은 주황) */
    .main-sidebar { 
     background-color: #F39C12 !important; /* 밝은 오렌지색 */
    }
    
    /* 활성 메뉴 (Primary Color) */
    .sidebar-menu>li.active>a, .sidebar-menu>li.menu-open>a {
     background-color: #C4593E !important; 
     color: #FAF3E8 !important;
    }
    
    /* 사이드바 텍스트 */
    .sidebar-menu a { 
     color: #333333 !important; 
     font-weight: 500;
    }
    
    /* Box Header 색상 */
    /* Musinsa danger (짙은 붉은 주황) */
    .box.box-solid.box-danger>.box-header, .small-box.bg-red { 
     background: #C4593E !important; 
     color: #FAF3E8 !important;
    }
    /* Info info (밝은 붉은 주황) */
    .box.box-solid.box-info>.box-header, .small-box.bg-purple { 
     background: #E77E6A !important; 
     color: #FAF3E8 !important;
    }
    /* Primary (짙은 남색) */
    .box.box-solid.box-primary>.box-header, .small-box.bg-blue { 
     background: #2C3E50 !important; 
     color: #FAF3E8 !important;
    }
        /* Value Box 배경색을 베이지색 계열로 변경 */
        .small-box { background-color: #FFFDF7 !important; color: #333333 !important; }
        .small-box h3, .small-box p { color: #333333 !important; }
    
    /* Plotly/ggplot 배경색 오버라이드 */
    .shiny-plot-output {
     background-color: #FFFDF7 !important; 
    }
        
        /* Value Header 및 Info Box의 Value 색상 조정 */
        .small-box .inner > h3 {
            font-family: 'Playfair Display', serif;
            color: #C4593E !important; /* 숫자는 짙은 주황색으로 강조 */
        }
   "))
    ),
    tabItems(
      # --- Tab 1: Fulfilment Analysis ---
      tabItem(tabName = "tab_fulfilment",
              fluidPage(
                titlePanel("Fulfilment Effect Analysis (Amazon FBA vs Merchant / 3PL)"),
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput("date_range", "Order Date", start = min_date, end = max_date, min = min_date, max = max_date),
                    selectInput("category", "Category", choices = c("All", category_choices), selected = "All"),
                    selectInput("city", "City", choices = c("All", city_choices), selected = "All"),
                    checkboxGroupInput("fulfilment", "Fulfilment Type", choices = c("Amazon (FBA)", "Merchant / 3PL"), selected = c("Amazon (FBA)", "Merchant / 3PL")),
                    checkboxGroupInput("price_bucket", "Price Range", choices = c("Low (<$3.6)", "Mid ($3.6–$8.4)", "High (≥$8.4)"), selected = c("Low (<$3.6)", "Mid ($3.6–$8.4)", "High (≥$8.4)")),
                    checkboxGroupInput("order_size", "Order Size", choices = c("Single", "Bulk"), selected = c("Single", "Bulk")),
                    radioButtons("promo_filter", "Promotion Filter", choices = c("All orders" = "all", "Non-promotion only" = "non", "Promotion only" = "promo"), selected = "all")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Overview (Tab 1)", h4("Fulfilment Performance – Executive Summary"), tableOutput("overview_table"), br(), h4("Key Comparison"), fluidRow(column(6, plotOutput("rate_plot", height = "300px")), column(6, plotOutput("sales_plot", height = "300px")))),
                      tabPanel("Deep Dive (Tab 2)", fluidRow(column(4, checkboxGroupInput("outcomes", "Outcome Metrics", choices = c("Orders" = "orders_total", "Shipped Rate (%)" = "shipped_rate", "Cancel Rate (%)" = "cancel_rate", "Total Sales" = "total_sales", "Cancelled Sales" = "cancelled_sales", "AOV" = "avg_order_value"), selected = c("orders_total", "shipped_rate", "cancel_rate")), radioButtons("viz_type", "Visualization Type", choices = c("Bar Chart" = "bar", "Scatter Plot" = "scatter"), inline = TRUE)), column(8, h4("Selected Outcome Comparison"), tableOutput("table2_summary"), br(), plotOutput("table2_plot", height = "400px"))))
                    )
                  )
                )
              )
      ),
      
      # --- Tab 2: GlobalThreads Analytics (RFM/Inventory) ---
      if (app2_available) tabItem(tabName = "tab_global",
                                  h2("2. GlobalThreads Analytics (RFM & Inventory)"),
                                  tabsetPanel(
                                    tabPanel("Market Pulse (Amazon)", icon = icon("amazon"), br(),
                                             fluidRow(valueBoxOutput("box_rev"), valueBoxOutput("box_orders"), valueBoxOutput("box_states")),
                                             fluidRow(box(title = "Top Categories by Order Volume", plotlyOutput("plot_cat"), width = 12)),
                                             fluidRow(box(title = "Top 10 Cities: Category Breakdown (Stacked)", plotlyOutput("plot_map"), width = 12)),
                                             
                                             fluidRow(
                                               box(
                                                 # 오류 문자 제거 완료
                                                 title = "State-wise Deep Dive: City & Product Sales",
                                                 width = 12, status = "info", solidHeader = TRUE,
                                                 # 오류 문자 제거 완료
                                                 selectInput("input_state_detailed", "Select State to Analyze:",
                                                             choices = if(exists("state_choices")) state_choices else c(),
                                                             selected = if(exists("state_choices") && length(state_choices)>0) state_choices[1] else NULL),
                                                 plotlyOutput("plot_city_dodge", height = "500px")
                                               )
                                             ),
                                             
                                             fluidRow(box(title = "🏆 Best Selling Product (SKU) by State", DTOutput("table_state_best"), width = 12, status = "primary", solidHeader = TRUE))
                                    ),
                                    
                                    tabPanel("VIP Optimizer (Intl RFM)", icon = icon("users"), br(),
                                             fluidRow(infoBoxOutput("box_champ_count"), infoBoxOutput("box_risk_count"), infoBoxOutput("box_avg_clv")),
                                             fluidRow(
                                               box(title = "RFM 4-Quadrant Analysis", plotlyOutput("plot_rfm"), width = 8),
                                               box(title = "Quadrant Thresholds (Median Split)", width = 4, status = "primary",
                                                   "Segments are divided by the data median:", br(),
                                                   tags$ul(
                                                     tags$li(tags$b("Median Recency:"), paste0(round(med_R, 0), " Days")),
                                                     tags$li(tags$b("Median Spend:"), paste0("$", comma(round(med_M, 0)))) 
                                                   ),
                                                   hr(), "Segment Logic:",
                                                   tags$ul(
                                                     tags$li(tags$b("Champions (Green):"), paste0("Visited ≤ ", round(med_R,0), " days & Spent ≥ $", comma(round(med_M,0)))), 
                                                     tags$li(tags$b("At Risk VIP (Red):"), paste0("Visited > ", round(med_R,0), " days & Spent ≥ $", comma(round(med_M,0)))), 
                                                     tags$li(tags$b("Recent Users (Blue):"), paste0("Visited ≤ ", round(med_R,0), " days & Spent < $", comma(round(med_M,0)))), 
                                                     tags$li(tags$b("Hibernating (Gray):"), paste0("Visited > ", round(med_R,0), " days & Spent < $", comma(round(med_M,0))))
                                                   )
                                               )
                                             ),
                                             fluidRow(box(title = "At-Risk VIP List", DTOutput("table_risk"), width = 12))
                                    ),
                                    
                                    tabPanel("Inventory Bridge", icon = icon("boxes"), br(),
                                             fluidRow(box(title = "Inventory Health Matrix (Stock vs Sales)", plotlyOutput("plot_matrix"), width = 12)),
                                             fluidRow(
                                               box(title = "Filter by Status", width = 3, selectInput("status_filter", "Select Status", choices = sort(unique(df_inventory$Status)), selected = c("Critical Risk", "Overstock"), multiple = TRUE)),
                                               box(title = "Inventory Details (Stock & Sales)", width = 9, DTOutput("table_inventory"))
                                             )
                                    )
                                  )
      ),
      
      # --- Tab 3: Musinsa EDA & ARIMA ---
      if (app3_available) tabItem(tabName = "tab_musinsa",
                                  h2("3. Musinsa India Market Strategy (EDA & Prediction)"),
                                  fluidRow(
                                    # status="danger" (Primary Color)
                                    box(title = "Analysis Control Panel", width = 3, status = "danger",
                                        dateRangeInput("dateRange", "1. 분석 기간 (Date):", start = min(df_musinsa$Date), end = max(df_musinsa$Date), min = min(df_musinsa$Date), max = max(df_musinsa$Date)),
                                        radioButtons("metricInput", "2. 분석 지표 (Metric):", choices = c("총 매출액 (Revenue/USD)" = "Amount", "주문 건수 (Count)" = "Count"), selected = "Amount"),
                                        selectInput("categoryInput", "3. 카테고리 (Category):", choices = c("All", categories_musinsa), selected = "All"),
                                        selectInput("statusInput", "4. 주문 상태 (Status):", choices = c("All", statuses_musinsa), selected = "All"),
                                        hr(), p("※ 데이터 범위: 22년 4월 ~ 6월", style="color:grey; font-size:0.8em;")
                                    ),
                                    tabBox(title = "Analysis Results", width = 9,
                                           tabPanel("📈 Trend Analysis", plotlyOutput("trendPlot", height = "500px")),
                                           tabPanel("🔍 Correlation", h4("1. 사이즈별 분포 (Size Distribution)"), p("가장 수요가 많은 사이즈를 파악하여 재고 계획을 수립합니다."), plotlyOutput("sizePlot", height = "400px"), hr(), h4("2. 카테고리별 객단가 분포 (Boxplot - USD)"), p("카테고리별 가격대(Min/Max/Median)와 이상치를 분석합니다."), plotlyOutput("boxplotPlot", height = "400px")),
                                           tabPanel("🌏 Regional Analysis", h4("지역별 매출 히트맵 (USD)"), conditionalPanel(condition = "output.mapAvailable == false", h5("⚠️ 지도를 불러올 수 없습니다. 인터넷 연결 및 패키지를 확인하세요.", style = "color:red")), plotlyOutput("statePlot", height = "650px")),
                                           tabPanel("📍 Order Map (USD)", h4("인도 도시별 판매 실적 지도 (USD)"),
                                                    radioButtons("map_metric", "시각화 기준",
                                                                 choices = c("총 판매 금액 (USD)" = "amount", "총 판매량 (Qty)" = "qty"),
                                                                 selected = "amount", inline = TRUE),
                                                    leafletOutput("salesMap", height = 600)),
                                           tabPanel("🤖 AI Prediction", h4("향후 30일 매출 시나리오 (ARIMA - USD)"), plotOutput("predPlot"), br(), verbatimTextOutput("predInsight"))
                                    )
                                  )
      )
    )
  )
)

# =========================================================
# 4. Server Logic (테마 색상 수정 적용)
# =========================================================

server_integrated <- function(input, output, session) {
  
  # --- 1. Fulfilment Analysis Logic (App 1) ---
  
  # 공통 필터
  filtered_data_app1 <- reactive({
    dat <- orders_geo
    
    if (!is.null(input$date_range)) {
      dat <- dat %>% filter(date >= input$date_range[1], date <= input$date_range[2])
    }
    if (input$category != "All") { dat <- dat %>% filter(category == input$category) }
    if (!is.null(input$city) && input$city != "All") {
      dat <- dat %>% filter(popular_ship_city == input$city)
    }
    
    dat <- dat %>% filter(fulfilment_mode %in% input$fulfilment, price_bucket %in% input$price_bucket, order_size %in% input$order_size)
    
    if (!is.null(input$promo_filter) && input$promo_filter != "all") {
      if (input$promo_filter == "promo") { dat <- dat %>% filter(is_promo) } else if (input$promo_filter == "non") { dat <- dat %>% filter(!is_promo) }
    }
    dat
  })
  
  # TAB 1 – Overview
  output$overview_table <- renderTable({
    summarise_by_mode(filtered_data_app1())
  })
  
  # 🌟 [수정] rate_plot 색상 통일
  output$rate_plot <- renderPlot({
    dat <- summarise_by_mode(filtered_data_app1()) %>% pivot_longer(cols = c(shipped_rate, cancel_rate), names_to = "metric", values_to = "value") %>% mutate(metric = factor(metric, levels = c("shipped_rate", "cancel_rate"), labels = metric_labels[c("shipped_rate", "cancel_rate")]))
    req(nrow(dat) > 0)
    ggplot(dat, aes(x = fulfilment_mode, y = value, fill = metric)) + 
      geom_col(position = "dodge") + 
      # 🌟 색상 변경: Primary (#C4593E)와 Accent (#F39C12) 적용
      scale_fill_manual(values = c("#C4593E", "#F39C12")) + 
      labs(x = "Fulfilment Type", y = "Rate (%)", title = "Shipped vs Cancel Rate") + theme_minimal()
  })
  
  # 🌟 [수정] sales_plot 색상 통일
  output$sales_plot <- renderPlot({
    dat <- summarise_by_mode(filtered_data_app1()) %>% select(fulfilment_mode, total_sales, cancelled_sales) %>% pivot_longer(cols = -fulfilment_mode, names_to = "metric", values_to = "value")
    req(nrow(dat) > 0)
    ggplot(dat, aes(x = fulfilment_mode, y = value, fill = metric)) + 
      geom_col(position = "dodge") + 
      scale_y_continuous(labels = comma) + 
      labs(x = "Fulfilment Type", y = CURRENCY_UNIT, title = "Sales vs Cancelled Sales", fill = "Metric") + 
      # 🌟 색상 변경: Dark (#2C3E50)와 Primary (#C4593E) 적용
      scale_fill_manual(values = c("#2C3E50", "#C4593E"), labels = metric_labels[c("total_sales", "cancelled_sales")]) + 
      theme_minimal()
  })
  
  # TAB 2 – Deep Dive
  selected_outcomes <- reactive({
    if (is.null(input$outcomes) || length(input$outcomes) == 0) { c("orders_total", "shipped_rate", "cancel_rate", "total_sales", "cancelled_sales", "avg_order_value") } else { input$outcomes }
  })
  
  output$table2_summary <- renderTable({
    dat <- summarise_by_mode(filtered_data_app1())
    req(nrow(dat) > 0)
    dat %>% select(fulfilment_mode, all_of(selected_outcomes())) %>% rename(!!!metric_labels)
  })
  
  output$table2_plot <- renderPlot({
    dat <- summarise_by_mode(filtered_data_app1())
    req(nrow(dat) > 0)
    outcomes <- selected_outcomes()
    
    if (input$viz_type == "bar") {
      dat_long <- dat %>% select(fulfilment_mode, all_of(outcomes)) %>% pivot_longer(cols = -fulfilment_mode, names_to = "metric", values_to = "value")
      # ⚠️ 지수 표기법 제거
      ggplot(dat_long, aes(x = fulfilment_mode, y = value, fill = fulfilment_mode)) + geom_col() + facet_wrap(~ metric, scales = "free_y", labeller = as_labeller(metric_labels)) + theme_minimal() + theme(legend.position = "none") + scale_y_continuous(labels = comma)
    } else {
      req(length(outcomes) == 2)
      # ⚠️ 지수 표기법 제거
      ggplot(dat, aes(x = .data[[outcomes[1]]], y = .data[[outcomes[2]]], label = fulfilment_mode)) + geom_point(size = 4) + geom_text_repel() + theme_minimal() + labs(x = metric_labels[outcomes[1]], y = metric_labels[outcomes[2]], title = "Outcome Trade-off (Scatter)") + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
    }
  })
  
  # Map View (Leaflet) -> Tab 3로 이동
  city_sales_aggregated <- reactive({
    filtered_ids <- filtered_data_app1() %>% pull(order_id)
    dat <- orders_geo %>% filter(order_id %in% filtered_ids) %>% filter(!is.na(Latitude), !is.na(Longitude))
    if (nrow(dat) == 0) return(tibble())
    
    dat %>% group_by(city_clean, Latitude, Longitude) %>% summarise(
      total_amount = sum(order_value, na.rm = TRUE), # order_value는 이미 USD
      total_qty = sum(qty, na.rm = TRUE),
      .groups = "drop"
    ) %>% filter(total_qty > 0 | total_amount > 0)
  })
  
  output$salesMap <- renderLeaflet({
    dat <- city_sales_aggregated()
    req(nrow(dat) > 0)
    
    sales_color_code <- if (input$map_metric == "amount") "darkred" else "darkblue"
    # ⚠️ USD로 변경
    sales_title_label <- if (input$map_metric == "amount") paste0("도시별 판매 금액 (", CURRENCY_UNIT, ")") else "도시별 판매량"
    amazon_marker_color <- "green"
    flipkart_marker_color <- "blue"
    
    dat <- dat %>% mutate(metric = if (input$map_metric == "amount") total_amount else total_qty)
    max_metric <- max(dat$metric, na.rm = TRUE); if (!is.finite(max_metric) || max_metric <= 0) max_metric <- 1
    
    leaflet(dat) %>% addTiles() %>% setView(lng = 78.9629, lat = 20.5937, zoom = 4) %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, radius = ~sqrt(metric / max_metric) * 15, stroke = FALSE, fillOpacity = 0.7, color = sales_color_code,
        # ⚠️ USD로 변경
        popup = ~paste0("<b>", city_clean, "</b><br>", "총 판매 금액: $", comma(total_amount), "<br>", "총 판매량: ", total_qty),
        group = "Sales Data"
      ) %>%
      addAwesomeMarkers(
        data = joined_flipkt, lng = ~Longitude, lat = ~Latitude, icon = awesomeIcons(icon = 'caret-up', iconColor = 'black', markerColor = flipkart_marker_color),
        popup = ~paste0("<b>Flipkart Warehouse</b><br>", "Name: ", `Warehouse Name`, "<br>", "Pincode: ", Pincode, "<br>", "Address: ", `Complete Address`), group = "Flipkart Warehouses"
      ) %>%
      addAwesomeMarkers(
        data = joined_amazon, lng = ~Longitude, lat = ~Latitude, icon = awesomeIcons(icon = 'caret-up', iconColor = 'black', markerColor = amazon_marker_color),
        popup = ~paste0("<b>Amazon FC</b><br>", "FC Code: ", `FC Code`, "<br>", "Pincode: ", Pincode, "<br>", "Address: ", Address), group = "Amazon Warehouses"
      ) %>%
      addLegend(position = "bottomright", colors = c(sales_color_code, flipkart_marker_color, amazon_marker_color), labels = c(sales_title_label, "Flipkart 물류창고 (▲)", "Amazon 물류창고 (▲)"), title = "지도 범례") %>%
      addLayersControl(baseGroups = c("Sales Data"), overlayGroups = c("Flipkart Warehouses", "Amazon Warehouses"), options = layersControlOptions(collapsed = FALSE))
  })
  
  
  # --- 2. GlobalThreads Analytics Logic (Tab 2) ---
  
  # Market Pulse
  # ⚠️ valueBox 금액에 comma() 적용 (지수 표기법 제거)
  output$box_rev <- renderValueBox({ valueBox(paste0("$", comma(round(sum(df_amazon$Amount, na.rm=T), 0))), "Revenue (USD)", icon=icon("dollar-sign"), color="green") })
  output$box_orders <- renderValueBox({ valueBox(comma(nrow(df_amazon)), "Orders", icon=icon("shopping-cart"), color="purple") }) # ⚠️ comma() 적용
  output$box_states <- renderValueBox({ valueBox(n_distinct(df_amazon$ship.state), "States", icon=icon("map-marker-alt"), color="blue") })
  
  output$plot_trend <- renderPlotly({ p <- ggplot(df_amazon %>% group_by(Date) %>% summarise(Sales=sum(Amount,na.rm=T)), aes(Date, Sales)) + geom_line(color="#007bff") + theme_minimal() + scale_y_continuous(labels = scales::comma) + labs(y = "Sales (USD)"); ggplotly(p) })
  
  # 🌟 [수정] plot_cat 색상 통일
  output$plot_cat <- renderPlotly({
    p <- ggplot(df_amazon %>% count(Category) %>% top_n(5,n), aes(x=reorder(Category,n), y=n)) +
      # 🌟 색상 변경: Primary (#C4593E) 적용
      geom_col(fill="#C4593E") + coord_flip() + theme_minimal() +
      labs(x = "Category", y = "Order Quantity (Count)") +
      # ⚠️ 지수 표기법 제거
      scale_y_continuous(labels = scales::comma)
    ggplotly(p)
  })
  
  # [✅ 변경] City Stacked Bar Plot
  output$plot_map <- renderPlotly({
    df_city <- df_amazon %>% mutate(City = str_to_title(ship.city)) %>% filter(!is.na(City) & City != "")
    top_cities <- df_city %>% count(City) %>% top_n(10, n) %>% pull(City)
    plot_data <- df_city %>% filter(City %in% top_cities) %>% count(City, Category)
    
    p <- ggplot(plot_data, aes(x = City, y = n, fill = Category)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      # ⚠️ 지수 표기법 제거
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Top 10 Cities", y = "Order Count", fill = "Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # [✅ 추가] State별 상세 분석: City별 Product Dodge Bar Plot
  output$plot_city_dodge <- renderPlotly({
    req(input$input_state_detailed) # State가 선택되어야 실행됨
    
    # 1. 선택된 State의 데이터 필터링
    state_data <- df_amazon %>%
      filter(ship.state == input$input_state_detailed) %>%
      mutate(City = str_to_title(ship.city)) %>%
      filter(!is.na(City) & City != "")
    
    # 2. 해당 State 내에서 매출 상위 8개 도시 선정 (너무 많으면 그래프가 복잡해지므로)
    top_cities_in_state <- state_data %>%
      group_by(City) %>%
      summarise(Total_Qty = sum(Qty, na.rm=T)) %>%
      slice_max(Total_Qty, n = 8) %>%
      pull(City)
    
    # 3. 그래프용 데이터 준비
    plot_data <- state_data %>%
      filter(City %in% top_cities_in_state) %>%
      group_by(City, Category) %>%
      summarise(Count = sum(Qty, na.rm=T), .groups="drop")
    
    # 4. Dodge Bar Plot 그리기
    p <- ggplot(plot_data, aes(x = City, y = Count, fill = Category)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(
        title = paste("Product Sales in Top Cities of", input$input_state_detailed),
        x = "City",
        y = "Quantity Sold",
        fill = "Category"
      ) +
      # 🌟 색상 팔레트 사용 (Set2는 테마에 잘 맞지 않을 수 있으나, 일단 유지하거나 직접 지정 가능)
      scale_fill_brewer(palette = "Set2") +
      # ⚠️ 지수 표기법 제거
      scale_y_continuous(labels = scales::comma) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )
    
    ggplotly(p)
  })
  
  # [✅ 추가] Best Selling Product (SKU) by State Table
  output$table_state_best <- renderDT({
    table_data <- df_amazon %>%
      filter(!is.na(ship.state) & ship.state != "") %>%
      mutate(State = str_to_title(ship.state)) %>%
      group_by(State, SKU, Category) %>%
      summarise(Revenue = sum(Amount, na.rm=TRUE), Qty = sum(Qty, na.rm=TRUE), .groups = "drop") %>%
      group_by(State) %>%
      slice_max(Revenue, n = 1, with_ties = FALSE) %>%
      arrange(desc(Revenue)) %>%
      select(State, SKU, Category, Revenue, Qty) %>%
      rename("Top Product (SKU)" = SKU, "Total Revenue (USD)" = Revenue, "Units Sold" = Qty)
    
    datatable(
      table_data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 'tp',
        # 🌟 DT Header 색상 통일
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#2C3E50', 'color': '#fff'});",
          "}"
        )
      )
    ) %>%
      formatCurrency('Total Revenue (USD)', currency = "$", digits = 0) %>%
      # ⚠️ Units Sold에 comma() 적용 (지수 표기법 제거)
      formatCurrency('Units Sold', currency = "", digits = 0, mark = ",") %>%
      formatStyle(
        'Total Revenue (USD)',
        # 🌟 Bar 색상 변경
        background = styleColorBar(table_data$`Total Revenue (USD)`, '#C4593E'),
        backgroundSize = '90% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        columns = colnames(table_data),
        fontSize = '14px'
      )
  })
  
  # VIP Optimizer (Intl RFM)
  # ⚠️ infoBox 금액에 comma() 적용 (지수 표기법 제거)
  output$box_champ_count <- renderInfoBox({ infoBox("Champions", comma(nrow(filter(df_rfm, Segment=="Champions"))), icon=icon("trophy"), color="green") })
  output$box_risk_count <- renderInfoBox({ infoBox("At Risk", comma(nrow(filter(df_rfm, Segment=="At Risk VIP"))), icon=icon("exclamation-triangle"), color="red") })
  output$box_avg_clv <- renderInfoBox({ infoBox("Avg CLV (USD)", paste0("$", comma(round(mean(df_rfm$Monetary),0))), icon=icon("chart-line"), color="blue") })
  output$plot_rfm <- renderPlotly({
    p <- ggplot(df_rfm, aes(x = Recency, y = Monetary, color = Segment, text = CUSTOMER)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_vline(xintercept = med_R, linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = med_M, linetype = "dashed", color = "gray50") +
      # ⚠️ 지수 표기법 제거
      scale_y_log10(labels = scales::comma) +
      # 🌟 RFM 색상 테마 유지 (Green/Red/Blue/Gray)
      scale_color_manual(values = c("Champions"="#2ECC71", "At Risk VIP"="#E74C3C", "Recent Users"="#3498DB", "Hibernating"="#95A5A6")) +
      theme_minimal() + labs(y = "Total Spend (USD, Log Scale)")
    ggplotly(p)
  })
  # ⚠️ Monetary에 comma() 적용 (지수 표기법 제거)
  output$table_risk <- renderDT({ df_rfm %>% filter(Segment=="At Risk VIP") %>% select(CUSTOMER, Recency, Monetary) %>% mutate(Monetary = paste0("$", comma(round(Monetary, 2)))) })
  
  # Inventory Bridge
  output$plot_matrix <- renderPlotly({
    p <- ggplot(df_inventory %>% filter(Stock < 500, Total_Sold < 500), aes(Stock, Total_Sold, color = Status, text = SKU)) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      # ⚠️ 지수 표기법 제거
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma)
    ggplotly(p)
  })
  # ⚠️ DT에 comma() 적용 (지수 표기법 제거)
  output$table_inventory <- renderDT({
    req(input$status_filter);
    df_inv <- df_inventory %>% filter(Status %in% input$status_filter) %>%
      select(SKU, Status, Stock, Total_Sold, Stock_Risk, Sales_Risk) %>%
      arrange(desc(Stock))
    
    datatable(df_inv, rownames = FALSE, options = list(pageLength = 10, dom = 'tp')) %>%
      formatCurrency(c('Stock', 'Total_Sold'), currency = "", digits = 0, mark = ",")
  })
  
  
  # --- 3. Musinsa EDA & ARIMA Logic (Tab 3) ---
  output$mapAvailable <- reactive({ !is.null(india_map) })
  outputOptions(output, "mapAvailable", suspendWhenHidden = FALSE)
  
  filtered_data_musinsa <- reactive({
    req(input$dateRange)
    temp <- df_musinsa %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    if (input$categoryInput != "All") { temp <- temp %>% filter(Category == input$categoryInput) }
    if (input$statusInput != "All") { temp <- temp %>% filter(Status == input$statusInput) }
    temp
  })
  
  # 🌟 [수정] trendPlot 색상 통일
  output$trendPlot <- renderPlotly({
    df <- filtered_data_musinsa(); req(nrow(df) > 0);
    # ⚠️ USD로 변환
    df_summ <- df %>% group_by(Date) %>% summarise(Val = if(input$metricInput=="Amount") INR_TO_USD(sum(Amount, na.rm=TRUE)) else n());
    y_lab <- if(input$metricInput=="Amount") "매출 (USD)" else "주문 건수";
    p <- ggplot(df_summ, aes(x=Date, y=Val)) +
      # 🌟 선 색상 변경: Primary (#C4593E) 적용
      geom_line(color="#C4593E", size=0.8) +
      geom_point(aes(text=paste("날짜:", Date, "<br>값:", comma(Val))), color="#C4593E", size=1.5) +
      geom_smooth(method="loess", se=FALSE, color="#E77E6A", linetype="dashed", size=0.5) +
      labs(y = y_lab, x = "날짜") +
      theme_minimal() +
      # ⚠️ 지수 표기법 제거
      scale_y_continuous(labels = scales::comma);
    ggplotly(p, tooltip="text")
  })
  
  output$sizePlot <- renderPlotly({
    df <- filtered_data_musinsa(); req(nrow(df) > 0);
    df_size <- df %>% count(Size);
    std_sizes <- c("XS","S","M","L","XL","XXL","3XL","4XL","5XL","6XL","Free");
    valid <- intersect(std_sizes, unique(df_size$Size));
    if(length(valid) > 0) df_size$Size <- factor(df_size$Size, levels = valid);
    p <- ggplot(df_size, aes(x=Size, y=n, fill=Size, text=paste("건수:", comma(n)))) +
      geom_bar(stat="identity") +
      scale_fill_viridis_d() +
      theme_minimal() +
      theme(legend.position="none") +
      labs(y="주문 건수", x="사이즈") +
      # ⚠️ 지수 표기법 제거
      scale_y_continuous(labels = scales::comma);
    ggplotly(p, tooltip="text")
  })
  
  output$boxplotPlot <- renderPlotly({
    df <- filtered_data_musinsa() %>% filter(Amount > 0); req(nrow(df) > 0);
    # ⚠️ USD로 변환
    df <- df %>% mutate(Amount_USD = INR_TO_USD(Amount));
    p <- ggplot(df, aes(x=Category, y=Amount_USD, fill=Category)) +
      geom_boxplot(outlier.colour = "red", outlier.size=0.5) +
      # ⚠️ 지수 표기법 제거
      scale_y_continuous(labels=scales::comma) +
      theme_minimal() +
      theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1)) +
      labs(y="주문 금액 (USD)", x="카테고리");
    ggplotly(p)
  })
  
  output$statePlot <- renderPlotly({
    req(india_map); df <- filtered_data_musinsa(); req(nrow(df) > 0);
    df <- df %>% mutate(State_Clean = case_when(ship.state == "PONDICHERRY" ~ "PUDUCHERRY", TRUE ~ ship.state));
    # ⚠️ USD로 변환
    state_summ <- df %>% group_by(State_Clean) %>% summarise(Val = if(input$metricInput=="Amount") INR_TO_USD(sum(Amount, na.rm=TRUE)) else n());
    map_data_sf <- india_map %>% left_join(state_summ, by=c("ST_NM_UPPER"="State_Clean"));
    map_data_sf <- map_data_sf %>% mutate(Val_Clean = replace_na(Val, 0));
    plot_ly() %>%
      # 🌟 히트맵 색상 변경 (Primary 계열)
      add_sf(data = map_data_sf, type = 'scattergeo', split = ~ST_NM, color = ~Val, colors = 'Reds', hoverinfo = 'text', text = ~paste0("지역: ", ST_NM, "<br>값: ", scales::comma(Val)), showlegend = FALSE) %>%
      colorbar(title = if(input$metricInput=="Amount") "매출 (USD)" else "건수", tickformat = ",") %>%
      layout(title = "지역별 매출/주문 분포", geo = list(scope = 'asia', center = list(lon = 78, lat = 22), showland = TRUE, subunitcolor = "#ffffff"))
  })
  
  output$predPlot <- renderPlot({
    df_src <- if(input$categoryInput == "All") df_musinsa else df_musinsa %>% filter(Category == input$categoryInput);
    # ⚠️ USD로 변환
    df_agg <- df_src %>% group_by(Date) %>% summarise(Total = INR_TO_USD(sum(Amount, na.rm=TRUE))) %>% arrange(Date);
    if(nrow(df_agg) < 14) return(NULL);
    full_dates <- seq(min(df_agg$Date), max(df_agg$Date), by="day");
    df_ts <- data.frame(Date=full_dates) %>% left_join(df_agg, by="Date");
    df_ts$Total[is.na(df_ts$Total)] <- 0;
    ts_obj <- ts(df_ts$Total, frequency=7);
    fit <- auto.arima(ts_obj);
    fc <- forecast(fit, h=30);
    autoplot(fc) +
      theme_minimal() +
      labs(title=paste("향후 30일 매출 예측 (", input$categoryInput, ")"), x="날짜", y="매출액 (USD)") +
      # ⚠️ 지수 표기법 제거
      scale_y_continuous(labels = scales::comma)
  })
  
  output$predInsight <- renderText({
    df_src <- if(input$categoryInput == "All") df_musinsa else df_musinsa %>% filter(Category == input$categoryInput);
    # ⚠️ USD로 변환
    df_agg <- df_src %>% group_by(Date) %>% summarise(Total = INR_TO_USD(sum(Amount, na.rm=TRUE))) %>% arrange(Date);
    if(nrow(df_agg) < 14) return("데이터가 부족하여 예측할 수 없습니다.");
    full_dates <- seq(min(df_agg$Date), max(df_agg$Date), by="day");
    df_ts <- data.frame(Date=full_dates) %>% left_join(df_agg, by="Date");
    df_ts$Total[is.na(df_ts$Total)] <- 0;
    ts_obj <- ts(df_ts$Total, frequency=7);
    fit <- auto.arima(ts_obj);
    fc <- forecast(fit, h=30);
    # ⚠️ comma() 적용 (지수 표기법 제거)
    paste0("========== [ 📊 AI 예측 리포트 ] ==========\n", "1. 예상 일 평균 매출: $", comma(round(mean(fc$mean))), "\n", "2. 향후 30일 총 예상 수익: $", comma(round(sum(fc$mean))), "\n\n", "💡 인사이트:\n", "안전 재고를 평균 예측치 대비 20% 이상 확보하는 것이 좋습니다.")
  })
}

# =========================================================
# 5. 앱 실행
# =========================================================
shinyApp(ui_integrated, server_integrated)