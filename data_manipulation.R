# R Script for Data Manipulation and Graph Generation for Public Economics Report

# 1. Load Libraries
# --------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
# Greyscale-distinguishable academic palette
library(colorspace)

# 2. Define Paths and Create Output Directory
# --------------------------------------------------
data_path <- "./data_xlsx/" # Directory containing the Excel files
output_path <- "./diagrams/" # Directory to save the generated plots

if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# 3. Helper function for initial Excel file loading
# --------------------------------------------------
# This function reads the first sheet of an Excel file into a dataframe.
# It's designed for the initial loading of all raw data files.
safe_read_excel_initial <- function(path) {
  sheet_to_read <- tryCatch({
    sheets <- excel_sheets(path)
    sheets[1] # Always read the first sheet for initial loading
  }, error = function(e) {
    stop(paste("Could not read sheets from file:", path, "Error:", e$message))
  })
  message(paste("Loading sheet:", sheet_to_read, "from", basename(path)))
  read_excel(path, sheet = sheet_to_read, col_types = "text") # Read all columns as text initially
}

# 4. Load all .xlsx files into global variables
# --------------------------------------------------
# This loop reads every Excel file in the data_xlsx directory
# and assigns its content to a global variable named after the file (with spaces replaced by underscores).
excel_files <- list.files(path = data_path, pattern = "\\.xlsx$", full.names = TRUE)

for (file_path in excel_files) {
  file_name <- tools::file_path_sans_ext(basename(file_path))
  file_name_clean <- gsub(" ", "_", file_name) # Replace spaces for valid R variable names
  df <- safe_read_excel_initial(file_path)
  assign(file_name_clean, df, envir = .GlobalEnv)
  message(paste("Loaded dataframe:", file_name_clean))
}

# 5. Data Processing and Graph Generation for each Report Section
# ----------------------------------------------------------------

# --- Common Data: GDP Data (used in multiple graphs) ---
# Source: GDP.xlsx (loaded as GDP_df)
# Expected columns: Year (column 1), GDP at Market Prices (column 3)
gdp_data_mn <- GDP %>%
  select(col_year = 1, col_gdp_at_mp = 3) %>%
  rename(Year = col_year, GDP_at_MP_Rs_Cr = col_gdp_at_mp) %>%
  na.omit() %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), GDP_at_MP_Rs_Cr = as.numeric(gsub("[^0-9.]", "", GDP_at_MP_Rs_Cr))) %>%
  filter(as.numeric(Year) <= 2023) # Filter data up to 2023

# Define academic theme for consistency
academic_theme <- theme_classic(base_size = 12) + 
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.8))

# Define colorblind-friendly palette that works in greyscale
# Colors chosen to have distinct lightness values when converted to greyscale
academic_colors <- c(
  "#E69F00", # Orange
  "#56B4E9", # Sky blue
  "#009E73", # Teal/green
  "#F0E442", # Yellow
  "#0072B2", # Dark blue
  "#D55E00", # Vermillion
  "#CC79A7", # Reddish purple
  "#999999", # Neutral grey
  "#8DA0CB", # Light indigo
  "#66C2A5", # Sea green
  "#FC8D62", # Coral
  "#FFD92F"  # Bright yellow
)

# --- Graph 1: The Historical Growth of the Public Sector ---
# Source: expenditure_combined.xlsx (loaded as expenditure_combined)
# Expected columns: Year (column 1), Total Expenditure (column 12)
# This graph shows Total Government Expenditure as a % of GDP over time.
total_exp_data_mn <- expenditure_combined %>%
  select(col_year = 1, col_total_exp = 12) %>%
  rename(Year = col_year, Total_Expenditure = col_total_exp) %>%
  na.omit() %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), Total_Expenditure = as.numeric(gsub("[^0-9.]", "", Total_Expenditure))) %>%
  filter(as.numeric(Year) <= 2022) # Filter data up to 2022

graph1_data_mn <- inner_join(gdp_data_mn, total_exp_data_mn, by = "Year") %>%
  mutate(Expenditure_as_perc_of_GDP = (Total_Expenditure / GDP_at_MP_Rs_Cr) * 100) %>%
  select(Year, Expenditure_as_perc_of_GDP) %>%
  arrange(as.numeric(Year)) # Sort by year for plotting

ggplot(graph1_data_mn, aes(x = as.numeric(Year), y = Expenditure_as_perc_of_GDP)) +
  geom_line(color = academic_colors[1], linewidth = 1.0) + 
  geom_point(color = academic_colors[1], size = 2.0, shape = 21, fill = "white", stroke = 1.5) +
  labs(title = "Graph 1: The Historical Growth of the Public Sector", 
       subtitle = "Total Government Expenditure as a % of GDP", 
       y = "Expenditure as % of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph1.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 1")

# --- Graph 2: Historical Trends in Key Expenditure Categories ---
# Source: expenditure_centre.xlsx and Expenditure_state.xlsx
# Expected columns (expenditure_centre): Year (1), Defence (3+9), Subsidies (5), Education (11)
# Expected columns (Expenditure_state): Year (1), Health (14), Social Services (9), Economic Services (8)
# This graph dis-aggregates spending into key categories as a % of GDP.
central_exp_key_cats_mn <- expenditure_centre %>%
  select(col_year = 1, col_rev_defence = 3, col_cap_defence = 9, col_education = 11, col_subsidies = 5) %>%
  rename(Year = col_year, Rev_Defence = col_rev_defence, Cap_Defence = col_cap_defence, Education = col_education, Subsidies = col_subsidies) %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), across(c(Rev_Defence, Cap_Defence, Subsidies, Education), ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  filter(as.numeric(Year) <= 2023) # Filter data up to 2023

state_exp_key_cats_mn <- expenditure_state %>%
  select(col_year = 1, col_health = 14, col_social_services = 9, col_economic_services = 8) %>%
  rename(Year = col_year, Health = col_health, Social_Services = col_social_services, Economic_Services = col_economic_services) %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), across(c(Health, Social_Services, Economic_Services), ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  filter(as.numeric(Year) <= 2023) # Filter data up to 2023

combined_exp_key_cats_mn <- inner_join(central_exp_key_cats_mn, state_exp_key_cats_mn, by = "Year", suffix = c("_centre", "_state")) %>%
  mutate(Defence = Rev_Defence + Cap_Defence) %>%
  select(Year, Defence, Education, Health, Social_Services, Economic_Services, Subsidies)

graph2_data_mn <- inner_join(gdp_data_mn, combined_exp_key_cats_mn, by = "Year") %>%
  mutate(Defence_perc_gdp = (Defence / GDP_at_MP_Rs_Cr) * 100, 
         Education_perc_gdp = Education, 
         Health_perc_gdp = Health, 
         Social_Services_perc_gdp = (Social_Services / GDP_at_MP_Rs_Cr) * 100, 
         Economic_Services_perc_gdp = (Economic_Services / GDP_at_MP_Rs_Cr) * 100, 
         Subsidies_perc_gdp = (Subsidies / GDP_at_MP_Rs_Cr) * 100) %>%
  select(Year, Defence_perc_gdp, Education_perc_gdp, Health_perc_gdp, Social_Services_perc_gdp, Economic_Services_perc_gdp, Subsidies_perc_gdp) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Percentage_of_GDP") %>%
  arrange(as.numeric(Year)) # Sort by year for plotting

# Plotting 6 separate graphs for Graph 2 with different line types
# Defence - Solid line
ggplot(filter(graph2_data_mn, Category == "Defence_perc_gdp"), aes(x = as.numeric(Year), y = Percentage_of_GDP)) +
geom_line(linewidth = 1.0, color = academic_colors[6], linetype = "solid") +
  geom_point(color = academic_colors[6], size = 1.5, shape = 16) +
  labs(title = "Graph 2: Defence Expenditure as % of GDP", y = "% of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph2_defence.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 2 - Defence")

# Subsidies - Solid line
ggplot(filter(graph2_data_mn, Category == "Subsidies_perc_gdp"), aes(x = as.numeric(Year), y = Percentage_of_GDP)) +
geom_line(linewidth = 1.0, color = academic_colors[6], linetype = "solid") +
  geom_point(color = academic_colors[6], size = 1.5, shape = 17) +
  labs(title = "Graph 2: Subsidies Expenditure as % of GDP", y = "% of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph2_subsidies.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 2 - Subsidies")

# Education - Solid line
ggplot(filter(graph2_data_mn, Category == "Education_perc_gdp"), aes(x = as.numeric(Year), y = Percentage_of_GDP)) +
geom_line(linewidth = 1.0, color = academic_colors[6], linetype = "solid") +
  geom_point(color = academic_colors[6], size = 1.5, shape = 15) +
  labs(title = "Graph 2: Education Expenditure as % of GDP", y = "% of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph2_education.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 2 - Education")

# Health - Solid line
ggplot(filter(graph2_data_mn, Category == "Health_perc_gdp"), aes(x = as.numeric(Year), y = Percentage_of_GDP)) +
geom_line(linewidth = 1.0, color = academic_colors[6], linetype = "solid") +
  geom_point(color = academic_colors[6], size = 1.5, shape = 18) +
  labs(title = "Graph 2: Health Expenditure as % of GDP", y = "% of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph2_health.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 2 - Health")

# Social Services - Solid line
ggplot(filter(graph2_data_mn, Category == "Social_Services_perc_gdp"), aes(x = as.numeric(Year), y = Percentage_of_GDP)) +
geom_line(linewidth = 1.0, color = academic_colors[6], linetype = "solid") +
  geom_point(color = academic_colors[6], size = 1.5, shape = 21, fill = "white", stroke = 1) +
  labs(title = "Graph 2: Social Services Expenditure as % of GDP", y = "% of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph2_social_services.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 2 - Social Services")

# Economic Services - Solid line
ggplot(filter(graph2_data_mn, Category == "Economic_Services_perc_gdp"), aes(x = as.numeric(Year), y = Percentage_of_GDP)) +
geom_line(linewidth = 1.0, color = academic_colors[6], linetype = "solid") +
  geom_point(color = academic_colors[6], size = 1.5, shape = 22, fill = "white", stroke = 1) +
  labs(title = "Graph 2: Economic Services Expenditure as % of GDP", y = "% of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph2_economic_services.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 2 - Economic Services")

# --- Graph 3: Expenditure by Level of Government ---
# Source: expenditure_centre.xlsx and Expenditure_state.xlsx
# Expected columns (expenditure_centre): Year (1), Total Expenditure (10)
# Expected columns (Expenditure_state): Year (1), Total Expenditure (5)
# This graph shows the percentage share of Central and State government expenditure.
central_exp_total_mn <- expenditure_centre %>% select(col_year = 1, col_total_exp = 10) %>% rename(Year = col_year, Total_Central_Exp = col_total_exp) %>% na.omit() %>% mutate(Year = str_trim(str_extract(Year, "\\d{4}")), Total_Central_Exp = as.numeric(gsub("[^0-9.]", "", Total_Central_Exp))) %>%
  filter(as.numeric(Year) <= 2023) # Filter data up to 2023
state_exp_total_mn <- expenditure_state %>% select(col_year = 1, col_total_exp = 6) %>% rename(Year = col_year, Total_State_Exp = col_total_exp) %>% na.omit() %>% mutate(Year = str_trim(str_extract(Year, "\\d{4}")), Total_State_Exp = as.numeric(gsub("[^0-9.]", "", Total_State_Exp))) %>%
  filter(as.numeric(Year) <= 2023) # Filter data up to 2023
graph3_data_mn <- inner_join(central_exp_total_mn, state_exp_total_mn, by = "Year") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  mutate(Total_Exp = Total_Central_Exp + Total_State_Exp) %>%
  summarise(Central_Share = (sum(Total_Central_Exp) / sum(Total_Exp)) * 100, State_Share = (sum(Total_State_Exp) / sum(Total_Exp)) * 100) %>%
  pivot_longer(cols = everything(), names_to = "Level_of_Government", values_to = "Percentage_Share")

ggplot(graph3_data_mn, aes(x = "", y = Percentage_Share, fill = Level_of_Government)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.8) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Central_Share" = academic_colors[1], "State_Share" = academic_colors[2]),
                    labels = c("Central Share", "State Share"),
                    name = "Government Level") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")) +
  labs(title = "Graph 3: Expenditure by Level of Government (2022)") +
  geom_text(aes(label = paste0(round(Percentage_Share,1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, fontface = "bold") +
  academic_theme
ggsave(paste0(output_path, "graph3.png"), width = 8, height = 8, dpi = 300)
print("Generated graph 3")

# --- Graph 4 & 5: Composition of Spending ---
# Source: expenditure_centre.xlsx and Expenditure_state.xlsx
# Expected columns (expenditure_centre): Year (1), Defence (3+9), Subsidies (5), Education (11)
# Expected columns (Expenditure_state): Year (1), Health (14), Social Services (9), Economic Services (8)
# This graph dis-aggregates spending into key categories as a % of GDP.

# Central Government Expenditure Components
central_exp_comp_mn <- expenditure_centre %>%
  select(col_year = 1, col_rev_defence = 3, col_cap_defence = 9, 
         col_education = 11, col_subsidies = 5, col_total = 10, 
         col_interest = 4) %>%
  rename(Year = col_year, Rev_Defence = col_rev_defence, 
         Cap_Defence = col_cap_defence, Education = col_education, 
         Subsidies = col_subsidies, Total_Exp = col_total,
         Interest = col_interest) %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), across(c(Rev_Defence, Cap_Defence, Subsidies, Education, Total_Exp, Interest), ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  mutate(Defence = Rev_Defence + Cap_Defence, Education = Education / 2) %>%
  filter(as.numeric(Year) <= 2022) # Filter data up to 2022

# State Government Expenditure Components
state_exp_comp_mn <- inner_join(expenditure_state, expenditure_centre, by = '1') %>%
  select(col_year = 1, col_health = 14, col_social_services = 9, col_economic_services = 8, 
         col_education = '11.y', col_total = 6) %>%
  rename(Year = col_year, Health = col_health, Social_Services = col_social_services, Economic_Services = col_economic_services, Education = col_education, Total_Exp = col_total) %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), across(c(Health, Social_Services, Economic_Services, Education, Total_Exp), ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  mutate(Education = Education / 2) %>%
  filter(as.numeric(Year) <= 2022) # Filter data up to 2022

combined_exp_comp_mn <- inner_join(central_exp_comp_mn, state_exp_comp_mn, by = "Year", suffix = c("_centre", "_state")) %>%
  mutate(Education = (Education_centre + Education_state), Total = Total_Exp_centre + Total_Exp_state) %>%
  select(Year, Defence, Education, Health, Social_Services, Economic_Services, Subsidies, Interest, Total)

# Preparing data for Graph 4
graph4_data_mn <- inner_join(gdp_data_mn, combined_exp_comp_mn, by = "Year") %>%
  filter(as.numeric(Year) == 2022)

graph4_data_mn <- graph4_data_mn %>%
  mutate(Defence_perc_Total = ((Defence) / Total) * 100,
         #Other_Social_Services_perc_Total = ((Social_Services - ((Education / 100) * GDP_at_MP_Rs_Cr) - ((Health / 100) * GDP_at_MP_Rs_Cr)) / Total) * 100,
         Education_perc_Total = (((Education / 100) * GDP_at_MP_Rs_Cr)/ Total)*100, 
         Health_perc_Total = (((Health / 100) * GDP_at_MP_Rs_Cr)/ Total)*100, 
         Economic_Services_perc_Total = (Economic_Services / Total) * 100, 
         Subsidies_perc_Total = (Subsidies / Total) * 100, 
         Interest_perc_Total = (Interest / Total) * 100,
         Others = (100-Defence_perc_Total-Education_perc_Total - Health_perc_Total
                    - Economic_Services_perc_Total-Subsidies_perc_Total-Interest_perc_Total)) %>%
  select(Year, Defence_perc_Total, Education_perc_Total, Health_perc_Total, Economic_Services_perc_Total, 
         Subsidies_perc_Total, Interest_perc_Total, Others) %>%
  # Combined Pivot for Graph 4
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Percentage_of_Total") %>%
  arrange(as.numeric(Year)) # Sort by year for plotting

# Graph 4 Plot: Composition of Central Government Spending
ggplot(graph4_data_mn, aes(x = "", y = Percentage_of_Total, fill = Category)) + 
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.5) + 
  coord_polar("y", start = 0) + 
  scale_fill_manual(values = c("Defence_perc_Total" = academic_colors[1],
                               "Education_perc_Total" = academic_colors[2], 
                               "Health_perc_Total" = academic_colors[3],
                               "Economic_Services_perc_Total" = academic_colors[4],
                               "Subsidies_perc_Total" = academic_colors[5],
                               "Interest_perc_Total" = academic_colors[6],
                               "Others" = academic_colors[7]),
                    labels = c("Defence", "Education", "Health", 
                               "Economic_Services", "Subsidies", "Interest Payment", "Others")) +
  labs(title = "Graph 4: Composition of General Government Spending (2022)") + 
  geom_text(aes(label = ifelse(Percentage_of_Total > 3, paste0(round(Percentage_of_Total,1), "%"), "")), 
            position = position_stack(vjust = 0.5), size = 3) +
  academic_theme
ggsave(paste0(output_path, "graph4.png"), width = 10, height = 8, dpi = 300)
print("Generated graph 4")

# Preparing data for Graph 5a
graph5a_data_mn <- inner_join(gdp_data_mn, central_exp_comp_mn, by = "Year") %>%
  mutate(Defence_perc_Total = ((Defence) / Total_Exp) * 100, 
         Education_perc_Total = (((Education / 100) * GDP_at_MP_Rs_Cr)/ Total_Exp) * 100, 
         Subsidies_perc_Total = (Subsidies / Total_Exp) * 100, 
         Interest_perc_Total = (Interest / Total_Exp) * 100,
         Others = (100-Defence_perc_Total-Education_perc_Total-Subsidies_perc_Total-Interest_perc_Total)) %>%
  select(Year, Defence_perc_Total, Education_perc_Total, 
         Subsidies_perc_Total, Interest_perc_Total, Others) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Percentage_of_Total") %>%
  arrange(as.numeric(Year)) # Sort by year for plotting

# Pivot for Graph 5a
graph5a_data_mn <- graph5a_data_mn %>%
  group_by(Category) %>%
  filter(as.numeric(Year) == 2022)

# Graph 5a Plot: Central Government Spending
ggplot(graph5a_data_mn, aes(x = "", y = Percentage_of_Total, fill = Category)) + 
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.5) + 
  coord_polar("y", start = 0) + 
  scale_fill_manual(values = c("Defence_perc_Total" = academic_colors[1],
                               "Education_perc_Total" = academic_colors[2], 
                               "Subsidies_perc_Total" = academic_colors[3],
                               "Interest_perc_Total" = academic_colors[4],
                               "Others" = academic_colors[5])) +
  academic_theme +
  labs(title = "Graph 5a: Central Government Spending (2022)") + 
  geom_text(aes(label = ifelse(Percentage_of_Total > 3, paste0(round(Percentage_of_Total,1), "%"), "")), 
            position = position_stack(vjust = 0.5), size = 3)
ggsave(paste0(output_path, "graph5a.png"), width = 8, height = 8, dpi = 300)
print("Generated graph 5a")

# Preparing data for Graph 5b
graph5b_data_mn <- inner_join(gdp_data_mn, state_exp_comp_mn, by = "Year") %>%
  mutate(
    #Other_Social_Services_perc_Total = ((Social_Services - ((Education / 100) * GDP_at_MP_Rs_Cr) - ((Health / 100) * GDP_at_MP_Rs_Cr)) / Total_Exp) * 100,
    Education_perc_Total = (((Education / 100) * GDP_at_MP_Rs_Cr)/ Total_Exp)*100,
    Health_perc_Total = (((Health / 100) * GDP_at_MP_Rs_Cr)/ Total_Exp)*100,
    Economic_Services_perc_Total = (Economic_Services / Total_Exp) * 100,
    Others = (100 - Education_perc_Total - Health_perc_Total - Economic_Services_perc_Total)) %>%
  select(Year, Education_perc_Total, Health_perc_Total,
         Economic_Services_perc_Total, Others) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Percentage_of_Total") %>%
  arrange(as.numeric(Year))

# Pivot for Graph 5b
graph5b_data_mn <- graph5b_data_mn %>%
  group_by(Category) %>%
  filter(as.numeric(Year) == 2022)

# Graph 5b Plot: State Government Spending
ggplot(graph5b_data_mn, aes(x = "", y = Percentage_of_Total, fill = Category)) + 
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.5) + 
  coord_polar("y", start = 0) +  
  scale_fill_manual(values = c("Education_perc_Total" = academic_colors[2],
                               "Health_perc_Total" = academic_colors[3],
                               "Economic_Services_perc_Total" = academic_colors[4],
                               "Others" = academic_colors[8]),
                    labels = c("Education", "Health", "Economic Services", "Others")) +
  academic_theme +
  labs(title = "Graph 5b: State Government Spending (2022)") + 
  geom_text(aes(label = ifelse(Percentage_of_Total > 3, paste0(round(Percentage_of_Total,1), "%"), "")), 
            position = position_stack(vjust = 0.5), size = 3)
ggsave(paste0(output_path, "graph5b.png"), width = 8, height = 8, dpi = 300)
print("Generated graph 5b")


# --- Graph 6: Historical Growth of Tax Revenue ---
# Source: revenue_centre.xlsx, revenue_state.xlsx (loaded as revenue_centre, revenue_state)
# Expected columns: Year, Total Tax Revenue
# This graph shows Total Tax Revenue as a % of GDP over time.
tax_revenue_data_mn <- inner_join(revenue_centre, revenue_state, by = "1") %>%
    select(col_year = 1, col_centre_tax_rev = '2.x', col_state_tax_rev = '3.y') %>%
    mutate(col_total_tax_rev = as.numeric(col_centre_tax_rev) + as.numeric(col_state_tax_rev)) %>%
    mutate(Year = str_trim(str_extract(col_year, "\\d{4}")), Total_Tax_Revenue = as.numeric(gsub("[^0-9.]", "", col_total_tax_rev))) %>%
    filter(as.numeric(Year) <= 2022) # Filter data up to 2022

graph6_data_mn <- inner_join(gdp_data_mn, tax_revenue_data_mn, by = "Year") %>%
    mutate(Tax_Revenue_as_perc_of_GDP = (Total_Tax_Revenue / GDP_at_MP_Rs_Cr) * 100,
           Year = as.numeric(Year)) %>%
    select(Year, Tax_Revenue_as_perc_of_GDP) %>%
    arrange(as.numeric(Year)) # Sort by year for plotting

ggplot(graph6_data_mn, aes(x = Year, y = Tax_Revenue_as_perc_of_GDP)) +
    geom_line(color = academic_colors[3], linewidth = 1.0) +
    geom_point(color = academic_colors[3], size = 1.5, shape = 16) +
    labs(title = "Graph 6: Historical Growth of Tax Revenue", 
         y = "Tax Revenue as % of GDP", x = "Year") +
    academic_theme
ggsave(paste0(output_path, "graph6.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 6")

# --- Graph 7: Tax Sources at Each Level of Government ---
# Source: revenue_combined.xlsx (assuming detailed tax sources by level are here)
# This graph shows the composition of tax revenue for Central and State governments and Comined in a poportional stacked bar chart.
# NOTE: This data is often complex and may require specific sheet/column mapping based on your file.
# For now, creating placeholder plot as detailed data is not directly available in a simple format.

# Central Government Tax Revenue Sources
central_tax_rev_mn <- revenue_centre %>%
  select(col_year = 1, col_pinc_tax = 4, col_corp_tax = 5, 
         col_excise_duty = 7, col_custom_duty = 8, col_total_direct = 3,
         col_total_indirect = 6, col_total_tax = 2) %>%
  rename(Year = col_year, Personal_Income_Tax = col_pinc_tax, 
         Corporate_Tax = col_corp_tax, Excise_Duty = col_excise_duty, 
         Custom_Duty = col_custom_duty, Direct_Tax = col_total_direct,
         Indirect_Tax = col_total_indirect, Total_Tax = col_total_tax) %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), 
         across(c(Personal_Income_Tax, Corporate_Tax, Excise_Duty,
                  Custom_Duty, Direct_Tax, Indirect_Tax, Total_Tax), 
                ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  mutate(Other_Indirect_Tax = Indirect_Tax - Excise_Duty - Custom_Duty, 
         Other_Direct_Tax = Direct_Tax - Personal_Income_Tax - Corporate_Tax) %>%
  filter(as.numeric(Year) <= 2022) # Filter data up to 2022

central_tax_rev_mn <- central_tax_rev_mn %>%
  select(Year, Personal_Income_Tax, Corporate_Tax, Excise_Duty,
         Custom_Duty, Other_Indirect_Tax, Other_Direct_Tax, Total_Tax)

# State Government Tax Revenue Sources
state_tax_rev_mn <- revenue_state %>%
  select(col_year = 1, col_sales_tax = 4, col_state_excise = 5, 
         col_union_excise = 8, col_income_tax = 7, 
         col_total = 2) %>%
  rename(Year = col_year, Sales_Tax = col_sales_tax, 
         State_Excise = col_state_excise, 
         Union_Excise = col_union_excise, 
         Income_Tax = col_income_tax, Total_Tax = col_total) %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), 
         across(c(Sales_Tax, State_Excise, Union_Excise, 
                  Income_Tax, Total_Tax), 
                ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  mutate(Other_Tax = Total_Tax - Sales_Tax - 
           State_Excise - Union_Excise - Income_Tax) %>%
  filter(as.numeric(Year) <= 2022) # Filter data up to 2022

# Combined Tax Revenue Sources
combined_tax_rev_mn <- inner_join(central_tax_rev_mn, state_tax_rev_mn, by = "Year", suffix = c("_centre", "_state")) %>%
  mutate(Other_Indirect_Tax = Total_Tax_centre + Total_Tax_state 
         - Personal_Income_Tax - Corporate_Tax - Other_Direct_Tax
         - Excise_Duty - Custom_Duty - Sales_Tax, Total = Total_Tax_centre + Total_Tax_state) %>%
  select(Year, Personal_Income_Tax, Corporate_Tax, Excise_Duty,
         Custom_Duty, Other_Indirect_Tax, Other_Direct_Tax,
         Sales_Tax, State_Excise, Total)

# Preparing data for Graph 7: Tax Sources at Each Level of Government
# This should show Central vs State tax composition in side-by-side stacked bars

# Get Central Government tax data for latest year
central_tax_latest <- central_tax_rev_mn %>%
  filter(as.numeric(Year) == max(as.numeric(Year), na.rm = TRUE)) %>%
  mutate(Government_Level = "Central") %>%
  select(-Total_Tax) %>%
  pivot_longer(cols = c(Personal_Income_Tax, Corporate_Tax, Excise_Duty, Custom_Duty, Other_Indirect_Tax, Other_Direct_Tax),
               names_to = "Tax_Category", values_to = "Amount") %>%
  mutate(Tax_Category = str_replace_all(Tax_Category, "_", " "))

# Get State Government tax data for latest year  
state_tax_latest <- state_tax_rev_mn %>%
  filter(as.numeric(Year) == max(as.numeric(Year), na.rm = TRUE)) %>%
  mutate(Government_Level = "State") %>%
  select(-Total_Tax, -Other_Tax) %>%
  pivot_longer(cols = c(Sales_Tax, State_Excise, Union_Excise, Income_Tax),
               names_to = "Tax_Category", values_to = "Amount") %>%
  mutate(Tax_Category = str_replace_all(Tax_Category, "_", " "))

# Combine the data
graph7_data_mn <- bind_rows(central_tax_latest, state_tax_latest) %>%
  filter(Amount > 0) %>%  # Remove negative or zero values
  group_by(Government_Level) %>%
  mutate(Total = sum(Amount, na.rm = TRUE),
         Percentage = (Amount / Total) * 100) %>%
  ungroup()

# Create Graph 7: Side-by-side stacked bar charts
ggplot(graph7_data_mn, aes(x = Government_Level, y = Percentage, fill = Tax_Category)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.3, width = 0.6) +
  scale_fill_manual(values = academic_colors, name = "Tax Category") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Graph 7: Tax Sources at Each Level of Government",
       subtitle = "Composition of Tax Revenue by Government Level (2022)",
       x = "Government Level", 
       y = "Percentage of Total Tax Revenue") +
  academic_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(size = 12, face = "bold")) +
  guides(fill = guide_legend(ncol = 3, title.position = "top"))
ggsave(paste0(output_path, "graph7.png"), width = 10, height = 8, dpi = 300)
print("Generated graph 7")

# --- Graph 8: Tax Sources at Each Level of Government ---
# Source: combined_deficit.xlsx
# Expected columns: Year (1), Fiscal Deficit (2)
# This graph shows the Fiscal Deficit as a % of GDP over time.
graph8_data_mn <- combined_deficit %>%
  select(col_year = 1, col_fiscal_deficit = 2) %>%
  rename(Year = col_year, Fiscal_Deficit_perc_GDP = col_fiscal_deficit) %>%
  na.omit() %>%
  mutate(Year = as.numeric(str_trim(str_extract(Year, "\\d{4}"))), Fiscal_Deficit_perc_GDP = as.numeric(gsub("[^0-9.]", "", Fiscal_Deficit_perc_GDP))) %>%
  filter(as.numeric(Year) <= 2022) # Filter data up to 2022

ggplot(graph8_data_mn, aes(x = Year, y = Fiscal_Deficit_perc_GDP)) +
  geom_line(color = academic_colors[1], linewidth = 1.0, linetype = "solid") +
  geom_point(color = academic_colors[1], size = 1.5, shape = 17) +
  labs(title = "Graph 8: The Fiscal Deficit Path", 
       subtitle = "Fiscal Deficit as a % of GDP", 
       y = "Fiscal Deficit as % of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph8.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 8")

# --- Graph 9: Decomposition of National Debt (Histograms) ---
# Source: government_debt.xlsx, household_debt.xlsx  
# This graph creates two separate histograms showing the distribution of Government and Household debt as % of GDP over 2.5-year intervals
# Based on Hindriks & Miles Fig. 4.20 and 4.21
# Expected columns: government_debt - Year (1), Combined Total Liabilities (7)
# Expected columns: household_debt - Year (1), Changes in financial liabilities (16)

# Prepare Government Debt data (2.5-year intervals)
gov_debt_data_mn <- government_debt %>%
  select(col_year = 1, col_debt = 7) %>%
  rename(Year = col_year, Government_Debt = col_debt) %>%
  na.omit() %>%
  mutate(Year = as.numeric(str_trim(str_extract(Year, "\\d{4}"))), 
         Government_Debt = as.numeric(gsub("[^0-9.]", "", Government_Debt))) %>%
  filter(Year <= 2023 & Year >= 1997) %>%
  # Filter to approximately 2.5-year intervals
  filter(Year %in% c(1997, 1999, 2002, 2004, 2007, 2009, 2012, 2014, 2017, 2019, 2022))

# Prepare Household Debt data (2.5-year intervals)
hh_debt_data_mn <- household_debt %>%
  select(col_year = 1, col_debt = 16) %>%
  rename(Year = col_year, Household_Debt = col_debt) %>%
  na.omit() %>%
  mutate(Year = as.numeric(str_trim(str_extract(Year, "\\d{4}"))), 
         Household_Debt = as.numeric(gsub("[^0-9.]", "", Household_Debt))) %>%
  filter(Year <= 2023 & Year >= 1997) %>%
  # Filter to approximately 2.5-year intervals - match the same pattern as government data
  filter(Year %in% c(1997, 1999, 2002, 2004, 2007, 2009, 2012, 2014, 2017, 2019, 2022))

# Prepare Private Debt data (2.5-year intervals)
ps_debt_data_mn <- household_debt %>%
  select(col_year = 1, col_debt = 17) %>%
  rename(Year = col_year, Private_Debt = col_debt) %>%
  na.omit() %>%
  mutate(Year = as.numeric(str_trim(str_extract(Year, "\\d{4}"))), 
         Private_Debt = as.numeric(gsub("[^0-9.]", "", Private_Debt))) %>%
  filter(Year <= 2021 & Year >= 1996) %>%
  # Filter to approximately 2.5-year intervals - match the same pattern as government data
  filter(Year %in% c(1996, 1998, 2001, 2003, 2006, 2008, 2011, 2013, 2016, 2018, 2021))

# Ensure Year columns are consistent (numeric) for joining
gdp_data_mn <- gdp_data_mn %>% mutate(Year = as.numeric(Year))

# Join with GDP data and calculate debt-to-GDP ratios
graph9_gov_data_mn <- inner_join(gdp_data_mn, gov_debt_data_mn, by = "Year") %>%
  mutate(Debt_perc_GDP = (Government_Debt / GDP_at_MP_Rs_Cr) * 100) %>%
  select(Year, Debt_perc_GDP) %>%
  arrange(Year)

graph9_hh_data_mn <- inner_join(gdp_data_mn, hh_debt_data_mn, by = "Year") %>%
  mutate(Debt_perc_GDP = (Household_Debt / GDP_at_MP_Rs_Cr) * 100) %>%
  select(Year, Debt_perc_GDP) %>%
  arrange(Year)

graph9_ps_data_mn <- ps_debt_data_mn %>%
  mutate(Debt_perc_GDP = Private_Debt) %>%
  select(Year, Debt_perc_GDP) %>%
  arrange(Year)

# Create Government Debt Bar Chart with Years on X-axis and Pattern-filled bars
gov_histogram <- ggplot(graph9_gov_data_mn, aes(x = factor(Year), y = Debt_perc_GDP)) +
  geom_bar(stat = "identity", fill = academic_colors[1], color = "black", linewidth = 0.5, width = 0.7) +
  scale_x_discrete(labels = graph9_gov_data_mn$Year) +
  labs(title = "Graph 9a: Government Debt as % of GDP",
       subtitle = "Evolution over 2.5-year intervals (1997-2022)",
       x = "Year",
       y = "Debt as % of GDP") +
  academic_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Create Domestic Private Sector Debt by Commercial Banks
ps_histogram <- ggplot(graph9_ps_data_mn, aes(x = factor(Year), y = Debt_perc_GDP)) +
  geom_bar(stat = "identity", fill = academic_colors[2], color = "black", linewidth = 0.5, width = 0.7) +
  scale_x_discrete(labels = graph9_hh_data_mn$Year) +
  labs(title = "Graph 9b: Domestic Private Sector Debt as % of GDP",
       subtitle = "Evolution over 2.5-year intervals (1997-2022)", 
       x = "Year",
       y = "Debt as % of GDP") +
  academic_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Create Household Debt Bar Chart with Years on X-axis and Pattern-filled bars
hh_histogram <- ggplot(graph9_hh_data_mn, aes(x = factor(Year), y = Debt_perc_GDP)) +
  geom_bar(stat = "identity", fill = academic_colors[2], color = "black", linewidth = 0.5, width = 0.7) +
  scale_x_discrete(labels = graph9_hh_data_mn$Year) +
  labs(title = "Graph 9c: Household Debt as % of GDP",
       subtitle = "Evolution over 2.5-year intervals (1997-2022)", 
       x = "Year",
       y = "Debt as % of GDP") +
  academic_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Save the 3 separate PNG files
ggsave(paste0(output_path, "graph9_public.png"), gov_histogram, width = 10, height = 6, dpi = 300)
ggsave(paste0(output_path, "graph9_household.png"), hh_histogram, width = 10, height = 6, dpi = 300)
ggsave(paste0(output_path, "graph9_private_sector.png"), ps_histogram, width = 10, height = 6, dpi = 300)
print("Generated graph9_public.png - Government debt evolution with academic greyscale bars")
print("Generated graph9_household.png - Household debt evolution with academic greyscale bars")
print("Generated graph9_private_sector.png - Domestic Private Sector Debt evolution with academic greyscale bars")

# --- Graph 10: The Trajectory of Government Debt ---
# Source: government_debt.xlsx
# Expected columns: Year (1), Debt as % of GDP (7)
# This graph shows the General Government Debt as a % of GDP over time.
graph10_data_mn <- government_debt %>%
  select(col_year = 1, col_debt = 7) %>%
  rename(Year = col_year, Debt = col_debt) %>%
  na.omit() %>%
  mutate(Year = str_trim(str_extract(Year, "\\d{4}")), Debt = as.numeric(gsub("[^0-9.]", "", Debt))) %>%
  filter(as.numeric(Year) <= 2022) # Filter data up to 2022

# Ensure consistent data types for Graph 10
graph10_data_mn <- graph10_data_mn %>%
  mutate(Year = as.numeric(Year))

graph10_data_mn <- inner_join(graph10_data_mn, gdp_data_mn, by = "Year") %>%
  mutate(Year = as.numeric(Year), Debt_perc_GDP = (Debt / GDP_at_MP_Rs_Cr)*100) %>%
  select(Year, Debt_perc_GDP)

ggplot(graph10_data_mn, aes(x = as.numeric(Year), y = Debt_perc_GDP)) +
  geom_line(color = academic_colors[1], linewidth = 1.0, linetype = "solid") +
  geom_point(color = academic_colors[1], size = 1.5, shape = 21, fill = academic_colors[7], stroke = 1) +
  labs(title = "Graph 10: The Trajectory of Government Debt", 
       subtitle = "General Government Debt as a % of GDP", 
       y = "Debt as % of GDP", x = "Year") +
  academic_theme
ggsave(paste0(output_path, "graph10.png"), width = 10, height = 6, dpi = 300)
print("Generated graph 10")


print("All graphs have been generated and saved to the 'diagrams' directory.")