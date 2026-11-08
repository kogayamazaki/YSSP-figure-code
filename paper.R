# Final energy use in Asia: total, transport, and shipping
# Run this script from the YSSP project root.

Sys.setenv(
  PATH = paste(Sys.getenv("PATH"), "C:/GAMS/win64/26.1", sep = ";")
)

library(gdxrrw)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(tidyr)

gdx_file <- file.path("data", "0707", "global_17_IAMC.gdx")
output_dir <- file.path("output", "paper")

if (!file.exists(gdx_file)) {
  stop(
    "GDX file not found: ", gdx_file,
    "\nRun this script from the YSSP project root."
  )
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

scenario_labels <- c(
  SSP2_BaU_NoCC_globalNH3_global2 = "Baseline",
  SSP2_600C_2020NDC_NoCC_globalNH3_global2 = "Mitigation",
  SSP2_600C_2020NDC_nh3_NoCC_globalNH3_global2 = "ammonia"
)

# R5ASIA is the Asian aggregate used in the existing analysis scripts.
iamc_all <- rgdx.param(gdx_file, "IAMC_Template")

iamc_raw <- iamc_all %>%
  filter(
    SCENARIO %in% names(scenario_labels),
    REMF == "R5ASIA"
  ) %>%
  mutate(
    Year = as.numeric(as.character(YEMF)),
    Scenario = unname(scenario_labels[as.character(SCENARIO)])
  ) %>%
  filter(Year >= 2015, Year <= 2100) %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    )
  )

# In the ammonia scenario, AIM's hydrogen category is interpreted as ammonia.
# The dedicated ammonia sub-variable is not added separately, so there is no
# double counting.
prepare_final_energy <- function(data, panel_label, carrier_map) {
  selected <- data %>%
    filter(VEMF %in% names(carrier_map)) %>%
    group_by(Scenario, Year, VEMF) %>%
    summarise(Value = sum(IAMC_Template, na.rm = TRUE), .groups = "drop")

  selected %>%
    mutate(
      Carrier = unname(carrier_map[as.character(VEMF)]),
      Carrier = if_else(
        as.character(Scenario) == "ammonia" &
          Carrier == "Hydrogen",
        "Ammonia",
        Carrier
      ),
      Panel = panel_label
    ) %>%
    select(Scenario, Year, Carrier, Value, Panel)
}

total_map <- c(
  Fin_Ene_Solids = "Solids",
  Fin_Ene_Liq = "Liquids",
  Fin_Ene_Gas = "Gases",
  Fin_Ene_Heat = "Heat",
  Fin_Ene_Ele = "Electricity",
  Fin_Ene_Solar = "Solar",
  Fin_Ene_Hyd = "Hydrogen"
)

transport_map <- c(
  Fin_Ene_Tra_Solids = "Solids",
  Fin_Ene_Tra_Liq = "Liquids",
  Fin_Ene_Tra_Gas = "Gases",
  Fin_Ene_Tra_Heat = "Heat",
  Fin_Ene_Tra_Ele = "Electricity",
  Fin_Ene_Tra_Hyd = "Hydrogen"
)

shipping_map <- c(
  Fin_Ene_Tra_Shi_Liq_Oil = "Oil",
  Fin_Ene_Tra_Shi_Liq_Bio = "Biomass",
  Fin_Ene_Tra_Shi_Gas = "Gases",
  Fin_Ene_Tra_Shi_Ele = "Electricity",
  Fin_Ene_Tra_Shi_Hyd = "Hydrogen"
)

plot_data <- bind_rows(
  prepare_final_energy(
    iamc_raw, "(a) Total final energy", total_map
  ),
  prepare_final_energy(
    iamc_raw, "(b) Transport sector", transport_map
  ),
  prepare_final_energy(
    iamc_raw, "(c) Shipping sector", shipping_map
  )
)

panel_levels <- c(
  "(a) Total final energy",
  "(b) Transport sector",
  "(c) Shipping sector"
)
carrier_levels <- c(
  "Solids", "Liquids", "Oil", "Biomass", "Gases", "Heat",
  "Electricity", "Solar", "Hydrogen", "Ammonia"
)

plot_data <- plot_data %>%
  mutate(
    Panel = factor(Panel, levels = panel_levels),
    Carrier = factor(Carrier, levels = carrier_levels)
  )

# Show only carriers with a non-zero value somewhere in the selected data.
# This removes empty legend entries such as Solar when AIM reports no value.
used_carriers <- carrier_levels[
  carrier_levels %in%
    unique(as.character(plot_data$Carrier[plot_data$Value > 1e-12]))
]

carrier_colours <- c(
  Solids = "#4D4D4D",
  Liquids = "#D55E00",
  Oil = "#A65628",
  Biomass = "#009E73",
  Gases = "#CC79A7",
  Heat = "#7A5195",
  Electricity = "#0072B2",
  Solar = "#E69F00",
  Hydrogen = "#56B4E9",
  Ammonia = "#E41A1C"
)

# A single faceted ggplot gives all nine panels one common legend and keeps
# fuel colours identical across the three energy-system levels.
paper_figure <- ggplot(
  plot_data,
  aes(x = Year, y = Value, fill = Carrier)
) +
  geom_area(position = "stack", colour = NA) +
  facet_grid(
    rows = vars(Panel),
    cols = vars(Scenario),
    scales = "free_y",
    switch = "y"
  ) +
  scale_x_continuous(
    breaks = c(2020, 2050, 2100),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(
    values = carrier_colours,
    breaks = used_carriers,
    drop = TRUE
  ) +
  labs(
    x = "Year",
    y = "Final energy consumption (EJ yr⁻¹)",
    fill = NULL
  ) +
  theme_bw(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    axis.text = element_text(colour = "black"),
    strip.background = element_rect(fill = "grey94", colour = "black"),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = grid::unit(1.1, "cm"),
    panel.spacing = grid::unit(0.7, "lines"),
    plot.margin = margin(6, 8, 6, 8)
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

print(paper_figure)

ggsave(
  file.path(output_dir, "final_energy_asia_three_panels.png"),
  plot = paper_figure,
  width = 190,
  height = 180,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(output_dir, "final_energy_asia_three_panels.pdf"),
  plot = paper_figure,
  width = 190,
  height = 180,
  units = "mm",
  device = "pdf"
)


# -----------------------------------------------------------------------------
# AIMHub-GAINS emission comparison (China + Japan + South Korea)
# -----------------------------------------------------------------------------
# The GAINS files and sector mapping follow data/dataiamc.R. Only sectors that
# can be matched consistently between the two models are retained. GAINS is
# already in kt yr-1; AIMHub is converted from Mt yr-1 to kt yr-1.

gains_sheet_map <- c(
  Baseline = 1,
  Mitigation = 2,
  ammonia = 3
)

gains_sources <- data.frame(
  Path = file.path(
    "iamc", "data", "RE", "emission", "emission",
    c(
      "emissGAINSSector (5).xlsx",  # South Korea, NOx
      "emissGAINSSector (6).xlsx",  # Japan, NOx
      "emissGAINSSector (9).xlsx",  # China, NOx
      "emissGAINSSector (3).xlsx",  # South Korea, NH3
      "emissGAINSSector (8).xlsx",  # Japan, NH3
      "emissGAINSSector (13).xlsx"  # China, NH3
    )
  ),
  Country = rep(c("South Korea", "Japan", "China"), times = 2),
  Pollutant = rep(c("NOx", "NH3"), each = 3),
  stringsAsFactors = FALSE
)

missing_gains_files <- gains_sources$Path[!file.exists(gains_sources$Path)]
if (length(missing_gains_files) > 0) {
  stop("Missing GAINS file(s): ", paste(missing_gains_files, collapse = ", "))
}

read_gains_sheet <- function(path, sheet, scenario, country, pollutant) {
  data <- read_excel(
    path,
    sheet = sheet,
    # These source workbooks contain an incorrect worksheet dimension.
    # An explicit range ensures that readxl imports all cells through 2050.
    range = "A8:N28",
    na = c("n.a", "n.a.", "NA", "")
  )
  names(data)[1] <- "GAINS_sector"

  data %>%
    select(where(~ !all(is.na(.x)))) %>%
    mutate(across(-GAINS_sector, ~ suppressWarnings(as.numeric(.x)))) %>%
    pivot_longer(
      cols = -GAINS_sector,
      names_to = "Year",
      values_to = "Emission"
    ) %>%
    mutate(
      Year = suppressWarnings(as.numeric(Year)),
      Scenario = scenario,
      Country = country,
      Pollutant = pollutant
    )
}

gains_emission_raw <- bind_rows(lapply(
  seq_len(nrow(gains_sources)),
  function(i) {
    bind_rows(lapply(
      names(gains_sheet_map),
      function(scenario) {
        read_gains_sheet(
          path = gains_sources$Path[i],
          sheet = unname(gains_sheet_map[scenario]),
          scenario = scenario,
          country = gains_sources$Country[i],
          pollutant = gains_sources$Pollutant[i]
        )
      }
    ))
  }
))

# Do not add aggregate and component rows together. These choices reproduce the
# existing comparison mapping while fixing the omitted non-road suffix mapping.
gains_emission <- gains_emission_raw %>%
  mutate(
    Sector = case_when(
      GAINS_sector %in% c(
        "Power and heating plants",
        "Fuel conversion",
        "Fuel production & distribution"
      ) ~ "Energy supply",
      GAINS_sector == "Residential combustion" ~ "Residential",
      GAINS_sector %in% c(
        "Industrial combustion",
        "Industrial processes"
      ) ~ "Industry",
      GAINS_sector %in% c(
        "Light duty vehicles",
        "Heavy duty vehicles-diesel",
        "Non-road machinery"
      ) ~ "Transportation",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(Sector),
    is.finite(Emission),
    Year >= 2015,
    Year <= 2050
  ) %>%
  group_by(Pollutant, Scenario, Year, Sector) %>%
  summarise(Emission = sum(Emission, na.rm = TRUE), .groups = "drop") %>%
  mutate(Model = "GAINS")

aim_emission_map <- data.frame(
  VEMF = c(
    "Emi_NOx_Ene_Sup",
    "Emi_NOx_Ene_Dem_Ind",
    "Emi_NOx_Ene_Dem_Res",
    "Emi_NOx_Ene_Dem_Tra",
    "Emi_NH3_Ene_Sup",
    "Emi_NH3_Ene_Dem_Ind",
    "Emi_NH3_Ene_Dem_Res",
    "Emi_NH3_Ene_Dem_Tra"
  ),
  Pollutant = rep(c("NOx", "NH3"), each = 4),
  Sector = rep(
    c("Energy supply", "Industry", "Residential", "Transportation"),
    times = 2
  ),
  stringsAsFactors = FALSE
)

# Japan and China are explicit AIMHub regions.
aim_emission_direct <- iamc_all %>%
  filter(
    SCENARIO %in% names(scenario_labels),
    REMF %in% c("JPN", "CHN"),
    VEMF %in% aim_emission_map$VEMF
  ) %>%
  inner_join(aim_emission_map, by = "VEMF") %>%
  transmute(
    Pollutant,
    Scenario = unname(scenario_labels[as.character(SCENARIO)]),
    Year = as.numeric(as.character(YEMF)),
    Sector,
    Country = if_else(REMF == "JPN", "Japan", "China"),
    Emission = IAMC_Template * 1000
  ) %>%
  filter(Year >= 2015, Year <= 2050)

# AIMHub does not have a standalone Korean region. koreafit estimates the
# Korean share of XSE using GAINS activity patterns. We calculate that share
# from final energy and apply it to XSE emissions. This keeps the Korean AIMHub
# estimate independent from the GAINS emission totals being compared.
korea_fit_file <- file.path("koreafit", "output", "AIM_KORS_V2.csv")
if (!file.exists(korea_fit_file)) {
  stop("Missing koreafit output: ", korea_fit_file)
}

korea_scenario_labels <- c(
  Baseline = "Baseline",
  `600C` = "Mitigation",
  `600C_nh3` = "ammonia"
)

korea_fit_raw <- read.csv(
  korea_fit_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

korea_final_energy <- korea_fit_raw %>%
  filter(
    SCENARIO %in% names(korea_scenario_labels),
    grepl(
      "^Final Energy\\|(Industry|Residential and Commercial|Transportation)\\|",
      VARIABLE
    )
  ) %>%
  pivot_longer(
    cols = matches("^X\\d{4}$"),
    names_to = "YearColumn",
    values_to = "KoreaFinalEnergy"
  ) %>%
  transmute(
    Scenario = unname(korea_scenario_labels[SCENARIO]),
    Year = as.numeric(sub("^X", "", YearColumn)),
    KoreaFinalEnergy = as.numeric(KoreaFinalEnergy)
  ) %>%
  filter(Year >= 2015, Year <= 2050) %>%
  group_by(Scenario, Year) %>%
  summarise(
    KoreaFinalEnergy = sum(KoreaFinalEnergy, na.rm = TRUE),
    .groups = "drop"
  )

# AIM_KORS_V2.csv inherits the AIMHub final-energy unit (EJ yr-1). Keep both
# Korea and XSE in EJ yr-1 when calculating the share; the previous XSE-only
# multiplication by 1000 understated Korea's share by a factor of 1000.
# Top-level carrier variables avoid overlap with their fuel subcategories.
xse_final_energy <- iamc_all %>%
  filter(
    SCENARIO %in% names(scenario_labels),
    REMF == "XSE",
    VEMF %in% names(total_map)
  ) %>%
  transmute(
    Scenario = unname(scenario_labels[as.character(SCENARIO)]),
    Year = as.numeric(as.character(YEMF)),
    XSEFinalEnergy = IAMC_Template
  ) %>%
  filter(Year >= 2015, Year <= 2050) %>%
  group_by(Scenario, Year) %>%
  summarise(
    XSEFinalEnergy = sum(XSEFinalEnergy, na.rm = TRUE),
    .groups = "drop"
  )

korea_share <- korea_final_energy %>%
  inner_join(xse_final_energy, by = c("Scenario", "Year")) %>%
  mutate(
    KoreaShare = KoreaFinalEnergy / XSEFinalEnergy,
    KoreaSharePercent = KoreaShare * 100
  )

# A scenario-specific share is calculated for every model year. Check that no
# available Korea/XSE year is silently lost before applying these time-varying
# shares to emissions.
korea_share_missing <- bind_rows(
  korea_final_energy %>% select(Scenario, Year),
  xse_final_energy %>% select(Scenario, Year)
) %>%
  count(Scenario, Year) %>%
  filter(n != 2)

if (nrow(korea_share_missing) > 0) {
  stop(
    "Korea and XSE final-energy years do not match: ",
    paste(
      paste(korea_share_missing$Scenario, korea_share_missing$Year, sep = "/"),
      collapse = ", "
    )
  )
}

if (
  nrow(korea_share) == 0 ||
    any(!is.finite(korea_share$KoreaShare)) ||
    any(korea_share$KoreaShare <= 0 | korea_share$KoreaShare >= 1)
) {
  stop("Invalid South Korean share calculated from koreafit output.")
}

write.csv(
  korea_share,
  file.path(output_dir, "korea_xse_final_energy_share.csv"),
  row.names = FALSE
)

korea_share_figure <- ggplot(
  korea_share,
  aes(x = Year, y = KoreaSharePercent, colour = Scenario)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.7) +
  scale_colour_manual(
    values = c(
      Baseline = "#F8766D",
      Mitigation = "#00BA38",
      ammonia = "#619CFF"
    ),
    breaks = c("Baseline", "Mitigation", "ammonia")
  ) +
  scale_x_continuous(breaks = seq(2015, 2050, 5)) +
  labs(
    title = "South Korea share of XSE final energy",
    subtitle = "Scenario- and year-specific shares used to estimate AIMHub emissions",
    x = "Year",
    y = "South Korea share of XSE final energy (%)",
    colour = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(output_dir, "korea_xse_final_energy_share.png"),
  plot = korea_share_figure,
  width = 9,
  height = 5.5,
  dpi = 400,
  bg = "white"
)

ggsave(
  file.path(output_dir, "korea_xse_final_energy_share.pdf"),
  plot = korea_share_figure,
  width = 9,
  height = 5.5,
  device = cairo_pdf
)

# -----------------------------------------------------------------------------
# Final energy use: China + Japan + South Korea
# -----------------------------------------------------------------------------
# China and Japan are explicit AIMHub regions. For South Korea, total and
# transport energy come from the GAINS-activity-based koreafit allocation.
# Since koreafit has no shipping variable, Korean shipping is estimated from
# XSE shipping using the scenario- and year-specific Korea/XSE share above.

china_japan_energy_raw <- iamc_all %>%
  filter(
    SCENARIO %in% names(scenario_labels),
    REMF %in% c("CHN", "JPN")
  ) %>%
  mutate(
    Year = as.numeric(as.character(YEMF)),
    Scenario = unname(scenario_labels[as.character(SCENARIO)])
  ) %>%
  filter(Year >= 2015, Year <= 2050)

china_japan_energy_plot <- bind_rows(
  prepare_final_energy(
    china_japan_energy_raw,
    "(a) Total final energy",
    total_map
  ),
  prepare_final_energy(
    china_japan_energy_raw,
    "(b) Transport sector",
    transport_map
  ),
  prepare_final_energy(
    china_japan_energy_raw,
    "(c) Shipping sector",
    shipping_map
  )
) %>%
  mutate(SourceMethod = "Direct AIMHub: China + Japan")

korea_energy_by_variable <- korea_fit_raw %>%
  filter(
    SCENARIO %in% names(korea_scenario_labels),
    grepl(
      "^Final Energy\\|(Industry|Residential and Commercial|Transportation)\\|",
      VARIABLE
    )
  ) %>%
  pivot_longer(
    cols = matches("^X\\d{4}$"),
    names_to = "YearColumn",
    values_to = "Value"
  ) %>%
  transmute(
    Scenario = unname(korea_scenario_labels[SCENARIO]),
    Year = as.numeric(sub("^X", "", YearColumn)),
    Variable = VARIABLE,
    Value = as.numeric(Value)
  ) %>%
  filter(Year >= 2015, Year <= 2050, is.finite(Value)) %>%
  group_by(Scenario, Year, Variable) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Carrier = case_when(
      grepl("\\|Solids\\|", Variable) ~ "Solids",
      grepl("\\|Liquids", Variable) ~ "Liquids",
      grepl("\\|Gases$", Variable) ~ "Gases",
      grepl("\\|Heat$", Variable) ~ "Heat",
      grepl("\\|Electricity$", Variable) ~ "Electricity",
      grepl("\\|Hydrogen$", Variable) ~ "Hydrogen",
      TRUE ~ NA_character_
    ),
    Carrier = if_else(
      Scenario == "ammonia" & Carrier == "Hydrogen",
      "Ammonia",
      Carrier
    )
  ) %>%
  filter(!is.na(Carrier))

korea_total_transport_plot <- bind_rows(
  korea_energy_by_variable %>%
    transmute(
      Scenario,
      Year,
      Carrier,
      Value,
      Panel = "(a) Total final energy"
    ),
  korea_energy_by_variable %>%
    filter(grepl("^Final Energy\\|Transportation\\|", Variable)) %>%
    transmute(
      Scenario,
      Year,
      Carrier,
      Value,
      Panel = "(b) Transport sector"
    )
) %>%
  mutate(SourceMethod = "koreafit allocation: South Korea")

korea_shipping_plot <- iamc_all %>%
  filter(
    SCENARIO %in% names(scenario_labels),
    REMF == "XSE",
    VEMF %in% names(shipping_map)
  ) %>%
  transmute(
    Scenario = unname(scenario_labels[as.character(SCENARIO)]),
    Year = as.numeric(as.character(YEMF)),
    VEMF,
    IAMC_Template
  ) %>%
  filter(Year >= 2015, Year <= 2050) %>%
  inner_join(
    korea_share %>% select(Scenario, Year, KoreaShare),
    by = c("Scenario", "Year")
  ) %>%
  mutate(IAMC_Template = IAMC_Template * KoreaShare) %>%
  prepare_final_energy(
    "(c) Shipping sector",
    shipping_map
  ) %>%
  mutate(SourceMethod = "XSE shipping times annual Korea share")

china_japan_korea_energy_components <- bind_rows(
  china_japan_energy_plot,
  korea_total_transport_plot,
  korea_shipping_plot
)

write.csv(
  china_japan_korea_energy_components %>%
    arrange(Panel, Scenario, Year, Carrier, SourceMethod),
  file.path(
    output_dir,
    "final_energy_china_japan_south_korea_components.csv"
  ),
  row.names = FALSE
)

china_japan_korea_energy_plot <- china_japan_korea_energy_components %>%
  group_by(Scenario, Year, Carrier, Panel) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    ),
    Panel = factor(Panel, levels = panel_levels),
    Carrier = factor(Carrier, levels = carrier_levels)
  )

china_japan_korea_used_carriers <- carrier_levels[
  carrier_levels %in% unique(
    as.character(
      china_japan_korea_energy_plot$Carrier[
        china_japan_korea_energy_plot$Value > 1e-12
      ]
    )
  )
]

china_japan_korea_energy_figure <- ggplot(
  china_japan_korea_energy_plot,
  aes(x = Year, y = Value, fill = Carrier)
) +
  geom_area(position = "stack", colour = NA) +
  facet_grid(
    rows = vars(Panel),
    cols = vars(Scenario),
    scales = "free_y",
    switch = "y"
  ) +
  scale_x_continuous(
    breaks = c(2020, 2035, 2050),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(
    values = carrier_colours,
    breaks = china_japan_korea_used_carriers,
    drop = TRUE
  ) +
  labs(
    x = "Year",
    y = "Final energy consumption (EJ yr⁻¹)",
    fill = NULL,
    caption = paste0(
      "Regional total: China, Japan, and South Korea. ",
      "South Korean shipping is estimated using its annual share of XSE final energy."
    )
  ) +
  theme_bw(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    axis.text = element_text(colour = "black"),
    strip.background = element_rect(fill = "grey94", colour = "black"),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = grid::unit(1.1, "cm"),
    panel.spacing = grid::unit(0.7, "lines"),
    plot.caption = element_text(size = 8, hjust = 0),
    plot.margin = margin(6, 8, 6, 8)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(
  file.path(
    output_dir,
    "final_energy_china_japan_south_korea_three_panels.png"
  ),
  plot = china_japan_korea_energy_figure,
  width = 190,
  height = 180,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(
    output_dir,
    "final_energy_china_japan_south_korea_three_panels.pdf"
  ),
  plot = china_japan_korea_energy_figure,
  width = 190,
  height = 180,
  units = "mm",
  device = "pdf"
)

aim_emission_korea <- iamc_all %>%
  filter(
    SCENARIO %in% names(scenario_labels),
    REMF == "XSE",
    VEMF %in% aim_emission_map$VEMF
  ) %>%
  inner_join(aim_emission_map, by = "VEMF") %>%
  transmute(
    Pollutant,
    Scenario = unname(scenario_labels[as.character(SCENARIO)]),
    Year = as.numeric(as.character(YEMF)),
    Sector,
    Country = "South Korea",
    XSEEmission = IAMC_Template * 1000
  ) %>%
  filter(Year >= 2015, Year <= 2050) %>%
  inner_join(
    korea_share %>% select(Scenario, Year, KoreaShare),
    by = c("Scenario", "Year")
  ) %>%
  # Apply the share for the same scenario and model year, allowing the Korean
  # contribution to evolve through 2050 instead of using a fixed base-year ratio.
  mutate(Emission = XSEEmission * KoreaShare) %>%
  select(-XSEEmission, -KoreaShare)

write.csv(
  aim_emission_korea %>%
    arrange(Pollutant, Scenario, Year, Sector),
  file.path(output_dir, "south_korea_aimhub_emissions_estimated.csv"),
  row.names = FALSE
)

aim_emission <- bind_rows(aim_emission_direct, aim_emission_korea) %>%
  group_by(Pollutant, Scenario, Year, Sector) %>%
  summarise(Emission = sum(Emission, na.rm = TRUE), .groups = "drop") %>%
  mutate(Model = "AIMHub")

emission_plot_data <- bind_rows(aim_emission, gains_emission) %>%
  mutate(
    Pollutant = factor(Pollutant, levels = c("NOx", "NH3")),
    Scenario = factor(Scenario, levels = c("Baseline", "Mitigation", "ammonia")),
    Model = factor(Model, levels = c("AIMHub", "GAINS")),
    Sector = factor(
      Sector,
      levels = c("Energy supply", "Industry", "Residential", "Transportation")
    )
  )

emission_sector_colours <- c(
  `Energy supply` = "#4E79A7",
  Industry = "#F28E2B",
  Residential = "#59A14F",
  Transportation = "#E15759"
)

# Fix each pollutant's y-axis maximum across every scenario and model. Invisible
# anchors keep separate scenario plots directly comparable.
emission_y_anchors <- emission_plot_data %>%
  group_by(Pollutant, Scenario, Model, Year) %>%
  summarise(Total = sum(Emission, na.rm = TRUE), .groups = "drop") %>%
  group_by(Pollutant) %>%
  summarise(Emission = max(Total, na.rm = TRUE) * 1.04, .groups = "drop") %>%
  tidyr::crossing(
    Model = factor(c("AIMHub", "GAINS"), levels = c("AIMHub", "GAINS"))
  ) %>%
  mutate(Year = 2015)

make_emission_scenario_plot <- function(scenario_name) {
  ggplot(
    emission_plot_data %>% filter(Scenario == scenario_name),
    aes(x = Year, y = Emission, fill = Sector)
  ) +
    geom_col(width = 4, colour = NA) +
    geom_blank(
      data = emission_y_anchors,
      aes(x = Year, y = Emission),
      inherit.aes = FALSE
    ) +
    facet_grid(
      rows = vars(Pollutant),
      cols = vars(Model),
      scales = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(
      breaks = c(2015, 2030, 2050),
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = emission_sector_colours, drop = FALSE) +
    labs(
      title = scenario_name,
      x = "Year",
      y = "Emissions (kt yr⁻¹)",
      fill = NULL
    ) +
    theme_bw(base_size = 8, base_family = "sans") +
    theme(
      plot.title = element_text(
        face = "bold", size = 11, hjust = 0.5,
        margin = margin(b = 4)
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
      axis.text = element_text(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "grey94", colour = "black"),
      strip.text = element_text(face = "bold"),
      strip.placement = "outside",
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.width = grid::unit(1.05, "cm"),
      panel.spacing = grid::unit(0.45, "lines"),
      plot.margin = margin(4, 4, 4, 4)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
}

scenario_names <- c("Baseline", "Mitigation", "ammonia")
emission_scenario_plots <- lapply(
  scenario_names,
  make_emission_scenario_plot
)

# Extract one common legend and remove the repeated legends from the panels.
legend_source <- ggplotGrob(emission_scenario_plots[[1]])
legend_candidates <- which(grepl("^guide-box", legend_source$layout$name))
legend_candidates <- legend_candidates[
  !vapply(
    legend_source$grobs[legend_candidates],
    inherits,
    logical(1),
    what = "zeroGrob"
  )
]
if (length(legend_candidates) == 0) {
  stop("Could not extract the emission-figure legend.")
}
emission_legend <- legend_source$grobs[[legend_candidates[1]]]

emission_panel_grob <- arrangeGrob(
  grobs = lapply(
    emission_scenario_plots,
    function(plot) plot + theme(legend.position = "none")
  ),
  ncol = 3,
  padding = grid::unit(0.25, "lines")
)

emission_comparison_figure <- arrangeGrob(
  emission_panel_grob,
  emission_legend,
  ncol = 1,
  heights = c(1, 0.08),
  bottom = grid::textGrob(
    "Regional total: China, Japan, and South Korea",
    gp = grid::gpar(fontsize = 8)
  )
)

grid::grid.newpage()
grid::grid.draw(emission_comparison_figure)

ggsave(
  file.path(output_dir, "emissions_aimhub_vs_gains.png"),
  plot = emission_comparison_figure,
  width = 230,
  height = 135,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(output_dir, "emissions_aimhub_vs_gains.pdf"),
  plot = emission_comparison_figure,
  width = 230,
  height = 135,
  units = "mm",
  device = "pdf"
)


# -----------------------------------------------------------------------------
# Population-weighted PM2.5 concentration: AIM-GEOS and GAINS
# -----------------------------------------------------------------------------

pm25_files <- c(
  geos_base = file.path(
    "data", "0707", "geos-chem",
    "05x05_Base_2015_off_estasia_pm25_Surface_Re_yearavg.nc4"
  ),
  geos_baseline_2050 = file.path(
    "data", "0707", "geos-chem",
    "05x05_Base_2050_off_estasia_pm25_Surface_Re_yearavg.nc4"
  ),
  geos_mitigation_2050 = file.path(
    "data", "0707", "geos-chem",
    "05x05_1p5C_2050_off_estasia_pm25_Surface_Re_yearavg.nc4"
  ),
  geos_ammonia_2050 = file.path(
    "data", "0707", "geos-chem",
    "05x05_NH3_2050_off_estasia_pm25_Surface_Re_yearavg.nc4"
  ),
  population = file.path("data", "populationSSP_update.gdx"),
  country_grid = file.path("data", "country0p5.txt"),
  receptor_lookup = file.path("data", "test_results.csv"),
  gains_base = file.path("data", "gains", "mortality_2020.csv"),
  gains_baseline_2050 = file.path(
    "data", "gains", "mortality_Baseline_2050.csv"
  ),
  gains_mitigation_2050 = file.path(
    "data", "gains", "mortality_600C_2050.csv"
  ),
  gains_ammonia_2050 = file.path(
    "data", "gains", "mortality_600C_NH3_2050.csv"
  )
)

pm25_shapefile <- file.path(
  "data", "GAINS_REGIONS_DW4", "GAINS_W_REGIONS_DW4_W84x.shp"
)
missing_pm25_files <- c(
  unname(pm25_files[!file.exists(pm25_files)]),
  if (!file.exists(pm25_shapefile)) pm25_shapefile
)
if (length(missing_pm25_files) > 0) {
  stop(
    "Files required for the PM2.5 figure were not found:\n",
    paste(missing_pm25_files, collapse = "\n")
  )
}

# Retain the population year here. The previous script dropped Syr before the
# join, causing both 2015 and 2050 populations to be attached to every cell.
pm25_population <- rgdx.param(
  pm25_files[["population"]], "populationSSP_update"
) %>%
  filter(ssp == "SSP2", Syr %in% c("2015", "2050")) %>%
  transmute(
    PopulationYear = as.integer(as.character(Syr)),
    GridX = as.numeric(as.character(Slng)),
    GridY = as.numeric(as.character(Slat)),
    Population = as.numeric(populationSSP_update)
  )

pm25_regions_sf <- sf::st_read(pm25_shapefile, quiet = TRUE)
pm25_regions_sf$RegionID <- match(
  pm25_regions_sf$IDREGIONS,
  unique(pm25_regions_sf$IDREGIONS)
)

pm25_region_template <- raster::raster(
  xmn = floor(sf::st_bbox(pm25_regions_sf)$xmin),
  xmx = ceiling(sf::st_bbox(pm25_regions_sf)$xmax),
  ymn = floor(sf::st_bbox(pm25_regions_sf)$ymin),
  ymx = ceiling(sf::st_bbox(pm25_regions_sf)$ymax),
  res = 0.5
)

pm25_region_raster <- raster::rasterize(
  pm25_regions_sf,
  pm25_region_template,
  field = "RegionID",
  fun = "first"
)

pm25_region_lookup <- tibble(
  RegionID = pm25_regions_sf$RegionID,
  ReceptorCode = pm25_regions_sf$IDREGIONS
) %>%
  distinct()

pm25_region_grid <- raster::as.data.frame(
  pm25_region_raster,
  xy = TRUE,
  na.rm = TRUE
) %>%
  setNames(c("Longitude", "Latitude", "RegionID")) %>%
  left_join(pm25_region_lookup, by = "RegionID") %>%
  transmute(
    # The GAINS raster centres are offset by 0.25 degrees in longitude
    # relative to the country-grid convention used in this project.
    Longitude = round(Longitude + 0.25, 2),
    Latitude = round(Latitude, 2),
    ReceptorCode
  )

pm25_country_grid <- read.delim(
  pm25_files[["country_grid"]],
  header = FALSE,
  col.names = c("ISO", "Dummy1", "GridY", "Dummy2", "GridX"),
  stringsAsFactors = FALSE
) %>%
  transmute(
    ISO,
    GridX = as.numeric(GridX),
    GridY = as.numeric(GridY)
  ) %>%
  # Border cells can be listed for more than one country. Country identity is
  # not used here, so keep each physical 0.5-degree grid cell only once.
  distinct(GridX, GridY, .keep_all = TRUE) %>%
  inner_join(pm25_population, by = c("GridX", "GridY")) %>%
  mutate(
    Longitude = round(-180 + (GridX - 1) * 0.5, 2),
    Latitude = round(90 - (GridY - 1) * 0.5 + 0.25, 2)
  ) %>%
  left_join(pm25_region_grid, by = c("Longitude", "Latitude"))

pm25_receptor_labels <- c(
  CHIN_BEIJ = "China: Beijing",
  CHIN_SHAN = "China: Shanghai",
  CHIN_GUAD = "China: Guangdong",
  JAPA_KANT = "Japan: Kanto",
  KORS_SOUT = "South Korea: Seoul-Incheon"
)

# test_results.csv contains the 101 GAINS receptor codes in the same order as
# the rows in the GAINS concentration files. Use it to retain every receptor,
# rather than relying on a manually written four-region mapping.
pm25_receptor_code_source <- read.csv(
  pm25_files[["receptor_lookup"]],
  check.names = TRUE,
  stringsAsFactors = FALSE
)
pm25_gains_name_source <- read.csv(
  pm25_files[["gains_base"]],
  skip = 8,
  check.names = TRUE,
  stringsAsFactors = FALSE
)

pm25_all_receptor_lookup <- tibble(
  GainsRow = as.integer(pm25_receptor_code_source[[1]]) + 1L,
  ReceptorCode = pm25_receptor_code_source$receptor
) %>%
  distinct(GainsRow, ReceptorCode) %>%
  left_join(
    tibble(
      GainsRow = seq_len(nrow(pm25_gains_name_source)),
      Receptor = pm25_gains_name_source[[1]]
    ),
    by = "GainsRow"
  ) %>%
  filter(!is.na(ReceptorCode), !is.na(Receptor)) %>%
  arrange(GainsRow)

if (nrow(pm25_all_receptor_lookup) != nrow(pm25_gains_name_source)) {
  stop(
    "The GAINS receptor-code lookup is incomplete: ",
    nrow(pm25_all_receptor_lookup), " of ",
    nrow(pm25_gains_name_source), " regions matched."
  )
}

population_weighted_geos <- function(path, scenario, population_year, period) {
  concentration <- raster::as.data.frame(
    raster::raster(path, varname = "PM25"),
    xy = TRUE
  ) %>%
    setNames(c("Longitude", "Latitude", "PM25")) %>%
    mutate(
      Longitude = round(Longitude, 2),
      Latitude = round(Latitude, 2)
    )

  concentration %>%
    inner_join(
      pm25_country_grid %>%
        filter(PopulationYear == population_year),
      by = c("Longitude", "Latitude")
    ) %>%
    filter(!is.na(PM25), !is.na(Population), !is.na(ReceptorCode)) %>%
    group_by(ReceptorCode) %>%
    summarise(
      PM25 = weighted.mean(PM25, Population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Model = "AIM-GEOS",
      Scenario = scenario,
      Period = period
    )
}

geos_pm25 <- bind_rows(
  population_weighted_geos(
    pm25_files[["geos_base"]], "Baseline", 2015, "Base year"
  ),
  population_weighted_geos(
    pm25_files[["geos_baseline_2050"]], "Baseline", 2050, "2050"
  ),
  population_weighted_geos(
    pm25_files[["geos_mitigation_2050"]], "Mitigation", 2050, "2050"
  ),
  population_weighted_geos(
    pm25_files[["geos_ammonia_2050"]], "ammonia", 2050, "2050"
  )
)

read_gains_pm25 <- function(path, scenario, period) {
  gains_data <- read.csv(
    path,
    skip = 8,
    check.names = TRUE,
    stringsAsFactors = FALSE
  )
  concentration_column <- grep(
    "^Mean\\.PM", names(gains_data), value = TRUE
  )[1]
  if (is.na(concentration_column)) {
    stop("GAINS PM2.5 concentration column not found in: ", path)
  }

  tibble(
    Receptor = gains_data[[1]],
    PM25 = as.numeric(gains_data[[concentration_column]]),
    Model = "GAINS",
    Scenario = scenario,
    Period = period
  )
}

gains_pm25 <- bind_rows(
  read_gains_pm25(
    pm25_files[["gains_base"]], "Baseline", "Base year"
  ),
  read_gains_pm25(
    pm25_files[["gains_baseline_2050"]], "Baseline", "2050"
  ),
  read_gains_pm25(
    pm25_files[["gains_mitigation_2050"]], "Mitigation", "2050"
  ),
  read_gains_pm25(
    pm25_files[["gains_ammonia_2050"]], "ammonia", "2050"
  )
) %>%
  left_join(
    select(pm25_all_receptor_lookup, ReceptorCode, Receptor),
    by = "Receptor"
  ) %>%
  filter(!is.na(ReceptorCode)) %>%
  select(-Receptor)

pm25_data <- bind_rows(geos_pm25, gains_pm25)

# Each model has one common base-year value. Replicate it across the mitigation
# and ammonia lines so all three scenario trajectories start from the same point.
pm25_base_copies <- pm25_data %>%
  filter(Scenario == "Baseline", Period == "Base year") %>%
  select(-Scenario) %>%
  tidyr::crossing(Scenario = c("Mitigation", "ammonia"))

pm25_plot_data <- bind_rows(pm25_data, pm25_base_copies) %>%
  mutate(
    Receptor = factor(
      unname(pm25_receptor_labels[ReceptorCode]),
      levels = unname(pm25_receptor_labels)
    ),
    Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    ),
    Period = factor(Period, levels = c("Base year", "2050"))
  ) %>%
  filter(!is.na(Receptor), !is.na(PM25))

pm25_scenario_colors <- c(
  Baseline = "#D55E00",
  Mitigation = "#0072B2",
  ammonia = "#009E73"
)

pm25_region_titles <- c(
  "(a) China: Beijing",
  "(b) China: Shanghai",
  "(c) Japan: Kanto",
  "(d) South Korea: Seoul-Incheon"
)

pm25_region_plots <- Map(
  function(receptor_name, panel_title) {
    ggplot(
      filter(pm25_plot_data, Receptor == receptor_name),
      aes(
        x = Period,
        y = PM25,
        color = Scenario,
        group = Scenario
      )
    ) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2.1) +
      facet_grid(. ~ Model) +
      scale_color_manual(values = pm25_scenario_colors, drop = FALSE) +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.08))
      ) +
      labs(
        title = panel_title,
        x = NULL,
        y = expression(Population-weighted~PM[2.5]~(mu*g~m^{-3})),
        color = NULL
      ) +
      theme_bw(base_size = 9) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
        plot.title = element_text(face = "bold", size = 10, hjust = 0),
        strip.background = element_rect(fill = "grey94", color = "black"),
        strip.text = element_text(face = "bold", size = 9),
        axis.text = element_text(color = "black"),
        legend.position = "top",
        legend.key.width = grid::unit(1.5, "lines"),
        plot.margin = margin(4, 5, 4, 5)
      )
  },
  receptor_name = unname(pm25_receptor_labels),
  panel_title = pm25_region_titles
)

pm25_legend_source <- ggplotGrob(pm25_region_plots[[1]])
pm25_legend_candidates <- which(
  grepl("^guide-box", pm25_legend_source$layout$name)
)
pm25_legend_candidates <- pm25_legend_candidates[
  !vapply(
    pm25_legend_source$grobs[pm25_legend_candidates],
    inherits,
    logical(1),
    what = "zeroGrob"
  )
]
if (length(pm25_legend_candidates) == 0) {
  stop("Could not extract the PM2.5-figure legend.")
}
pm25_legend <- pm25_legend_source$grobs[[pm25_legend_candidates[1]]]

pm25_panel_grob <- arrangeGrob(
  grobs = lapply(
    pm25_region_plots,
    function(plot) plot + theme(legend.position = "none")
  ),
  ncol = 2,
  padding = grid::unit(0.35, "lines")
)

pm25_comparison_figure <- arrangeGrob(
  pm25_legend,
  pm25_panel_grob,
  ncol = 1,
  heights = c(0.08, 1),
  bottom = grid::textGrob(
    "Base year: 2015 for AIM-GEOS and 2020 for GAINS",
    gp = grid::gpar(fontsize = 8)
  )
)

grid::grid.newpage()
grid::grid.draw(pm25_comparison_figure)

ggsave(
  file.path(output_dir, "pm25_concentration_aim_geos_vs_gains.png"),
  plot = pm25_comparison_figure,
  width = 210,
  height = 165,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(output_dir, "pm25_concentration_aim_geos_vs_gains.pdf"),
  plot = pm25_comparison_figure,
  width = 210,
  height = 165,
  units = "mm",
  device = "pdf"
)


# All East Asian receptor regions covered by both AIM-GEOS and GAINS: six
# regions per page. The GEOS-Chem files used here have an East Asian domain;
# GAINS-only regions outside that domain are therefore not shown as misleading
# empty AIM-GEOS panels.
pm25_common_receptor_codes <- intersect(
  unique(geos_pm25$ReceptorCode),
  unique(gains_pm25$ReceptorCode)
)
pm25_comparison_receptor_lookup <- pm25_all_receptor_lookup %>%
  filter(ReceptorCode %in% pm25_common_receptor_codes) %>%
  arrange(GainsRow)

pm25_all_plot_data <- bind_rows(pm25_data, pm25_base_copies) %>%
  filter(ReceptorCode %in% pm25_common_receptor_codes) %>%
  left_join(
    select(pm25_comparison_receptor_lookup, ReceptorCode, Receptor),
    by = "ReceptorCode"
  ) %>%
  mutate(
    Receptor = factor(
      Receptor,
      levels = pm25_comparison_receptor_lookup$Receptor
    ),
    Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    ),
    Period = factor(Period, levels = c("Base year", "2050"))
  ) %>%
  filter(!is.na(Receptor), !is.na(PM25))

write.csv(
  pm25_all_plot_data %>%
    arrange(Receptor, Model, Scenario, Period),
  file.path(output_dir, "pm25_concentration_all_regions.csv"),
  row.names = FALSE
)

make_pm25_all_region_plot <- function(receptor_name, panel_number) {
  ggplot(
    filter(pm25_all_plot_data, Receptor == receptor_name),
    aes(
      x = Period,
      y = PM25,
      color = Scenario,
      group = Scenario
    )
  ) +
    geom_line(linewidth = 0.65) +
    geom_point(size = 1.7) +
    ggrepel::geom_text_repel(
      data = filter(pm25_all_plot_data, Receptor == receptor_name, Period == "2050"),
      aes(label = sprintf("%.2f", PM25)),
      direction = "y", size = 2.3, max.overlaps = Inf,
      seed = 2050, show.legend = FALSE
    ) +
    facet_grid(. ~ Model) +
    scale_color_manual(values = pm25_scenario_colors, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      title = paste0("(", panel_number, ") ", receptor_name),
      x = NULL,
      y = expression(PM[2.5]~(mu*g~m^{-3})),
      color = NULL
    ) +
    theme_bw(base_size = 8) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.25),
      plot.title = element_text(face = "bold", size = 8.5, hjust = 0),
      strip.background = element_rect(fill = "grey94", color = "black"),
      strip.text = element_text(face = "bold", size = 8),
      axis.text = element_text(color = "black", size = 7),
      axis.title.y = element_text(size = 8),
      legend.position = "none",
      plot.margin = margin(3, 4, 3, 4)
    )
}

pm25_all_receptor_names <- as.character(
  pm25_comparison_receptor_lookup$Receptor
)
pm25_all_region_plots <- Map(
  make_pm25_all_region_plot,
  receptor_name = pm25_all_receptor_names,
  panel_number = seq_along(pm25_all_receptor_names)
)

pm25_regions_per_page <- 6L
pm25_page_groups <- split(
  seq_along(pm25_all_region_plots),
  ceiling(seq_along(pm25_all_region_plots) / pm25_regions_per_page)
)

pm25_all_region_pages <- lapply(
  seq_along(pm25_page_groups),
  function(page_number) {
    page_indices <- pm25_page_groups[[page_number]]
    page_panels <- arrangeGrob(
      grobs = pm25_all_region_plots[page_indices],
      ncol = 2,
      nrow = 3,
      padding = grid::unit(0.25, "lines")
    )

    arrangeGrob(
      pm25_legend,
      page_panels,
      ncol = 1,
      heights = c(0.055, 1),
      top = grid::textGrob(
        paste0(
          "Population-weighted PM2.5 concentration — all East Asian regions ",
          "(", page_number, "/", length(pm25_page_groups), ")"
        ),
        gp = grid::gpar(fontsize = 10, fontface = "bold")
      ),
      bottom = grid::textGrob(
        "Base year: 2015 for AIM-GEOS and 2020 for GAINS",
        gp = grid::gpar(fontsize = 7.5)
      )
    )
  }
)

pm25_all_png_dir <- file.path(output_dir, "pm25_all_regions_pages")
if (dir.exists(pm25_all_png_dir)) {
  # Remove stale generated pages when the number of pages changes.
  unlink(pm25_all_png_dir, recursive = TRUE, force = TRUE)
}
dir.create(pm25_all_png_dir, recursive = TRUE, showWarnings = FALSE)

for (page_number in seq_along(pm25_all_region_pages)) {
  ggsave(
    file.path(
      pm25_all_png_dir,
      sprintf("pm25_all_regions_page_%02d.png", page_number)
    ),
    plot = pm25_all_region_pages[[page_number]],
    width = 210,
    height = 297,
    units = "mm",
    dpi = 400,
    bg = "white"
  )
}

pm25_all_pdf_file <- file.path(
  output_dir,
  "pm25_concentration_aim_geos_vs_gains_all_regions.pdf"
)
grDevices::pdf(
  pm25_all_pdf_file,
  width = 210 / 25.4,
  height = 297 / 25.4,
  onefile = TRUE
)
for (page in pm25_all_region_pages) {
  grid::grid.newpage()
  grid::grid.draw(page)
}
grDevices::dev.off()


# Single-page, all-region figures in the style of output/GAINS_PM25.png.
# AIM-GEOS and GAINS are kept as separate figures so every regional panel
# remains readable and can use its own y-axis range.
plot_pm25_all_regions_single_page <- function(model_name, file_stem) {
  model_data <- pm25_all_plot_data %>%
    filter(Model == model_name) %>%
    mutate(
      Year = case_when(
        Period == "2050" ~ 2050,
        model_name == "GAINS" ~ 2020,
        TRUE ~ 2015
      ),
      ReceptorCode = factor(
        ReceptorCode,
        levels = pm25_comparison_receptor_lookup$ReceptorCode
      )
    )

  # Show the common base-year value once and all three scenario values in 2050.
  label_data <- model_data %>%
    filter(
      Period == "2050" |
        (Period == "Base year" & Scenario == "Baseline")
    )

  base_year <- ifelse(model_name == "GAINS", 2020, 2015)
  scenario_colors_reference <- c(
    Baseline = "#F8766D",
    Mitigation = "#00BA38",
    ammonia = "#619CFF"
  )

  figure <- ggplot(
    model_data,
    aes(
      x = Year,
      y = PM25,
      color = Scenario,
      group = Scenario
    )
  ) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 1.8) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = sprintf("%.2f", PM25)),
      size = 3.0,
      direction = "y",
      box.padding = 0.25,
      point.padding = 0.15,
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 1204,
      show.legend = FALSE
    ) +
    facet_wrap(~ ReceptorCode, scales = "free_y", ncol = 6) +
    scale_color_manual(
      values = scenario_colors_reference,
      breaks = c("Baseline", "Mitigation", "ammonia"),
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(base_year, 2050),
      limits = c(base_year - 2, 2052),
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.13, 0.16))) +
    labs(
      title = paste0(
        model_name,
        ": Population-weighted PM2.5 concentration by scenario"
      ),
      x = "Year",
      y = expression(Population-weighted~PM[2.5]~(mu*g~m^{-3})),
      color = "Scenario"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_line(color = "grey93", linewidth = 0.25),
      panel.grid.major = element_line(color = "grey87", linewidth = 0.35),
      panel.spacing = grid::unit(0.75, "lines"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text = element_text(size = 8, color = "black"),
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.key.width = grid::unit(1.3, "lines"),
      plot.margin = margin(8, 10, 8, 10)
    )

  ggsave(
    file.path(output_dir, paste0(file_stem, ".png")),
    plot = figure,
    width = 500,
    height = 250,
    units = "mm",
    dpi = 400,
    bg = "white"
  )
  ggsave(
    file.path(output_dir, paste0(file_stem, ".pdf")),
    plot = figure,
    width = 500,
    height = 250,
    units = "mm",
    device = "pdf"
  )

  figure
}

pm25_gains_all_regions_figure <- plot_pm25_all_regions_single_page(
  "GAINS",
  "GAINS_PM25_all_east_asia_regions"
)
pm25_geos_all_regions_figure <- plot_pm25_all_regions_single_page(
  "AIM-GEOS",
  "AIM_GEOS_PM25_all_east_asia_regions"
)


# -----------------------------------------------------------------------------
# Additional PM2.5 figures: country totals and selected receptor regions
# -----------------------------------------------------------------------------

# Population totals for each receptor are used to aggregate regional mean
# concentrations to national population-weighted means. SSP2 population in
# 2015 is used for the base year and SSP2 population in 2050 for 2050.
pm25_receptor_population <- pm25_country_grid %>%
  filter(
    ReceptorCode %in% pm25_common_receptor_codes,
    !is.na(Population),
    Population > 0
  ) %>%
  group_by(ReceptorCode, PopulationYear) %>%
  summarise(
    ReceptorPopulation = sum(Population, na.rm = TRUE),
    .groups = "drop"
  )

pm25_country_lookup <- tibble(
  ReceptorCode = pm25_comparison_receptor_lookup$ReceptorCode
) %>%
  mutate(
    Country = case_when(
      grepl("^CHIN_", ReceptorCode) ~ "China",
      grepl("^JAPA_", ReceptorCode) ~ "Japan",
      grepl("^KORS_", ReceptorCode) ~ "South Korea",
      grepl("^KORN_", ReceptorCode) ~ "North Korea",
      ReceptorCode == "MONG_WHOL" ~ "Mongolia",
      ReceptorCode == "TAIW_WHOL" ~ "Taiwan",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Country))

pm25_country_levels <- c(
  "China",
  "Japan",
  "South Korea",
  "North Korea",
  "Taiwan"
)

pm25_country_plot_data <- bind_rows(pm25_data, pm25_base_copies) %>%
  filter(ReceptorCode %in% pm25_common_receptor_codes) %>%
  mutate(
    PopulationYear = if_else(Period == "2050", 2050L, 2015L)
  ) %>%
  left_join(
    pm25_receptor_population,
    by = c("ReceptorCode", "PopulationYear")
  ) %>%
  left_join(pm25_country_lookup, by = "ReceptorCode") %>%
  filter(!is.na(Country), Country %in% pm25_country_levels, !is.na(ReceptorPopulation)) %>%
  group_by(Country, Model, Scenario, Period) %>%
  summarise(
    PM25 = weighted.mean(
      PM25,
      ReceptorPopulation,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Country = factor(Country, levels = pm25_country_levels),
    Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    ),
    Period = factor(Period, levels = c("Base year", "2050"))
  ) %>%
  filter(!is.na(PM25), !is.nan(PM25))

write.csv(
  pm25_country_plot_data %>%
    arrange(Country, Model, Scenario, Period),
  file.path(output_dir, "pm25_concentration_country_totals.csv"),
  row.names = FALSE
)

pm25_selected_codes <- c(
  "CHIN_BEIJ",
  "CHIN_SHAN",
  "CHIN_GUAD",
  "JAPA_KANT",
  "KORS_SOUT"
)

pm25_selected_plot_data <- pm25_all_plot_data %>%
  filter(ReceptorCode %in% pm25_selected_codes) %>%
  mutate(
    ReceptorCode = factor(
      ReceptorCode,
      levels = pm25_selected_codes
    )
  )

write.csv(
  pm25_selected_plot_data %>%
    arrange(ReceptorCode, Model, Scenario, Period),
  file.path(output_dir, "pm25_concentration_selected_regions.csv"),
  row.names = FALSE
)

plot_pm25_focused_facets <- function(
  data,
  model_name,
  facet_column,
  title_text,
  file_stem,
  ncol,
  width_mm,
  height_mm
) {
  model_data <- data %>%
    filter(Model == model_name) %>%
    mutate(
      Year = case_when(
        Period == "2050" ~ 2050,
        model_name == "GAINS" ~ 2020,
        TRUE ~ 2015
      ),
      Facet = .data[[facet_column]]
    )

  label_data <- model_data %>%
    filter(
      Period == "2050" |
        (Period == "Base year" & Scenario == "Baseline")
    )

  base_year <- ifelse(model_name == "GAINS", 2020, 2015)
  scenario_colors_reference <- c(
    Baseline = "#F8766D",
    Mitigation = "#00BA38",
    ammonia = "#619CFF"
  )

  figure <- ggplot(
    model_data,
    aes(
      x = Year,
      y = PM25,
      color = Scenario,
      group = Scenario
    )
  ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.3) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = sprintf("%.2f", PM25)),
      size = 3.4,
      direction = "y",
      box.padding = 0.3,
      point.padding = 0.18,
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 1204,
      show.legend = FALSE
    ) +
    facet_wrap(~ Facet, scales = "free_y", ncol = ncol) +
    scale_color_manual(
      values = scenario_colors_reference,
      breaks = c("Baseline", "Mitigation", "ammonia"),
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(base_year, 2050),
      limits = c(base_year - 2, 2052),
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.18))) +
    labs(
      title = paste0(model_name, ": ", title_text),
      x = "Year",
      y = expression(Population-weighted~PM[2.5]~(mu*g~m^{-3})),
      color = "Scenario"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_line(color = "grey93", linewidth = 0.25),
      panel.grid.major = element_line(color = "grey87", linewidth = 0.35),
      panel.spacing = grid::unit(0.9, "lines"),
      plot.title = element_text(size = 17, face = "bold", hjust = 0),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 8.5, color = "black"),
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 10.5),
      legend.text = element_text(size = 10),
      plot.margin = margin(8, 10, 8, 10)
    )

  ggsave(
    file.path(output_dir, paste0(file_stem, ".png")),
    plot = figure,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = 400,
    bg = "white"
  )
  ggsave(
    file.path(output_dir, paste0(file_stem, ".pdf")),
    plot = figure,
    width = width_mm,
    height = height_mm,
    units = "mm",
    device = "pdf"
  )

  figure
}

pm25_gains_country_figure <- plot_pm25_focused_facets(
  pm25_country_plot_data,
  "GAINS",
  "Country",
  "Country-level population-weighted PM2.5 concentration",
  "GAINS_PM25_country_totals",
  ncol = 4,
  width_mm = 330,
  height_mm = 180
)

pm25_geos_country_figure <- plot_pm25_focused_facets(
  pm25_country_plot_data,
  "AIM-GEOS",
  "Country",
  "Country-level population-weighted PM2.5 concentration",
  "AIM_GEOS_PM25_country_totals",
  ncol = 4,
  width_mm = 330,
  height_mm = 180
)

pm25_gains_selected_figure <- plot_pm25_focused_facets(
  pm25_selected_plot_data,
  "GAINS",
  "ReceptorCode",
  "PM2.5 concentration in selected receptor regions",
  "GAINS_PM25_selected_regions",
  ncol = 2,
  width_mm = 250,
  height_mm = 190
)

pm25_geos_selected_figure <- plot_pm25_focused_facets(
  pm25_selected_plot_data,
  "AIM-GEOS",
  "ReceptorCode",
  "PM2.5 concentration in selected receptor regions",
  "AIM_GEOS_PM25_selected_regions",
  ncol = 2,
  width_mm = 250,
  height_mm = 190
)


# -----------------------------------------------------------------------------
# One combined figure: country totals and selected receptor regions
# -----------------------------------------------------------------------------

pm25_selected_location_lookup <- c(
  CHIN_BEIJ = "China: Beijing",
  CHIN_SHAN = "China: Shanghai",
  CHIN_GUAD = "China: Guangdong",
  JAPA_KANT = "Japan: Kanto",
  KORS_SOUT = "South Korea: Seoul-Incheon"
)

pm25_combined_country_data <- pm25_country_plot_data %>%
  transmute(
    Section = "Country total",
    Location = as.character(Country),
    Model,
    Scenario,
    Period,
    PM25
  )

pm25_combined_selected_data <- pm25_selected_plot_data %>%
  transmute(
    Section = "Selected receptor region",
    Location = unname(
      pm25_selected_location_lookup[as.character(ReceptorCode)]
    ),
    Model,
    Scenario,
    Period,
    PM25
  )

pm25_combined_location_levels <- c(
  paste0("Country total | ", pm25_country_levels),
  paste0(
    "Selected region | ",
    unname(pm25_selected_location_lookup)
  )
)

pm25_combined_comparison_data <- bind_rows(
  pm25_combined_country_data,
  pm25_combined_selected_data
) %>%
  mutate(
    Panel = if_else(
      Section == "Country total",
      paste0("Country total | ", Location),
      paste0("Selected region | ", Location)
    ),
    Panel = factor(Panel, levels = pm25_combined_location_levels),
    Year = case_when(
      Period == "2050" ~ 2050,
      Model == "GAINS" ~ 2020,
      TRUE ~ 2015
    ),
    Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    )
  ) %>%
  filter(!is.na(Panel), !is.na(PM25))

write.csv(
  pm25_combined_comparison_data %>%
    arrange(Panel, Model, Scenario, Year),
  file.path(
    output_dir,
    "pm25_country_and_selected_regions_model_comparison.csv"
  ),
  row.names = FALSE
)

pm25_combined_label_data <- pm25_combined_comparison_data %>%
  filter(Year == 2050) %>%
  mutate(
    ValueLabel = paste0(
      if_else(Model == "AIM-GEOS", "A: ", "G: "),
      sprintf("%.1f", PM25)
    )
  )

pm25_combined_scenario_colors <- c(
  Baseline = "#F8766D",
  Mitigation = "#00BA38",
  ammonia = "#619CFF"
)
pm25_combined_model_linetypes <- c(
  "AIM-GEOS" = "solid",
  "GAINS" = "22"
)
pm25_combined_model_shapes <- c(
  "AIM-GEOS" = 16,
  "GAINS" = 17
)

pm25_country_selected_comparison_figure <- ggplot(
  pm25_combined_comparison_data,
  aes(
    x = Year,
    y = PM25,
    color = Scenario,
    linetype = Model,
    shape = Model,
    group = interaction(Model, Scenario)
  )
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.7) +
  ggrepel::geom_text_repel(
    data = pm25_combined_label_data,
    aes(label = ValueLabel),
    size = 3.15,
    direction = "y",
    box.padding = 0.2,
    point.padding = 0.12,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 1204,
    show.legend = FALSE
  ) +
  facet_wrap(~ Panel, scales = "free_y", ncol = 4) +
  scale_color_manual(
    values = pm25_combined_scenario_colors,
    breaks = c("Baseline", "Mitigation", "ammonia"),
    drop = FALSE
  ) +
  scale_linetype_manual(values = pm25_combined_model_linetypes) +
  scale_shape_manual(values = pm25_combined_model_shapes) +
  scale_x_continuous(
    breaks = c(2015, 2020, 2050),
    limits = c(2013, 2053),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.13, 0.2))) +
  labs(
    title = "Population-weighted PM2.5 concentration: model comparison",
    subtitle = paste(
      "Country totals and selected receptor regions;",
      "A = AIM-GEOS, G = GAINS in value labels"
    ),
    x = "Year",
    y = expression(Population-weighted~PM[2.5]~(mu*g~m^{-3})),
    color = "Scenario",
    linetype = "Model",
    shape = "Model"
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 2)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.22),
    panel.grid.major = element_line(color = "grey87", linewidth = 0.32),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.title = element_text(size = 21, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12.5, hjust = 0),
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 10, color = "black"),
    strip.text = element_text(size = 11.5, face = "bold"),
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11.5),
    plot.margin = margin(8, 10, 8, 10)
  )

ggsave(
  file.path(
    output_dir,
    "PM25_country_and_selected_regions_model_comparison.png"
  ),
  plot = pm25_country_selected_comparison_figure,
  width = 420,
  height = 285,
  units = "mm",
  dpi = 400,
  bg = "white"
)

ggsave(
  file.path(
    output_dir,
    "PM25_country_and_selected_regions_model_comparison.pdf"
  ),
  plot = pm25_country_selected_comparison_figure,
  width = 420,
  height = 285,
  units = "mm",
  device = "pdf"
)

# A clearly named alias pairs this concentration figure with the premature-
# mortality model-comparison figure used in the paper.
ggsave(
  file.path(
    output_dir,
    "PM25_concentration_country_and_selected_model_comparison.png"
  ),
  plot = pm25_country_selected_comparison_figure,
  width = 420,
  height = 285,
  units = "mm",
  dpi = 400,
  bg = "white"
)
ggsave(
  file.path(
    output_dir,
    "PM25_concentration_country_and_selected_model_comparison.pdf"
  ),
  plot = pm25_country_selected_comparison_figure,
  width = 420,
  height = 285,
  units = "mm",
  device = "pdf"
)


# -----------------------------------------------------------------------------
# PM2.5-attributable premature mortality: AIM-GEOS and GAINS comparison
# -----------------------------------------------------------------------------

# AIM-GEOS deaths come from SpatialHealth IER Mortality, not test_results.csv.
source("health/prog/read_ier_mortality.R")
mortality_aim_geos <- read_ier_mortality() %>%
  filter(ReceptorCode %in% pm25_common_receptor_codes)
# Health figure selection: five countries and five receptor regions.
mortality_country_levels <- c("China", "Japan", "South Korea", "North Korea", "Taiwan")
mortality_selected_location_lookup <- c(
  CHIN_BEIJ = "China: Beijing", CHIN_SHAN = "China: Shanghai",
  CHIN_GUAD = "China: Guangdong", JAPA_KANT = "Japan: Kanto",
  KORS_SOUT = "South Korea: Seoul-Incheon"
)
mortality_selected_codes <- names(mortality_selected_location_lookup)
mortality_combined_location_levels <- c(
  paste0("Country total | ", mortality_country_levels),
  paste0("Selected region | ", unname(mortality_selected_location_lookup))
)
read_gains_mortality <- function(path, scenario, period) {
  gains_data <- read.csv(
    path,
    skip = 8,
    check.names = TRUE,
    stringsAsFactors = FALSE
  )
  mortality_column <- grep(
    "^Premature.deaths", names(gains_data), value = TRUE
  )[1]
  if (is.na(mortality_column)) {
    stop("GAINS premature-deaths column not found in: ", path)
  }

  tibble(
    Receptor = gains_data[[1]],
    Deaths = as.numeric(gains_data[[mortality_column]]),
    Model = "GAINS",
    Scenario = scenario,
    Period = period
  )
}

mortality_gains <- bind_rows(
  read_gains_mortality(
    pm25_files[["gains_base"]], "Baseline", "Base year"
  ),
  read_gains_mortality(
    pm25_files[["gains_baseline_2050"]], "Baseline", "2050"
  ),
  read_gains_mortality(
    pm25_files[["gains_mitigation_2050"]], "Mitigation", "2050"
  ),
  read_gains_mortality(
    pm25_files[["gains_ammonia_2050"]], "ammonia", "2050"
  )
) %>%
  left_join(
    select(pm25_all_receptor_lookup, ReceptorCode, Receptor),
    by = "Receptor"
  ) %>%
  filter(ReceptorCode %in% pm25_common_receptor_codes) %>%
  select(-Receptor)

# The GAINS base-year file contains one common baseline. Replicate that point so
# mitigation and ammonia trajectories begin at the same observed/modelled year.
mortality_gains_base_copies <- mortality_gains %>%
  filter(Scenario == "Baseline", Period == "Base year") %>%
  select(-Scenario) %>%
  tidyr::crossing(Scenario = c("Mitigation", "ammonia"))

mortality_regional_data <- bind_rows(
  mortality_aim_geos,
  mortality_gains,
  mortality_gains_base_copies
) %>%
  mutate(
    Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    ),
    Period = factor(Period, levels = c("Base year", "2050"))
  ) %>%
  filter(!is.na(Deaths))

# Death counts are additive, unlike concentration. National totals are the sum
# of all receptor-region deaths belonging to that country or territory.
mortality_country_data <- mortality_regional_data %>%
  left_join(pm25_country_lookup, by = "ReceptorCode") %>%
  filter(!is.na(Country), Country %in% mortality_country_levels) %>%
  group_by(Country, Model, Scenario, Period) %>%
  summarise(
    Deaths = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = factor(Country, levels = mortality_country_levels))

mortality_selected_data <- mortality_regional_data %>%
  filter(ReceptorCode %in% mortality_selected_codes) %>%
  mutate(
    ReceptorCode = factor(
      ReceptorCode,
      levels = mortality_selected_codes
    )
  )

write.csv(
  mortality_country_data %>%
    arrange(Country, Model, Scenario, Period),
  file.path(output_dir, "premature_mortality_country_totals.csv"),
  row.names = FALSE
)
write.csv(
  mortality_selected_data %>%
    arrange(ReceptorCode, Model, Scenario, Period),
  file.path(output_dir, "premature_mortality_selected_regions.csv"),
  row.names = FALSE
)

mortality_combined_country_data <- mortality_country_data %>%
  transmute(
    Section = "Country total",
    Location = as.character(Country),
    Model,
    Scenario,
    Period,
    Deaths
  )

mortality_combined_selected_data <- mortality_selected_data %>%
  transmute(
    Section = "Selected receptor region",
    Location = unname(
      mortality_selected_location_lookup[as.character(ReceptorCode)]
    ),
    Model,
    Scenario,
    Period,
    Deaths
  )

mortality_combined_comparison_data <- bind_rows(
  mortality_combined_country_data,
  mortality_combined_selected_data
) %>%
  mutate(
    Panel = if_else(
      Section == "Country total",
      paste0("Country total | ", Location),
      paste0("Selected region | ", Location)
    ),
    Panel = factor(Panel, levels = mortality_combined_location_levels),
    Year = case_when(
      Period == "2050" ~ 2050,
      Model == "GAINS" ~ 2020,
      TRUE ~ 2015
    ),
    Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    ),
    MortalityThousand = Deaths / 1000
  ) %>%
  filter(!is.na(Panel), !is.na(MortalityThousand))

write.csv(
  mortality_combined_comparison_data %>%
    arrange(Panel, Model, Scenario, Year),
  file.path(
    output_dir,
    "premature_mortality_country_and_selected_model_comparison.csv"
  ),
  row.names = FALSE
)

mortality_combined_label_data <- mortality_combined_comparison_data %>%
  filter(Year == 2050) %>%
  mutate(
    ValueLabel = paste0(
      if_else(Model == "AIM-GEOS", "A: ", "G: "),
      sprintf("%.1f", MortalityThousand)
    )
  )

mortality_country_selected_comparison_figure <- ggplot(
  mortality_combined_comparison_data,
  aes(
    x = Year,
    y = MortalityThousand,
    color = Scenario,
    linetype = Model,
    shape = Model,
    group = interaction(Model, Scenario)
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.0) +
  ggrepel::geom_text_repel(
    data = mortality_combined_label_data,
    aes(label = ValueLabel),
    size = 2.5,
    direction = "y",
    box.padding = 0.2,
    point.padding = 0.12,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 1204,
    show.legend = FALSE
  ) +
  facet_wrap(~ Panel, scales = "free_y", ncol = 4) +
  scale_color_manual(
    values = pm25_combined_scenario_colors,
    breaks = c("Baseline", "Mitigation", "ammonia"),
    drop = FALSE
  ) +
  scale_linetype_manual(values = pm25_combined_model_linetypes) +
  scale_shape_manual(values = pm25_combined_model_shapes) +
  scale_x_continuous(
    breaks = c(2015, 2020, 2050),
    limits = c(2013, 2053),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.13, 0.2))) +
  labs(
    title = "PM2.5-attributable premature mortality: model comparison",
    subtitle = paste(
      "Country totals and selected receptor regions;",
      "A = AIM-GEOS, G = GAINS in value labels"
    ),
    x = "Year",
    y = expression(Premature~deaths~(thousand~persons~yr^{-1})),
    color = "Scenario",
    linetype = "Model",
    shape = "Model"
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 2)
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.22),
    panel.grid.major = element_line(color = "grey87", linewidth = 0.32),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.title = element_text(size = 17, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 7.5, color = "black"),
    strip.text = element_text(size = 9.2, face = "bold"),
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(size = 9.5),
    legend.text = element_text(size = 9),
    plot.margin = margin(8, 10, 8, 10)
  )

ggsave(
  file.path(
    output_dir,
    "premature_mortality_country_and_selected_model_comparison.png"
  ),
  plot = mortality_country_selected_comparison_figure,
  width = 420,
  height = 285,
  units = "mm",
  dpi = 400,
  bg = "white"
)

ggsave(
  file.path(
    output_dir,
    "premature_mortality_country_and_selected_model_comparison.pdf"
  ),
  plot = mortality_country_selected_comparison_figure,
  width = 420,
  height = 285,
  units = "mm",
  device = "pdf"
)


# -----------------------------------------------------------------------------
# Change from each model's base year: concentration and premature mortality
# -----------------------------------------------------------------------------

make_regional_percent_change_figure <- function(
    data,
    value_column,
    figure_title,
    file_stem) {
  location_levels <- unique(as.character(data$Location))

  change_data <- data %>%
    transmute(
      Section,
      Location,
      Model = as.character(Model),
      Scenario = as.character(Scenario),
      PeriodKey = if_else(Period == "2050", "Value2050", "BaseValue"),
      Value = .data[[value_column]]
    ) %>%
    group_by(Section, Location, Model, Scenario, PeriodKey) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = PeriodKey, values_from = Value) %>%
    mutate(
      ChangePercent = 100 * (Value2050 / BaseValue - 1),
      Section = factor(
        Section,
        levels = c("Country total", "Selected receptor region")
      ),
      Location = factor(Location, levels = rev(location_levels)),
      Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
      Scenario = factor(
        Scenario,
        levels = c("Baseline", "Mitigation", "ammonia")
      ),
      BaseYear = if_else(Model == "AIM-GEOS", 2015L, 2020L)
    ) %>%
    filter(
      is.finite(BaseValue),
      BaseValue > 0,
      is.finite(Value2050),
      is.finite(ChangePercent)
    )

  write.csv(
    change_data %>%
      arrange(Section, Location, Model, Scenario) %>%
      mutate(Location = as.character(Location)),
    file.path(output_dir, paste0(file_stem, ".csv")),
    row.names = FALSE
  )

  figure <- ggplot(
    change_data,
    aes(
      x = ChangePercent,
      y = Location,
      colour = Scenario,
      shape = Scenario
    )
  ) +
    geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.55) +
    geom_segment(
      aes(x = 0, xend = ChangePercent, yend = Location),
      linewidth = 0.7,
      alpha = 0.45
    ) +
    geom_point(size = 3.2) +
    facet_grid(
      rows = vars(Section),
      cols = vars(Model),
      scales = "free_y",
      space = "free_y"
    ) +
    scale_colour_manual(
      values = pm25_combined_scenario_colors,
      breaks = c("Baseline", "Mitigation", "ammonia"),
      drop = FALSE
    ) +
    scale_shape_manual(
      values = c(Baseline = 16, Mitigation = 17, ammonia = 15),
      breaks = c("Baseline", "Mitigation", "ammonia"),
      drop = FALSE
    ) +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    labs(
      title = figure_title,
      subtitle = paste(
        "Change in 2050 relative to each model's base year;",
        "AIM-GEOS: 2015, GAINS: 2020"
      ),
      x = "Change from base year (%)",
      y = NULL,
      colour = "Scenario",
      shape = "Scenario",
      caption = "Negative values indicate reductions from the model-specific base year."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.major.y = element_line(colour = "grey91", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.spacing = grid::unit(1.1, "lines"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11.5, hjust = 0),
      axis.title.x = element_text(size = 12.5),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.text.y = element_text(size = 10.5, colour = "black"),
      strip.background = element_rect(fill = "grey94", colour = "grey35"),
      strip.text = element_text(size = 11.5, face = "bold"),
      legend.position = "top",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10.5),
      plot.caption = element_text(size = 9, hjust = 0),
      plot.margin = margin(8, 12, 8, 8)
    )

  ggsave(
    file.path(output_dir, paste0(file_stem, ".png")),
    plot = figure,
    width = 300,
    height = 220,
    units = "mm",
    dpi = 400,
    bg = "white"
  )
  ggsave(
    file.path(output_dir, paste0(file_stem, ".pdf")),
    plot = figure,
    width = 300,
    height = 220,
    units = "mm",
    device = "pdf"
  )

  list(data = change_data, figure = figure)
}

pm25_regional_percent_change <- make_regional_percent_change_figure(
  data = pm25_combined_comparison_data,
  value_column = "PM25",
  figure_title = "Change in population-weighted PM2.5 concentration",
  file_stem = "PM25_concentration_change_from_base_year_by_region"
)

mortality_regional_percent_change <- make_regional_percent_change_figure(
  data = mortality_combined_comparison_data,
  value_column = "MortalityThousand",
  figure_title = "Change in PM2.5-attributable premature mortality",
  file_stem = "premature_mortality_change_from_base_year_by_region"
)


# -----------------------------------------------------------------------------
# Grid-based GEOS-Chem PM2.5 maps over East Asia
# -----------------------------------------------------------------------------

# Show the native 0.5-degree model cells without spatial interpolation. This
# initial window is deliberately broad; after reading, the figure is tightened
# to the actual non-missing GEOS-Chem East Asia domain.
pm25_map_extent <- c(
  xmin = 85,
  xmax = 150,
  ymin = 15,
  ymax = 60
)

read_geos_pm25_grid <- function(path, scenario) {
  pm25_raster <- raster::raster(path, varname = "PM25")
  pm25_raster <- raster::crop(
    pm25_raster,
    raster::extent(
      pm25_map_extent[["xmin"]],
      pm25_map_extent[["xmax"]],
      pm25_map_extent[["ymin"]],
      pm25_map_extent[["ymax"]]
    )
  )

  raster::as.data.frame(pm25_raster, xy = TRUE, na.rm = TRUE) %>%
    setNames(c("Longitude", "Latitude", "PM25")) %>%
    mutate(Scenario = scenario)
}

pm25_geos_grid_2050 <- bind_rows(
  read_geos_pm25_grid(
    pm25_files[["geos_baseline_2050"]],
    "Baseline"
  ),
  read_geos_pm25_grid(
    pm25_files[["geos_mitigation_2050"]],
    "Mitigation"
  ),
  read_geos_pm25_grid(
    pm25_files[["geos_ammonia_2050"]],
    "ammonia"
  )
) %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Mitigation", "ammonia")
    )
  )

# The NetCDF files use a global coordinate skeleton but contain results only in
# the nested East Asia domain. Derive the plotted extent from valid cells so the
# paper figure does not contain a large empty western/northern area.
pm25_map_extent <- c(
  xmin = floor(min(pm25_geos_grid_2050$Longitude, na.rm = TRUE) - 0.25),
  xmax = ceiling(max(pm25_geos_grid_2050$Longitude, na.rm = TRUE) + 0.25),
  ymin = floor(min(pm25_geos_grid_2050$Latitude, na.rm = TRUE) - 0.25),
  ymax = ceiling(max(pm25_geos_grid_2050$Latitude, na.rm = TRUE) + 0.25)
)
pm25_longitude_breaks <- pretty(
  unname(pm25_map_extent[c("xmin", "xmax")]),
  n = 5
)
pm25_latitude_breaks <- pretty(
  unname(pm25_map_extent[c("ymin", "ymax")]),
  n = 5
)

# Three complementary differences isolate general mitigation and the additional
# effect of the ammonia pathway. Positive values mean a higher PM2.5 level in
# the first scenario named in the panel label.
pm25_geos_grid_differences <- pm25_geos_grid_2050 %>%
  mutate(Scenario = as.character(Scenario)) %>%
  tidyr::pivot_wider(
    names_from = Scenario,
    values_from = PM25
  ) %>%
  transmute(
    Longitude,
    Latitude,
    `Mitigation - Baseline` = Mitigation - Baseline,
    `ammonia - Baseline` = ammonia - Baseline,
    `ammonia - Mitigation` = ammonia - Mitigation
  ) %>%
  tidyr::pivot_longer(
    cols = -c(Longitude, Latitude),
    names_to = "Comparison",
    values_to = "PM25_difference"
  ) %>%
  mutate(
    Comparison = factor(
      Comparison,
      levels = c(
        "Mitigation - Baseline",
        "ammonia - Baseline",
        "ammonia - Mitigation"
      )
    )
  )

# The two changes relative to Baseline form one directly comparable figure.
# The ammonia-versus-Mitigation comparison is shown separately below using its
# own expanded, explicitly labelled scale.
pm25_geos_vs_baseline_differences <- pm25_geos_grid_differences %>%
  filter(
    Comparison %in% c(
      "Mitigation - Baseline",
      "ammonia - Baseline"
    )
  ) %>%
  droplevels()

# Use the existing GAINS polygons only as geographic reference lines. They do
# not alter or aggregate the GEOS-Chem grid values.
# Repair occasional self-intersections in the full historical GAINS layer
# before cropping. Cropping an invalid polygon first can itself fail.
pm25_previous_s2_state <- sf::sf_use_s2(FALSE)
pm25_map_boundaries <- pm25_regions_sf %>%
  sf::st_transform(3857) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)
pm25_map_boundaries <- suppressWarnings(
  sf::st_crop(
    pm25_map_boundaries,
    xmin = pm25_map_extent[["xmin"]],
    xmax = pm25_map_extent[["xmax"]],
    ymin = pm25_map_extent[["ymin"]],
    ymax = pm25_map_extent[["ymax"]]
  )
)
invisible(sf::sf_use_s2(pm25_previous_s2_state))

pm25_absolute_limit <- ceiling(
  max(pm25_geos_grid_2050$PM25, na.rm = TRUE) / 5
) * 5
pm25_difference_limit <- ceiling(
  max(
    abs(pm25_geos_vs_baseline_differences$PM25_difference),
    na.rm = TRUE
  )
)

pm25_grid_map_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "grey96", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey82", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = "grey35"),
    strip.text = element_text(size = 13.5, face = "bold", color = "black"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10.5, color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.margin = margin(6, 8, 6, 8)
  )

pm25_geos_absolute_grid_map <- ggplot() +
  geom_raster(
    data = pm25_geos_grid_2050,
    aes(x = Longitude, y = Latitude, fill = PM25)
  ) +
  geom_sf(
    data = pm25_map_boundaries,
    fill = NA,
    color = "grey25",
    linewidth = 0.18,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Scenario, nrow = 1) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(0, pm25_absolute_limit),
    oob = scales::squish,
    name = expression(PM[2.5]~(mu*g~m^{-3}))
  ) +
  coord_sf(
    xlim = unname(pm25_map_extent[c("xmin", "xmax")]),
    ylim = unname(pm25_map_extent[c("ymin", "ymax")]),
    expand = FALSE,
    datum = sf::st_crs(4326)
  ) +
  scale_x_continuous(breaks = pm25_longitude_breaks) +
  scale_y_continuous(breaks = pm25_latitude_breaks) +
  labs(
    title = "GEOS-Chem PM2.5 concentration over East Asia",
    subtitle = "2050 annual mean; native 0.5-degree grid and a common color scale",
    x = "Longitude",
    y = "Latitude"
  ) +
  pm25_grid_map_theme

pm25_geos_difference_grid_map <- ggplot() +
  geom_raster(
    data = pm25_geos_vs_baseline_differences,
    aes(x = Longitude, y = Latitude, fill = PM25_difference)
  ) +
  geom_sf(
    data = pm25_map_boundaries,
    fill = NA,
    color = "grey25",
    linewidth = 0.18,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Comparison, nrow = 1) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#E8DDB5",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-pm25_difference_limit, pm25_difference_limit),
    oob = scales::squish,
    name = expression(Delta*PM[2.5]~(mu*g~m^{-3}))
  ) +
  coord_sf(
    xlim = unname(pm25_map_extent[c("xmin", "xmax")]),
    ylim = unname(pm25_map_extent[c("ymin", "ymax")]),
    expand = FALSE,
    datum = sf::st_crs(4326)
  ) +
  scale_x_continuous(breaks = pm25_longitude_breaks) +
  scale_y_continuous(breaks = pm25_latitude_breaks) +
  labs(
    title = "GEOS-Chem PM2.5 concentration differences relative to Baseline",
    subtitle = paste(
      "2050 annual mean; common symmetric color scale;",
      "blue indicates a decrease and red an increase"
    ),
    x = "Longitude",
    y = "Latitude"
  ) +
  pm25_grid_map_theme

pm25_geos_grid_map_figure <- gridExtra::arrangeGrob(
  pm25_geos_absolute_grid_map,
  pm25_geos_difference_grid_map,
  ncol = 1,
  heights = c(1, 1)
)

write.csv(
  pm25_geos_grid_2050 %>%
    arrange(Scenario, Latitude, Longitude),
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_scenarios.csv"),
  row.names = FALSE
)
write.csv(
  pm25_geos_vs_baseline_differences %>%
    arrange(Comparison, Latitude, Longitude),
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_differences.csv"),
  row.names = FALSE
)
write.csv(
  pm25_geos_vs_baseline_differences %>%
    arrange(Comparison, Latitude, Longitude),
  file.path(
    output_dir,
    "GEOS_Chem_PM25_grid_2050_vs_baseline_differences.csv"
  ),
  row.names = FALSE
)
write.csv(
  pm25_geos_grid_differences %>%
    arrange(Comparison, Latitude, Longitude),
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_all_differences.csv"),
  row.names = FALSE
)

ggsave(
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_scenarios.png"),
  plot = pm25_geos_absolute_grid_map,
  width = 330,
  height = 125,
  units = "mm",
  dpi = 400,
  bg = "white"
)
ggsave(
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_scenarios.pdf"),
  plot = pm25_geos_absolute_grid_map,
  width = 330,
  height = 125,
  units = "mm",
  device = "pdf"
)
ggsave(
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_differences.png"),
  plot = pm25_geos_difference_grid_map,
  width = 330,
  height = 125,
  units = "mm",
  dpi = 400,
  bg = "white"
)
ggsave(
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_differences.pdf"),
  plot = pm25_geos_difference_grid_map,
  width = 330,
  height = 125,
  units = "mm",
  device = "pdf"
)
ggsave(
  file.path(
    output_dir,
    "GEOS_Chem_PM25_grid_2050_vs_baseline_differences.png"
  ),
  plot = pm25_geos_difference_grid_map,
  width = 280,
  height = 130,
  units = "mm",
  dpi = 400,
  bg = "white"
)
ggsave(
  file.path(
    output_dir,
    "GEOS_Chem_PM25_grid_2050_vs_baseline_differences.pdf"
  ),
  plot = pm25_geos_difference_grid_map,
  width = 280,
  height = 130,
  units = "mm",
  device = "pdf"
)
ggsave(
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_scenarios_and_differences.png"),
  plot = pm25_geos_grid_map_figure,
  width = 330,
  height = 245,
  units = "mm",
  dpi = 400,
  bg = "white"
)
ggsave(
  file.path(output_dir, "GEOS_Chem_PM25_grid_2050_scenarios_and_differences.pdf"),
  plot = pm25_geos_grid_map_figure,
  width = 330,
  height = 245,
  units = "mm",
  device = "pdf"
)


# -----------------------------------------------------------------------------
# Main paper figure: ammonia pathway versus Mitigation
# -----------------------------------------------------------------------------

# The common difference scale above is retained for cross-comparison among all
# scenarios. This focused figure uses a clearly labelled, zero-centred expanded
# scale for the paper's primary ammonia-versus-Mitigation comparison. Limits
# include every valid model cell; no percentile clipping is applied.
pm25_ammonia_mitigation_focus <- pm25_geos_grid_2050 %>%
  mutate(Scenario = as.character(Scenario)) %>%
  tidyr::pivot_wider(
    names_from = Scenario,
    values_from = PM25
  ) %>%
  transmute(
    Longitude,
    Latitude,
    Mitigation,
    ammonia,
    Absolute_difference = ammonia - Mitigation,
    Relative_difference = 100 * (ammonia - Mitigation) / Mitigation
  ) %>%
  filter(
    is.finite(Absolute_difference),
    is.finite(Relative_difference)
  )

pm25_ammonia_absolute_limit <- ceiling(
  max(
    abs(pm25_ammonia_mitigation_focus$Absolute_difference),
    na.rm = TRUE
  ) * 10
) / 10
pm25_ammonia_relative_limit <- ceiling(
  max(
    abs(pm25_ammonia_mitigation_focus$Relative_difference),
    na.rm = TRUE
  ) * 2
) / 2

pm25_ammonia_absolute_summary <- sprintf(
  paste0(
    "Full range: %+.2f to %+.2f ",
    "\u00b5g m\u207b\u00b3; unweighted grid-cell mean: %+.2f \u00b5g m\u207b\u00b3"
  ),
  min(
    pm25_ammonia_mitigation_focus$Absolute_difference,
    na.rm = TRUE
  ),
  max(
    pm25_ammonia_mitigation_focus$Absolute_difference,
    na.rm = TRUE
  ),
  mean(
    pm25_ammonia_mitigation_focus$Absolute_difference,
    na.rm = TRUE
  )
)
pm25_ammonia_relative_summary <- sprintf(
  paste0(
    "Full range: %+.2f to %+.2f%%; ",
    "unweighted grid-cell mean: %+.2f%%"
  ),
  min(
    pm25_ammonia_mitigation_focus$Relative_difference,
    na.rm = TRUE
  ),
  max(
    pm25_ammonia_mitigation_focus$Relative_difference,
    na.rm = TRUE
  ),
  mean(
    pm25_ammonia_mitigation_focus$Relative_difference,
    na.rm = TRUE
  )
)

pm25_ammonia_absolute_map <- ggplot(
  pm25_ammonia_mitigation_focus,
  aes(x = Longitude, y = Latitude)
) +
  geom_raster(aes(fill = Absolute_difference)) +
  geom_contour(
    aes(z = Absolute_difference),
    breaks = 0,
    color = "grey10",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_sf(
    data = pm25_map_boundaries,
    fill = NA,
    color = "grey25",
    linewidth = 0.18,
    inherit.aes = FALSE
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#E8DDB5",
    high = "#B2182B",
    midpoint = 0,
    limits = c(
      -pm25_ammonia_absolute_limit,
      pm25_ammonia_absolute_limit
    ),
    breaks = seq(-1.5, 1.5, by = 0.5),
    name = expression(Delta*PM[2.5]~(mu*g~m^{-3}))
  ) +
  coord_sf(
    xlim = unname(pm25_map_extent[c("xmin", "xmax")]),
    ylim = unname(pm25_map_extent[c("ymin", "ymax")]),
    expand = FALSE,
    datum = sf::st_crs(4326)
  ) +
  scale_x_continuous(breaks = pm25_longitude_breaks) +
  scale_y_continuous(breaks = pm25_latitude_breaks) +
  labs(
    title = "Absolute difference",
    subtitle = pm25_ammonia_absolute_summary,
    x = "Longitude",
    y = "Latitude",
    caption = "ammonia - Mitigation; dashed contour indicates zero"
  ) +
  pm25_grid_map_theme +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = grid::unit(58, "mm"),
      barheight = grid::unit(4, "mm")
    )
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10.5),
    plot.caption = element_text(size = 9.5, hjust = 0),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

pm25_ammonia_relative_map <- ggplot(
  pm25_ammonia_mitigation_focus,
  aes(x = Longitude, y = Latitude)
) +
  geom_raster(aes(fill = Relative_difference)) +
  geom_contour(
    aes(z = Relative_difference),
    breaks = 0,
    color = "grey10",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_sf(
    data = pm25_map_boundaries,
    fill = NA,
    color = "grey25",
    linewidth = 0.18,
    inherit.aes = FALSE
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#E8DDB5",
    high = "#B2182B",
    midpoint = 0,
    limits = c(
      -pm25_ammonia_relative_limit,
      pm25_ammonia_relative_limit
    ),
    breaks = seq(-6, 6, by = 2),
    name = expression(Delta*PM[2.5]~("%"))
  ) +
  coord_sf(
    xlim = unname(pm25_map_extent[c("xmin", "xmax")]),
    ylim = unname(pm25_map_extent[c("ymin", "ymax")]),
    expand = FALSE,
    datum = sf::st_crs(4326)
  ) +
  scale_x_continuous(breaks = pm25_longitude_breaks) +
  scale_y_continuous(breaks = pm25_latitude_breaks) +
  labs(
    title = "Relative difference",
    subtitle = pm25_ammonia_relative_summary,
    x = "Longitude",
    y = "Latitude",
    caption = paste0(
      "100 x (ammonia - Mitigation) / Mitigation; ",
      "dashed contour indicates zero"
    )
  ) +
  pm25_grid_map_theme +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = grid::unit(58, "mm"),
      barheight = grid::unit(4, "mm")
    )
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10.5),
    plot.caption = element_text(size = 9.5, hjust = 0),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

pm25_ammonia_mitigation_focus_figure <- gridExtra::arrangeGrob(
  pm25_ammonia_absolute_map,
  pm25_ammonia_relative_map,
  ncol = 2,
  top = grid::textGrob(
    "PM2.5 concentration difference: ammonia versus Mitigation (2050)",
    x = 0,
    hjust = 0,
    gp = grid::gpar(fontface = "bold", fontsize = 19)
  )
)

write.csv(
  pm25_ammonia_mitigation_focus %>%
    arrange(Latitude, Longitude),
  file.path(
    output_dir,
    "GEOS_Chem_PM25_grid_2050_ammonia_minus_mitigation.csv"
  ),
  row.names = FALSE
)

ggsave(
  file.path(
    output_dir,
    "GEOS_Chem_PM25_grid_2050_ammonia_vs_mitigation_focus.png"
  ),
  plot = pm25_ammonia_mitigation_focus_figure,
  width = 330,
  height = 155,
  units = "mm",
  dpi = 400,
  bg = "white"
)
ggsave(
  file.path(
    output_dir,
    "GEOS_Chem_PM25_grid_2050_ammonia_vs_mitigation_focus.pdf"
  ),
  plot = pm25_ammonia_mitigation_focus_figure,
  width = 330,
  height = 155,
  units = "mm",
  device = "pdf"
)

# -----------------------------------------------------------------------------
# PM2.5 percentage change relative to the same-year Baseline scenario
# -----------------------------------------------------------------------------
# This section can also be run on its own from the project root after the
# combined comparison CSV above has been generated.
# Change (%) = 100 * (scenario - Baseline) / Baseline, within each model,
# location and year. This is NOT change from the historical base year.
if (dir.exists(".r-library")) {
  .libPaths(c(normalizePath(".r-library"), .libPaths()))
}
library(dplyr)
library(ggplot2)

output_dir <- file.path("output", "paper")
pm25_baseline_source <- read.csv(
  file.path(output_dir, "pm25_country_and_selected_regions_model_comparison.csv"),
  stringsAsFactors = FALSE
)
pm25_baseline_panel_levels <- unique(pm25_baseline_source$Panel)
pm25_baseline_reference <- pm25_baseline_source %>%
  filter(Scenario == "Baseline") %>%
  select(Panel, Model, Year, BaselinePM25 = PM25)

if (anyDuplicated(pm25_baseline_reference[c("Panel", "Model", "Year")])) {
  stop("Duplicate Baseline concentrations for a model, location and year.")
}
pm25_baseline_change_data <- pm25_baseline_source %>%
  left_join(pm25_baseline_reference, by = c("Panel", "Model", "Year"))
if (any(!is.finite(pm25_baseline_change_data$BaselinePM25) |
        pm25_baseline_change_data$BaselinePM25 <= 0)) {
  stop("Baseline concentrations must be available, finite and positive.")
}
pm25_baseline_change_data <- pm25_baseline_change_data %>%
  mutate(
    ChangePercent = 100 * (PM25 - BaselinePM25) / BaselinePM25,
    Panel = factor(Panel, levels = pm25_baseline_panel_levels),
    Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
    Scenario = factor(Scenario, levels = c("Baseline", "Mitigation", "ammonia"))
  )
pm25_baseline_change_labels <- pm25_baseline_change_data %>%
  filter(Year == 2050, Scenario != "Baseline") %>%
  mutate(ValueLabel = paste0(
    if_else(Model == "AIM-GEOS", "A: ", "G: "),
    sprintf("%+.1f%%", ChangePercent)
  ))

pm25_baseline_change_figure <- ggplot(
  pm25_baseline_change_data %>% filter(Scenario != "Baseline"),
  aes(x = Year, y = ChangePercent, color = Scenario,
      linetype = Model, shape = Model,
      group = interaction(Model, Scenario))
) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.7) +
  ggrepel::geom_text_repel(
    data = pm25_baseline_change_labels,
    aes(label = ValueLabel),
    size = 3.15, direction = "y", box.padding = 0.25,
    point.padding = 0.12, min.segment.length = 0,
    max.overlaps = Inf, seed = 1204, show.legend = FALSE
  ) +
  facet_wrap(~ Panel, ncol = 4) +
  scale_color_manual(values = c(
    Mitigation = "#00BA38", ammonia = "#619CFF"
  ), breaks = c("Mitigation", "ammonia")) +
  scale_linetype_manual(values = c("AIM-GEOS" = "solid", "GAINS" = "22")) +
  scale_shape_manual(values = c("AIM-GEOS" = 16, "GAINS" = 17)) +
  scale_x_continuous(
    breaks = c(2015, 2020, 2050), limits = c(2013, 2053),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.16, 0.12))
  ) +
  labs(
    title = "Population-weighted PM2.5: percentage change relative to the Baseline scenario",
    subtitle = paste(
      "Country totals and selected receptor regions;",
      "each scenario is compared with the same model's Baseline in the same year."
    ),
    caption = paste(
      "Percentage change = 100 x (scenario concentration - Baseline concentration) / Baseline concentration.",
      "Negative values indicate lower PM2.5 concentrations than Baseline; endpoint labels: A = AIM-GEOS, G = GAINS.",
      sep = "\n"
    ),
    x = "Year", y = "PM2.5 concentration change relative to Baseline (%)",
    color = "Scenario", linetype = "Model", shape = "Model"
  ) +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2), shape = guide_legend(order = 2)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.22),
    panel.grid.major = element_line(color = "grey87", linewidth = 0.32),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.title = element_text(size = 21, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12.5, hjust = 0),
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 10, color = "black"),
    strip.text = element_text(size = 11.5, face = "bold"),
    legend.position = "top", legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11.5),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(8, 10, 8, 10)
  )

write.csv(
  pm25_baseline_change_data %>% arrange(Panel, Model, Scenario, Year),
  file.path(output_dir, "PM25_concentration_change_from_baseline_scenario.csv"),
  row.names = FALSE
)
ggsave(
  file.path(output_dir, "PM25_concentration_change_from_baseline_scenario.png"),
  plot = pm25_baseline_change_figure,
  width = 420, height = 285, units = "mm", dpi = 400, bg = "white"
)
ggsave(
  file.path(output_dir, "PM25_concentration_change_from_baseline_scenario.pdf"),
  plot = pm25_baseline_change_figure,
  width = 420, height = 285, units = "mm", device = "pdf"
)





# -----------------------------------------------------------------------------
# Premature deaths percentage change relative to the same-year Baseline scenario
# -----------------------------------------------------------------------------
# This section can also be run on its own from the project root after the
# combined comparison CSV above has been generated.
# Change (%) = 100 * (scenario - Baseline) / Baseline, within each model,
# location and year. This is NOT change from the historical base year.
if (dir.exists(".r-library")) {
  .libPaths(c(normalizePath(".r-library"), .libPaths()))
}
library(dplyr)
library(ggplot2)

output_dir <- file.path("output", "paper")
mortality_baseline_source <- read.csv(
  file.path(output_dir, "premature_mortality_country_and_selected_model_comparison.csv"),
  stringsAsFactors = FALSE
)
mortality_baseline_panel_levels <- unique(mortality_baseline_source$Panel)
mortality_baseline_reference <- mortality_baseline_source %>%
  filter(Scenario == "Baseline") %>%
  select(Panel, Model, Year, BaselineDeaths = Deaths)

if (anyDuplicated(mortality_baseline_reference[c("Panel", "Model", "Year")])) {
  stop("Duplicate Baseline death counts for a model, location and year.")
}
mortality_baseline_change_data <- mortality_baseline_source %>%
  left_join(mortality_baseline_reference, by = c("Panel", "Model", "Year"))
if (any(!is.finite(mortality_baseline_change_data$BaselineDeaths) |
        mortality_baseline_change_data$BaselineDeaths <= 0)) {
  stop("Baseline death counts must be available, finite and positive.")
}
mortality_baseline_change_data <- mortality_baseline_change_data %>%
  mutate(
    ChangePercent = 100 * (Deaths - BaselineDeaths) / BaselineDeaths,
    Panel = factor(Panel, levels = mortality_baseline_panel_levels),
    Model = factor(Model, levels = c("AIM-GEOS", "GAINS")),
    Scenario = factor(Scenario, levels = c("Baseline", "Mitigation", "ammonia"))
  )
mortality_baseline_change_labels <- mortality_baseline_change_data %>%
  filter(Year == 2050, Scenario != "Baseline") %>%
  mutate(ValueLabel = paste0(
    if_else(Model == "AIM-GEOS", "A: ", "G: "),
    sprintf("%+.2f%%", ChangePercent)
  ))

mortality_baseline_change_figure <- ggplot(
  mortality_baseline_change_data %>% filter(Scenario != "Baseline"),
  aes(x = Year, y = ChangePercent, color = Scenario,
      linetype = Model, shape = Model,
      group = interaction(Model, Scenario))
) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.7) +
  ggrepel::geom_text_repel(
    data = mortality_baseline_change_labels,
    aes(label = ValueLabel),
    size = 3.15, direction = "y", box.padding = 0.5,
    point.padding = 0.12, min.segment.length = 0,
    max.overlaps = Inf, max.iter = 100000, max.time = 2, force = 5, seed = 1204, show.legend = FALSE
  ) +
  facet_wrap(~ Panel, ncol = 4) +
  scale_color_manual(values = c(
    Mitigation = "#00BA38", ammonia = "#619CFF"
  ), breaks = c("Mitigation", "ammonia")) +
  scale_linetype_manual(values = c("AIM-GEOS" = "solid", "GAINS" = "22")) +
  scale_shape_manual(values = c("AIM-GEOS" = 16, "GAINS" = 17)) +
  scale_x_continuous(
    breaks = c(2015, 2020, 2050), limits = c(2013, 2053),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.16, 0.12))
  ) +
  labs(
    title = "PM2.5-attributable premature deaths: percentage change relative to Baseline",
    subtitle = paste(
      "Country totals and selected receptor regions;",
      "each scenario is compared with the same model's Baseline in the same year."
    ),
    caption = paste(
      "Percentage change = 100 x (scenario deaths - Baseline deaths) / Baseline deaths.",
      "Negative values indicate fewer premature deaths than Baseline; endpoint labels: A = AIM-GEOS, G = GAINS.",
      sep = "\n"
    ),
    x = "Year", y = "Premature deaths change relative to Baseline (%)",
    color = "Scenario", linetype = "Model", shape = "Model"
  ) +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2), shape = guide_legend(order = 2)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.22),
    panel.grid.major = element_line(color = "grey87", linewidth = 0.32),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.title = element_text(size = 21, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12.5, hjust = 0),
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 10, color = "black"),
    strip.text = element_text(size = 11.5, face = "bold"),
    legend.position = "top", legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11.5),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(8, 10, 8, 10)
  )

write.csv(
  mortality_baseline_change_data %>% arrange(Panel, Model, Scenario, Year),
  file.path(output_dir, "premature_mortality_change_from_baseline_scenario.csv"),
  row.names = FALSE
)
ggsave(
  file.path(output_dir, "premature_mortality_change_from_baseline_scenario.png"),
  plot = mortality_baseline_change_figure,
  width = 420, height = 285, units = "mm", dpi = 400, bg = "white"
)
ggsave(
  file.path(output_dir, "premature_mortality_change_from_baseline_scenario.pdf"),
  plot = mortality_baseline_change_figure,
  width = 420, height = 285, units = "mm", device = "pdf"
)
