library(tidyverse)
directory_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"
file_name <- "Way3 2018.csv"
file_path <- file.path(directory_path, file_name)

# Read the CSV file
way3_2018_data <- read.csv(file_path)
View(way3_2018_data)

# Load dplyr package
library(dplyr)
# Load dplyr package
library(dplyr)

# Assuming way3_2018_data is your dataframe

# Filter columns to include only the ones you want to rename
way3_2018_data_filtered <- way3_2018_data %>%
  select(
    `x_70_`,
    ch4_mole_fraction,
    ch4_mixing_ratio,
    co2_mole_fraction,
    co2_mixing_ratio,
    co2_flux,
    ch4_flux,
    h2o_mole_fraction,
    h2o_mixing_ratio,
    h2o_flux,
    #`shf_Avg.1./shf_Avg.2./shf_Avg.3.`,
    H,
    LE,
    #NF,  # Duplicate NF keys are ignored
    H_strg,
    LE_strg,
    air_pressure,
    RH,
    sonic_temperature,
    #NF,  # Duplicate NF keys are ignored
    #`Needs to be derived`,  # Duplicate "Needs to be derived" are ignored
    qc_co2_flux,
    qc_ch4_flux,
    qc_H,
    qc_LE,
    qc_Tau,
    co2_var,
    co2_strg,
    ch4_strg,
    u_var,
    v_var,
    w_var,
    wind_dir,
    #NF,  # Duplicate NF keys are ignored
    wind_speed,
    max_wind_speed,
    X_z_d__L,
    air_temperature,
    VPD,
    #NF,  # Duplicate NF keys are ignored
    LW_IN_Avg,
    LW_OUT_Avg,
    PAR_IN_Avg,
    PAR_OUT_Avg,
    SW_IN_Avg,
    SW_OUT_Avg,
    SWC_1_1_1,
    #`TS_2_1_2/TS_2_2_2`,
    #`WTD_Avg/Lvl_m_Avg`,
    L,
    Tau
    #`u*/u_`
  )

# Rename the filtered columns
way3_2018_data_f1iltered <- way3_2018_data_filtered %>%
  rename(
    FETCH_70 = `x_70_`,
    CH4 = ch4_mole_fraction,
    CH4_MIXING_RATIO = ch4_mixing_ratio,
    CO2 = co2_mole_fraction,
    CO2_MIXING_RATIO = co2_mixing_ratio,
    FC = co2_flux,
    FCH4 = ch4_flux,
    H2O = h2o_mole_fraction,
    H2O_MIXING_RATIO = h2o_mixing_ratio,
    FH2O = h2o_flux,
    #G = `shf_Avg.1./shf_Avg.2./shf_Avg.3.`,
    H = H,
    LE = LE,
    #SG = NF,  # Duplicate NF keys are ignored
    SH = H_strg,
    SLE = LE_strg,
    PA = air_pressure,
    RH = RH,
    T_SONIC = sonic_temperature,
    #T_SONIC_SIGMA = NF,  # Duplicate NF keys are ignored
    #GPP = `Needs to be derived`,  # Duplicate "Needs to be derived" are ignored
    #NEE = `Needs to be derived`,  # Duplicate "Needs to be derived" are ignored
    #RECO = `Needs to be derived`,  # Duplicate "Needs to be derived" are ignored
    FC_SSITC_TEST = qc_co2_flux,
    FCH4_SSITC_TEST = qc_ch4_flux,
    H_SSITC_TEST = qc_H,
    LE_SSITC_TEST = qc_LE,
    TAU_SSITC_TEST = qc_Tau,
    CO2_SIGMA = co2_var,
    SC = co2_strg,
    SCH4 = ch4_strg,
    U_SIGMA = u_var,
    V_SIGMA = v_var,
    W_SIGMA = w_var,
    WD = wind_dir,
    #WD_SIGMA = NF,  # Duplicate NF keys are ignored
    WS = wind_speed,
    WS_MAX = max_wind_speed,
    ZL = X_z_d__L,
    TA = air_temperature,
    VPD = VPD,
    #P = NF,  # Duplicate NF keys are ignored
    LW_IN = LW_IN_Avg,
    LW_OUT = LW_OUT_Avg,
    PPFD_IN = PAR_IN_Avg,
    PPFD_OUT = PAR_OUT_Avg,
    SW_IN = SW_IN_Avg,
    SW_OUT = SW_OUT_Avg,
    SWC = SWC_1_1_1,
    #TS = `TS_2_1_2/TS_2_2_2`,
    #WTD = `WTD_Avg/Lvl_m_Avg`,
    MO_LENGTH = L,
    TAU = Tau
    #USTAR = `u*/u_`
  )

# Print the column FC_SSITC_TEST
View(way3_2018_data_filtered)
