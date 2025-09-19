LFG AmeriFlux Data Submission Report
Fourth Round of Submission

This repository contains data and code for the fourth round of data submission to the AmeriFlux network. The data processing and quality control steps detailed here were performed in preparation for this submission.
Authors and Contributors

    Maria and Kevin: Master file compilation, including merging Biomet and soil data and running Eddy Pro in Advanced mode.

    Colby: Provided the code for master file compilation.

    Bea: Provided LAI, canopy height, and water table depth data.

    Marret: Corrected LAI and canopy height data.

    Riasad Mahbub: Developed the data post-processing code and analysis detailed in this report. The code is available at the link below.

Master File Compilation

Master files were compiled by merging all Biomet and soil data, and running the full year of Horst-corrected 30-minute flux data through Eddy Pro. Eddy Pro used temperature and relative humidity data from the Biomet dataset.
Data Processing and Quality Control

The following sections detail the specific data corrections and quality control (QA/QC) steps applied to the datasets.
LAI and Canopy Height Data

LAI and canopy height data were corrected and provided by Bea and Marret. Duplicate dates from Way 3 East and Way 3 West samples were averaged. The 8-day resolution data was gap-filled to a 30-minute resolution using the na.approx function from the zoo package in R.

    Source Files:

        2018-2020LAI_Canopy.xlsx

        LAIMasterFileScatteringCorrections2021.xlsx

        RiceFieldDataCanopyLAI2021.xlsx

        LAI_MasterFileScatteringCorrections2022.xlsx

        RiceFieldDataCanopyLAI2022.xlsx

        RiceFieldDataCanopyLAI2023.xlsx

        RiceFieldDataCanopyLAI_2024_Last.xlsx

Water Table Depth (WTD) Data

WTD data from the Unilever Tower Station was sourced from Bea for the years 2020 through 2023. Units were converted from centimeters to meters.

    Source Files:

        MasterFile_Way3_SoilProf_VWC.xlsx

        MasterFile_Way4SoilProfile.xlsx

        2021_MasterFile_Way3_SoilProf_All.xlsx

        2021_MasterFile_Way4SoilProfile.xlsx

        2022_Way4_SoilProfile_MasterFile.xlsx

        2022Way3SoilProfileMasterFile.xlsx

        2023Way3SoilProfileMasterFile copy.xlsx

        2023Way4_SoilProfile_MasterFile copy.xlsx

Data Corrections and Unit Conversions

    Intervals: The aggregate_to_30min function was used to convert data with 5- or 15-minute intervals into standard 30-minute intervals by rounding timestamps.

    Rainfall Data: The merge_p_rain_tot function replaced the P_RAIN_Tot column in Way 3 data with corresponding values from Way 4 data.

    Air Pressure: The convert_air_pressure_to_kpa function was used to convert air pressure values from pascals (Pa) to kilopascals (kPa) for the years 2018-2021.

    Latent Heat (LE) Data: Summer 2021 LE data for Way 3 was replaced with corrected values from an external file (eddypro_LE_Fix_2023_12_06_full_output_2023-12-06T144759_adv.csv).

    Ustar Outliers: Outliers in the ustar column were identified and removed for specific datasets based on their relationship with wind speed (WS). A linear regression was performed, and residuals exceeding a threshold (4x SD for 2018, 2021 and 3.75x SD for 2023, 2024) were replaced with NaN.

    Temperature Data: A three-step filtering process was applied to replace out-of-range temperature values with NaN.

Variable Naming and Alignment

    Column names were changed and units were standardized according to the AmeriFlux variable naming convention (see: https://ameriflux.lbl.gov/data/aboutdata/data-variables/).

    A Timestamp Alignment Module was used to detect and correct timestamp misalignment by comparing measured incoming radiation with potential shortwave radiation.

Physical Value Filtering

A percentile-based filtering method was used to identify and remove abnormal values for a range of variables by replacing them with NaN.

    Columns Checked:

        FCH4, FC_1_1_1, FH20_1_1_1, FETCH_MAX, FETCH_70, FETCH_90, SH, SLE, SC, PA, CO2_SIGMA, U_SIGMA, VPD, V_SIGMA, TAU, LW_IN, LW_OUT, LE, SWC_1_1_1, TS_1_1_1, TA_1_1_1, PPFD_IN.

    Additional Filter: WTD_1_1_1 values less than -0.5 or greater than 5 were replaced with NA.

Known Issues and Limitations

    The regression analysis evaluating the relationship between USTAR and WS failed for the years 2018, 2023, and 2024.

    The FH2O and TAU variables for 2021 failed the physical range check.

    The FH2O values for 2019 and 2021 exceed expected physical limits.

    The WS and USTAR relationship showed a slope deviation greater than 20% for 2018, 2023, and 2024.

Data Availability

    Code: https://github.com/RiasadMahbub/PostProcessingECdataLandscapeFlux/tree/main/AmerifluDataSubmission_LandscapeFlux

    AmeriFlux Submission Data: Box\Field_Data\AmeriFlux_Submission_Figures\OutputLocalProcessedData_AFguidedSubmitted\AmerifluxSubmission

    Lab Research Data: Box\Field_Data\AmeriFlux_Submission_Figures\OutputLocalProcessedData_AFguidedSubmitted\ForLabResearachPurposeMoreColumns