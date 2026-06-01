# script written to work with REddyProc library to partition Way 3 NEE fluxes

library(REddyProc)

# load data
fname <- '/Users/will/Documents/SIF_hyperspec/Met_EC_data_GS-2024/Way3_2024/NEE_partitioning/REddyProc_qcLE-qcH-filt_2024-04-14_2024-08-05.txt'
data_init <- fLoadTXTIntoDataframe(fname)

# create POSIX timestamps from time columns
data <- fConvertTimeToPosix(data_init, 'YDH', Year='Year', Day='DoY', Hour='Hour')

# create reference class for post-processing
EC_proc <- sEddyProc$new('US-HRC', data, c('NEE','Rg','Tair','VPD', 'Ustar'))

# get u-star threshold values
## to accommodate size of data, reduce size of records for temp class from 100 to 25, minRecordsWithinYear to 1700 (just shy of n good NEE data)
EC_proc$sEstimateUstarScenarios(nSample=100L, probs=c(0.05, 0.5, 0.95), 
                                ctrlUstarSub = usControlUstarSubsetting(taClasses = 7, UstarClasses = 20,
                                                                        swThr = 10, minRecordsWithinTemp = 25,
                                                                        minRecordsWithinSeason = 160, minRecordsWithinYear = 1700,
                                                                        isUsingOneBigSeasonOnFewRecords = TRUE))
EC_proc$sGetEstimatedUstarThresholdDistribution()

# for now, estiamte u-star threshold as that for season (mean value, can use 95% later on to be more conservative)
ustar_thresh <- EC_proc$`.->sUSTAR`$uStar[1]
print(ustar_thresh)

# gapfill the data (use all u-star scenarios)
EC_proc$sMDSGapFillUStarScens('NEE')

# now do partitioning
## set location info, fill any gaps in Tair, VPD, and Rg
EC_proc$sSetLocationInfo(LatDeg=34.59, LongDeg=-91.75, TimeZoneHour=-5)
EC_proc$sMDSGapFill('Tair', FillAll=FALSE, minNWarnRunLength=NA)
EC_proc$sMDSGapFill('VPD', FillAll=FALSE, minNWarnRunLength=NA)
EC_proc$sMDSGapFill('Rg', FillAll=FALSE, minNWarnRunLength=NA)

# nighttime partitioning (Reichstein et al. (2005))
## A note that u-star 95% threshold omits too many points for a successful partitioning
EC_proc$sMRFluxPartitionUStarScens()
# briefly look at what new columns are created there
grep("GPP.*_f$|Reco",names(EC_proc$sExportResults()), value = TRUE)

# daytime partitioning (Lasslop et al. (2010)) 
EC_proc$sGLFluxPartitionUStarScens()
# again look at new columns from this 
grep("GPP.*_f$|Reco",names(EC_proc$sExportResults()), value = TRUE)
## looks like partitioning ran all the way through! Good

# now save these initial results out
FilledEddyData <- EC_proc$sExportResults()
CombinedData <- cbind(data, FilledEddyData)
fWriteDataframeToFile(CombinedData, 'Way3-2024_Run1.txt', Dir='/Users/will/Documents/SIF_hyperspec/Met_EC_data_GS-2024/Way3_2024/NEE_partitioning/REddyProc')
