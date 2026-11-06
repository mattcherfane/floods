https://stn.wim.usgs.gov/STNDataPortal/#


wget -O filtered_hwms.csv "https://stn.wim.usgs.gov/STNServices/HWMs/FilteredHWMs.csv?Event=180&EventType=&EventStatus=0&States=&County=&HWMType=&HWMQuality=&HWMEnvironment=&SurveyComplete=&StillWater=" -o wget_filtered_hwms.txt

wget -O filtered_sensors.csv "https://stn.wim.usgs.gov/STNServices/Instruments/FilteredInstruments.csv?Event=180&EventType=&EventStatus=0&States=&County=&SensorType=&CurrentStatus=&CollectionCondition=&DeploymentType=" -o wget_filtered_sensors.txt

wget -O peak_summary.csv "https://stn.wim.usgs.gov/STNServices/PeakSummaries/FilteredPeaks.csv?Event=180&EventType=&EventStatus=0&States=&County=&StartDate=08/01/2017&EndDate=09/30/2017" -o wget_peak_summary.txt