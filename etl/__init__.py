import os
import pandas as pd
from datetime import timedelta


def concatMonthlyData(dir):
    callRecord = []
    rawDataColumnNames = []
    for i in range(1,13):
        filename = dir+'/Israel_Bank_Data/'+str(i)+'.txt'
        # print filename
        with open(filename, 'r') as readTXT:
            rawDataColumnNames = readTXT.readline().strip().split('\t')
            for row in readTXT:
                callRecord.append(row.strip().split('\t'))
        readTXT.close()

    with open(dir+'/Israel_Bank_Data/1999.txt', 'w') as writeTXT:
        # print rawDataColumnNames
        writeTXT.write('\t'.join(rawDataColumnNames) +'\n')
        for row in callRecord:
            writeTXT.write('\t'.join(row)+'\n')
    writeTXT.close()
    return callRecord, rawDataColumnNames


def dataETL(dir):
    callRecord = []
    callRecordColNames =[]
    if not os.path.isfile(dir+'/Israel_Bank_Data/1999.txt'):
        callRecord, callRecordColNames = concatMonthlyData()
    else:
        with open(dir+'/Israel_Bank_Data/1999.txt', 'r') as readTXT:
            callRecordColNames = readTXT.readline().strip().split('\t')
            for row in readTXT:
                callRecord.append(row.strip().split('\t'))
        readTXT.close()
    return callRecord, callRecordColNames


def dataClean(dir,rawRecord,rawRecordColNames):
    rmPhantomCallRecord = []
    for row in rawRecord:
        if row[12] != 'PHANTOM':
            rmPhantomCallRecord.append(row)

    with open(dir+'/Israel_Bank_Data/1999_rmPHANTOM.txt', 'w') as writeTXT:
        # print rawDataColumnNames
        writeTXT.write('\t'.join(rawRecordColNames) + '\n')
        for row in rmPhantomCallRecord:
            writeTXT.write('\t'.join(row) + '\n')
    writeTXT.close()

    return rmPhantomCallRecord


def callVolumeStat(callRecord):
    date_stats = {}
    vruline_stats = {}
    type_stats = {}
    outcome_stats ={}
    
    for record in callRecord:
        if record[0] not in vruline_stats:
            vruline_stats[record[0]]=1
        else:
            vruline_stats[record[0]]+=1
            
        if record[4] not in type_stats:
            type_stats[record[4]]=1
        else:
            type_stats[record[4]]+=1
            
        if record[5] not in date_stats:
            date_stats[record[5]]=1
        else:
            date_stats[record[5]]+=1
        
        if record[12] not in outcome_stats:
            outcome_stats[record[12]]=1
        else:
            outcome_stats[record[12]]+=1
    
    sortDate = pd.to_datetime(date_stats.keys(), format='%y%m%d').sort_values(ascending=True)
    
    recordStartDate = sortDate[0].strftime(format='%y%m%d')
    recordEndDate = sortDate[len(sortDate)-1].strftime(format='%y%m%d')

    return date_stats, vruline_stats, type_stats, outcome_stats, recordStartDate, recordEndDate


def callVolumeAGG(callRecord, callRecordColName, aggInterval, recordStartDate, recordEndDate, outputFile, dir):
    callRecordDF = pd.DataFrame(callRecord, columns=callRecordColName)
    callRecordArrTimeIndex = pd.DatetimeIndex(pd.to_datetime(callRecordDF.date + "\t" + callRecordDF.vru_entry))
    callRecordArrTimeFullRange = pd.date_range(start=recordStartDate, end=pd.datetime.strptime(recordEndDate, '%y%m%d')+ timedelta(days=1), freq=aggInterval)
    callRecordArrTimeSeries = pd.Series(1, index=callRecordArrTimeIndex)
    callRecordArrTimeFullRangeSeries = dict()
    
    if aggInterval == 'D':
        callRecordArrTimeAGGSeries = callRecordArrTimeSeries.resample(rule='D').sum()
    elif aggInterval == 'H':
        callRecordArrTimeAGGSeries = callRecordArrTimeSeries.resample(rule='H').sum()
    else:
        print 'Aggregation Interval '+ aggInterval + '\t is not supported'

    callRecordArrTimeAGGSeries = callRecordArrTimeAGGSeries.fillna(value=0)

    for time in callRecordArrTimeFullRange:
        if time in callRecordArrTimeAGGSeries.keys():
            callRecordArrTimeFullRangeSeries[time] = callRecordArrTimeAGGSeries.get_value(time)
        elif time <= pd.datetime.strptime(recordEndDate, '%y%m%d')+timedelta(hours=23):
            callRecordArrTimeFullRangeSeries[time] = 0           
    callRecordArrTimeFullRangeSeriesFinal = pd.Series(data=callRecordArrTimeFullRangeSeries)
    
    if outputFile == True:
        callRecordArrTimeFullRangeSeriesFinal.to_csv(path=dir+'/Israel_Bank_Data/agg_'+aggInterval+'.csv', index=True, header=False, sep=',')

    return callRecordArrTimeFullRangeSeriesFinal

