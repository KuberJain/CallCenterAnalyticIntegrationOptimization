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
        if row[12] != 'PHANTOM' and row[4] != 'AA':
            rmPhantomCallRecord.append(row)
        if row[4] =='AA':
            print row

    with open(dir+'/Israel_Bank_Data/1999_rmPHANTOM.txt', 'w') as writeTXT:
        # print rawDataColumnNames
        writeTXT.write('\t'.join(rawRecordColNames) + '\n')
        for row in rmPhantomCallRecord:
            writeTXT.write('\t'.join(row) + '\n')
    writeTXT.close()

    return rmPhantomCallRecord

def callVolumeAGG(callRecord, callRecordColName, aggInterval, recordStartDate, recordEndDate, outputFile, dir):
    callRecordDF = pd.DataFrame(callRecord, columns=callRecordColName)
    callRecordArrTimeIndex = pd.DatetimeIndex(pd.to_datetime(callRecordDF.date + "\t" + callRecordDF.vru_entry))
    callRecordArrTimeFullRange = pd.date_range(start=recordStartDate,
                                               end=pd.datetime.strptime(recordEndDate, '%y%m%d') + timedelta(days=1),
                                               freq=aggInterval)
    callRecordArrTimeSeries = pd.Series(1, index=callRecordArrTimeIndex)
    callRecordArrTimeFullRangeSeries = dict()

    if aggInterval == 'D':
        callRecordArrTimeAGGSeries = callRecordArrTimeSeries.resample(rule='D').sum()
    elif aggInterval == 'H':
        callRecordArrTimeAGGSeries = callRecordArrTimeSeries.resample(rule='H').sum()
    else:
        print 'Aggregation Interval ' + aggInterval + '\t is not supported'

    callRecordArrTimeAGGSeries = callRecordArrTimeAGGSeries.fillna(value=0)

    for time in callRecordArrTimeFullRange:
        if time in callRecordArrTimeAGGSeries.keys():
            callRecordArrTimeFullRangeSeries[time] = callRecordArrTimeAGGSeries.get_value(time)
        elif time <= pd.datetime.strptime(recordEndDate, '%y%m%d') + timedelta(hours=23):
            callRecordArrTimeFullRangeSeries[time] = 0
    callRecordArrTimeFullRangeSeriesFinal = pd.Series(data=callRecordArrTimeFullRangeSeries)

    if outputFile == True:
        callRecordArrTimeFullRangeSeriesFinal.to_csv(path=dir + '/Israel_Bank_Data/agg_' + aggInterval + '.csv',
                                                     index=True, header=False, sep=',')

    return callRecordArrTimeFullRangeSeriesFinal



