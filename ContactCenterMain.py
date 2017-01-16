'''
Created on Nov 26, 2015

This script is to open a main extrance to a contact center simulation based decision support system

@author: bzheng
'''

import os
import etl
import pandas as pd
import visualization

from etl import callStats
from etl import agentStats

def main():
    dir = os.path.dirname(__file__)
    print dir
    
    rawCallRecord, rawCallRecordColName = etl.dataETL(dir)

    callRecord = []

    if not os.path.isfile(dir+'/Israel_Bank_Data/1999_rmPHANTOM.txt'):
        callRecord = etl.dataClean(dir, rawCallRecord, rawCallRecordColName)
    else:
        with open(dir+'/Israel_Bank_Data/1999_rmPHANTOM.txt', 'r') as readTXT:
            callRecordName = readTXT.readline().strip().split('\t')
            for row in readTXT:
                callRecord.append(row.strip().split('\t'))
        readTXT.close()

    date_stats, vruline_stats, type_stats, outcome_stats, recordStartDate, recordEndDate = etl.callStats.volume(
        callRecord)

    print date_stats
    print vruline_stats
    print type_stats
    print outcome_stats
    print recordStartDate
    print recordEndDate

    visualization.barplot(dir, date_stats.keys(), date_stats.values(),
                          'Israel Bank Contact Center Daily Volume in 1999', 'Date', 'Number of Calls', 0.2)
    visualization.barplot(dir, vruline_stats.keys(), vruline_stats.values(),
                          'Israel Bank Contact Center Cumulative VRU Line Call Volume in 1999', 'VRU Line',
                          'Number of Calls', 0.2)
    visualization.barplot(dir, type_stats.keys(), type_stats.values(),
                          'Israel Bank Contact Center Cumulative Call Volume Over Types in 1999', 'Type',
                          'Number of Calls', 0.2)
    visualization.barplot(dir, outcome_stats.keys(), outcome_stats.values(),
                          'Israel Bank Contact Center Cumulative Call Volume Over Status in 1999', 'Status',
                          'Number of Calls', 0.2)

    if not os.path.isfile(dir+'/Israel_Bank_Data/agg_D.csv'):
        callVolumeD = etl.callVolumeAGG(callRecord, rawCallRecordColName, 'D', recordStartDate, recordEndDate, True,
                                        dir)
    else:
        callVolumeD = pd.read_csv(dir+'/Israel_Bank_Data/agg_D.csv')

    if not os.path.isfile(dir+'/Israel_Bank_Data/agg_H.csv'):
        callVolumeH = etl.callVolumeAGG(callRecord, rawCallRecordColName, 'H', recordStartDate, recordEndDate, True,
                                        dir)
    else:
        callVolumeH = pd.read_csv(dir+'/Israel_Bank_Data/agg_H.csv')

    agentSkill = agentStats.skill(callRecord)
    visualization.histplot(dir, agentSkill, 6, 'Israel Bank Contact Center Histogram of Number of Skills',
                        'Number of Skills', 'Number of Agents')

    return None

if __name__ == '__main__':
    main()