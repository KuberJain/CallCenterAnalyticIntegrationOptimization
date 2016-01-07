'''
Created on Nov 26, 2015

This script is to open a main extrance to a contact center simulation based decision support system

@author: bzheng
'''

import os
import etl
import visualization

def main():
    dir = os.path.dirname(__file__)
    print dir
    
    rawCallRecord, rawCallRecordColName = etl.dataETL(dir)
    date_stats, vruline_stats, type_stats, outcome_stats, recordStartDate, recordEndDate = etl.callVolumeStat(rawCallRecord)
    
    #print date_stats
    #print vruline_stats
    #print type_stats
    #print outcome_stats
    #print recordStartDate
    #print recordEndDate
    #visualization.barplot(date_stats.keys(), date_stats.values(), 'Israel Bank Contact Center Daily Volume in 1999', 'Date', 'Number of Calls', 0.2)
    #visualization.barplot(vruline_stats.keys(), vruline_stats.values(), 'Israel Bank Contact Center Cumulative VRU Line Call Volume in 1999', 'Date', 'Number of Calls', 0.2)
    #visualization.barplot(type_stats.keys(), type_stats.values(), 'Israel Bank Contact Center Cumulative Call Volume Over Types in 1999', 'Date', 'Number of Calls', 0.2)
    
    callRecord = etl.dataClean(rawCallRecord)
    etl.callVolumeAGG(callRecord,rawCallRecordColName, 'D', recordStartDate, recordEndDate, True, dir)
    etl.callVolumeAGG(callRecord,rawCallRecordColName, 'H', recordStartDate, recordEndDate, True, dir)
    return None

if __name__ == '__main__':
    main()