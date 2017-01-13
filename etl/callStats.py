import pandas as pd
from datetime import timedelta

def volume(callRecord):
    date_stats = {}
    vruline_stats = {}
    type_stats = {}
    outcome_stats = {}

    for record in callRecord:
        if record[0].strip() not in vruline_stats:
            vruline_stats[record[0].strip()] = 1
        else:
            vruline_stats[record[0].strip()] += 1

        if record[4].strip() not in type_stats:
            type_stats[record[4].strip()] = 1
        else:
            type_stats[record[4].strip()] += 1

        if record[5].strip() not in date_stats:
            date_stats[record[5].strip()] = 1
        else:
            date_stats[record[5].strip()] += 1

        if record[12].strip() not in outcome_stats:
            outcome_stats[record[12].strip()] = 1
        else:
            outcome_stats[record[12].strip()] += 1

    sortDate = pd.to_datetime(date_stats.keys(), format='%y%m%d').sort_values(ascending=True)

    recordStartDate = sortDate[0].strftime(format='%y%m%d')
    recordEndDate = sortDate[len(sortDate) - 1].strftime(format='%y%m%d')

    return date_stats, vruline_stats, type_stats, outcome_stats, recordStartDate, recordEndDate
