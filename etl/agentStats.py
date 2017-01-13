import os
import pandas as pd
from datetime import timedelta

def agentServVol(callRecord):
    callRecordDF = pd.DataFrame.from_records(callRecord, columns=['vruline', 'call_id',	'customer_id',
                                                                  'priority', 'type', 'date', 'vru_entry', 'vru_exit',
                                                                  'vru_time', 'q_start', 'q_exit', 'q_time', 'outcome',
                                                                  'ser_start', 'ser_exit', 'ser_time', 'server'])
    agentSkill = callRecordDF[['server', 'type', 'call_id']].groupby(by=['server', 'type']).count()

    print agentSkill.shape

    return agentSkill


def skill(callRecord):

    return