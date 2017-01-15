import os
import pandas as pd
from datetime import timedelta

def servVol(callRecord):
    callRecordDF = pd.DataFrame.from_records(callRecord, columns=['vruline', 'call_id',	'customer_id',
                                                                  'priority', 'type', 'date', 'vru_entry', 'vru_exit',
                                                                  'vru_time', 'q_start', 'q_exit', 'q_time', 'outcome',
                                                                  'ser_start', 'ser_exit', 'ser_time', 'server'])
    agentSkillVolume = callRecordDF[['server', 'type', 'call_id']].groupby(by=['server', 'type'], as_index=False).count()

    print agentSkillVolume.head(5)

    return agentSkillVolume


def skill(callRecord):
    callRecordDF = pd.DataFrame.from_records(callRecord, columns=['vruline', 'call_id',	'customer_id',
                                                                  'priority', 'type', 'date', 'vru_entry', 'vru_exit',
                                                                  'vru_time', 'q_start', 'q_exit', 'q_time', 'outcome',
                                                                  'ser_start', 'ser_exit', 'ser_time', 'server'])
    agentSkill = callRecordDF[['server', 'type']].groupby(by=['server']).type.nunique()
    print agentSkill.head()


    return agentSkill