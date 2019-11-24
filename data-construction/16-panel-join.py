import pandas as pd
import numpy as np
full_data_path = '/home/user/projects/urban/data/processed/descriptive/monthly_events_panel_200.csv'
data_all = []
vintages_2018 = [('2018', '{0:0=2d}'.format(x)) for x in range(12, 13)] 
vintages_2019 = [('2019', '{0:0=2d}'.format(x)) for x in range(1, 8)] 
vintages_all = vintages_2018 + vintages_2019

for vintage in vintages_all:
    year, month = vintage
    output_file_name = f'covisits_coevents_{year}_{month}_200.csv'
    output_file_path = ('/home/user/projects/urban/data/processed/descriptive/'
            + output_file_name)
    data_all.append(output_file_path)

data = []
for data_path in data_all:
    data.append(pd.read_csv(data_path, dtype = {'cbg': pd.Int64Dtype(),
        'cbsa': pd.Int64Dtype()}))

data = pd.concat(data, ignore_index = False)
data.to_csv(full_data_path, index = False)
