import pandas as pd
from dotenv import load_dotenv
import os
import numpy as np
from sklearn.preprocessing import LabelEncoder


############################
#### Data Loading Class ####
############################



class dsCompile:

    def __init__(self):
        pass

    @staticmethod
    def dataset_pointer(title='FILEPATH3'):

        base_dir = os.path.dirname(os.path.abspath(__file__))
        env_path = os.path.join(base_dir,'..','.env')
        load_dotenv(dotenv_path=env_path)
        print("Path to .env file:", env_path)
        print("FILEPATH:", os.getenv(title))

        fpath = os.getenv(title)
        return fpath
    
    def dataset_loader(self):
        rd = dsCompile()
        path = rd.dataset_pointer()
        df = pd.read_csv(path)
        return df
    



class featEng:
    
    def __init__(self,cat_cols):
        self.cat_cols = cat_cols

    @staticmethod
    def qtr_map(row):
        if row['qtr1'] == 1:
            return 1
        elif row['qtr2'] == 1:
            return 2
        elif row['qtr3'] == 1:
            return 3
        elif row['qtr4'] == 1:
            return 4
        elif row['ot'] == 1:
            return 5
        else:
            return np.nan


    def feature_addition(self,data):
        df = data
        df['opp_team'] = np.where(df['home_team'] == 'KC', df['away_team'],
                                  np.where(df['away_team'] == 'KC', df['home_team'], None))

        df['quarter'] = df.apply(featEng.qtr_map, axis=1)

        df['cum_play_count_by_quarter'] = df.groupby(['game_id', 'quarter']).cumcount() + 1

        #Shows how aggressive play calling is 
        df['game_velocity'] = df['cum_play_count_by_quarter']/(df['quarter_elapsed'] + 1)


        df['rush_ratio'] = df['cum_rush_attmpt']/df['cum_play_count']


        df['pass_ratio'] = df['cum_pass_attmpt']/df['cum_play_count']


        df['passing_efficiency'] = df['cum_incmpl_pass']/df['cum_play_count']


        df['scoring_efficiency'] = (df['cum_chiefs_scr'] + 1) / (df['cum_opps_scr'] + 1)


        df['tot_cum_scor'] = df['cum_chiefs_scr'] + df['cum_opps_scr']

        df['3rd_down_efficiency'] = (df['cum_3dc'] + 1)/(df['cum_3df'] + 1)

        df['4th_down_efficiency'] = (df['cum_4dc'] + 1)/(df['cum_4df'] + 1)

        df['avg_penalty_cost'] = np.where(
            df['cum_penalty_yards'] == 0,
            0,
            np.where(
                df['cum_ydsgain'] <= 0,
                df['cum_penalty_yards'],  #Basically will perform the same as 13/1 if 13/any negative number
                df['cum_penalty_yards'] / df['cum_ydsgain']
            )
        )

        df['avg_penalty_risk'] = df['cum_penalty_yards']/(df['cum_penalty'] + 1e-6)

        df['qb_risk'] = (df['cum_qb_hits'] + df['cum_sacks'])/(2*df['cum_play_count'])

        return df
        

    def cat_transform(self,data):

        df = data
    
        for col in self.cat_cols:
            le = LabelEncoder()
            df[col] = le.fit_transform(df[col])

        return df


