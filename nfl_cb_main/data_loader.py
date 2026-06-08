import pandas as pd
from dotenv import load_dotenv
import os
import numpy as np
import joblib
from sklearn.preprocessing import OrdinalEncoder


############################
#### Data Loading Class ####
############################



class dsCompile:

    def __init__(self):
        """Initialize the dsCompile data loading helper."""
        pass

    def __repr__(self):
        return f"{self.__class__.__name__}()"

    @staticmethod
    def dataset_pointer(title='FILEPATH3'):
        """Resolve a dataset file path from an environment variable.

        Args:
            title: Name of the environment variable holding the file path.

        Returns:
            The file path string loaded from the environment, or None if unset.
        """
        base_dir = os.path.dirname(os.path.abspath(__file__))
        env_path = os.path.join(base_dir,'..','.env')
        load_dotenv(dotenv_path=env_path)
        print("Path to .env file:", env_path)
        print("FILEPATH:", os.getenv(title))

        fpath = os.getenv(title)
        return fpath
    
    def dataset_loader(self):
        """Load the compiled dataset CSV referenced by FILEPATH3.

        Returns:
            A pandas DataFrame containing the loaded dataset.
        """
        rd = dsCompile()
        path = rd.dataset_pointer()
        df = pd.read_csv(path)
        return df
    



class featEng:
    
    def __init__(self,cat_cols):
        """Initialize feature engineering with categorical column names.

        Args:
            cat_cols: List of column names to ordinally encode during transformation.
        """
        self.cat_cols = cat_cols
        self.encoder = None

    def __repr__(self):
        return f"featEng(cat_cols={self.cat_cols!r})"

    @staticmethod
    def qtr_map(row):
        """Map one-hot quarter indicator columns to a single quarter number.

        Args:
            row: A pandas Series containing qtr1, qtr2, qtr3, qtr4, and ot fields.

        Returns:
            The quarter number (1–5) or NaN if no quarter flag is set.
        """
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
        """Derive model features from raw cumulative play-by-play columns.

        Args:
            data: Input DataFrame with cumulative and team-level columns.

        Returns:
            The input DataFrame with engineered feature columns appended.
        """
        df = data
        df['opp_team'] = np.where(df['home_team'] == 'KC', df['away_team'],
                                  np.where(df['away_team'] == 'KC', df['home_team'], None))

        df['quarter'] = df.apply(featEng.qtr_map, axis=1)

        df['cum_play_count_by_quarter'] = df.groupby(['game_id', 'quarter']).cumcount() + 1

        #Shows how aggressive play calling is 
        df['game_velocity'] = df['cum_play_count_by_quarter']/(df['quarter_elapsed'] + 1)


        df['rush_ratio'] = df['cum_rush_attmpt']/df['cum_play_count']


        df['pass_ratio'] = df['cum_pass_attmpt']/df['cum_play_count']


        df['incomplete_pass_rate'] = df['cum_incmpl_pass']/df['cum_play_count']


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

        df['score_momentum'] = (
            df.groupby('game_id')['cum_score_dif'].diff(3).fillna(0)
        )

        df['yards_per_play'] = df['cum_ydsgain'] / df['cum_play_count']

        df['sack_rate'] = df['cum_sacks'] / (df['cum_pass_attmpt'] + 1e-6)

        df['tfl_rate'] = df['cum_tfl'] / df['cum_play_count']

        df['game_clock_pressure'] = (
            (df['quarter'] - 1) * 900 + (900 - df['quarter_seconds_remaining'])
        ) / 3600

        return df
        

    def cat_transform(self,data):
        """Ordinally encode configured categorical columns and store the fitted encoder.

        Args:
            data: Input DataFrame containing the categorical columns.

        Returns:
            The DataFrame with categorical columns replaced by integer codes.
        """
        df = data.copy()
        self.encoder = OrdinalEncoder(
            handle_unknown="use_encoded_value",
            unknown_value=-1
        )
        df[self.cat_cols] = self.encoder.fit_transform(df[self.cat_cols])
        return df

    def transform_new(self, data):
        """Apply the fitted encoder to new data without refitting.

        Args:
            data: Input DataFrame containing the categorical columns.

        Returns:
            The DataFrame with categorical columns replaced by integer codes.
        """
        if self.encoder is None:
            raise ValueError("Encoder not fitted. Call cat_transform() or load_encoder() first.")
        df = data.copy()
        df[self.cat_cols] = self.encoder.transform(df[self.cat_cols])
        return df

    def save_encoder(self, path):
        """Serialize the fitted encoder to disk.

        Args:
            path: File path where the encoder will be saved.

        Returns:
            None.
        """
        if self.encoder is None:
            raise ValueError("No encoder to save. Call cat_transform() first.")
        joblib.dump(self.encoder, path)

    def load_encoder(self, path):
        """Load a serialized encoder from disk.

        Args:
            path: File path to a previously saved encoder.

        Returns:
            None.
        """
        self.encoder = joblib.load(path)


