

#################################
####### Train/Tune Class ########
#################################

from sklearn.metrics import accuracy_score, log_loss, roc_auc_score, precision_score, recall_score, f1_score


class TTD:

    def __init__(self,model_type,model):
        self.model_type = model_type
        self.model = model


    def ttd_year_splits(self, dataset, x_col, y_col, time_col='season',
               train_seasons=None, dev_seasons=None, test_seasons=None):

        if train_seasons is None or test_seasons is None:
            raise ValueError("Must provide train_seasons and test_seasons.")

        train_df = dataset[dataset[time_col].isin(train_seasons)].copy()
        test_df = dataset[dataset[time_col].isin(test_seasons)].copy()

        X_train = train_df[x_col]
        y_train = train_df[y_col]

        X_test = test_df[x_col]
        y_test = test_df[y_col]

        if dev_seasons:
            dev_df = dataset[dataset[time_col].isin(dev_seasons)].copy()
            X_dev = dev_df[x_col]
            y_dev = dev_df[y_col]
        else:
            X_dev = None
            y_dev = None

        return X_train, X_dev, X_test, y_train, y_dev, y_test
    

    def train(self,X_train,y_train):

        if self.model_type == 'bin-xgb':
            return self.model.fit(X_train,y_train)
       
    
    def predict(self, X):
        return self.model.predict(X)

    def predict_proba(self, X):
        return self.model.predict_proba(X)
       

    def evaluate(self,X_test,y_test,threshold=0.5):

        probs = self.predict_proba(X_test)[:, 1]
        preds = (probs >= threshold).astype(int)
        
        return {
            'accuracy': accuracy_score(y_test, preds),
            'log_loss': log_loss(y_test, probs),
            'auc': roc_auc_score(y_test, probs),
            'precision': precision_score(y_test, preds),
            'recall': recall_score(y_test, preds),
            'f1': f1_score(y_test, preds)
        }
    
