

#################################
####### Train/Tune Class ########
#################################

from sklearn.metrics import accuracy_score, log_loss, roc_auc_score, precision_score, recall_score, f1_score


class TTD:

    def __init__(self,model_type,model):
        """Initialize a train-tune-deploy wrapper around a model instance.

        Args:
            model_type: Identifier for the model pipeline (e.g. ``'bin-xgb'``).
            model: A scikit-learn–compatible estimator to train and evaluate.
        """
        self.model_type = model_type
        self.model = model

    def __repr__(self):
        return f"TTD(model_type={self.model_type!r}, model={self.model!r})"

    def ttd_year_splits(self, dataset, x_col, y_col, time_col='season',
               train_seasons=None, dev_seasons=None, test_seasons=None):
        """Split a dataset into train, optional dev, and test sets by season.

        Args:
            dataset: Full DataFrame containing features, target, and time column.
            x_col: Feature column name or list of feature column names.
            y_col: Target column name.
            time_col: Column used to assign rows to season-based splits.
            train_seasons: Season values included in the training set.
            dev_seasons: Optional season values included in the dev set.
            test_seasons: Season values included in the test set.

        Returns:
            A tuple ``(X_train, X_dev, X_test, y_train, y_dev, y_test)``; dev
            arrays are ``None`` when ``dev_seasons`` is not provided.
        """
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
        """Fit the wrapped model on training features and labels.

        Args:
            X_train: Training feature matrix.
            y_train: Training target vector.

        Returns:
            The fitted model returned by the underlying estimator's ``fit`` method.
        """
        if self.model_type == 'bin-xgb':
            return self.model.fit(X_train,y_train)
       
    
    def predict(self, X):
        """Generate class predictions for the given features.

        Args:
            X: Feature matrix to predict on.

        Returns:
            An array of predicted class labels.
        """
        return self.model.predict(X)

    def predict_proba(self, X):
        """Generate class probability estimates for the given features.

        Args:
            X: Feature matrix to score.

        Returns:
            A 2-D array of predicted class probabilities.
        """
        return self.model.predict_proba(X)
       

    def evaluate(self,X_test,y_test,threshold=0.5):
        """Compute classification metrics at a probability threshold.

        Args:
            X_test: Test feature matrix.
            y_test: True test labels.
            threshold: Probability cutoff for assigning the positive class.

        Returns:
            A dict of metric names to scalar scores (accuracy, log_loss, auc,
            precision, recall, and f1).
        """
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
    
