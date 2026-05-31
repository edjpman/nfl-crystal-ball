
############################
####### Model Class ########
############################


from xgboost import XGBClassifier

class Classifiers:

    def __init__(self):
        """Initialize the Classifiers model factory."""
        pass

    def __repr__(self):
        return f"{self.__class__.__name__}()"

    @staticmethod
    def summary(model):
        """Print evaluation-metric guidance for a supported model type.

        Args:
            model: Model identifier string (e.g. ``'XGB'``).

        Returns:
            None.
        """
        if model == 'XGB':
            print('XGBoost Summary:\n - For binary eval: logloss, error, auc')

    def xgb(self,objective='binary:logistic',
                                    eval_metric='logloss',
                                    scale_pos_weight=None,
                                    n_estimators=100,
                                    max_depth=4,
                                    max_leaves=0,
                                    learning_rate=0.05,
                                    random_state=42,
                                    min_split_loss=0.1,
                                    reg_alpha=0.1,
                                    reg_lambda=1.0,
                                    min_child_weight=1,
                                    colsample_bytree=0.8
                                    ):
        """Build and return a configured XGBoost binary classifier.

        Args:
            objective: XGBoost learning objective.
            eval_metric: Metric used during training evaluation.
            scale_pos_weight: Weight for the positive class in imbalanced data.
            n_estimators: Number of boosting rounds.
            max_depth: Maximum tree depth.
            max_leaves: Maximum number of leaves; 0 uses depth-based growth.
            learning_rate: Step size shrinkage.
            random_state: Random seed for reproducibility.
            min_split_loss: Minimum loss reduction required to split a node.
            reg_alpha: L1 regularization term on weights.
            reg_lambda: L2 regularization term on weights.
            min_child_weight: Minimum sum of instance weight in a child node.
            colsample_bytree: Subsample ratio of columns when constructing each tree.

        Returns:
            An unfitted ``XGBClassifier`` instance with the given hyperparameters.
        """
        return XGBClassifier(objective=objective,
                                    scale_pos_weight=scale_pos_weight,
                                    eval_metric=eval_metric,
                                    n_estimators=n_estimators,
                                    max_depth=max_depth,
                                    max_leaves=max_leaves,
                                    learning_rate=learning_rate,
                                    random_state=random_state,
                                    min_split_loss=min_split_loss,
                                    reg_alpha=reg_alpha,
                                    reg_lambda=reg_lambda,
                                    min_child_weight=min_child_weight,
                                    colsample_bytree=colsample_bytree)
    

    

        


