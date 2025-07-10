
############################
####### Model Class ########
############################


from xgboost import XGBClassifier

class Classifiers:

    def __init__(self):
        pass


    @staticmethod
    def summary(model):
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
    

    

        



