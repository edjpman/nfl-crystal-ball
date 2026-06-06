import os

import matplotlib.pyplot as plt
import numpy as np
from sklearn.calibration import calibration_curve
from sklearn.isotonic import IsotonicRegression
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import brier_score_loss


class ProbabilityCalibrator:
    """Post-hoc probability calibrator for classifier raw scores."""

    def __init__(self, method='platt'):
        """Initialize the calibrator with a scaling method.

        Args:
            method: Calibration method; ``'platt'`` (sigmoid) or ``'isotonic'``.
        """
        if method not in ('platt', 'isotonic'):
            raise ValueError("method must be 'platt' or 'isotonic'")
        self.method = method
        self._calibrator = None

    def __repr__(self):
        fitted = self._calibrator is not None
        return f"ProbabilityCalibrator(method={self.method!r}, fitted={fitted})"

    def fit(self, y_true, y_proba):
        """Fit the calibrator on held-out raw probabilities and true labels.

        Platt scaling fits a logistic regression (equivalent to
        ``CalibratedClassifierCV`` with ``method='sigmoid'``). Isotonic fits a
        monotonic non-parametric mapping.

        Args:
            y_true: True binary labels.
            y_proba: Raw predicted probabilities from the base classifier.

        Returns:
            self.
        """
        y_true = np.asarray(y_true)
        y_proba = np.asarray(y_proba)

        if self.method == 'platt':
            self._calibrator = LogisticRegression(solver='lbfgs', max_iter=1000)
            self._calibrator.fit(y_proba.reshape(-1, 1), y_true)
        else:
            self._calibrator = IsotonicRegression(out_of_bounds='clip')
            self._calibrator.fit(y_proba, y_true)

        return self

    def calibrate(self, y_proba):
        """Map raw probabilities to calibrated probabilities.

        Args:
            y_proba: Raw predicted probabilities from the base classifier.

        Returns:
            Calibrated probability estimates.
        """
        if self._calibrator is None:
            raise ValueError("Calibrator not fitted. Call fit() first.")

        y_proba = np.asarray(y_proba)
        if self.method == 'platt':
            return self._calibrator.predict_proba(y_proba.reshape(-1, 1))[:, 1]
        return self._calibrator.predict(y_proba)

    def brier_score(self, y_true, y_proba):
        """Compute the Brier score for predicted probabilities.

        Args:
            y_true: True binary labels.
            y_proba: Predicted probabilities (raw or calibrated).

        Returns:
            Brier score loss; lower is better.
        """
        return brier_score_loss(y_true, y_proba)

    def plot_calibration_curve(
        self,
        y_true,
        y_proba,
        n_bins=10,
        title='Calibration Curve',
        save_path=None,
    ):
        """Plot a reliability diagram with a perfect-calibration reference line.

        Args:
            y_true: True binary labels.
            y_proba: Predicted probabilities to evaluate.
            n_bins: Number of bins for ``calibration_curve``.
            title: Plot title.
            save_path: Optional file path to save the figure.

        Returns:
            Tuple of ``(fig, ax)``.
        """
        fraction_pos, mean_pred = calibration_curve(
            y_true, y_proba, n_bins=n_bins, strategy='uniform'
        )

        fig, ax = plt.subplots(figsize=(8, 6))
        ax.plot(
            mean_pred,
            fraction_pos,
            's-',
            color='#E31837',
            label='Model',
        )
        ax.plot([0, 1], [0, 1], '--', color='gray', label='Perfect calibration')
        ax.set_xlabel('Mean predicted probability')
        ax.set_ylabel('Fraction of positives')
        ax.set_title(title)
        ax.legend()
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)

        if save_path:
            save_dir = os.path.dirname(save_path)
            if save_dir:
                os.makedirs(save_dir, exist_ok=True)
            fig.savefig(save_path, bbox_inches='tight')

        plt.show()
        return fig, ax
