import numpy as np
import pandas as pd
import statsmodels.formula.api as smf


def rdestimate(data, y, x, controls=None, cutpoint=0, weights=1):
    """ Wrapper around `smf.wls` to produce `RDestimate`"""
    data["TREATED"] = np.where(data[x] >= cutpoint, 1, 0)
    equation = f"{y} ~ TREATED + {x}"
    if controls is not None:
        if isinstance(controls, list):
            eq_controls = " + ".join(controls)
        elif isinstance(controls, str):
            eq_controls = controls
        else:
            print(type(controls), "controls should be either list or str")
            eq_controls = ""
        equation += eq_controls
    return smf.wls(equation, data=data, weights=weights)


data = pd.read_csv("../data/AEJfigs.csv")
df = data.iloc[:, :8].copy()
model = rdestimate(
        data=df,
        y="all",
        x="agecell",
        cutpoint=21
    )
print(model.fit().summary())
