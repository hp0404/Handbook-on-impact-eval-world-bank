import numpy as np 
import pandas as pd
from scipy import stats
from statsmodels.api import OLS


def run_ttest(table, values, by, two_sample=True):
    t, pvalue = stats.ttest_ind(
        table.loc[table[by].eq(1), values],
        table.loc[table[by].eq(0), values],
        equal_var=two_sample
    )
    test_type = "Two Sample t-test" if two_sample else "Welch Two Sample t-test"
    print(f"{test_type}; data: {values} by {by}; {t=}, {pvalue=}")
    
    
def run_linregress(table, values, by):
    slope, intercept, r_value, p_value, std_err = stats.linregress(
        table.loc[table[by].eq(1), values],
        table.loc[table[by].eq(0), values],
    )
    print(f"{slope=}, {intercept=}, {r_value=}, {p_value=}, {std_err=}")


data = pd.read_csv("./../data/hh_98.csv").rename(columns={"Unnamed: 0": "X"})

###########
# Subset
###########

df = data.assign(
    lexptot=np.log(1 + data["exptot"]), 
    lnland=np.log(1 + data["hhland"] / 100), 
    vill=data["thanaid"] * 10 + data["villid"]
)
df["progvillm"] = df.groupby("vill")["dmmfd"].transform("max")
df["progvillf"] = df.groupby("vill")["dfmfd"].transform("max")

################################
# Impacts of program placement
################################

# t-test

run_ttest(df, "lexptot", "progvillf")
run_ttest(df, "lexptot", "progvillm")