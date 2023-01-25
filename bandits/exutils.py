import pandas as pd

def read_pickle(task_id: int, from_path: str="~/pcmabinf/OpenML-CC18/"):
    return pd.read_pickle(from_path + f"{task_id}")


