from lib.env.global_var import *
from lib.func.shelve_store import *
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import xlrd

def top_sector_generate():
    data_folder_str = econ_data_path
    file_name = 'benchmark_data.xlsx'
    print data_folder_str+file_name
    xls = xlrd.open_workbook(data_folder_str+file_name, on_demand=True)
    benchmark_names = map(str,xls.sheet_names())

    # sheet BMNAME, ISHAF US Equity(diversified asset) and SPX Index are not wanted 
    benchmark_names = np.setdiff1d(benchmark_names,['BMNAME','ISHAF US Equity','SPX Index'])
    for benchmark in benchmark_names:
        temp = pd.read_excel(data_folder_str+file_name,sheetname = benchmark).iloc[:,[0,1]]
        temp.index = temp.iloc[:,0]
        temp = temp.iloc[:,[1]].dropna()
        
        var_name = "_".join(benchmark.split(" "))
        exec(var_name+" = temp")
        print(var_name+" declared")

    # variable name cannot include white-spaces
    var_names = map(lambda x: "_".join(x.split(" ")),benchmark_names)
    # calculate log return
    return_name_list =[]
    for var_name in var_names:
        exec(var_name+"_return = "+var_name+".apply(np.log).diff()")
        print(var_name+"_return declared")
        return_name_list.append(var_name+"_return")


    returns_df = pd.DataFrame()
    for return_series in return_name_list:
        exec("temp = "+return_series)
        returns_df = pd.concat([returns_df,temp],1)
    returns_df = returns_df.dropna()

    top_sector_returns = pd.DataFrame(columns = returns_df.columns)
    for dt_index in returns_df.index:
        top_sector_returns = top_sector_returns.append(returns_df.loc[dt_index,:].nlargest(3).to_frame().T)

    top_sector = top_sector_returns.notnull()
    store_variable(shelve_dir, ['top_sector'], {'top_sector': top_sector})
    
    return top_sector

