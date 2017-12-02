import pandas as pd
import numpy as np
import os
from sklearn.linear_model import LogisticRegression
import matplotlib.pyplot as plt
from sklearn import preprocessing

def read_data(directory_str,sheet_index):
    df = pd.read_excel(directory_str,sheet_index)
    df.index = df.iloc[:,0]
    df = df.iloc[:,[1]]
    df = df.dropna()
    # convert index to pd.DatetimeIndex
    if type(df.index) != pd.DatetimeIndex:
        df.index = pd.DatetimeIndex(df.index)
    df = df.replace(".",np.nan)
    return df

def read_csv(directory_str):
    data = pd.read_csv(directory_str)
    df = pd.DataFrame(pd.to_numeric(data.iloc[:,1].replace(".",np.nan)).values,index = data.iloc[:,0].values)
    df.columns = data.iloc[:,[1]].columns
    df = df.dropna()
    # convert index to pd.DatetimeIndex
    if type(df.index) != pd.DatetimeIndex:
        df.index = pd.DatetimeIndex(df.index)
    return df

def model_data(x,y):
    merged_data = pd.concat([y,x],1).resample("M").last().dropna()
    y = merged_data[y.columns].loc[merged_data.index,:]
    x = merged_data[x.columns].loc[merged_data.index,:]
    return(x,y)

def quaterly_to_monthly(df):
    # if the index is not pd.DatetimeIndex, convert it
    if type(df.index) != pd.DatetimeIndex:
        df.index = pd.DatetimeIndex(df.index)
        
    resampled_df = df.resample("M").last()
    for int_index in range(len(resampled_df)):
        value = resampled_df.iloc[int_index,:].values[0]
        if not np.isnan(value):
            # copy quater data
            previouse_quater = value
            quater_index = 1
        if np.isnan(value) and quater_index ==1:
            # filling first value
            resampled_df.iloc[int_index,:] = previouse_quater
            quater_index += 1
        if np.isnan(value) and quater_index ==2:
            # filling the second value
            resampled_df.iloc[int_index,:] = previouse_quater
            quater_index = 1
    return(resampled_df)

def logistic_reg_sector_index(data_set_selection, sector_ind, base_benchmark, 
    print_plot=False, penalty = 'l2', C = 1.0):
    x = pd.concat(data_set_selection,1).resample("M").last().dropna()
    # normalize x
    temp = preprocessing.scale(x,axis=0)
    x = pd.DataFrame(temp,index = x.index,columns = x.columns)
    # return space
    x = x.shift()/x -1

    ##### Make this an input instead
    #sector_ind = read_data(benchmark_str,sector_ind_sheetname)
    print("Sector selected: ")
    sector_str_name = str(sector_ind.columns[0])
    print(sector_str_name)
    sector_ind_ret = sector_ind.apply(np.log).diff()

    sector_ind_ret,base_benchmark =model_data(sector_ind_ret,base_benchmark)
    ##### Should make this an input instead of benchmark
    defeat_benchmark = pd.DataFrame((sector_ind_ret.values>base_benchmark.values),index= sector_ind_ret.index,columns=['beat_benchmark'])*1


    x,y = model_data(x,defeat_benchmark)

    train_index = x.index[0:len(x.index)*3/4]
    validate_index = pd.DatetimeIndex(np.setdiff1d(x.index,train_index))

    logistic  = LogisticRegression(penalty=penalty, C = C)
    logistic.fit(x.loc[train_index,:],y.loc[train_index,:])

    y_predict = pd.DataFrame(logistic.predict(x.loc[validate_index,:]),index = validate_index)

    print("Coeff")
    print(pd.DataFrame(logistic.coef_,columns = x.columns))
    if print_plot:
        plt.figure(figsize=(15,5))
        plt.plot(y,'r-')
        plt.plot(y_predict,'b--')
        plt.show()

    comparison_df = pd.concat([y,y_predict],1).dropna()
    score_df = pd.DataFrame(map(lambda x: x[0]==x[1],comparison_df.values))*1
    score = np.mean(score_df)

    print("prediction rate: "+str(round(score*100,3))+"%")
    print("\n\n")

    return(sector_str_name,score.values[0], logistic)


econ_dir_str = os.path.join(os.getcwd(),'../data/Economics/')
benchmark_str = econ_dir_str+'benchmark_data.xlsx'
spx_str = econ_dir_str+'SP500.csv'

csv_name_list = ['CPI.csv','GDP.csv','DGS10.csv','HPI.csv','PAYEMS.csv','TEDRATE.csv','FEDFUNDS.csv','NETEXP.csv',
'PCE.csv','UNRATE.csv','CSENT.csv','OAS.csv','RECESSION.csv','VIXCLS.csv']
# read csv files, put it into monthly series
for file_name in csv_name_list:
	file_dir_str = econ_dir_str+file_name
	variable_name = file_name.split(".")[0]
	print(variable_name+"defined")
	exec(variable_name+"= quaterly_to_monthly(read_csv('"+file_dir_str+"'))")

spx = read_csv(spx_str).resample("M").last()
spx_ret = spx.apply(np.log).diff().dropna()


data_set_selection = [CPI,GDP,HPI,DGS10,TEDRATE,FEDFUNDS,PCE,UNRATE,RECESSION,VIXCLS]

#Call
#logistic_reg_sector_index(data_set_selection, sector_ind, base_benchmark=spx_ret, print_plot=False)