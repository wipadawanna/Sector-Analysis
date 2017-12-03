import pandas as pd
import numpy as np
import os
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import KFold

import matplotlib.pyplot as plt
from sklearn import preprocessing
from sklearn import metrics

import statsmodels.api as sm

def read_data(directory_str,sheet_index=0):
    data = pd.read_excel(directory_str,sheet_index)
    df = pd.DataFrame(pd.to_numeric(data.iloc[:,1].replace(".",np.nan)).values,index = data.iloc[:,0].values)
    df.columns = data.iloc[:,[1]].columns
    df = df.dropna()
    # convert index to pd.DatetimeIndex
    if type(df.index) != pd.DatetimeIndex:
        df.index = pd.DatetimeIndex(df.index)
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

#############################

def mean_squared_log_error(y_true,y_pred):
    diff = (y_true+1).apply(np.log).values - (y_pred+1).apply(np.log).values
    return np.sqrt(np.mean(diff**2))

def accuracy_score_function(y_true, y_pred):
    temp = pd.concat([y_true, y_pred],1).dropna()
    score_df = pd.DataFrame(map(lambda x: x[0]==x[1],temp.values))*1
    score = np.mean(score_df)
    return(score)

def evaluate_metric(y_true,y_pred,function='mse'):
    if function == 'evs':
        metric_value = metrics.explained_variance_score(y_true, y_pred)
    elif function == 'mae':
        metric_value = metrics.mean_absolute_error(y_true, y_pred)
    elif function == 'mse':
        metric_value = metrics.mean_squared_error(y_true, y_pred)
    elif function == 'msle':
        metric_value = mean_squared_log_error(y_true, y_pred)
    elif function == 'mdae':
        metric_value = metrics.median_absolute_error(y_true, y_pred)
    elif function == 'r2':
        metric_value = metrics.r2_score(y_true, y_pred)
    elif function =='acf':
        metric_value = accuracy_score_function(y_true, y_pred)
    return metric_value

def data_prep(sector_dir_str,sector_sheet_int,spx_dir_str, data_set_selection):
    sector_ind = read_data(sector_dir_str,sector_sheet_int)
    sector_ind_ret = sector_ind.shift()/sector_ind - 1
    
    spx = read_csv(spx_dir_str).resample("M").last()
    spx_ret = spx.shift()/spx -1

    sector_ind_ret,spx_ret = model_data(sector_ind_ret,spx_ret)
    # defeat_benchmark = pd.DataFrame((sector_ind_ret.values>spx_ret.values),
    #                                 index= sector_ind_ret.index,columns=['beat_benchmark'])*1
    defeat_benchmark = pd.DataFrame((sector_ind_ret.values),
                                    index= sector_ind_ret.index,columns=['beat_benchmark'])*1
    
    x = pd.concat(data_set_selection,1).dropna()
    #x = pd.DataFrame(preprocessing.scale(x), index = x.index, columns=x.columns)
    #x = x.shift()/x -1
    x,y = model_data(x,defeat_benchmark)
    #y = pd.DataFrame(preprocessing.scale(y), index = y.index, columns=y.columns)
    return(x,y)

def K_Fold_evaluation_logistic(x,y,n,penalty,C,evaluation_metric='mse'):
    kf = KFold(n_splits=n)
    kf.get_n_splits(x)

    # x : train_dict[key][0]
    # y : train_dict[key][1]
    train_dict ={}
    test_dict = {}
    ind = 1
    for train_index, test_index in kf.split(x):
        x_train, x_test = x.iloc[train_index,:],x.iloc[test_index,:]
        y_train, y_test = y.iloc[train_index,:],y.iloc[test_index,:]

        train_dict[ind] = [x_train,y_train]
        test_dict[ind] = [x_test,y_test]
        ind += 1
        
    metric_list =[]
    for select_index in range(1,n+1):
        x_train_model = train_dict[select_index][0]
        x_test_model = test_dict[select_index][0]

        y_train_model = train_dict[select_index][1]
        y_test_model = test_dict[select_index][1]


        logistic  = LogisticRegression(penalty=penalty, C = C)
        logistic.fit(x_train_model,y_train_model)

        y_predict = pd.DataFrame(logistic.predict(x_test_model),index = x_test_model.index)
        metric_value = evaluate_metric(y_test_model,y_predict,evaluation_metric)
        metric_list.append(metric_value)
    
    # the higher the better
    overall_metric = np.mean(metric_list)
    # np.prod(map(lambda x: x,metric_list))
    out_df = pd.DataFrame([penalty,1/C,overall_metric],index = ['penalty','lambda','overall_metric']).T
    return(out_df)

def K_Fold_evaluation_linear(x,y,n,evaluation_metric='r2'):
    kf = KFold(n_splits=n)
    kf.get_n_splits(x)

    # x : train_dict[key][0]
    # y : train_dict[key][1]
    train_dict ={}
    test_dict = {}
    ind = 1
    for train_index, test_index in kf.split(x):
        x_train, x_test = x.iloc[train_index,:],x.iloc[test_index,:]
        y_train, y_test = y.iloc[train_index,:],y.iloc[test_index,:]

        train_dict[ind] = [x_train,y_train]
        test_dict[ind] = [x_test,y_test]
        ind += 1
        
    metric_list =[]
    res_list = []
    for select_index in range(1,n+1):
        x_train_model = train_dict[select_index][0]
        x_test_model = test_dict[select_index][0]

        y_train_model = train_dict[select_index][1]
        y_test_model = test_dict[select_index][1]


        # linear = LinearRegression()
        # linear.fit(x_train_model,y_train_model)

        # import pdb
        # pdb.set_trace()
        linear = sm.OLS(y_train_model, x_train_model)
        results = linear.fit()

        res_list.append(results)

        y_predict = pd.DataFrame(results.predict(x_test_model),index = x_test_model.index)
        metric_value = evaluate_metric(y_test_model,y_predict,evaluation_metric)
        metric_list.append(metric_value)
    
    # the higher the better
    overall_metric = np.mean(metric_list)
    # np.prod(map(lambda x: x,metric_list))
    out_df = pd.DataFrame([overall_metric],index = ['overall_metric']).T
    return(res_list, out_df)


### Directory settings
current_dir = os.getcwd()
os.chdir("..")
benchmark_dir_str = os.path.join(os.getcwd(), 'data/CFRM521_final_project/').replace('\\','/')
bldata_dir_str = benchmark_dir_str + 'BL_Econ/'
econ_dir_str = os.path.join(os.getcwd(), 'data/Economics/').replace('\\','/')
shelve_dir = os.path.join(os.getcwd(), 'tmp/').replace('\\','/')
os.chdir(current_dir)

benchmark_str = benchmark_dir_str+'benchmark_data.xlsx'
spx_str = benchmark_dir_str+'SPX500.csv'

csv_name_list = ['CPI.csv','GDP.csv','DGS10.csv','HPI.csv','PAYEMS.csv','TEDRATE.csv','FEDFUNDS.csv','NETEXP.csv',
'PCE.csv','UNRATE.csv','CSENT.csv','OAS.csv','RECESSION.csv','VIXCLS.csv']

bldata_name_list = ['OIL.xlsx', 'YIELD_SLOPE.xlsx', 'GOLD.xlsx']
# read csv files, put it into monthly series
for file_name in csv_name_list:
	file_dir_str = econ_dir_str+file_name
	variable_name = file_name.split(".")[0]
	print(variable_name+"defined")
	exec(variable_name+"= quaterly_to_monthly(read_csv('"+file_dir_str+"'))")

for file_name in bldata_name_list:
    file_dir_str = bldata_dir_str + file_name
    variable_name = file_name.split(".")[0]
    print(variable_name+"defined")
    exec(variable_name+"= quaterly_to_monthly(read_data('"+file_dir_str+"'))")    

#data_set_selection = [CPI,GDP,HPI,DGS10,TEDRATE,FEDFUNDS,PCE,UNRATE,RECESSION,VIXCLS,OIL,YIELD_SLOPE,GOLD]

# econ_data_dict = {
#     'CPALTT01USQ657N' : CPI,
#     'GDP': GDP,
#     'CSUSHPINSA': HPI
# }
data_set_selection = [GDP,HPI,DGS10,TEDRATE,FEDFUNDS,PCE,UNRATE,RECESSION,VIXCLS,OIL,YIELD_SLOPE,GOLD]


## Save data_set_selection to shelve
print shelve_dir + 'data_set_selection.out'
file_shelve = shelve_dir + 'data_set_selection.out'
store_variable(file_shelve, ['data_set_selection'], {'data_set_selection': data_set_selection}, option = 'n')


x,y = data_prep(benchmark_str,8,spx_str, data_set_selection)

n = 6
# index = 1
# penalty_list = ['l1','l2']
# C_list = [1e-5,1e-4,1e-3,1e-2,1e-1,1,10.0,100.0,1000.0]
# overall_df = pd.DataFrame(index = map(lambda x:1/x,C_list),columns=penalty_list)
# overall_df.index.name = 'lambda'
# overall_df.columns.name = 'penalty'
# for penalty in penalty_list:
#     for C in C_list:
#         result_df = K_Fold_evaluation(x,y,n,penalty,C,evaluation_metric='acf')
#         result_df.index = [index]
#         index += 1
#         overall_df.loc[1/C,penalty]=result_df.loc[:,'overall_metric'].values[0]

# print overall_df
data = pd.concat([y,x],1)
data.to_csv('C:/Users/globa/OneDrive/Documents/CFRM/CFRM521/xydata.csv')

#result_df = K_Fold_evaluation_logistic(x,y,n,penalty = 'l2',C = 1e-1, evaluation_metric='acf')
#results_list, res = K_Fold_evaluation_linear(x,y,n,evaluation_metric='r2')
# linear = sm.OLS(y, x)
# results = linear.fit()