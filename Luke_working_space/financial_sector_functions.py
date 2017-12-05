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

def read_data(directory_str,sheet_index=0,scale = False):
    data = pd.read_excel(directory_str,sheet_index)
    df = pd.DataFrame(pd.to_numeric(data.iloc[:,1].replace(".",np.nan)).values,index = data.iloc[:,0].values)
    df.columns = data.iloc[:,[1]].columns
    df = df.dropna()
    # convert index to pd.DatetimeIndex
    if type(df.index) != pd.DatetimeIndex:
        df.index = pd.DatetimeIndex(df.index)
    if scale==True:
        df = pd.DataFrame(preprocessing.scale(df),index = df.index,columns=df.columns)
    return df


def read_csv(directory_str,scale = False):
    data = pd.read_csv(directory_str)
    df = pd.DataFrame(pd.to_numeric(data.iloc[:,1].replace(".",np.nan)).values,index = data.iloc[:,0].values)
    df.columns = data.iloc[:,[1]].columns
    df = df.dropna()
    # convert index to pd.DatetimeIndex
    if type(df.index) != pd.DatetimeIndex:
        df.index = pd.DatetimeIndex(df.index)
    if scale==True:
        df = pd.DataFrame(preprocessing.scale(df),index = df.index,columns=df.columns)
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
    if type(metric_value)==pd.Series:
        metric_value = metric_value.values[0]
    return metric_value

def return_column(dataframe,column_str):
    dataframe.loc[:,[column_str]] = dataframe.loc[:,[column_str]].shift()/dataframe.loc[:,[column_str]] -1
    print(column_str+" changed to return space")

def data_prep(sector_dir_str,sector_sheet_int,spx_dir_str, data_set_selection,return_column_list):
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

    if len(return_column_list)>0:
        for column_str in return_column_list:
            return_column(x,column_str)

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



def KF_logistic_eval(x,y,n = 3,penalty ='l1',C = 1/(10.0),evaluation_metric = 'r2'):
    kf = KFold(n_splits=n)
    kf.get_n_splits(x)

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
    coef_df = pd.DataFrame(columns = x.columns)
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
        out_sample_date_start = str(y_test_model.index.min().date())
        out_sample_date_end = str(y_test_model.index.max().date())
        date_df = pd.DataFrame([out_sample_date_start+" ~ "+out_sample_date_end],index = ['out_sampe'],columns = [select_index -1]).T
        
        model_info = pd.DataFrame(logistic.coef_,columns = x.columns,index = [select_index-1])
    #     pd.concat([date_df,pd.DataFrame(logistic.coef_,columns = x.columns,index = [select_index-1])],1)
        coef_df = pd.concat([coef_df,model_info])

    return(metric_list,coef_df)