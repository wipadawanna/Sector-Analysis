### Directory settings
current_dir = os.getcwd()
os.chdir("..")
benchmark_dir_str = os.path.join(os.getcwd(), 'data/CFRM521_final_project/').replace('\\','/')
bldata_dir_str = benchmark_dir_str + 'BL_Econ/'
econ_dir_str = os.path.join(os.getcwd(), 'data/Economics/').replace('\\','/')
sector_dir_str = os.path.join(os.getcwd(), 'data/SectorRelated/').replace('\\','/')
shelve_dir = os.path.join(os.getcwd(), 'tmp/').replace('\\','/')
os.chdir(current_dir)

benchmark_str = benchmark_dir_str+'benchmark_data.xlsx'
spx_str = benchmark_dir_str+'SPX500.csv'

csv_name_list = ['CPI.csv','GDP.csv','DGS10.csv','HPI.csv','PAYEMS.csv','TEDRATE.csv','FEDFUNDS.csv','NETEXP.csv',
'PCE.csv','UNRATE.csv','CSENT.csv','OAS.csv','RECESSION.csv','VIXCLS.csv']

bldata_name_list = ['OIL.xlsx', 'YIELD_SLOPE.xlsx', 'GOLD.xlsx']

# fred econ data
for csv_file in csv_name_list:
    var_name = csv_file.split(".")[0]
    exec(var_name+" = read_csv( '"+econ_dir_str+csv_file+"',scale = False )")

# # take some series to be returns
# GDP_ret = GDP.shift()/GDP-1
# GDP_ret = quaterly_to_monthly(GDP_ret.dropna())
# GDP_ret.columns=[map(str,GDP.columns)[0]+"_ret"]

# PCE = PCE.resample("M").last()
# PCE_ret = (PCE.shift()/PCE -1).dropna()
# PCE_ret.columns = [map(str,PCE.columns)[0]+"_ret"]

# BLB econ data
# for excel_file in bldata_name_list:
#     var_name = excel_file.split(".")[0]
#     exec(var_name+" = read_data( '" + bldata_dir_str+excel_file+"',scale = False )")

# blb_list = map(lambda x:x.split(".")[0],bldata_name_list)

# OIL=OIL.resample("M").last()
# fred_list = map(lambda x: x.split(".")[0],csv_name_list)
# ret_list = ['GDP','PCE']
# for ret in ret_list:
#     fred_list[fred_list.index(ret)] =ret+"_ret"

# x_list = fred_list+blb_list
# var_list_str =""
# for var in x_list:
#     var_list_str += ','+var
# var_list_str = var_list_str[1:]

# for var in x_list:
#     exec(var+" = quaterly_to_monthly("+var+")")

# exec("x = pd.concat("+"["+var_list_str+"]"+",1)")


# x = x.resample("M").last().dropna()


# y variable

# sector_ind = read_data(benchmark_str,8)
# sector_ind_ret = (sector_ind.shift()/sector_ind - 1).dropna()
# # sector_ind_ret.head()
# spx_ind = read_csv(spx_str)
# spx_ind_ret = (spx_ind.shift()/spx_ind - 1).dropna()
# # spx_ind_ret.head()
# sector_ind_ret,spx_ind_ret = model_data(sector_ind_ret,spx_ind_ret)

# sector_beat_spx = pd.DataFrame((sector_ind_ret.values>spx_ind_ret.values),index = sector_ind_ret.index)*1
# sector_beat_spx.columns = ['sector_beat_spx']
# # sector_beat_spx.head(2)

# # merge data index
# x,sector_beat_spx = model_data(x,sector_beat_spx)

# # x=pd.DataFrame(preprocessing.scale(x),index = x.index,columns =x.columns)
# y=sector_beat_spx