from urllib.request import urlopen
from json import loads
import pandas as pd
import feather

years = list(range(2009, 2016))
grades = list(range(0, 13))

# first get the leaid for all NY schools
years = [str(y) for y in years]
grades = [str(g) for g in grades]
urls = []

for year in years:
    t = "https://educationdata-stg.urban.org/api/v1/school-districts/ccd/directory/" + year + "/?fips=36"
    urls.append(t)

dir_dat = pd.DataFrame()

for url in urls:
    response = urlopen(url)
    data = loads(response.read())
    next = data['next']
    data = pd.DataFrame.from_dict(data['results'])
    data['years'] = year
    data = data[['leaid', 'year', 'enrollment']]
    dir_dat = dir_dat.append(data)

    while next != None:
        print(next)
        response = urlopen(next)
        data = loads(response.read())
        next = data['next']
        data = pd.DataFrame.from_dict(data['results'])
        data['years'] = year
        data = data[['leaid', 'year', 'enrollment']]
        dir_dat = dir_dat.append(data)

dir_dat = dir_dat.dropna()

type(dir_dat['leaid'])
leaids = [dir_dat['leaid'].drop_duplicates())

urls = []

for year in years:
    t = 'https://educationdata-stg.urban.org/api/v1/school-districts/ccd/enrollment/' + year + '/grade-99/race/'
    urls.append(t)

response = urlopen(urls[0])
data = loads(response.read())
next = data['next']
data['next'] == None
ccd_dat = pd.DataFrame.from_dict(data['results'])
list(ccd_dat.leaid) in (dir_dat.leaid)
any(ccd_dat.leaid.isin(dir_dat.leaid))

ccd_dat = pd.DataFrame()

for url in urls:
    response = urlopen(url)
    data = loads(response.read())
    next = data['next']
    data = pd.DataFrame.from_dict(data['results'])

    if any(data.leaid.isin(dir_dat.leaid)):
        ccd_dat = ccd_dat.append(data)

    while next != None:
        print(next)
        response = urlopen(next)
        data = loads(response.read())
        next = data['next']
        data = pd.DataFrame.from_dict(data['results'])

        if any(data.leaid.isin(dir_dat.leaid)):
            ccd_dat = ccd_dat.append(data)


ccd_dat.shape
ccd_dat = ccd_dat[ccd_dat.leaid.isin(dir_dat.leaid)]
ccd_dat.reset_index(inplace = True)

ccd_dat = ccd_dat.drop(['index'], axis=1)

feather.write_dataframe(ccd_dat, 'ccd_district_data.feather')


# School level dataset #
urls = []

for year in years:
    t = "https://educationdata-stg.urban.org/api/v1/schools/ccd/directory/" + year + "/?fips=36"
    urls.append(t)

dir_dat = pd.DataFrame()

for url in urls:
    response = urlopen(url)
    data = loads(response.read())
    next = data['next']
    data = pd.DataFrame.from_dict(data['results'])
    dir_dat = dir_dat.append(data)

    while next != None:
        print(next)
        response = urlopen(next)
        data = loads(response.read())
        next = data['next']
        data = pd.DataFrame.from_dict(data['results'])
        dir_dat = dir_dat.append(data)

dir_dat = dir_dat.dropna()


# Finance
url = 'https://educationdata-stg.urban.org/api/v1/schools/ccd/enrollment/2009/grade-3/race/sex/'
response = urlopen(url)
data = loads(response.read())
data = pd.DataFrame.from_dict(data['results'])
data['rev_state_total']
data['exp_total'] / data['enrollment_fall_school']
cols = list(data.columns)
