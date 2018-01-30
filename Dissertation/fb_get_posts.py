from facepy import GraphAPI
import facebook
import pandas as pd
import json
import feather
import requests


path = 'fb_group_list.feather'
fb_groups = feather.read_dataframe(path)
ids = fb_groups['ID'] # Keep only the group id numbers
ids = ids.drop([4]) # This group is no longer on FB!

# Split ids into groups to batch process the API resquests
len(ids)
ids_1 = ids[0:12]
ids_2 = ids[12:24]
ids_3 = ids[24:35]
ids_4 = ids[35:]
len(ids_1) + len(ids_2) + len(ids_3) + len(ids_4)


# Prepare API call to Facebook
app_id = '173430183186975'
app_secret = 'a52bef71d6f940cf6d50af5c86775174'

def get_fb_token(app_id, app_secret):
    payload = {'grant_type': 'client_credentials', 'client_id': app_id, 'client_secret': app_secret}
    file = requests.post('https://graph.facebook.com/oauth/access_token?', params = payload)
    #print file.text #to test what the FB api responded with
    result = file.json()['access_token']
    #print file.text #to test the TOKEN
    return result

access_token = get_fb_token(app_id, app_secret)
graph = GraphAPI(access_token) # Access token for FB API

gens_1 = []
len(gens_1)
# Loop over ids in ids_1
for i in ids_1:
    path = i + '/feed?fields=message,created_time,link,comments{created_time,message,like_count,id},shares,type,likes.summary(total_count)'

    gens = graph.get(path=path, page=True, retry=25)

    gens_1.append(gens)

posts_1 = []
for i in gens_1:
    gen = i
    l = []

    for page in gen:
        posts = page['data']
        l.append(posts)

    posts_1.append(l)

len(posts_1)
with open('posts_1.txt', 'w') as outfile:
    json.dump(posts_1, outfile)


# Loop over ids in ids_2
access_token = get_fb_token(app_id, app_secret)

graph = GraphAPI(access_token)
gens_2 = []
del ids_2[20]
len(ids_2)
ids_2[22]
for i in ids_2[1:]:
    path = i + '/feed?fields=message,created_time,link,comments{created_time,message,like_count,id},shares,type,likes.summary(total_count)'

    gens = graph.get(path=path, page=True, retry=25)

    gens_2.append(gens)

posts_2 = []
for i in gens_2:
    gen = i
    l = []

    for page in gen:
        posts = page['data']
        l.append(posts)

    posts_2.append(l)

len(posts_2)
[len(p) for p in posts_2]

# posts_2[0] = '141680156005331'
# posts_2[1] = '211021932327048'
# posts_2[2] = '919482254795282'
# posts_2[3] = '1408400549375189'
# posts_2[4] = '195585637128093'
# posts_2[5] = '637911876408075'
# posts_2[6] = '908368225945135'
# posts_2[7] = '243277169072113'
# posts_2[8] = '1442000276026038'
# posts_2[9] = '1533170270278932'
# posts_2[10] = '446780315402578'


with open('posts_2.txt', 'w') as outfile:
    json.dump(posts_2, outfile)

# Loop over ids in ids_3
access_token = get_fb_token(app_id, app_secret)

graph = GraphAPI(access_token)
len(ids_3)
gens_3 = []
for i in ids_3:
    path = i + '/feed?fields=message,created_time,link,comments{created_time,message,like_count,id},shares,type,likes.summary(total_count)'

    gens = graph.get(path=path, page=True, retry=25)

    gens_3.append(gens)
len(gens_3)
posts_3 = []
for i in gens_3:
    gen = i
    l = []

    for page in gen:
        posts = page['data']
        l.append(posts)

    posts_3.append(l)

len(posts_3)

with open('posts_3.txt', 'w') as outfile:
    json.dump(posts_3, outfile)


# Loop over ids in ids_4
access_token = get_fb_token(app_id, app_secret)

graph = GraphAPI(access_token)
gens_4 = []
for i in ids_4:
    path = i + '/feed?fields=message,created_time,link,comments{created_time,message,like_count,id},shares,type,likes.summary(total_count)'

    gens = graph.get(path=path, page=True, retry=25)

    gens_4.append(gens)

posts_4 = []
for i in gens_4:
    gen = i
    l = []

    for page in gen:
        posts = page['data']
        l.append(posts)

    posts_4.append(l)
len(posts_4)

with open('posts_4.txt', 'w') as outfile:
    json.dump(posts_4, outfile)






t = [[item for sublist in p for item in sublist] for p in posts_1]
t = [item for sublist in t for item in sublist]
t = [json_normalize(i) for i in t]

comments_list = []
for i in t:
    idn = i['id'][0][0:15]
    d = i['comments.data']
    df = pd.DataFrame([])
    for row in d:
        if len(row) == 0:
            continue
        c = pd.DataFrame.from_dict(json_normalize(row), orient='columns')
        c['group_id'] = idn
        df = df.append(c)
    comments.append(df)

t[1].head()
t[1][['created_time', 'id', 'likes.summary.total_count', 'link', 'message', 'shares.count', 'type']].head()

t[1].iloc[:,[7,8,15,16,17,18,19]].head()
