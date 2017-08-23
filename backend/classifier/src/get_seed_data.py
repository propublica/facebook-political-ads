"""get_seed_data.py

Usage: python get_seed_data.py [config_file] 

Writes a json file of facebook posts to use as seed to
political/non-political classifier
"""
import csv
import facebook
import json
import os
import requests
import sys

graph_token_url = 'https://graph.facebook.com/oauth/access_token?' \
                  'client_id={}&client_secret={}' \
                  '&grant_type=client_credentials'

def fetch_last_n_posts(pagename, total_posts, graph):
    try:
        posts = graph.request('/'+pagename+'/posts')
    except:
        return []
    page_count = 0
    post_bodies = []
    while posts:
        for post in posts['data']:
            if 'message' in post:
                post_bodies.append(post['message'])
                if len(post_bodies) >= total_posts:
                    break
        if 'paging' in posts and len(post_bodies) < total_posts:
            posts = requests.get(posts['paging']['next']).json()
            page_count += 1
        else :
            break
    return post_bodies    

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("No config file provided!")
        print("Usage: python get_seed_data.py [seed_config_filename]")
        exit()

    with open(sys.argv[1], 'r') as f:
        config = json.load(f)

    res = requests.get(graph_token_url.format(
                        os.environ['FACEBOOK_APP_ID'],
                        os.environ['FACEBOOK_APP_SECRET']))

    access_token = json.loads(res.text)['access_token']
    graph = facebook.GraphAPI(access_token, version=2.7)

    messages = dict()
    messages['political'] = [x.replace('\n', ' ') 
                                for pagename in config['political_fb_pages'] 
                                for x in fetch_last_n_posts(pagename, 
                                                            config['messages_per_page'],
                                                            graph)]
    messages['non_political'] = [x.replace('\n', ' ') 
                                    for pagename in config['non_political_fb_pages'] 
                                    for x in fetch_last_n_posts(pagename,
                                                                config['messages_per_page'],
                                                                graph)]
    with open(config['output_file'], 'w') as f:
        json.dump(messages, f)


