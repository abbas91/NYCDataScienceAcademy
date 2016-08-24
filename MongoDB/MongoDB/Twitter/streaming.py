from config import get_config
from db import save_to_mongo
import tweepy
import time
import json

env = get_config()

consumer_key = env.get('CONSUMER_KEY')
consumer_secret = env.get('CONSUMER_SECRET')
access_token = env.get('ACCESS_TOKEN')
access_token_secret = env.get('ACCESS_TOKEN_SECRET')

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth)

# Check the documentation here: http://tweepy.readthedocs.io/en/v3.5.0/
# Override tweepy.StreamListener to add logic to on_data, on_status, on_error

class MyStreamListener(tweepy.StreamListener):

    def on_status(self, status):
        print(status.text)

    def on_data(self, data):

        try:
            tweet = json.loads(data)
            save_to_mongo(tweet, 'twitter', 'tweets')
        except(BaseException, e):
            print(e)
            time.sleep(2)
            pass

    def on_error(self, status):
        print(status)


def main():
    mystreamlistener = MyStreamListener()
    mystream = tweepy.Stream(auth=api.auth, listener=mystreamlistener)

    hash_list = ['Trump', 'Hillary']
    mystream.filter(track=hash_list)

if __name__ == '__main__':
    main()
