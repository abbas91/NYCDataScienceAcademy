import os
import functools
from ConfigParser import ConfigParser
from StringIO import StringIO


def parse_config(raw):
    config = ConfigParser()
    if raw.strip().startswith('['):
        config.readfp(StringIO(raw))
    else:
        config.readfp(StringIO('\n[environment]\n' + raw))
        config.get = functools.partial(config.get, 'environment')
    print(type(config))
    return config


def get_config():
    if os.path.isfile('.env'):
        raw_config = open('.env').read()
        print(raw_config)
        config = parse_config(raw_config)
    else:
        config = os.environ

    return config
