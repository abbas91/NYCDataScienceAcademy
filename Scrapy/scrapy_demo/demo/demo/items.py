# -*- coding: utf-8 -*-

from scrapy import Item, Field


class DemoItem(Item):
    film = Field()
    year = Field()
    awards = Field()
    nominations = Field()
