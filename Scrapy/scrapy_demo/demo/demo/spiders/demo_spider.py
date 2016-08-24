from scrapy import Spider
from scrapy.selector import Selector
from demo.items import DemoItem

class DemoSpider(Spider):
	name = 'demo'
	allowed_urls = ['en.wikipedia.org']
	start_urls = ['https://en.wikipedia.org/wiki/List_of_Academy_Award-winning_films']

	def parse(self, response):
		rows = response.xpath('//*[@id="mw-content-text"]/table[1]/tr').extract()

		for row in rows:
			film = Selector(text=row).xpath('//td[1]/i/a/text()').extract()
			year = Selector(text=row).xpath('//td[2]/a/text()').extract()
			awards = Selector(text=row).xpath('//td[3]/text()').extract()
			nominations = Selector(text=row).xpath('//td[4]/text()').extract()


			item = DemoItem()
			item['film'] = film
			item['year'] = year
			item['awards'] = awards
			item['nominations'] =nominations

			yield item