import csv



with open('texasLastStatement.csv') as data:
	spamreader = csv.reader(data, delimiter=',', quotechar='|')
	columns = next(spamreader)
	words = next(spamreader)
	print(words[-1])


def analyzeWords(words):
	score = 0
	