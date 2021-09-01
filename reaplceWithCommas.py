with open('wordSentiment/AFINN-111.txt') as fin, open('newfile.txt', 'w') as fout:
    for line in fin:
        fout.write(line.replace('\t', ','))
