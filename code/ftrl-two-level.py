# time pypy-2.4 -u runmodel.py | tee output_0.txt
from FM_FTRL_machine import *
import random
from math import log
import sys

random.seed(5)  # seed random variable for reproducibility

reportFrequency = 40000
trainingFile = '../Data/prepared_fe_train.csv'
validatingFile = '../Data/prepared_fe_test.csv'

fm_dim, fm_initDev = 12, .02
    
alpha, beta = .1, 1

alpha_fm, beta_fm = .01, 1.

D = 2 ** 20

L1, L2 = 1.0, .1
L1_fm, L2_fm = 1.0, .1

n_epochs = 1

start = datetime.now()

learner = FM_FTRL_machine(fm_dim, fm_initDev, L1, L2, L1_fm, L2_fm, D, alpha, beta, alpha_fm = alpha_fm, beta_fm = beta_fm)

print("Start Training:")

for e in range(n_epochs):
    learner.L1_fm = 0 if e == 0 else L1_fm
    learner.L2_fm = 0 if e == 0 else L2_fm
    
    progressiveLoss, progressiveCount = 0., 0.

    for t, x, y in data(trainingFile, D):
        p = learner.predict(x)
        loss = logLoss(p, y)
        learner.update(x, p, y)
            
        progressiveLoss += loss
        progressiveCount += 1.
        if t % reportFrequency == 0:                
            print("Epoch %d\tcount: %d\tProgressive Loss: %f" % (e, t, progressiveLoss / progressiveCount))
        
with open('submission.csv', 'w') as output:
    output.write('id,ARR_DEL15\n')
    for t, line in enumerate(DictReader(open(validatingFile), delimiter=',')):
        ID = int(line['id'])
        del line['id']
        
        x = []
        for key in line:
            value = line[key]
            index = abs(hash(key+'_'+str(value))) % D + 1  # zero index for bias term
            x.append(index)
        
        pred = learner.predict(x)
        output.write('%s,%s\n' % (ID, str(pred)))


print("Process finished")
sys.exit(1)
