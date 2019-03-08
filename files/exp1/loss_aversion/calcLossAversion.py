import os, string, popen2, re
import numpy as np
import scipy as sp
from scipy import stats
from types import *
from matplotlib import pyplot as plt
import pandas as pd
from pandas import Series,DataFrame
import os.path
import statsmodels.api as sm
import patsy

BEHAV_PATH = os.getcwd()

ALL_SUBS = []#range(403, 406)

for n in range(403, 550):
    filename = BEHAV_PATH+ '/' + str(n) + "_accepted.npy"  
    print filename
    if os.path.isfile(filename) and n not in [413, 512]:
    	print n
        ALL_SUBS.append(n)
    else: 
        continue


print ALL_SUBS
ACCEPT = 1
REJECT = 0

POS_OFFER = 0 # The first part of the offer is always positive
NEG_OFFER = 1

def make_loss_frames():
    for sub in ALL_SUBS:
        
        filename = BEHAV_PATH+'/'+ str(sub)+"_loss_aversion_frame.csv"
        if os.path.isfile(filename):
            print sub,"exists"
        else:
            decisions = []
            pos = []
            neg = []
            accepted = np.load(BEHAV_PATH+'/'+ str(sub)+"_accepted.npy")
            rejected = np.load(BEHAV_PATH+'/'+ str(sub)+"_rejected.npy")
            
            for i,offer in enumerate(accepted):
                decisions.append(ACCEPT)
                pos.append(accepted[i][POS_OFFER])
                neg.append(accepted[i][NEG_OFFER])
            for j,off in enumerate(rejected):
                decisions.append(REJECT)
                pos.append(rejected[j][POS_OFFER])
                neg.append(rejected[j][NEG_OFFER])
            
            frameDict = {'decision':decisions, 'gains':pos, 'losses':neg}
            frame = DataFrame(data =frameDict)
            frame.to_csv(filename, sep='\t')

def calc_betas(filename):
    frame = pd.read_table(filename)
    frame['intercept']=1.0
    train_cols = frame.columns[2:]
    logit = sm.Logit(frame['decision'],frame[train_cols])
    result = logit.fit()
    loss_beta = result.params['losses']
    gain_beta = result.params['gains']
    LAMBDA = loss_beta/gain_beta
    #print result.summary()
    return loss_beta,gain_beta, LAMBDA
    
def main():
    loss_betas = []
    gain_betas = []
    lambdas = []
    make_loss_frames()
    for sub in ALL_SUBS:
        filename = '%s_loss_aversion_frame.csv' % sub
        loss,gain,lam = calc_betas(filename)
        print str(sub) + ":"
        print loss,gain,lam
        loss_betas.append(loss)
        gain_betas.append(gain)
        lambdas.append(lam)
    L_A_dict = {'subNum': ALL_SUBS, 'gain_betas':gain_betas, \
                'loss_betas':loss_betas, 'lambdas':lambdas}
    L_A_frame = DataFrame(data = L_A_dict, index=ALL_SUBS)
    L_A_frame.to_csv('All_lambdas.csv', sep='\t')
if __name__ == '__main__':
    main()
    
    
    
    
    

    
    
