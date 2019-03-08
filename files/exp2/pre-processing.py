import os
import pandas as pd
import numpy as np
from ast import literal_eval
from pandas import Series, DataFrame
from math import sqrt
from scipy import stats


raw_data_folder   = 'data/'
stand_data_folder = '/standard_data/'
cwd = os.getcwd()
LEVENE_SIG = .05
PEN_VALS   = [0, 100, 500]
all_subs   = []

for n in range(516, 600):
	try:
		temp_frame = pd.read_csv('%s%s_reach_test_output.csv' % (raw_data_folder, n), index_col=0)
		if n not in [521, 999]:
			all_subs.append(n)
	except:
		continue

def main():

	subs = []
	space_rem_train = []
	space_rem_test = []
	space_rem = []
	time_rem_train = []
	time_rem_test = []
	time_rem = []

	train_trials = []
	test_trials = []

	for sub in all_subs:
		print(sub)
		print("--------")
		'''0'''
		test_data    = DataFrame()
		train_data   = DataFrame()
		'''1'''
		test_data  = pd.read_csv('%s%s_reach_test_output.csv' % (raw_data_folder, sub))
		train_data = pd.read_csv('%s%s_reach_train_output.csv' % (raw_data_folder, sub)) #index_col = 0
		'''2'''
		add_standard_pokes(test_data)
		add_standard_pokes(train_data)
		'''3'''
		s_time_rem_test  = remove_mistimed_reaches(test_data)
		s_time_rem_train = remove_mistimed_reaches(train_data)

		s_time_rem = s_time_rem_train+s_time_rem_test
		'''4'''
		correct_bias(test_data)
		correct_bias(train_data)
		'''5'''
		reflect_pokes(test_data)
		reflect_pokes(train_data)
		'''6'''
		init_train_var = compute_train_var(train_data)
		init_test_var  = compute_var(test_data)
		init_cond_var  = compute_cond_var(test_data)
		'''7'''
		s_space_rem_train = remove_outliers(train_data, init_train_var)
		s_space_rem_test = remove_outliers(test_data, init_test_var)

		s_space_rem = s_space_rem_test+s_space_rem_train
		'''8'''
		new_train_var = compute_train_var(train_data)
		new_test_var  = compute_var(test_data)
		new_cond_var  = compute_cond_var(test_data)
		'''9'''
		add_train_var(test_data, new_train_var)
		add_test_var(test_data, new_test_var)
		add_cond_var(test_data, new_cond_var)
		'''10'''
		loss_aversion  = get_lambda(sub)
		add_lambda(test_data, loss_aversion)
		'''11'''
		add_test_heuristic_estimate(test_data)
		add_training_heuristic_estimate(train_data, test_data)
		
		train_data['subject'] = [sub]*len(train_data['block'])
		test_data['subject'] = [sub]*len(test_data['block'])
		
		'''12'''
		test_data.to_csv(cwd+stand_data_folder+'%s_standard_test_data.csv' % (sub))
		train_data.to_csv(cwd+stand_data_folder+'%s_standard_train_data.csv' %(sub))
		subs.append(sub)
		# spatial outliers removed
		space_rem_train.append(s_space_rem_train)
		space_rem_test.append(s_space_rem_test)
		space_rem.append(s_space_rem)
		# temporal outliers removed
		time_rem_train.append(s_time_rem_train)
		time_rem_test.append(s_time_rem_test)
		time_rem.append(s_time_rem)
		
		train_trials.append(len(train_data))
		train_trials.append(len(test_data))
		#stand_test_data.to_csv(cwd+stand_data_folder+'%s_standard_no_block_1_test_data.csv' % (sub))


	outlier_dict = {"sub": subs, "space_removed_train": space_rem_train,
					"space_removed_test": space_rem_test, "space_removed_total": space_rem,
					"time_removed_train": time_rem_train, "time_removed_test": time_rem_test,
					"time_removed_total": time_rem}

	#trial_num_dict = {"sub": subs, "train_trials": train_trials,
				   	 # "test_trials": test_trials}

	outlier_df = pd.DataFrame(data = outlier_dict)
	#trial_df = pd.DataFrame(data = trial_num_dict)


#replaces standard poke with NaN if
#reach was too soon or poke was too late
def remove_mistimed_reaches(standard_frame):
	slow_trash = 0
	soon_trash = 0
	for n in range(0, len(standard_frame.index)):
		if standard_frame.iloc[n]['too_slow'] == 1:
			standard_frame.iloc[n, standard_frame.columns.get_loc('standardPokePosX')] = float('nan')
			standard_frame.iloc[n, standard_frame.columns.get_loc('standardPokePosY')] = float('nan')
			slow_trash += 1
		elif standard_frame.iloc[n]['too_soon'] == 1:
			standard_frame.iloc[n, standard_frame.columns.get_loc('standardPokePosX')] = float('nan')
			standard_frame.iloc[n, standard_frame.columns.get_loc('standardPokePosY')] = float('nan')
			soon_trash += 1

	print("Number of slow pokes removed: ")
	print(slow_trash)
	print("Number of soon pokes removed: ")
	print(soon_trash)

	return slow_trash+soon_trash

#Computes and returns the mean of the x and y variances of
#a standardized data set
def compute_var(standard_frame):
	x = standard_frame.standardPokePosX.dropna()
	y = standard_frame.standardPokePosY.dropna()

	x_var = np.nanvar(x)
	y_var = np.nanvar(y)

	if stats.levene(x, y)[1] <= LEVENE_SIG:
		print("X and Y variances are significantly different!")
		var = x_var
	else:
		var = np.mean([x_var, y_var])

	return var

#Computes and returns the mean of the x and y variances of
#last 5 blocks of standard training data
def compute_train_var(standard_frame):
	x = standard_frame[standard_frame['block']>5].standardPokePosX.dropna()
	y = standard_frame[standard_frame['block']>5].standardPokePosY.dropna()

	x_var = np.nanvar(x)
	y_var = np.nanvar(y)

	if stats.levene(x, y)[1] <= LEVENE_SIG:
		print("X and Y variances in training are significantly different!")
		var = x_var
	else:
		var = np.mean([x_var, y_var])

	return var

#Computes the mean of the x and y variances for each condition of a standardized data set
#Returns a dictionary of these condition-specific variances
def compute_cond_var(standard_test_frame):

	cond_vars = {'0':[], '100':[], '500':[]}

	for p in PEN_VALS:
		x = standard_test_frame[standard_test_frame['pen_val'] == -p].standardPokePosX.dropna()
		y = standard_test_frame[standard_test_frame['pen_val'] == -p].standardPokePosY.dropna()

		x_var = np.nanvar(x)
		y_var = np.nanvar(y)

		if stats.levene(x, y)[1] <= LEVENE_SIG:
			print("X and Y variances for condiiton %s are significantly different!" %(p))
			var = x_var
		else:
			var = np.mean([x_var, y_var])

		cond_vars[str(p)].append(var)

	return cond_vars

#Retrieves and returns the loss aversion score for a participant
def get_lambda(subject_number):

	lambda_frame = pd.read_table(cwd + '/loss_aversion/All_lambdas.csv')
	lambda_val   = lambda_frame[lambda_frame['subNum'] == subject_number].lambdas.mean()

	return lambda_val

#Modifies a standardized data set, removing points
#with x or y values outside of a +/- 3 standard deviations away from mean
def remove_outliers(standard_frame, variance):
	trash_count = 0
	sd = sqrt(variance)

	for n in range(0, len(standard_frame.index)):
		x = standard_frame.iloc[n]['standardPokePosX']
		y = standard_frame.iloc[n]['standardPokePosY']

		avg_x = standard_frame.standardPokePosX.mean()
		avg_y = standard_frame.standardPokePosY.mean()

		x_within = avg_x-3*sd < x < avg_x+3*sd
		y_within = avg_y-3*sd < y < avg_y+3*sd

		if not x_within or not y_within:
			standard_frame.iloc[n, standard_frame.columns.get_loc('standardPokePosX')] = float('nan')
			standard_frame.iloc[n, standard_frame.columns.get_loc('standardPokePosY')] = float('nan')
			trash_count += 1

	return trash_count
	print("Number of outlier pokes removed: ")
	print(trash_count)

def add_standard_pokes(raw_data_frame):
	'''
	Alters all pokes to be as though they were responses to a (0, 0) target location
	Adds these 'normal' pokes to the data frame,
	returns standardized data frame with two new columns
	'''
	standard_x_list = []
	standard_y_list = []

	for n in range(0, len(raw_data_frame.index)):

		targ_x = raw_data_frame.iloc[n]['targ_x']
		targ_y = raw_data_frame.iloc[n]['targ_y']

		poke_x = raw_data_frame.iloc[n]['poke_x']
		poke_y = raw_data_frame.iloc[n]['poke_y']

		standard_x = poke_x - targ_x
		standard_y = poke_y - targ_y

		standard_x_list.append(standard_x)
		standard_y_list.append(standard_y)

	raw_data_frame['standardPokePosX'] = standard_x_list
	raw_data_frame['standardPokePosY'] = standard_y_list


def reflect_pokes(standard_data_frame):
	'''
	if the penalty is to the right of the target, reflect the points over the y axis
	this collapses the data across right and left penalty positions
	'''
	standardpokes = []
	for n in range(0, len(standard_data_frame.index)):

		if standard_data_frame.iloc[n]['pen_side'] == 'right':
			standard_x = -standard_data_frame.iloc[n]['standardPokePosX']
		else:
			standard_x = standard_data_frame.iloc[n]['standardPokePosX']

		standardpokes.append(standard_x)

	standard_data_frame['standardPokePosX'] = standardpokes

def compute_bias(standard_data_frame):
	'''
	Computes the vertical and horizontal "bias"
	of the participant's end points
	'''
	x_bias = standard_data_frame.standardPokePosX.mean()
	y_bias = standard_data_frame.standardPokePosY.mean()
	standard_data_frame['x_bias'] = [x_bias]*len(standard_data_frame)
	standard_data_frame['y_bias'] = [y_bias]*len(standard_data_frame)

	return [x_bias, y_bias]


def correct_bias(standard_data_frame):
	'''
	Corrects for the vertical and horizontal biases computed above by
	subtracting the biases from all standard x and y positions
	'''
	[x_bias, y_bias] = compute_bias(standard_data_frame)

	x = standard_data_frame['standardPokePosX']
	x = x - x_bias
	standard_data_frame['standardPokePosX'] = x

	y = standard_data_frame['standardPokePosY']
	y = y - y_bias
	standard_data_frame['standardPokePosY'] = y


#Adds a column of repeating training variance values
def add_train_var(standard_test_frame, training_var):
	size = len(standard_test_frame.index)
	train_var_list = [training_var]*size
	standard_test_frame['train_var'] = train_var_list

#Adds a column of repeating test variance values
def add_test_var(standard_test_frame, test_var):
	size = len(standard_test_frame.index)
	test_var_list = [test_var]*size
	standard_test_frame['test_var'] = test_var_list

#Adds a column of condition-specific variance values
def add_cond_var(standard_test_frame, cond_vars):
	cond_var_list = []
	for n in range(0, len(standard_test_frame.index)):
		pen = standard_test_frame.iloc[n]['pen_val']
		cond_var_list.append(cond_vars[str(-pen)][0])

	standard_test_frame['cond_var'] = cond_var_list

#Adds a column of repeating loss aversion scores
def add_lambda(standard_test_frame, lambda_val):
	size = len(standard_test_frame.index)
	lambda_list = [lambda_val]*size
	standard_test_frame['lambda'] = lambda_list

#Adds columns for x and y pos of heuristic model prediction
#"Pick adequate point in block 1, always aim at this same point thereafter"
def add_test_heuristic_estimate(standard_test_frame):

	#block 1 subframe
	block1_frame = standard_test_frame[standard_test_frame['block'] == 1]

	#let average x,y of block 1 be the x,y aimpoint predicted by our heuristic model
	heur_x = 16#block1_frame.standardPokePosX.mean()
	heur_y = 0#block1_frame.standardPokePosY.mean()

	#add repeating lists of heuristic x,y values to standard data frame
	size = len(standard_test_frame.index)
	standard_test_frame['test_heur_y'] = [heur_y]*size
	standard_test_frame['test_heur_x'] = [heur_x]*size

#Adds columns for x and y pos of heuristic model prediction
#estimate = mean end-point of last 5 blocks of training
def add_training_heuristic_estimate(standard_train_frame, standard_test_frame):

	#last 5 block training subframe
	end_train_frame = standard_train_frame[standard_train_frame['block'] > 5]

	#let average x,y of last 5 blocks of trainig be the x,y aimpoint predicted by our heuristic model
	heur_x = end_train_frame.standardPokePosX.mean()
	heur_y = end_train_frame.standardPokePosY.mean()

	#add repeating lists of heuristic x,y values to standard data frame
	size = len(standard_test_frame.index)
	standard_test_frame['train_heur_y'] = [heur_y]*size
	standard_test_frame['train_heur_x'] = [heur_x]*size


def remove_block_1(standard_test_frame):
	standard_test_frame = standard_test_frame[standard_test_frame['block'] != 1]

if __name__ == '__main__':
	main()
