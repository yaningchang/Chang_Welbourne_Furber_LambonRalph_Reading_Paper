%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Please refer to Chang, Welbourne, Furber and Lambon Ralph (2024) paper for the model architecture and training/testing procedures.
The simulations are run using the Lens simulator: https://ni.cmu.edu/~plaut/Lens/

Created by Ya-Ning Chang, 28 Dec 2023
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Simulation_Scripts folder contains all of the files needed to train and test the simulations.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

triangle_reading.in: a script for creating a triangle model of reading
training.in: a script for intact model training
reading_examples.ex.gz: training corpus
PA_Model.in: a script for creating a Pure Alexia (PA) model and recovery
PD_Model.in: a script for creating a Phonological Dyslexia (PD) model and recovery
SD_Model.in: a script for creating a Semantic Dementia (SD) model and recovery
Trained_Weights_for_Intact_Model: Weight files of the 20 versions of the intact model at the end of the reading training 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Simulation_Data folder contains the simulation results and R scripts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Intact:
  intact_naming.csv: word naming data for the intact model
  intact_naming.txt: nonword naming data for the intact model
  intact_freq_con.csv: frequency and consistency data for the intact model
PA:
  PA_naming.csv: word naming data for the PA model
  PA_naming.txt: nonword naming data for the PA model
  PA_freq_con.csv: frequency and consistency data for the PA model
PD:
  PD_naming.csv: word naming data for the PD model
  PD_naming.txt: nonword naming data for the PD model
  PD_freq_con.csv: frequency and consistency data for the PD model
SD:
  SD_naming.csv: word naming data for the SD model
  SD_naming.txt: nonword naming data for the SD model
  SD_freq_con.csv: frequency and consistency data for the SD model
Norms:
  words_lexical_variables: lexical variables for all the English words in the training corpus
  WL357.txt: test stimuli for the word length effects
R_Scripts:
  AllModels_Freq_Con_NW.R: test the frequency and consistency effect and nonword reading for all the models
  AllModels_WL.R: test the word length effect for all the models
  