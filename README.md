# PostProcessingECdataLandscapeFlux
This repository contains the code that were used by the landscape flux group to analyse the post processed EC data.

Filenames:

LEfixWay3Correction109slurm.py: This file corrects the Way 3 H2O molar density based on James correction where 1.09 span value was used as the incorrect span values. The file was run to see the plot between the corrected H2O Molar density and incorrect Molar density
SlurmScriptLEFCorrectionWay3.sh: File that runs LEfixWay3Correction109slurm.py on the AHPCC server

LEfixWay3LEcorrected.py: This file corrects the Way 3 H2O molar density and exports the files
SlurmScriptLEFCorrectedWay3.sh: File that runs LEfixWay3LEcorrected.py on the AHPCC server



LEfixWay3Ratioslurm.py: Plots way 3 H2O Molar density ratio
SlurmScriptLEFix1.sh: File that runs LEfixWay3Ratioslurm.py on the AHPCC server
 
LEfixWay4Ratioslurm.py: Plots way 4 H2O Molar density ratio
SlurmScriptLEFix1Way4.sh: File that runs LEfixWay4Ratioslurm.py on the AHPCC server

The workflow:<br>
LE fix function names in the right order<br>
Calculate X_incorrect from the density_incorrect: fsolve(equation, x_initial_guess, args=(a, b, c, density, pressure)) <br>
Calculate Transmittance from the X_incorrect : calculate_transmittance <br>
Calculate X_correct from the transmittance : calculate_x <br>
Calculate the H2O Molar Density from x with the factory corrected span and zero <br>

One way to check the code is to see if the same values are generated using the same values for factory corrected span and zero and spring span and spring zero. <br>
