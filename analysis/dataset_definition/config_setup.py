import argparse
parser = argparse.ArgumentParser() # Instantiate parser

# Configuration for add measures

## Study period configuration
parser.add_argument("--patient_measures", action= 'store_true', help = "Sets measures defaults to patient-level subgroups.")
parser.add_argument("--practice_measures", action= 'store_true', help = "Sets measures defaults to practice-level subrgoups.")
parser.add_argument("--CS", action = 'store_true', help = "The study period is cross-sectional.")
parser.add_argument("--Long", action = 'store_true', help = "The study period is longitudinal.")
## Measures configuration
parser.add_argument("--Age", action = 'store_true', help = "Gets measures for age if flag is added to action.")
parser.add_argument("--Sex", action = 'store_true', help = "Gets measures for sex if flag is added to action.") 
parser.add_argument("--Ethnicity", action = 'store_true', help = "Gets measures for ethnicity if flag is added to action..") 
parser.add_argument("--IMD", action= 'store_true', help = "Gets measures for IMD if flag is added to action.")
parser.add_argument("--Rurality", action= 'store_true', help = "Gets measures for rurality if flag is added to action.")
parser.add_argument("--Smoking", action= 'store_true', help = "Gets measures for smoking if flag is added to action.")
parser.add_argument("--Multimorbidity", action= 'store_true', help = "Gets measures for Multimorbidity if flag is added to action.")
parser.add_argument("--Consultation", action= 'store_true', help = "Gets measures for consultation rate if flag is added to action.")
parser.add_argument("--ec", action= 'store_true', help = "Gets measures for A&E attendance if flag is added to action.")
parser.add_argument("--apc", action= 'store_true', help = "Gets measures for hospital admission if flag is added to action.")
parser.add_argument("--ec_ACSCs", action= 'store_true', help = "Gets measures for A&E due to ACSCs if flag is added to action.")
parser.add_argument("--apc_ACSCs", action= 'store_true', help = "Gets measures for hospitcal admission due to ACSCs if flag is added to action.")

## Configuration for interval date input
parser.add_argument("--start_cohort", help="cohort start date")

args = parser.parse_args() # Stores arguments in 'args'

# Extract arguments into variables
Long = args.Long
CS = args.CS
patient_measures = args.patient_measures
practice_measures = args.practice_measures
Age = args.Age
Sex = args.Sex
Ethnicity = args.Ethnicity
IMD = args.IMD
Rurality = args.Rurality
Smoking = args.Smoking
Multimorbidity = args.Multimorbidity
Consultation = args.Consultation
ec = args.ec
apc = args.apc
ec_ACSCs = args.ec_ACSCs
apc_ACSCs = args.apc_ACSCs
start_cohort = args.start_cohort