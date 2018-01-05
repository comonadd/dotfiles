import sys

# Volumes of typical containers (in ml)
TEASPOON_VOL = 4.92892159
TBLSPOON_VOL = 14.7867648
CUP_VOL = 240

# Get the volume and mass
vol = int(sys.argv[1])
mass = int(sys.argv[2])

# Calculate the coefficient
k = mass / vol

# Calculate the amounts
tea_spoon_amount = k * TEASPOON_VOL
tbl_spoon_amount = k * TBLSPOON_VOL
cup_amount = k * CUP_VOL

# Print the result
print('Mass in 1 tea spoon: {}'.format(tea_spoon_amount))
print('Mass in 1 table spoon: {}'.format(tbl_spoon_amount))
print('Mass in 1 cup: {}'.format(cup_amount))
print('Mass in 100ml: {}'.format(k * 100))
