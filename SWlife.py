import pandas as pd

############################################################
# Life tables
############################################################

############################################################
# Male life table
############################################################

# survivors from 100000 births
MlivesArray = [100000, 99262, 99213, 99182, 99158, 99138, 99120, 99104, 99089, 99075, 99065, 99056, 99047, 99032, 99007,
               98968, 98912, 98841, 98754, 98654, 98541, 98414, 98275, 98128, 97980, 97834, 97693, 97555, 97419, 97284,
               97147, 97009, 96868, 96724, 96576, 96423, 96264, 96096, 95919, 95729, 95525, 95303, 95062, 94800, 94517,
               94209, 93875, 93513, 93120, 92691, 92224, 91716, 91165, 90572, 89940, 89270, 88558, 87800, 86995, 86138,
               85227, 84254, 83217, 82111, 80935, 79684, 78351, 76929, 75411, 73792, 72066, 70223, 68254, 66161, 63947,
               61612, 59147, 56545, 53811, 50951, 47974, 44882, 41683, 38401, 35063, 31699, 28341, 25024, 21785, 18670,
               15722, 12986, 10501, 8297, 6393, 4794, 3496, 2479, 1711, 1149, 754, 481, 298, 179, 104, 58, 31, 16, 8, 4,
               2, 1, 0, 0, 0, 0, 0, 0, 0, 0]
MlivesSeries = pd.Series(MlivesArray)

# life expectancy
MLEarray = [75.38, 74.94, 73.98, 73.00, 72.02, 71.03, 70.04, 69.05, 68.06, 67.07, 66.08, 65.09, 64.09, 63.10, 62.12,
            61.14, 60.18, 59.22, 58.27, 57.33, 56.40, 55.47, 54.54, 53.63, 52.71, 51.78, 50.86, 49.93, 49.00, 48.07,
            47.13, 46.20, 45.27, 44.33, 43.40, 42.47, 41.54, 40.61, 39.68, 38.76, 37.84, 36.93, 36.02, 35.12, 34.22,
            33.33, 32.45, 31.57, 30.71, 29.84, 28.99, 28.15, 27.32, 26.49, 25.68, 24.87, 24.06, 23.26, 22.48, 21.69,
            20.92, 20.16, 19.40, 18.66, 17.92, 17.19, 16.48, 15.77, 15.08, 14.40, 13.73, 13.08, 12.44, 11.82, 11.21,
            10.62, 10.04, 9.48, 8.94, 8.41, 7.90, 7.41, 6.94, 6.49, 6.06, 5.65, 5.26, 4.89, 4.55, 4.22, 3.92, 3.64,
            3.38, 3.15, 2.93, 2.75, 2.58, 2.44, 2.30, 2.19, 2.07, 1.96, 1.85, 1.75, 1.66, 1.56, 1.47, 1.39, 1.30, 1.22,
            1.15, 1.07, 1.00, 0.94, 0.87, 0.81, 0.75, 0.70, 0.64, 0.59]
MLEseries = pd.Series(MLEarray)

# death probability
MdeathrateArray = [0.007379, 0.000494, 0.000317, 0.000241, 0.000200, 0.000179, 0.000166, 0.000152, 0.000133, 0.000108,
                   0.000089, 0.000094, 0.000145, 0.000252, 0.000401, 0.000563, 0.000719, 0.000873, 0.001017, 0.001148,
                   0.001285, 0.001412, 0.001493, 0.001513, 0.001487, 0.001446, 0.001412, 0.001389, 0.001388, 0.001405,
                   0.001428, 0.001453, 0.001487, 0.001529, 0.001584, 0.001651, 0.001737, 0.001845, 0.001979, 0.002140,
                   0.002323, 0.002526, 0.002750, 0.002993, 0.003257, 0.003543, 0.003856, 0.004208, 0.004603, 0.005037,
                   0.005512, 0.006008, 0.006500, 0.006977, 0.007456, 0.007975, 0.008551, 0.009174, 0.009848, 0.010584,
                   0.011407, 0.012315, 0.013289, 0.014326, 0.015453, 0.016723, 0.018154, 0.019732, 0.021468, 0.023387,
                   0.025579, 0.028032, 0.030665, 0.033467, 0.036519, 0.040010, 0.043987, 0.048359, 0.053140, 0.058434,
                   0.064457, 0.071259, 0.078741, 0.086923, 0.095935, 0.105937, 0.117063, 0.129407, 0.143015, 0.157889,
                   0.174013, 0.191354, 0.209867, 0.229502, 0.250198, 0.270750, 0.290814, 0.310029, 0.328021, 0.344422,
                   0.361644, 0.379726, 0.398712, 0.418648, 0.439580, 0.461559, 0.484637, 0.508869, 0.534312, 0.561028,
                   0.589079, 0.618533, 0.649460, 0.681933, 0.716029, 0.751831, 0.789422, 0.828894, 0.870338, 0.913855]
MdeathrateSeries = pd.Series(MdeathrateArray)

############################################################
# Female life table
############################################################

FlivesArray = [100000, 99390, 99347, 99322, 99303, 99288, 99275, 99262, 99250, 99238, 99228, 99218, 99208, 99196, 99180,
               99160, 99133, 99101, 99064, 99025, 98983, 98939, 98894, 98846, 98796, 98746, 98694, 98640, 98584, 98527,
               98466, 98403, 98336, 98266, 98190, 98108, 98020, 97925, 97822, 97709, 97586, 97452, 97305, 97144, 96968,
               96776, 96566, 96336, 96087, 95819, 95530, 95219, 94885, 94526, 94143, 93737, 93304, 92841, 92342, 91804,
               91220, 90585, 89895, 89147, 88340, 87473, 86537, 85524, 84427, 83236, 81944, 80537, 79008, 77355, 75580,
               73679, 71638, 69441, 67090, 64587, 61930, 59109, 56112, 52942, 49608, 46123, 42504, 38776, 34973, 31141,
               27333, 23610, 20038, 16680, 13593, 10824, 8415, 6384, 4726, 3415, 2411, 1659, 1111, 722, 454, 275, 160,
               90, 48, 24, 11, 5, 2, 1, 0, 0, 0, 0, 0, 0]
FlivesSeries = pd.Series(FlivesArray)

FLEarray = [80.43, 79.92, 78.95, 77.97, 76.99, 76.00, 75.01, 74.02, 73.03, 72.04, 71.04, 70.05, 69.06, 68.07, 67.08,
            66.09, 65.11, 64.13, 63.15, 62.18, 61.20, 60.23, 59.26, 58.29, 57.32, 56.35, 55.38, 54.40, 53.44, 52.47,
            51.50, 50.53, 49.56, 48.60, 47.64, 46.68, 45.72, 44.76, 43.81, 42.86, 41.91, 40.97, 40.03, 39.10, 38.17,
            37.24, 36.32, 35.41, 34.50, 33.59, 32.69, 31.80, 30.91, 30.02, 29.14, 28.27, 27.40, 26.53, 25.67, 24.82,
            23.97, 23.14, 22.31, 21.49, 20.69, 19.89, 19.10, 18.32, 17.55, 16.79, 16.05, 15.32, 14.61, 13.91, 13.22,
            12.55, 11.90, 11.26, 10.63, 10.03, 9.43, 8.86, 8.31, 7.77, 7.26, 6.77, 6.31, 5.87, 5.45, 5.06, 4.69, 4.36,
            4.04, 3.76, 3.50, 3.26, 3.05, 2.87, 2.70, 2.54, 2.39, 2.25, 2.11, 1.98, 1.86, 1.74, 1.62, 1.52, 1.41, 1.31,
            1.22, 1.13, 1.05, 0.97, 0.89, 0.82, 0.75, 0.70, 0.64, 0.59]
FLEseries = pd.Series(FLEarray)

FdeathrateArray =[0.006096, 0.000434, 0.000256, 0.000192, 0.000148, 0.000136, 0.000128, 0.000122, 0.000115, 0.000106,
                  0.000100, 0.000102, 0.000120, 0.000157, 0.000209, 0.000267, 0.000323, 0.000369, 0.000401, 0.000422,
                  0.000441, 0.000463, 0.000483, 0.000499, 0.000513, 0.000528, 0.000544, 0.000563, 0.000585, 0.000612,
                  0.000642, 0.000678, 0.000721, 0.000771, 0.000830, 0.000896, 0.000971, 0.001056, 0.001153, 0.001260,
                  0.001377, 0.001506, 0.001650, 0.001810, 0.001985, 0.002174, 0.002375, 0.002582, 0.002794, 0.003012,
                  0.003255, 0.003517, 0.003782, 0.004045, 0.004318, 0.004619, 0.004965, 0.005366, 0.005830, 0.006358,
                  0.006961, 0.007624, 0.008322, 0.009046, 0.009822, 0.010698, 0.011702, 0.012832, 0.014103, 0.015526,
                  0.017163, 0.018987, 0.020922, 0.022951, 0.025147, 0.027709, 0.030659, 0.033861, 0.037311, 0.041132,
                  0.045561, 0.050698, 0.056486, 0.062971, 0.070259, 0.078471, 0.087713, 0.098064, 0.109578, 0.122283,
                  0.136190, 0.151300, 0.167602, 0.185078, 0.203700, 0.222541, 0.241317, 0.259716, 0.277409, 0.294054,
                  0.311697, 0.330399, 0.350223, 0.371236, 0.393510, 0.417121, 0.442148, 0.468677, 0.496798, 0.526605,
                  0.558202, 0.591694, 0.627196, 0.664827, 0.704717, 0.747000, 0.789422, 0.828894, 0.870338, 0.913855]
FdeathrateSeries = pd.Series(FdeathrateArray)

