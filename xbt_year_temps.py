
#
# CAST                        		2102812	WOD Unique Cast Number	WOD code
# NODC Cruise ID              		US-48959
# Originators Station ID      		               			alpha
# Originators Cruise ID
# Latitude                    		-62.13	decimal degrees
# Longitude                   		-60.62	decimal degrees
# Year                        		1969
# Month                       		5
# Day                         		28
# Time                        		0.5	decimal hours (UT)
# METADATA
# Country                     		             US	NODC code	UNITED STATES
# Accession Number            		7500750	NODC code
# Platform                    		2508	OCL code	HERO
# Institute                   		413	NODC code	US DOC NOAA NMFS (WASH; D. C.)
# probe_type                  		2	OCL_code	XBT
# Calibration temperature     		16.5	degrees Celsius
# Digitization method         		2	NODC code 0612	A-D CONVERSION FROM ORIGINAL
# Digitization interval       		1	NODC code 0613	FIXED INTERVAL LE 0.1 METER AND LE 0.1 DEG C
# Data treatment and storage m		24	NODC code 0614	DUAL DIGITIZATION AND AVERAGING; COMPRESSION; FIT WITHIN 0.2 DEG C
# systematic_fix              		5	WOD code	Levitus et al.; 2009 applied (XBT/MBT)
# Database origin             		1	WOD code	NODC archive (1992)
# Instrument       	Temperature	2	WOD code	XBT: TYPE UNKNOWN
# VARIABLES 	Depth     	F	O	Temperatur	F	O
# UNITS     	m         	 	 	degrees C
# Prof-Flag 	          	0	 	          	0
# 1	0	0	 	-1.65	0


def in_circle(center_x, center_y, radius, x, y):
    square_dist = (center_x - x) ** 2 + (center_y - y) ** 2
    return square_dist <= radius ** 2


def progress(count, total, suffix=''):
    bar_len = 60
    filled_len = int(round(bar_len * count / float(total)))

    percents = round(100.0 * count / float(total), 1)
    bar = '=' * filled_len + '-' * (bar_len - filled_len)

    sys.stdout.write('[%s] %s%s ...%s\r' % (bar, percents, '%', suffix))
    sys.stdout.flush()

file_in = ""

Latitude = 0
Longitude = 0

Year = []
Month = 0
Day = 0
depth_temp = {}
depth_range = range(0, 2000, 5)

# location ={'Bismark' : [-64 52.45801, -63 39.5166]
# }
radius = 0.5

data_dic = {}

total_line =
count = 0
for line in open(file_in, "r"):
    progress(count, total_line, suffix='')
    count += 1
    line = line.strip('\n')
    line = line.split('\t')
    if line[0] == 'Latitude':
        Latitude = line[1]
        continue

    elif line[0] == 'Longitude':
        Longitude = line[1]
        continue

    elif line[0] == 'Year':
        Year = line[1]
        continue

    elif line[0] == 'Month':
        Month = line[1]
        continue

    elif line[0] == 'Day':
        Day = line[1]
        continue

    elif line[2] in depth_range:
        depth_temp[line[2]] = data_dic.get(line[5], default=[])
        continue

    elif line[1] == 'END OF VARIABLES SECTION':
        if in_circle(location[NAMEOFPLACE][0], location[NAMEOFPLACE][0], radius, Latitude, Longitude):
            for key in depth_temp.keys():
                data_dic[month][key] = data_dic.get(
                    data_dic[month][key].append(depth_temp[key]), default=[depth_temp[key]])
            # clear out for next vals
            Latitude = 0
            Longitude = 0
            Month = 0
            Day = 0
            depth_temp = {}
            continue
        else:
            # clear out for next vals if values not in r radius around catch
            # sites
            Latitude = 0
            Longitude = 0
            Month = 0
            Day = 0
            depth_temp = {}
            continue
