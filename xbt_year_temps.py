
import sys
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata


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

file_in = "./WOD.27428.XBT.csv"

Latitude = 0
Longitude = 0

Year = []
Month = 0
Day = 0
depth_temp = {}
depth_range = range(0, 2500, 5)

location = {'Bismark': [-64.866794, -63.650144]
            }
radius = 0.5

data_dic = {}

total_line = sum(1 for line in open(file_in))

count = 0
for line in open(file_in, "r"):
    # progress(count, total_line, suffix='')
    count += 1
    line = line.strip('\n')
    line = line.replace(' ', '')
    line = line.split(',')
    # print line
    if line[0] == 'Latitude':
        Latitude = line[2]
        continue

    elif line[0] == 'Longitude':
        Longitude = line[2]
        continue

    elif line[0] == 'Year':
        Year = line[2]
        continue

    elif line[0] == 'Month':
        Month = line[2]
        continue

    elif line[0] == 'Day':
        Day = line[2]
        continue

    elif line[1][0:-1].isdigit():
        # print line[1]
        if int(line[1][0:-1]) in depth_range:
            # print line[4]
            try:
                depth_temp[int(line[1][0:-1])] = float(line[4])
                continue
            except ValueError:
                continue

    elif line[0] == 'ENDOFVARIABLESSECTION':
        # print Latitude, Longitude, Month, depth_temp
        # print depth_temp.items()
        if in_circle(float(location['Bismark'][0]), float(location['Bismark'][1]), float(radius), float(Latitude), float(Longitude)):
            for depth_key in depth_temp.keys():
                data_dic[Month] = data_dic.get(Month, {})
                data_dic[Month][depth_key] = data_dic[Month].get(
                    depth_key, [])
                data_dic[Month][depth_key].append(depth_temp[depth_key])

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
print "done"

# print data_dic.values()[1:10]

# print data_dic['1'].items()[1:5]


# from the docs:

# If interpolation is None, default to rc image.interpolation. See also
# the filternorm and filterrad parameters. If interpolation is 'none', then
# no interpolation is performed on the Agg, ps and pdf backends. Other
# backends will fall back to 'nearest'.
#
# http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.imshow

# methods = [None, 'none', 'nearest', 'bilinear', 'bicubic', 'spline16',
#            'spline36', 'hanning', 'hamming', 'hermite', 'kaiser', 'quadric',
#            'catrom', 'gaussian', 'bessel', 'mitchell', 'sinc', 'lanczos']

# Convert from pandas dataframes to numpy arrays
X, Y, Z = np.array([]), np.array([]), np.array([])

for month_key in sorted(data_dic.keys()):
    for depth_key in sorted(data_dic[month_key].keys()):
        X = np.append(X, int(month_key))
        Y = np.append(Y, int(depth_key))
        Z = np.append(Z, np.average(data_dic[month_key][depth_key]))

# for i in range(10):
#     print X[i]
#
# for i in range(10):
#     print Y[i]
#
# for i in range(10):
#     print Z[i]


# create x-y points to be used in heatmap
xi = np.linspace(X.min(), X.max(), 100)
yi = np.linspace(Y.min(), Y.max(), 100)

# Z is a matrix of x-y values
zi = griddata((X, Y), Z, (xi[None, :], yi[:, None]), method='cubic')

# I control the range of my colorbar by removing data
# outside of my range of interest
zmin = -3
zmax = 5
zi[(zi < zmin) | (zi > zmax)] = None

# Create the contour plot
CS = plt.contourf(xi, yi, zi, 15, cmap=plt.cm.rainbow,
                  vmax=zmax, vmin=zmin)
ax = plt.gca()
ax.invert_yaxis()
ax.grid(True)
plt.colorbar()
plt.show()
