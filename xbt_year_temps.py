
import sys
import matplotlib.pyplot as plt
import numpy as np


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
    #progress(count, total_line, suffix='')
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
            print line[4]
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

print data_dic['1'].items()[1:5]


# from the docs:

# If interpolation is None, default to rc image.interpolation. See also
# the filternorm and filterrad parameters. If interpolation is 'none', then
# no interpolation is performed on the Agg, ps and pdf backends. Other
# backends will fall back to 'nearest'.
#
# http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.imshow

methods = [None, 'none', 'nearest', 'bilinear', 'bicubic', 'spline16',
           'spline36', 'hanning', 'hamming', 'hermite', 'kaiser', 'quadric',
           'catrom', 'gaussian', 'bessel', 'mitchell', 'sinc', 'lanczos']

grid = np.random.rand(4, 4)

fig, axes = plt.subplots(3, 6, figsize=(12, 6),
                         subplot_kw={'xticks': [], 'yticks': []})

fig.subplots_adjust(hspace=0.3, wspace=0.05)

for ax, interp_method in zip(axes.flat, methods):
    ax.imshow(grid, interpolation=interp_method)
    ax.set_title(interp_method)

plt.show()
