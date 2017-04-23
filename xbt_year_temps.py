# -*- coding: utf-8 -*-

import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib import colors

from matplotlib import cm

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

Latitude = 0
Longitude = 0

Year = []
Country = []
Project = []
coordinates_used = []
Month = 0
Day = 0
depth_range = range(0, 2500, 5)

location = {"Bismark_Strait": [-64.866794, -63.650144],
            "Gerlache_Strait": [-64.716771, -63.016668],
            "Hugo_Island": [-64.683492, -65.516931],
            "Dallman_Bay": [-63.900278, -62.76677]}


catch_depths = {"Bismark_Strait": [170, 200],
                "Gerlache_Strait": [150, 400],
                "Hugo_Island": [650, 700],
                "Dallman_Bay": [175, 180]}

# Each degree of latitude is approximately 69 miles (111 kilometers) apart.
radius = 1.5

count_data_sets = 0
# main dictionary for whole dataset
data_dic = {}
##########################################################################
# Main data extraction function


def extract_temp_depth_data(file_in, location_name, radius):
    print " "
    print "Working on:", file_in
    print "Distance around", location_name, location[location_name], ":", radius * 111.045, "km"

    total_line = sum(1 for line in open(file_in, "r"))
    count = 0
    depth_temp = {}
    for line in open(file_in, "r"):
        progress(count, total_line, suffix='')
        count += 1
        line = line.strip('\n')
        line = line.replace(' ', '')
        line = line.split(',')
        if line[0] == 'Latitude':
            global Latitude
            Latitude = line[2]
            continue

        elif line[0] == 'Longitude':
            global Longitude
            Longitude = line[2]
            continue

        elif line[0] == 'Year':
            global Year
            Year_it = line[2]
            continue

        elif line[0] == 'Month':
            global Month
            Month = line[2]
            continue

        elif line[0] == 'Day':
            global Day
            Day = line[2]
            continue

        elif line[1][0:-1].isdigit():
            if int(line[1][0:-1]) in depth_range:
                try:
                    depth_temp[int(line[1][0:-1])] = float(line[4])
                    continue
                except ValueError:
                    continue

        # elif line[0] == 'Project':
        #     global Project
        #
        #     if line[4] not in Project:
        #         Project.append(line[4])
        #     elif line[4] == "DEEPFREEZE":
        #         global Deep_Freeze_years
        #         Deep_Freeze_years.append(Year_it)
        #     else:
        #         continue

        elif line[0] == 'Country':
            global Country
            if line[4] not in Country:
                Country.append(line[4])
            else:
                continue

        elif line[0] == 'ENDOFVARIABLESSECTION':
            # print Latitude, Longitude, Month, depth_temp
            # print depth_temp.items()
            if in_circle(float(location[location_name][0]), float(location[location_name][1]), float(radius), float(Latitude), float(Longitude)):
                # organize data into 3d dictionary
                for depth_key in depth_temp.keys():
                    global data_dic
                    data_dic[Month] = data_dic.get(Month, {})
                    data_dic[Month][depth_key] = data_dic[Month].get(
                        depth_key, [])
                    data_dic[Month][depth_key].append(depth_temp[depth_key])
                # Save meta data
                coordinates_used.append([Latitude, Longitude])
                Year.append(Year_it)

                # clear out for next vals
                Latitude = 0
                Longitude = 0
                Month = 0
                Day = 0
                depth_temp = {}
                global count_data_sets
                count_data_sets += 1
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
##########################################################################

# Make grid graph


def make_grid_graph(data_dictionary, stat, location_name, radius, shade):
    # Convert from pandas dataframes to numpy arrays
    X, Y, Z = np.array([]), np.array([]), np.array([])

    if stat == "count":
        for month_key in sorted(data_dictionary.keys()):
            for depth_key in sorted(data_dictionary[month_key].keys()):
                X = np.append(X, int(month_key))
                Y = np.append(Y, int(depth_key))
                Z = np.append(Z, len(data_dictionary[month_key][depth_key]))

        # create x-y points to be used in heatmap
        xi = np.linspace(X.min(), X.max(), 1000)
        yi = np.linspace(Y.min(), Y.max(), 1000)

        # Z is a matrix of x-y values
        zi = griddata((X, Y), Z, (xi[None, :], yi[:, None]), method='linear')

        #  methods : linear, nearest, cubic

        # I control the range of my colorbar by removing data
        # outside of my range of interest
        zmin = 0
        # zmax = 10000
        # zi[(zi < zmin) | (zi > zmax)] = None

        # Create the contour plot
        CS = plt.contourf(xi, yi, zi, 20, cmap='plasma')
        # vmax=zmax, vmin=zmin)

        ax = plt.gca()
        months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        plt.xticks(np.arange(1, 13, 1.0), months, size='small', rotation=17)

        zc = CS.collections[6]
        plt.setp(zc, linewidth=4)
        ax.invert_yaxis()
        if shade == 1:
            ax.fill_between(np.arange(1, 13, 1.0), catch_depths[location_name][0], catch_depths[
                location_name][1], color="", edgecolor="yellow", linewidth=2)
        ax.grid(True)
        ax.set_xlabel('Month')
        ax.set_ylabel('Depth (m)')
        ax.set_title("%s" % (location_name))
        cb = plt.colorbar()
        cb.set_label("# of Data Points")
    elif stat == "variance":
        for month_key in sorted(data_dictionary.keys()):
            for depth_key in sorted(data_dictionary[month_key].keys()):
                X = np.append(X, int(month_key))
                Y = np.append(Y, int(depth_key))
                Z = np.append(Z, np.var(data_dictionary[month_key][depth_key]))

        # create x-y points to be used in heatmap
        xi = np.linspace(X.min(), X.max(), 1000)
        yi = np.linspace(Y.min(), Y.max(), 1000)

        # Z is a matrix of x-y values
        zi = griddata((X, Y), Z, (xi[None, :], yi[:, None]), method='linear')

        #  methods : linear, nearest, cubic

        # I control the range of my colorbar by removing data
        # outside of my range of interest
        zmin = 0
        # zmax = 10000
        # zi[(zi < zmin) | (zi > zmax)] = None

        # Create the contour plot
        CS = plt.contourf(xi, yi, zi, 20, cmap='viridis')
        # vmax=zmax, vmin=zmin)

        ax = plt.gca()
        months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        plt.xticks(np.arange(1, 13, 1.0), months, size='small', rotation=17)

        zc = CS.collections[6]
        plt.setp(zc, linewidth=4)
        ax.invert_yaxis()
        if shade == 1:
            ax.fill_between(np.arange(1, 13, 1.0), catch_depths[location_name][0], catch_depths[
                location_name][1], color="", edgecolor="yellow", linewidth=2)
        ax.grid(True, color='dimgrey')
        ax.set_xlabel('Month')
        ax.set_ylabel('Depth (m)')
        ax.set_title("%s" % (location_name))
        cb = plt.colorbar()
        cb.set_label("Variance ($^\circ$C)")
    else:
        for month_key in sorted(data_dictionary.keys()):
            for depth_key in sorted(data_dictionary[month_key].keys()):
                X = np.append(X, int(month_key))
                Y = np.append(Y, int(depth_key))
                Z = np.append(Z, np.mean(
                    data_dictionary[month_key][depth_key]))
        # create x-y points to be used in heatmap
        xi = np.linspace(X.min(), X.max(), 1000)
        yi = np.linspace(Y.min(), Y.max(), 1000)

        # Z is a matrix of x-y values
        zi = griddata((X, Y), Z, (xi[None, :], yi[:, None]), method='linear')

        # I control the range of my colorbar by removing data
        # outside of my range of interest
        zmin = -2
        zmax = 2
        zi[(zi < zmin) | (zi > zmax)] = None

        # Create the contour plot
        CS = plt.contourf(xi, yi, zi, 20, cmap='bwr',
                          vmax=zmax, vmin=zmin, extend="both")

        ax = plt.gca()
        # plt.xticks(np.arange(1, 13, 1.0))
        months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        plt.xticks(np.arange(1, 13, 1.0), months, size='small', rotation=17)
        color_ticks = np.linspace(-2, 2, 9, endpoint=True)

        zc = CS.collections[6]
        plt.setp(zc, linewidth=4)
        ax.invert_yaxis()
        ax.grid(True)

        ax.set_xlabel('Month')
        ax.set_ylabel('Depth (m)')
        ax.set_title("%s" % (location_name))
        # cb = plt.colorbar(cmap='bwr', orientation='vertical', ticklocation='auto', extend='both',
        # spacing='uniform', ticks=color_ticks, format=None, drawedges=False,
        # filled=True, extendfrac=None, extendrect=False, label="Temperature
        # ($^\circ$C)")
        CS.cmap.set_under('blue')
        CS.cmap.set_over('red')
        CS.set_clim(-2, 2)
        plt.colorbar(label="Temperature ($^\circ$C)")
        if shade == 1:
            ax.fill_between(np.arange(1, 13, 1.0), catch_depths[location_name][0], catch_depths[
                location_name][1], color="", edgecolor="yellow", linewidth=2)
    output_dir = "./"
    if shade == 1:
        plt.savefig(output_dir +
                    "Monthly_Temp_v_Depth_%s_radius_%s_%s_SHADE.png" % (location_name, radius, stat), dpi=200, rasterized=True)
    else:
        plt.savefig(output_dir +
                    "Monthly_Temp_v_Depth_%s_radius_%s_%s.png" % (location_name, radius, stat), dpi=200, rasterized=True)
    plt.close()
##########################################################################

# Import data
# Xbt data


def run_all_for_rad(location_name, radius, shade):
    files_location = './WOD_2_all_sensors/'
    file_list = ['use_ocldb1483123265.6373.APB.csv',
                 'use_ocldb1483123265.6373.CTD.csv',
                 'use_ocldb1483123265.6373.GLD.csv',
                 'use_ocldb1483123265.6373.MBT.csv',
                 'use_ocldb1483123265.6373.OSD.csv',
                 'use_ocldb1483123265.6373.PFL.csv',
                 'use_ocldb1483123265.6373.XBT.csv']
    #file_list = ['use_ocldb1483123265.6373.XBT.csv']
    for file_name in file_list:
        file_it = files_location + file_name
        extract_temp_depth_data(file_it, location_name, radius)
        print ""

    print ""
    print "Data set spans from  year ", min(Year), "to", max(Year)
    print "Months covered with data:"
    for i in sorted(data_dic.keys()):
        print i
    print ""
    print "Number of XBT/CTD casts used:", count_data_sets

    # Output meta data source:
    country_out = open("Monthly_Temp_v_Depth_%s_radius_%s_SOURCE_country.tsv" %
                       (location_name, radius), 'w')
    for country_val in Country:
        country_out.write("%s\n" % (country_val))
    country_out.close

    project_out = open("Monthly_Temp_v_Depth_%s_radius_%s_SOURCE_Project.tsv" %
                       (location_name, radius), 'w')
    for project_val in Project:
        project_out.write("%s\n" % (project_val))
    project_out.close

    time_span_out = open("Monthly_Temp_v_Depth_%s_radius_%s_SOURCE_time_span.txt" %
                         (location_name, radius), 'w')
    time_span_out.write("Data set spans from  year %s to %s \n" %
                        (min(Year), max(Year)))
    time_span_out.write("Months covered with data:\n")
    for i in sorted(data_dic.keys()):
        time_span_out.write("%s\n" % (i))
    time_span_out.write("\n")
    time_span_out.write("Number of XBT/CTD casts used:%s \n" %
                        (count_data_sets))
    # time_span_out.write("Project: DeepFreeze years: %s to %s" %
    #                     (min(Deep_Freeze_years), max(Deep_Freeze_years)))
    time_span_out.close

    make_grid_graph(data_dic, "stat", location_name, radius, shade)

    make_grid_graph(data_dic, "count", location_name, radius, shade)

    make_grid_graph(data_dic, "variance", location_name, radius, shade)
    # CLEAR dictionary
    global data_dic
    data_dic = {}
    global Year
    Year = []
    global Project
    Project = []
    global Country
    Country = []
    # global Deep_Freeze_years
    # Deep_Freeze_years = []
    global coordinates_used

    cordout = open("Monthly_Temp_v_Depth_%s_radius_%s.tsv" %
                   (location_name, radius), 'w')
    for cord in coordinates_used:
        cordout.write("%s\t%s\n" % (cord[0], cord[1]))
    cordout.close
    coordinates_used = []
    global count_data_sets
    count_data_sets = 0

for loc in location.keys():
    #run_all_for_rad(loc, 0.5,1)
    run_all_for_rad(loc, 1, 0)
    run_all_for_rad(loc, 1, 1)  # <- RUN FOR BEST FIGS
    # run_all_for_rad(loc, 1.5)
