import plotly.plotly as py
import pandas as pd

df = pd.read_csv(
    'https://raw.githubusercontent.com/plotly/datasets/master/2015_06_30_precipitation.csv')

scl = [0, "rgb(150,0,90)"], [0.125, "rgb(0, 0, 200)"], [0.25, "rgb(0, 25, 255)"],\
    [0.375, "rgb(0, 152, 255)"], [0.5, "rgb(44, 255, 150)"], [0.625, "rgb(151, 255, 0)"],\
    [0.75, "rgb(255, 234, 0)"], [0.875, "rgb(255, 111, 0)"], [
    1, "rgb(255, 0, 0)"]

data = [dict(
    lat=df['Lat'],
    lon=df['Lon'],
    text=df['Globvalue'].astype(str) + ' inches',
    marker=dict(
        color=df['Globvalue'],
        colorscale=scl,
        reversescale=True,
        opacity=0.7,
        size=2,
        colorbar=dict(
            thickness=10,
            titleside="right",
            outlinecolor="rgba(68, 68, 68, 0)",
            ticks="outside",
            ticklen=3,
            showticksuffix="last",
            ticksuffix=" inches",
            dtick=0.1
        ),
    ),
    type='scattergeo'
)]

layout = dict(
    geo=dict(
        scope='north america',
        showland=True,
        landcolor="rgb(212, 212, 212)",
        subunitcolor="rgb(255, 255, 255)",
        countrycolor="rgb(255, 255, 255)",
        showlakes=True,
        lakecolor="rgb(255, 255, 255)",
        showsubunits=True,
        showcountries=True,
        resolution=50,
        projection=dict(
            type='conic conformal',
            rotation=dict(
                lon=-100
            )
        ),
        lonaxis=dict(
            showgrid=True,
            gridwidth=0.5,
            range=[-140.0, -55.0],
            dtick=5
        ),
        lataxis=dict(
            showgrid=True,
            gridwidth=0.5,
            range=[20.0, 60.0],
            dtick=5
        )
    ),
    title='US Precipitation 06-30-2015<br>Source: <a href="http://water.weather.gov/precip/">NOAA</a>',
)
fig = {'data': data, 'layout': layout}
py.iplot(fig, filename='precipitation')
