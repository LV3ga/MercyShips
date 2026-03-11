import plotly.express as px
import pandas as pd

# Getting university dataset
df = pd.read_csv("C:\\MercyShips\\Coding\\Projects\\ETA_Data.csv")

# Generating map
fig = px.scatter_mapbox(df,                                 # dataset
                        lon = df['Longitude'],              # longitude
                        lat = df['Latitude'],               # latitude
                        zoom = 3,                           # starting zoom for map
                        color = df['Accreditation'],      # Assigns color to point based on value in specified column
                        width = 1500,                       # width of display window
                        height = 900)                       # height of display window


fig.update_layout(mapbox_style="open-street-map")           # map used in displaying
fig.update_layout(margin={"r":0, "t":70, "l":0, "b":20})    # margins for map
fig.show()