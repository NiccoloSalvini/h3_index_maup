import h3
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats

# Create a global hexagonal grid using H3 at different resolutions
def create_hex_grid(resolution):
    hexagons = list(h3.geo_to_h3(-90, -180, resolution))
    for lat in range(-89, 90):
        for lon in range(-179, 180):
            hexagon = h3.geo_to_h3(lat, lon, resolution)
            if hexagon not in hexagons:
                hexagons.append(hexagon)
    return hexagons

# Generate simulated data for each hexagonal cell
def generate_data(hexagons, mu=0, sigma=0.1):
    data = []
    for hexagon in hexagons:
        data.append(np.random.normal(mu, sigma))
    return data

# Assign the simulated data points to the nearest hexagonal cell
def assign_data_to_hexagons(hexagons, data):
    data_dict = {hexagon: val for hexagon, val in zip(hexagons, data)}
    return data_dict

# Calculate the mean value for each hexagonal cell
def calculate_mean_values(hexagons, data_dict):
    means = []
    for hexagon in hexagons:
        data = data_dict.get(hexagon)
        if data is not None:
            means.append(np.mean(data))
        else:
            means.append(np.nan)
    return means

# Aggregate the data to coarser levels of granularity
def aggregate_data(hexagons, data_dict, aggregation_method='sum', new_resolution=0):
    new_hexagons = create_hex_grid(new_resolution)
    new_data_dict = {}
    for new_hexagon in new_hexagons:
        hexagons_in_new_hexagon = list(h3.polyfill(new_hexagon, new_resolution))
        if aggregation_method == 'sum':
            data_in_new_hexagon = sum([data_dict.get(hexagon, 0) for hexagon in hexagons_in_new_hexagon])
        elif aggregation_method == 'mean':
            data_in_new_hexagon = np.mean([data_dict.get(hexagon, np.nan) for hexagon in hexagons_in_new_hexagon])
        new_data_dict[new_hexagon] = data_in_new_hexagon
    return new_data_dict

# Fit a spatial autoregressive model to the data
def spatial_autoregression(hexagons, data_dict):
    X = np.zeros((len(hexagons), len(hexagons)))
    y = np.zeros((len(hexagons), 1))
    
    for i, hexagon_i in enumerate(hexagons):
        weights = h3.h3_neighbors(hexagon_i)
        row = np.zeros((1, len(hexagons)))
        for j, hexagon_j in enumerate(hexagons):
            if hexagon_j in weights:
                row[0, j] = 1 / len(weights)
        X[i,:] = row
        y[i] = data_dict.get(hexagon_i, np.nan)
        
    model = stats.models.spatial_lag.ML_Error(y, X)
    fit_results = model.fit()
    return fit_results

# Perform Monte Carlo simulation for uncertainty analysis
def simulate_uncertainty(hexagons, data_dict, n_simulations=100):
    simulations = np.zeros((n_simulations, len(hexagons)))
    for i in range(n_simulations):
        data = generate_data(hexagons)
        new_data_dict = assign_data_to_hexagons(hexagons, data)
        means = calculate_mean_values(hexagons, new_data_dict)
        simulations[i,:] = means
    return simulations

# Example Usage:
hexagons = create_hex_grid(10)
data = generate_data(hexagons)
data_dict = assign_data_to_hexagons(hexagons, data)
original_means = calculate_mean_values(hexagons, data_dict)

# Aggregate data to coarser level of granularity and calculate means
new_data_dict = aggregate_data(hexagons, data_dict, aggregation_method='sum', new_resolution=5)
new_means = calculate_mean_values(hexagons, new_data_dict)
 
# Fit a spatial autoregressive model to the original data
fit_results = spatial_autoregression(hexagons, data_dict)
spatial_residuals = fit_results.resid_response

# Simulate uncertainty using Monte Carlo method
simulations = simulate_uncertainty(hexagons, data_dict, n_simulations=100)
stds = np.std(simulations, axis=0)

# Plot the results
sns.scatterplot(x=original_means, y=new_means, alpha=0.5)
plt.xlabel('Original Mean Values')
plt.ylabel('New Mean Values')
plt.title('Aggregation of Hexagonal Cells')
plt.show()

sns.scatterplot(x=original_means, y=spatial_residuals, alpha=0.5)
plt.xlabel('Original Mean Values')
plt.ylabel('Spatial Residuals')
plt.title('Spatial Autoregressive Model')
plt.show()

sns.scatterplot(x=original_means, y=stds, alpha=0.5)
plt.xlabel('Original Mean Values')
plt.ylabel('Standard Deviation of Means')
plt.title('Uncertainty Analysis')
plt.show()
