import pandas as pd
import numpy as np

# Load data from CSV files, skipping rows with missing 'Name'
units_df = pd.read_csv('creatures.csv').dropna(subset=['Name'])
weapons_df = pd.read_csv('weapons.csv').dropna(subset=['Name'])

# Function to calculate hits
def calculate_hits(shots, hit_roll):
    return np.sum(np.random.randint(1, 7, shots) >= hit_roll)

# Function to calculate wounds
def calculate_wounds(hits, strength, toughness):
    if strength >= 2 * toughness:
        wound_roll = 2
    elif strength > toughness:
        wound_roll = 3
    elif strength == toughness:
        wound_roll = 4
    elif strength < toughness:
        wound_roll = 5
    else:
        wound_roll = 6
    return np.sum(np.random.randint(1, 7, hits) >= wound_roll)

# Function to calculate unsaved wounds
def calculate_unsaved_wounds(hits, save, inv, ap):
    effective_save = max(save - ap, inv)
    return np.sum(np.random.randint(1, 7, hits) < effective_save)

# Function to calculate damage
def calculate_damage(unsaved_wounds, damage, fnp):
    total_damage = unsaved_wounds * damage
    if fnp > 0:
        fnp_rolls = np.random.randint(1, 7, total_damage)
        total_damage = np.sum(fnp_rolls < fnp)
    return total_damage

# Simulate 5 rounds of shooting
rounds = 5
results = []

for _, unit in units_df.iterrows():
    for _, weapon in weapons_df.iterrows():
        total_unsaved_wounds = 0
        total_damage = 0
        for _ in range(rounds):
            hits = calculate_hits(weapon['Shots'], weapon['Hit'])
            unsaved_wounds = calculate_unsaved_wounds(hits, unit['Save'], unit['Inv'], weapon['AP'])
            damage = calculate_damage(unsaved_wounds, weapon['Damage'], unit['FNP'])
            total_unsaved_wounds += unsaved_wounds
            total_damage += damage
        results.append({
            "Faction": unit['Faction'],
            "Army": unit['Army'],
            "Unit": unit['Name'],
            "Weapon": weapon['Name'],
            "Unsaved Wounds": total_unsaved_wounds,
            "Total Damage": total_damage
        })

# Convert results to DataFrame
results_df = pd.DataFrame(results)

# Save results to CSV
results_df.to_csv('results.csv', index=False)
print("Results have been saved to results.csv")
