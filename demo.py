import pandas as pd
import os
import openpyxl
print("openpyxl installed successfully!")


directory_path = r'path'
file_paths = [os.path.join(directory_path, f) for f in os.listdir(directory_path) if f.endswith('.txt')]


# Function to process each stock file
def process_stock_file_fixed(file_path):
    # Extract stock code from the filename
    stock_code = os.path.basename(file_path).split('#')[1].split('.')[0]

    # Read the file using fixed width since tab separation seems inconsistent
    with open(file_path, 'r', encoding='GBK') as file:
        lines = file.readlines()

    # Extract stock name from the first line
    stock_name = lines[0].split()[1]

    # Create a DataFrame skipping the first two lines (header and column names)
    data = pd.DataFrame([line.strip().split() for line in lines[2:]],
                        columns=['日期', '开盘', '最高', '最低', '收盘', '成交量', '成交额'])

    # Convert numeric values from string to float for Open and Close
    data['开盘'] = pd.to_numeric(data['开盘'], errors='coerce')
    data['收盘'] = pd.to_numeric(data['收盘'], errors='coerce')

    # Add stock code and name
    data['股票代码'] = stock_code
    data['股票名称'] = stock_name

    # Select required columns
    data = data[['日期', '开盘', '收盘', '股票代码', '股票名称']]
    return data


# Directory where the files are stored
directory_path = r'path'

# List of file paths
file_paths = [os.path.join(directory_path, f) for f in os.listdir(directory_path) if f.endswith('.txt')]

# Process each file and combine them
combined_data_fixed = pd.concat([process_stock_file_fixed(fp) for fp in file_paths], ignore_index=True)

# Path where the combined Excel file will be saved
output_path_fixed = os.path.join(directory_path, 'Combined_Stock_Data_Fixed.xlsx')
combined_data_fixed.to_excel(output_path_fixed, index=False)

print(f'Data combined and saved to {output_path_fixed}')

