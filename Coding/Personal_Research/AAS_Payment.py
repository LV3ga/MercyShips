import gspread
import os
import pandas as pd
import io
import google.auth
import base64
import mimetypes
from datetime import datetime
#import smtplib
#from PIL import ImageTk,Image
from email.message import EmailMessage
from email.message import EmailMessage
from email.mime.audio import MIMEAudio
from email.mime.base import MIMEBase
from email.mime.image import MIMEImage
from email.mime.text import MIMEText

from datetime import date, timedelta

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
#from googleapiclient.discovery import build
#from googleapiclient.errors import HttpError
#from google.oauth2 import service_account
from oauth2client.service_account import ServiceAccountCredentials
#from googleapiclient.http import MediaIoBaseDownload



# Connecting to Google Sheets and getting tables
scope = ['https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name('aas_client_secret.json', scope)
client = gspread.authorize(creds)
sheet_id = "1EH9LXXC60qgib1c2hlIrOu7KW7DojhwL1Vi2eNjEabQ"

workbook  = client.open_by_key(sheet_id)
sheet_students = workbook.worksheet("Students")
sheet_classes = workbook.worksheet("Classes")
sheet_payment = workbook.worksheet("Payment History 2026")
students_row_list = sheet_students.get_all_values()
classes_row_list = sheet_classes.get_all_values()
payment_row_list = sheet_payment.get_all_values()
class_selected = "Wednesday Night" 

# Dataframes for classes and students
classes_df = pd.DataFrame(classes_row_list[1:], columns = classes_row_list[0])
students_df = pd.DataFrame(students_row_list[1:], columns = students_row_list[0])
students_email = students_df['Email']
students_name = students_df['Name']





def get_wix_payments(students_email):

    # Get current year and month
    now = datetime.now()
    current_year = str(now.year)
    current_month_num = now.month
    previous_month_num = (current_month_num - 2) % 12 + 1
    current_month = get_month_string(current_month_num)
    previous_month = get_month_string(previous_month_num)
             
    # Data frame for wix website
    wix_data = pd.read_csv("Orders.csv")
    print(wix_data['Contact email'])
    print(wix_data['Date created'])
    print(wix_data['Price'])
    

    # Getting rows with emails from craigs class, from the current year
    # past two months.
    wix_data_AAS = wix_data[['Date created', 'Contact email', 'Price']]
    wix_data_AAS = wix_data_AAS[wix_data_AAS['Contact email'].isin(students_email)]
    wix_data_AAS = wix_data_AAS[wix_data_AAS['Date created'].str.contains(current_year)]
   # wix_data_AAS = wix_data_AAS[wix_data_AAS['Date created'].str.contains('|'.join([current_month, previous_month]))]


    print(wix_data_AAS.columns.get_loc('Date created'))
    
    # Getting data ready for spreadsheet
    wix_data_AAS['Name'] = wix_data_AAS['Contact email'].map(get_name_from_email)
    wix_data_AAS['Method'] = "Wix"
    wix_data_AAS['Month'] = "Jun"

    for i in range(len(wix_data_AAS)):
        wix_data_AAS.iloc[i, wix_data_AAS.columns.get_loc('Month')] = wix_data_AAS.iloc[i, wix_data_AAS.columns.get_loc('Date created')][:3]


    # Renaming columns for updating google sheets
    wix_data_AAS.rename(columns={'Date created': 'Date', 'Contact email' : 'Email', 'Price' : 'Amount'}, inplace=True)


    # Removing Rows with 0 in the Amount
    wix_data_AAS = wix_data_AAS[wix_data_AAS['Amount'] > 0]


    # Reordering Columns for Spreadsheet
    wix_data_AAS = wix_data_AAS[['Name', 'Email', 'Month', 'Date', 'Amount', 'Method']]

    
    
    # Turning output into list of lists for exporting to google sheets
    wix_data_AAS_export = wix_data_AAS.to_numpy().tolist()

    print(wix_data_AAS_export)

    print(payment_row_list)

    # Convert string payments to floats in payment_row_records
    for record in payment_row_list:
        try:
            record[4] = float(record[4])
        except:
            None

    # See if wix_data_AAS_export contains records that are already in the Google Sheets
    wix_data_AAS_export_final = []
    for record in wix_data_AAS_export:
        if record not in payment_row_list:
            wix_data_AAS_export_final.append(record)



    # Exporting to google sheets
    sheet_payment.append_rows(wix_data_AAS_export_final, value_input_option = "USER_ENTERED")


#def get_venmo_payments()


#def get_zelle_payments()


#def organize_all_payments()


#def update_spreadsheet():


#def identify_missed_payments()

def get_month_string(month):
    match month:
        case 1:
            month = "Jan"
        case 2:
            month = "Feb"
        case 3:
            month = "Mar"
        case 4:
            month = "Apr"
        case 5:
            month = "May"
        case 6:
            month = "Jun"
        case 7:
            month = "Jul"
        case 8:
            month = "Aug"
        case 9:
            month = "Sep"
        case 10:
            month = "Oct"
        case 11:
            month = "Nov"
        case 12:
            month = "Dec"

    return month


def get_name_from_email(email):
    return students_name[students_email == email].values[0]

    
get_wix_payments(students_email)

