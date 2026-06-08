import gspread
import os
import pandas as pd
import io
import google.auth
import base64
import mimetypes
import smtplib
from tkinter import *
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
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
from google.oauth2 import service_account
from oauth2client.service_account import ServiceAccountCredentials
from googleapiclient.http import MediaIoBaseDownload

#Functions
def show():
    myLabel = Label(root, text=clicked.get()).pack()


def get_next_saturday(start_date=None):
    if start_date is None:
        start_date = date.today()
        
    # Saturday is 5 (Monday is 0, Sunday is 6)
    # If today is Saturday, this calculates the date of next week's Saturday
    days_ahead = 5 - start_date.weekday()
    if days_ahead <= 0: 
        days_ahead += 7
        
    return start_date + timedelta(days=days_ahead)



def send_email(message, data):
    email = "Lucasvega771@gmail.com"
    receiver_email = ["lucas.vega.acting@gmail.com", "lucasvega771@gmail.com"]
    subject = "Class " + str(get_next_saturday())
    final_message = "Hello all! \n\n Here are the assignments for this week: \n\n\n " + message + "\n\n Reach out with any questions! \n\n-AAS"
    text = f"Subject: {subject}\n\n{final_message}"
    server = smtplib.SMTP("smtp.gmail.com", 587)
    server.starttls()

    msg = EmailMessage()
    msg['Subject'] = subject
    msg['From'] = email
    msg['To'] = receiver_email
    msg.set_content(text)

    file_paths = list(set([data[0][1], data[1][1], data[2][1]]))
    
    for i in range(len(file_paths)):
        with open(file_paths[i], 'rb') as f:
            file_data = f.read()
            file_name = os.path.basename(file_path)

        msg.add_attachment(
            file_data,
            filename = file_name
        )


    #Sending email
    server.login(email, "xejp wftc foos wguz") # this is the key you get from google App Password page.
    server.sendmail(email, receiver_email, text)
    print("The email has been sent.")

    

def save_data(name1_label, select_scene1, select_partner1, character1,
              name2_label, select_scene2, select_partner2, character2,
              name3_label, select_scene3, select_partner3, character3):
    cdata = [name1_label, select_scene1, select_partner1, character1]
    jdata = [name2_label, select_scene2, select_partner2, character2]
    ldata = [name3_label, select_scene3, select_partner3, character3]

    data = [cdata, jdata, ldata]
    scene_data_final = ""

    for i in range(len(data)):
        if data[i][0] == data[i][2]:
            scene_data_final = scene_data_final + data[i][0] + " will work " + data[i][3] + " in " + data[i][1] + "\n"
            continue
        for j in range(i, len(data)):
            if data[i][2] == data[j][0]:
                scene_data_final = scene_data_final + data[i][0] + " (" + data[i][3] + ") and " + data[j][0] + " (" + data[j][3] + ") will work the scene in " + data[i][1] +  "\n"

                
    send_email(scene_data_final, data)
                
                

   
    
def get_class_time(root, selected_class):
    root.destroy()
    
    selected_students_df = students_df[students_df["Class"].str.contains(selected_class, na=False)]
    name_options = selected_students_df["Name"]
    #name_options.loc[-1] = ["None"]
    root_assignments = Tk()

    
    name1_label = Label(root_assignments, text = "Craig").grid(row = 0, column = 0)
    partner1 = StringVar()
    partner1.set("Select Partner")
    select_partner1 = OptionMenu(root_assignments, partner1, *name_options).grid(row = 0, column = 1)

    scene_label1 = StringVar()
    scene_label1.set("Select Scene")
    select_scene1 = OptionMenu(root_assignments, scene_label1, *scene_list).grid(row = 0, column = 2)

    default_char1 = StringVar()
    default_char1.set("Enter Character")
    character1 = Entry(root_assignments, textvariable = default_char1).grid(row = 0, column = 3)

    
    name2_label = Label(root_assignments, text = "Jessica Culaciati").grid(row = 1, column = 0)
    partner2 = StringVar()
    partner2.set("Select Partner")
    select_partner2 = OptionMenu(root_assignments, partner2, *name_options).grid(row = 1, column = 1)

    scene_label2 = StringVar()
    scene_label2.set("Select Scene")
    select_scene2 = OptionMenu(root_assignments, scene_label2, *scene_list).grid(row = 1, column = 2)

    default_char2 = StringVar()
    default_char2.set("Enter Character")
    character2 = Entry(root_assignments, textvariable = default_char2).grid(row = 1, column = 3)

    
    name3_label = Label(root_assignments, text = "Lucas Vega").grid(row = 2, column = 0)
    partner3 = StringVar()
    partner3.set("Select Partner")
    
    select_partner3 = OptionMenu(root_assignments, partner3, *name_options).grid(row = 2, column = 1)
    scene_label3 = StringVar()
    scene_label3.set("Select Scene")
    select_scene3 = OptionMenu(root_assignments, scene_label3, *scene_list).grid(row = 2, column = 2)

    default_char3 = StringVar()
    default_char3.set("Enter Character")
    character3 = Entry(root_assignments, textvariable = default_char3).grid(row = 2, column = 3)



    btn = Button(root_assignments, text = "submit", command = lambda:save_data("Craig Nigh", scene_label1.get(), partner1.get(), default_char1.get(), "Jessica Culaciati",scene_label2.get(), partner2.get(), default_char2.get(), "Lucas Vega", scene_label3.get(), partner3.get(), default_char3.get())).grid(row = 5, column = 0)


    


# Scene and Variables
scene_list = os.listdir("Scenes")


# Connecting to Google Sheets and getting tables
scope = ['https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name('aas_client_secret.json', scope)
client = gspread.authorize(creds)
sheet_id = "1EH9LXXC60qgib1c2hlIrOu7KW7DojhwL1Vi2eNjEabQ"

workbook  = client.open_by_key(sheet_id)
sheet_students = workbook.worksheet("Students")
sheet_classes = workbook.worksheet("Classes")
students_row_list = sheet_students.get_all_values()
classes_row_list = sheet_classes.get_all_values()
class_selected = "Wednesday Night" 

# Dataframes for classes and students
classes_df = pd.DataFrame(classes_row_list[1:], columns = classes_row_list[0])
students_df = pd.DataFrame(students_row_list[1:], columns = students_row_list[0])
print(classes_df)

classes_list = classes_df["Time"]

root = Tk()
root.title('AAS Management System')
root.geometry("600x600")

clicked = StringVar()
clicked.set("Select Class")

drop = OptionMenu(root, clicked, *classes_list)
drop.pack()

submit_button = Button(root, text="Submit", command = lambda:get_class_time(root,clicked.get()))
submit_button.pack()

root.mainloop()


# Getting email data ready
email = "Lucasvega771@gmail.com"
receiver_email = ["lucas.vega.acting@gmail.com", "lucasvega771@gmail.com"]

subject = "TEST"
message = "This is a test of my email automation"

text = f"Subject: {subject}\n\n{message}"

server = smtplib.SMTP("smtp.gmail.com", 587)
server.starttls()


# Sending email
#server.login(email, "xejp wftc foos wguz") # this is the key you get from google App Password page.
#server.sendmail(email, receiver_email, text)
#print("The email has been sent.")
