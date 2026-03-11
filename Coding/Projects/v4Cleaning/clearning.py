from random_word import RandomWords
import random
import string


length = 25
r = RandomWords()
newfile = open("C:\MyLearning\LinuxCommands\grepFilePractice.txt", "a")


for i in range(10000):
    print(i)
    newstring = r.get_random_word() + ''.join(random.choices(string.ascii_letters + string.digits, k=length))
    newfile.write(newstring)


newfile.close()
            


            
