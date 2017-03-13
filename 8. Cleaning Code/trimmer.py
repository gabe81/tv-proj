#delete files not in csv
import os
import pandas as pd
import shutil

csv = "/Users/Fedor/Desktop/tv-final/2. Metadata/Film_Scripts_Master.csv"
cur_path = "/Users/Fedor/Desktop/tv-final/1. Final Scripts/Film_Scripts_Final/"
dest_path = "/Users/Fedor/Desktop/tv-final/1. Final Scripts/Film_Scripts_Final_2/"

df = pd.read_csv(csv)
title = df['title'].tolist()
title = [s + ".txt" for s in title]
#print title

for i in os.listdir(cur_path):
    if i in title:
        shutil.move(cur_path + i, dest_path + i)
        print "bam"
