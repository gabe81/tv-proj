import os
import pandas as pd
import shutil

csv = "/Users/Fedor/Desktop/tv-final/2. Metadata/Film_Scripts_Master.csv"
cur_path = "/Users/Fedor/Desktop/tv-final/3. Scenes Per Show/Film_Scenes/"
dest_path = "/Users/Fedor/Desktop/tv-final/3. Scenes Per Show/Film_Scenes_cut/"

df = pd.read_csv(csv)
#df = df[df.year < 1987]
#print df
title = df['title'].tolist()
title_aug = [w.replace('.txt', '_Scenes.csv') for w in title]

#print title_aug
for i in os.listdir(cur_path):
    if i not in title_aug:
        shutil.move(cur_path + i, dest_path + i)
        print "bam"
