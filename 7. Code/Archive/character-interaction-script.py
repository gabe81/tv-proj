import csv
import os
import re
import codecs

## Program Design
## go through each line and find scene start "EXT" or "INT" write into column 1 "J+1" to denote the scene number.
## regex find newline allcaps to locate and extract direct speech
## INT/EXT regex: "[[E][X][T][.]|[[I][N][T][.]"
## Character direct speech regex (in theory): "^.*[A-Z]{2,}\W*$"
##

scriptdir = "/Users/Fedor/Desktop/tv-final/1. Final Scripts/Film_Scripts_Final/"
outdir = "/Users/Fedor/Desktop/tv-final/3. Scenes Per Show/Film_Scripts_Output/"
scenesdir = "/Users/Fedor/Desktop/tv-final/3. Scenes Per Show/Film_Scripts_All_Scenes/"

# approvedcharacters = list()

# with open('TV_Characters_Approved.csv', 'r') as m:
#     mreader = csv.reader(m, delimiter = ',')
#     for approvchar in mreader:
#         approvedcharacters.append(approvchar[1])
        #print approvedcharacters
#print approvedcharacters

extint = re.compile("[E][X][T][.]|[[I][N][T][.]")
notchar = re.compile("[a-z]")
char = re.compile("[A-Z](.*)[A-Z]")

scriptnames = os.listdir(scriptdir)

for files in scriptnames:
    if "txt" in files:
        script = open(scriptdir + files).readlines()
        chardict = {}
        j = 0
        i = 0
        z = 0
        rootname = re.sub(".txt", "", files)
        with open(outdir + rootname + '_Interactions.csv', "wb") as f:
            writer = csv.writer(f, delimiter=",")
            with open(scenesdir + rootname + '_Scenes.csv', "wb") as g:
                gwriter = csv.writer(g, delimiter=",")
                #with open(outdir + rootname + '_Chars.csv', "wb") as h:
                    #hwriter = csv.writer(h, delimiter=",")
                for line in script:
                    #i records total script line numbers
                    i = i+1
                    # z records all scenes except last scene
                    #z = 0
                    scene = re.search(extint, line)
                    if scene > 0:
                        scene = scene.group()
                        j = j+1
                        if j == 1:

                            prevscene = re.sub(r'[^A-Z ]+', '', line)
                            continue
                        #print (j, prevscene, i)
                        charlist = chardict.keys()
                        if not charlist:
                            #print (j-1, prevscene, "NC", "NC")
                            writer.writerow([str(j-1), prevscene.encode('utf-8'), "NC", "NC"])
                        m = 0
                        for name in charlist:
                            for k in range(charlist.index(name) + 1, len(charlist)):
                                #print (j-1, prevscene, name, charlist[k])
                                m = m + 1
                                writer.writerow([str(j-1), prevscene.encode('utf-8'), name.encode('utf-8'), charlist[k].encode('utf-8')])
                                #gwriter.writerow([str(j-1), prevscene.encode('utf-8'), len(charlist)])
                                #print name
                                #hwriter.writerow([name.encode('utf-8'), str(j)])
                        gwriter.writerow([str(j-1), prevscene.encode('utf-8'), i-z])
                        z = i
                        chardict = {}
                        #prevscene = line
                        prevscene = re.sub(r'[^A-Z ]+', '', line)
                        continue
                    notcharacter = re.search(notchar, line)
                    if notcharacter > 0:
                        continue
                    else:
                        character = re.search(char, line)
                        if character > 0 and j > 0:
                            charname = character.group().encode('utf-8')
                            print charname
                            #if charname not in approvedcharacters:
                                #continue
                            #else:
                            print ("APPROVED:", charname)
                            chardict.update({charname : 0})
    else:
        continue





