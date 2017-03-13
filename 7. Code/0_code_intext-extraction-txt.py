#import csv
import os
import re
#import codecs

## Program Design
## go through each line and find scene start "EXT" or "INT" write into column 1 "J+1" to denote the scene number.
## regex find newline allcaps to locate and extract direct speech
## INT/EXT regex: "[[E][X][T][.]|[[I][N][T][.]"
## Character direct speech regex (in theory): "^.*[A-Z]{2,}\W*$"
##

scriptdir = "SOURCE_DIRECTORY"
#outdir = "./TV_Scripts_All_SceneText/"
scenesdir = "TARGET_DIRECTORY"

extint = re.compile("[E][X][T][.]|[[I][N][T][.]")
notchar = re.compile("[a-z]")
char = re.compile("[A-Z](.*)[A-Z]")

ext = re.compile("[E][X][T]")
_int = re.compile("[I][N][T]")

scriptnames = os.listdir(scriptdir)

for files in scriptnames:
    if "txt" in files:
        script = open(scriptdir + files).readlines()
        #chardict = {}
        j = 0
        i = 0
        z = 0
        rootname = re.sub(".txt", "", files)
        int_scene_text = open(scenesdir + "FILM-INT/" + rootname + '_INT.txt', "wb")
        ext_scene_text = open(scenesdir + "FILM-EXT/" + rootname + '_EXT.txt', "wb")
        for line in script:
            #i records total script line numbers
            i = i+1
            # z records all scenes except last scene
            #z = 0
            scene = re.search(extint, line)
            if scene > 0:
                scene = scene.group()
                j = j+1
                scene_name = re.sub(r'[^A-Z ]+', '', line)
                scene_name = scene_name.strip()
                scene_name = re.sub(r'\s+', '_', scene_name)
                print (rootname, scene_name)
                if j > 1:
                    #scene_file.close()
                    print "Art washes away from the soul the dust of everyday life. -- Pablo Picasso"
                #scene_file = open(outdir + scene_name + '.txt', "wb")
                #continue
            else:
                if j > 0:
                    #scene_file.write(line)
                    scene_search_ext = re.search(ext, scene_name)
                    scene_search_int = re.search(_int, scene_name)
                    if scene_search_ext > 0:
                        ext_scene_text.write(line)
                    if scene_search_int > 0:
                        int_scene_text.write(line)
        int_scene_text.close()
        ext_scene_text.close()
    else:
        continue
