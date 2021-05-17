#!/usr/bin/python
# -*- coding: utf-8 -*-

import subprocess,shlex,os

# READ LIST OF FIGUREGEN INPUTS IE. 'LS *.INP > INPUTLIST.TXT'

t = open('tracklist.txt','r')
lines = t.readlines()
trk = []
for x in lines:
    trk.append(x.strip())
    print trk
t.close()

f = open('inputlist.txt','r')
# DO FIGURES
j = 0
for i in f:

   cmd = "cp " + trk[j] + " fort.22"
   os.system(cmd)
   j = j + 1
   os.system("parse22.sh")

   inp = shlex.split('FigureGen -I '+i)

   p = subprocess.Popen(inp)

   os.waitpid(p.pid,0)

