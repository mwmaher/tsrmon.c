# tsrmon.mak
# by Michael W. Maher
# 11/27/90
#
# A make file for Microsoft MAKE
#

# .OBJ file from .C file
tsrmon.obj: tsrmon.c
cl /AS /Od /Zi /c tsrmon.c

# MAKE the executable
tsrmon.exe: tsrmon.obj tsrca.obj
LINK $**, $@ /EXEPACK

