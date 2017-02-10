# Microsoft Developer Studio Generated NMAKE File, Based on oobr.dsp
!IF "$(CFG)" == ""
CFG=oobr - Win32 Debug
!MESSAGE No configuration specified. Defaulting to oobr - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "oobr - Win32 Release" && "$(CFG)" != "oobr - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "oobr.mak" CFG="oobr - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "oobr - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "oobr - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "oobr - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "oobr.hlp" "$(OUTDIR)\oobr.exe"

!ELSE 

ALL : "oobr.hlp" "$(OUTDIR)\oobr.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\dbl-msw.obj"
	-@erase "$(INTDIR)\draw.obj"
	-@erase "$(INTDIR)\input.obj"
	-@erase "$(INTDIR)\intf-msw.obj"
	-@erase "$(INTDIR)\oobr.res"
	-@erase "$(INTDIR)\tree.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\oobr.exe"
	-@erase "oobr.hlp"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "_CONSOLE" /D "WIN32" /D "NDEBUG" /D\
 "_WINDOWS" /D "WIN32_LEAN_AND_MEAN" /D "MSW" /Fp"$(INTDIR)\oobr.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release/
CPP_SBRS=.

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\oobr.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\oobr.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib comctl32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\oobr.pdb" /machine:I386 /out:"$(OUTDIR)\oobr.exe" 
LINK32_OBJS= \
	"$(INTDIR)\dbl-msw.obj" \
	"$(INTDIR)\draw.obj" \
	"$(INTDIR)\input.obj" \
	"$(INTDIR)\intf-msw.obj" \
	"$(INTDIR)\oobr.res" \
	"$(INTDIR)\tree.obj"

"$(OUTDIR)\oobr.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "oobr - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "oobr.hlp" "$(OUTDIR)\oobr.exe"

!ELSE 

ALL : "oobr.hlp" "$(OUTDIR)\oobr.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\dbl-msw.obj"
	-@erase "$(INTDIR)\draw.obj"
	-@erase "$(INTDIR)\input.obj"
	-@erase "$(INTDIR)\intf-msw.obj"
	-@erase "$(INTDIR)\oobr.res"
	-@erase "$(INTDIR)\tree.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\oobr.exe"
	-@erase "$(OUTDIR)\oobr.ilk"
	-@erase "$(OUTDIR)\oobr.pdb"
	-@erase "oobr.hlp"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /FI"stdlib.h" /FI"crtdbg.h" /D\
 "_CONSOLE" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "WIN32_LEAN_AND_MEAN" /D\
 "MSW" /D "_CRTDBG_MAP_ALLOC" /Fp"$(INTDIR)\oobr.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\oobr.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\oobr.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib comctl32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\oobr.pdb" /debug /machine:I386 /out:"$(OUTDIR)\oobr.exe"\
 /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\dbl-msw.obj" \
	"$(INTDIR)\draw.obj" \
	"$(INTDIR)\input.obj" \
	"$(INTDIR)\intf-msw.obj" \
	"$(INTDIR)\oobr.res" \
	"$(INTDIR)\tree.obj"

"$(OUTDIR)\oobr.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(CFG)" == "oobr - Win32 Release" || "$(CFG)" == "oobr - Win32 Debug"
SOURCE=".\dbl-msw.c"
DEP_CPP_DBL_M=\
	".\dbl.h"\
	".\intf.h"\
	

"$(INTDIR)\dbl-msw.obj" : $(SOURCE) $(DEP_CPP_DBL_M) "$(INTDIR)"


SOURCE=.\draw.c

!IF  "$(CFG)" == "oobr - Win32 Release"

DEP_CPP_DRAW_=\
	".\dbl.h"\
	".\defs.h"\
	".\intf.h"\
	".\tree.h"\
	

"$(INTDIR)\draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "oobr - Win32 Debug"

DEP_CPP_DRAW_=\
	".\dbl.h"\
	".\defs.h"\
	".\intf.h"\
	".\tree.h"\
	

"$(INTDIR)\draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"


!ENDIF 

SOURCE=.\input.c
DEP_CPP_INPUT=\
	".\defs.h"\
	".\input.h"\
	".\tree.h"\
	

"$(INTDIR)\input.obj" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"


SOURCE=".\intf-msw.c"

!IF  "$(CFG)" == "oobr - Win32 Release"

DEP_CPP_INTF_=\
	".\dbl.h"\
	".\input.h"\
	".\intf.h"\
	".\tree.h"\
	

"$(INTDIR)\intf-msw.obj" : $(SOURCE) $(DEP_CPP_INTF_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "oobr - Win32 Debug"

DEP_CPP_INTF_=\
	".\dbl.h"\
	".\input.h"\
	".\intf.h"\
	".\tree.h"\
	

"$(INTDIR)\intf-msw.obj" : $(SOURCE) $(DEP_CPP_INTF_) "$(INTDIR)"


!ENDIF 

SOURCE=.\oobr.rc
DEP_RSC_OOBR_=\
	".\oobr.ico"\
	

"$(INTDIR)\oobr.res" : $(SOURCE) $(DEP_RSC_OOBR_) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\oobrhelp.hpj

!IF  "$(CFG)" == "oobr - Win32 Release"

InputPath=.\oobrhelp.hpj

"oobr.hlp"	 : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	hcw /e /a /c oobrhelp.hpj

!ELSEIF  "$(CFG)" == "oobr - Win32 Debug"

InputPath=.\oobrhelp.hpj

"oobr.hlp"	 : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	hcw /e /a /c oobrhelp.hpj

!ENDIF 

SOURCE=.\tree.c

!IF  "$(CFG)" == "oobr - Win32 Release"

DEP_CPP_TREE_=\
	".\dbl.h"\
	".\defs.h"\
	".\intf.h"\
	".\tree.h"\
	

"$(INTDIR)\tree.obj" : $(SOURCE) $(DEP_CPP_TREE_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "oobr - Win32 Debug"

DEP_CPP_TREE_=\
	".\dbl.h"\
	".\defs.h"\
	".\intf.h"\
	".\tree.h"\
	

"$(INTDIR)\tree.obj" : $(SOURCE) $(DEP_CPP_TREE_) "$(INTDIR)"


!ENDIF 


!ENDIF 

