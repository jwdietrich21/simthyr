#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling simthyr
/usr/bin/as -o SimThyr.o SimThyr.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyr; fi
rm SimThyr.s
echo Linking SimThyr
OFS=$IFS
IFS="
"
/usr/bin/ld /Developer/SDKs/MacOSX10.4u.sdk//usr/lib/crt1.o  -framework Carbon -framework OpenGL -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib -macosx_version_min 10.4   -dead_strip -no_dead_strip_inits_and_terms  -multiply_defined suppress -L. -o SimThyr `cat link.res` -pagezero_size 0x10000
if [ $? != 0 ]; then DoExitLink SimThyr; fi
IFS=$OFS
