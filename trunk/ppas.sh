#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling tornado
/usr/bin/as -o tornado.o tornado.s -arch ppc
if [ $? != 0 ]; then DoExitAsm tornado; fi
rm tornado.s
echo Assembling simthyrmain
/usr/bin/as -o simthyrmain.o simthyrmain.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyrmain; fi
rm simthyrmain.s
echo Assembling simthyr
/usr/bin/as -o SimThyr.o SimThyr.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyr; fi
rm SimThyr.s
echo Linking SimThyr
OFS=$IFS
IFS="
"
/usr/bin/ld /Developer/SDKs/MacOSX10.4u.sdk//usr/lib/crt1.o  -macosx_version_min 10.4   -dead_strip -no_dead_strip_inits_and_terms  -multiply_defined suppress -L. -o SimThyr `cat link.res` -pagezero_size 0x10000
if [ $? != 0 ]; then DoExitLink SimThyr; fi
IFS=$OFS
