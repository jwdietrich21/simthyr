#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling simthyrtypes
/usr/bin/as -o simthyrtypes.o simthyrtypes.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyrtypes; fi
rm simthyrtypes.s
echo Assembling simthyrlog
/usr/bin/as -o simthyrlog.o simthyrlog.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyrlog; fi
rm simthyrlog.s
echo Assembling simthyrservices
/usr/bin/as -o simthyrservices.o simthyrservices.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyrservices; fi
rm simthyrservices.s
echo Assembling handlenotifier
/usr/bin/as -o handlenotifier.o handlenotifier.s -arch ppc
if [ $? != 0 ]; then DoExitAsm handlenotifier; fi
rm handlenotifier.s
echo Assembling simthyrplot
/usr/bin/as -o simthyrplot.o simthyrplot.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyrplot; fi
rm simthyrplot.s
echo Assembling simthyrprediction
/usr/bin/as -o simthyrprediction.o simthyrprediction.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyrprediction; fi
rm simthyrprediction.s
echo Assembling simulator
/usr/bin/as -o simulator.o simulator.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simulator; fi
rm simulator.s
echo Assembling structureparameters
/usr/bin/as -o structureparameters.o structureparameters.s -arch ppc
if [ $? != 0 ]; then DoExitAsm structureparameters; fi
rm structureparameters.s
echo Assembling simoptions
/usr/bin/as -o simoptions.o simoptions.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simoptions; fi
rm simoptions.s
echo Assembling launchdialog
/usr/bin/as -o launchdialog.o launchdialog.s -arch ppc
if [ $? != 0 ]; then DoExitAsm launchdialog; fi
rm launchdialog.s
echo Assembling showips
/usr/bin/as -o showips.o showips.s -arch ppc
if [ $? != 0 ]; then DoExitAsm showips; fi
rm showips.s
echo Assembling versionsupport
/usr/bin/as -o versionsupport.o versionsupport.s -arch ppc
if [ $? != 0 ]; then DoExitAsm versionsupport; fi
rm versionsupport.s
echo Assembling handlepreferences
/usr/bin/as -o handlepreferences.o handlepreferences.s -arch ppc
if [ $? != 0 ]; then DoExitAsm handlepreferences; fi
rm handlepreferences.s
echo Assembling aboutdialog
/usr/bin/as -o aboutdialog.o aboutdialog.s -arch ppc
if [ $? != 0 ]; then DoExitAsm aboutdialog; fi
rm aboutdialog.s
echo Assembling showaboutmodel
/usr/bin/as -o showaboutmodel.o showaboutmodel.s -arch ppc
if [ $? != 0 ]; then DoExitAsm showaboutmodel; fi
rm showaboutmodel.s
echo Assembling sensitivityanalysis
/usr/bin/as -o sensitivityanalysis.o sensitivityanalysis.s -arch ppc
if [ $? != 0 ]; then DoExitAsm sensitivityanalysis; fi
rm sensitivityanalysis.s
echo Assembling tornado
/usr/bin/as -o tornado.o tornado.s -arch ppc
if [ $? != 0 ]; then DoExitAsm tornado; fi
rm tornado.s
echo Assembling scenariohandler
/usr/bin/as -o scenariohandler.o scenariohandler.s -arch ppc
if [ $? != 0 ]; then DoExitAsm scenariohandler; fi
rm scenariohandler.s
echo Assembling simthyrmain
/usr/bin/as -o simthyrmain.o simthyrmain.s -arch ppc
if [ $? != 0 ]; then DoExitAsm simthyrmain; fi
rm simthyrmain.s
echo Assembling splash
/usr/bin/as -o splash.o splash.s -arch ppc
if [ $? != 0 ]; then DoExitAsm splash; fi
rm splash.s
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
