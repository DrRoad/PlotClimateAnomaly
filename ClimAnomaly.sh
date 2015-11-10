#!/bin/bash
#if [$# -ne 3]
#then
#  echo "Usage: `basename $0` {futureacebdata} {baseacebdata} {climate_variable} {pngoutput}"
#  exit -1
#fi

#set -x

inputtype=$1
plottype=$2
plotformat=$3
future=$4
base=$5
clim=$6
png=$7
command -v R >/dev/null 2>&1 || { echo >&2 "'R' is required by this tool but was not found on path"; exit 1; }

INSTALL_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
aceb2cvs_jar=$INSTALL_DIR/aceb2csv-1.0-beta3.jar
rclimanomaly=$INSTALL_DIR/ClimAnomaly.r
quadui=$INSTALL_DIR/quadui-1.3.2.jar

#echo $future
#cp $future $INSTALL_DIR
#echo $base
#cp $base $INSTALL_DIR

#read future data package
if [ "$inputtype" == "aceb" ]
then
declare -i count
count=0
while read line
do
    data=`echo $line | awk '{ print $1 }'`
    if [ $count -gt 0]
    then
    echo data_$count: $data
    cp $data $PWD/future.aceb
    java -Xms256m -Xmx768m -jar $aceb2cvs_jar $PWD/future.aceb $PWD
    rm future.aceb
    fi
    count=$count+1
done < "$future"


#read base data package
count=0
while read line
do
    data=`echo $line | awk '{ print $1 }'`
    if [ $count -gt 0 ]
    then
    echo data_$count: $data
    cp $data $PWD/base.aceb
    java -Xms256m -Xmx768m -jar $aceb2cvs_jar $PWD/base.aceb $PWD
    rm base.aceb
    fi
    count=$count+1
done < "$base"
fi




if [ "$inputtype" == "zip" ]
then
declare -i count
count=0
while read line
do
data=`echo $line | awk '{ print $1 }'`
if [ $count -gt 0 ]
then
echo data_$count: $data
cp $data $PWD/future.zip
cd $PWD
java -jar $quadui -cli -clean -n -aceb future.zip ./
for f in *.aceb
do
futurefile=${f%}
java -Xms256m -Xmx768m -jar $aceb2cvs_jar $futurefile ./
rm $futurefile
done
rm future.zip
fi
count=$count+1
done < "$future"


#read base data package
count=0
while read line
do
data=`echo $line | awk '{ print $1 }'`
if [ $count -gt 0 ]
then
echo data_$count: $data
cp $data $PWD/base.zip
cd $PWD
java -jar $quadui -cli -clean -n -aceb base.zip ./
for f in *.aceb
do
basefile=${f%}
java -Xms256m -Xmx768m -jar $aceb2cvs_jar $basefile ./
rm $basefile
done


#unzip -o -q base.zip -d $PWD/
rm base.zip
fi
count=$count+1
done < "$base"
fi


xvfb-run R --no-save --vanilla --slave --args $PWD $clim $plottype $plotformat $png < $rclimanomaly

exit
