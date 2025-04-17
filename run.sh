#!/bin/bash

yead_day=""
arg_interact="0"
arg_nobuild="0"

cmd="cabal " # run ou repl

while true ; do
    case "$1" in
	-i|--interact)
	    arg_interact="1"; shift 1;;
    -n|--no_build)
        arg_nobuild="1"; shift 1;;
	-h|--help)
	    echo "usage $0 [-i/--interact] [-n/--no_build] yy_dd"
        echo "(works by setting up a link from src/advent_yy_dd.hs to app/Main.hs)"
	    exit;;
	*)
        # need one file name
        if [ "$#" -lt 1 ];
        then echo "illegal number of parameters"
             echo "Usage : run [-i] yy_dd"
             exit;
        fi
        year_day=$1;
	    break;;
    esac
done

year=${year_day:0:2}
advent_name="src/advent"$year_day".hs";
link_name="app/Main"$year".hs"
target_name="Advent"$year

if [ "$arg_interact" == "1" ]; then
    cmd=$cmd"repl "$target_name
else
    cmd=$cmd"run "$target_name
fi

if test -f $advent_name; then
    echo $advent_name" EXISTS"
    echo "rm -f "$link_name
    rm -f $link_name
    echo "ln -s ../"$advent_name" "$link_name
    ln -s "../"$advent_name $link_name
    if [ "$arg_nobuild" == "0" ]; then
        echo "cabal clean"
        cabal clean
        # echo "cabal build $link_name"
        # cabal build $link_name
        echo "cabal build "$target_name
        cabal build $target_name
    fi
    echo $cmd
    $cmd
else
    echo $advent_name" DOES NOT exist"
    exit;
fi
