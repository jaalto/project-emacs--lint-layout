#!/bin/bash
#
#  lint.sh -- A static analysis tool for programming languages
#
#  Copyright
#
#       Copyright (C) 2009-2013 Jari Aalto
#
#   License
#
#       This program is free software; you can redistribute it and/or
#       modify it under the terms of the GNU General Public License as
#       published by the Free Software Foundation; either version 2 of
#       the License, or (at your option) any later version.
#
#       This program is distributed in the hope that it will be useful, but
#       WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#       General Public License for more details.
#
#       You should have received a copy of the GNU General Public License
#       along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   Install
#
#       Make symlinks to this program to use psecific Lint features by default
#
#           install -m 755 lint.sh /usr/local/bin
#           cd /usr/local/bin
#
#           ln -s lint.sh javalint
#           ln -s lint.sh sqllint
#           ln -s lint.sh phplint
#
#   Call syntax
#
#       <program name> --help
#
#   Requires
#
#       GNU binutils, grep, find etc and Emacs

# Make sure this program is run under Bash because not all /bin/sh
# support $()

( eval "[[ 1 ]]" 2> /dev/null ) || exec /bin/bash "$0" "$@"

# Locations

PROGRAM_DIR=$(cd $(dirname $0); pwd)

EMACS_LIBDIR=${EMACS_LIBDIR:-/home/staff11/jaalto/public_html/bin}
EMACS_PROGRAM=${EMACS_PROGRAM:-lint-layout}
EMACS_OPTIONS="--batch -q --no-site-file"
EMACS_BIN=${EMACS_BIN:-emacs}

# System variables

PROGRAM=$(basename $0)
VERSION="2013.1003.0410"

# Run in clean environment

TZ=Europe/Helsinki
LC_ALL=C
LANG=C
unset DEBUG VERBOSE WHITESPACE_OPT TYPE

# Temporary directories and files

TMPDIR=${TMPDIR:-/tmp}
TMPBASE=$TMPDIR/${LOGNAME:-${USER:-nobody}}.javalint-$$.tmp

# Date

DATE_NOW_ISO=$(date "+%Y-%m-%d")
DATE_NOW_YYYY=${DATE_NOW_ISO%%-*}
DATE_NOW_MM=${DATE_NOW_ISO%-*}
DATE_NOW_MM=${DATE_NOW_MM#*-}
DATE_NOW_MM_NAME=$(date "+%b")

FIND_OPT="-iname *.php\
 -o -iname *.java \
 -o -iname *.css \
 -o -iname *.sql \
"

#######################################################################
#
#   Utilities: Help and Definitons
#
#######################################################################

Help ()
{
    local program="file"
    local languages=${TYPE:-"Java, PHP, SQL and CSS"}
    local type="
    -t, --type TYPE
        Only used with --recursive option. Check only certain type
        of file extensions. Allowed values for TYPE: java, php, css
        and sql.

"
    [ "$TYPE" ] && type=""


    case "$TYPE" in
        java) file=Program ;;
    esac

    echo "\
SYNOPSIS
    $PROGRAM [options] FILE ...

DESCRIPTION
    A static style checking Lint tool for programming languages:
    $languages

OPTIONS
    -D, --debug [LEVEL]
        Enable debug. If LEVEL is set, enable all (development option).

    -l, --libdir DIR
        Define code check library directory (development option).

    -r, --recursive DIR
        Run style checks recursively for all files in DIR (slow).
$type
    -w, --whitespace
        Run whitespace checks only.

    -h, --help
        Display help.

    -V, --version
        Display version number.

EXAMPLES
    Check style of a single file:

        $PROGRAM $file${TYPE:+.$TYPE}

    Check directory recursively for files

        $PROGRAM -r directory/

STANDARDS
    For Java, see official Code Conventions for the Java Programming
    Language at <http://www.oracle.com/technetwork/java/codeconv-138413.html>.

COPYRIGHT
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version."

    exit 0
}

AtExit ()
{
    rm -f "$TMPBASE"*
}

Warn ()
{
    echo "$*" >&2
}

Die ()
{
    Warn "$*"
    exit 1
}

#######################################################################
#
#   Emacs lint library
#
#######################################################################

EmacsLib ()
{
    local base="$EMACS_LIBDIR/$EMACS_PROGRAM"
    local base2="$PROGRAM_DIR/$EMACS_PROGRAM"
    local lib try

    for try in "$base.elc" "$base.el" "$base2.elc" "$base2.el"
    do
        if [ -f "$try" ]; then
            lib="$try"
            break;
        fi
    done

    if [ "$lib" ]; then
        echo "$lib"
    else
        return 1
    fi
}

EmacsCall ()
{
    local args="$*"  # For debug only
    local debug="(setq find-file-hook nil)"
    local opt=""

    if [ "$DEBUG" ]; then
        opt="$opt --debug-init"
        debug="(setq find-file-hook nil lint-layout-debug t debug-on-error t)"
        set -x
    fi

    local lib=$(EmacsLib)

    if [ ! "$lib" ]; then
        Die "Abort. Lint library is not currently available"
    fi

    # lint-layout-check-whitespace
    # lint-layout-check-batch-generic-command-line
    # lint-layout-java-check-all-tests
    local function=lint-layout-java-check-all-tests
    local eval="(lint-layout-check-batch-generic-command-line)"

    if [ "$WHITESPACE_OPT" ]; then
        function=lint-layout-check-whitespace
        eval="(progn (lint-layout-check-batch-command-line '($function)))"
    fi

    local java="(progn (setq lint-layout-check-java-generic-functions (append '(lint-layout-java-check-doc-missing lint-layout-generic-check-doc-main lint-layout-whitespace-multiple-newlines lint-layout-whitespace-at-eob) lint-layout-check-java-code-functions)) (lint-layout-check-batch-generic-command-line))"

    case "$TYPE" in
        *java*)
            eval="$java"
            ;;
    esac

    # For single files, turn on options

    if [ ! "$TYPE" ] && [ $# -eq 1 ]; then
        case "$*" in
            *.java)
                eval="$java"
                ;;
        esac
    fi

    $EMACS_BIN \
        $EMACS_OPTIONS \
        $opt \
        --eval "$debug" \
        -l "$lib" \
        --eval "$eval" \
        "$@" 2>&1 |
        egrep -v 'byte-compiled|Loading'

    [ "$DEBUG" ] && set +x
}

EmacsCallList ()
{
    local file

    while read file
    do
        EmacsCall "$file"
    done < "${1:-/dev/null}"
}

#######################################################################
#
#   Main functionality
#
#######################################################################

CheckFile ()
{
    local file=$1

    if [ ! "$file" ]; then
        return 1
    fi

    if [ ! -f "$file" ]; then
        Warn "Warn: no such file, ignored $file"
        return 1
    fi

    if [ ! -r "$file" ]; then
        Warn "Warn: file not readable, ignored $file"
        return 1
    fi

    EmacsCall "$file"
}

Version ()
{
    lib=$(EmacsLib)
    lib=${lib%c}  # not compiled file

    if [ ! "$lib" ]; then
        Die "Version information not available"
    fi

    awk '/def.*version-time/ { sub(/^[^0-9]+/,""); sub(/.$/,""); print; exit}' "$lib"

    exit 0
}

Main ()
{
    case "$0" in
        *java*) TYPE=java ;;
        *php*) TYPE=php ;;
        *css*) TYPE=css ;;
        *sql*) TYPE=sql ;;
    esac

    while :
    do
        case "$1" in
            -D | --debug)
                shift
                DEBUG=debug
                case "$1" in
                    [0-9]*)
                        set -x
                        shift
                        ;;
                esac
                ;;
            -h | --help)
                Help
                ;;
            -l | --libdir)
                shift
                if [ ! "$1" ]; then
                    Die "Missing --libdir ARG"
                elif [ ! -d "$1" ]; then
                    Die "ERROR: Not a directory: $1"
                else
                    EMACS_LIBDIR="$1"
                fi
                shift
                ;;
            -r | --recursive)
                shift
                recursive=$1
                shift
                if [ ! "$recursive" ]; then
                    Die "ERROR: missing --recursive ARG. See --help."
                fi

                case "$resursive" in
                    -* ) Die "ERROR: Invalid --recursive $recursive." \
                             "See --help."
                        ;;
                esac

                if [ ! -d "$recursive" ]; then
                    Die "ERROR: no such directory $recursive"
                fi
                ;;
            -t | --type)
                shift
                TYPE=$1
                [ "$TYPE" ] || Die "ERROR: missing --type ARG. See --help."

                case "$TYPE" in
                    java | php | inc | sql | css)
                        ;;
                    *)  Die "ERROR: Unknown --type $TYPE. See --help."
                        ;;
                esac
                shift
                ;;
            -w | --whitespace)
                shift
                WHITESPACE_OPT=whitespace
                ;;
            -v | --verbose)
                VERBOSE="verbose"
                shift
                ;;
            -V | --version)
                Version
                ;;
            -*)
                Warn "Unknown option: $1" >&2
                shift
                ;;
            *)  # no more options. Stop while loop
                break
                ;;
        esac
    done

    if [ "$recursive" ]; then

        # Not whitespace path safe

        # find "$recursive" -iname "*.java" > $TMPBASE.files
        # EmacsCall $(< $TMPBASE.files)

        local opt

        if [ "$TYPE" ]; then
            find "$recursive" -iname "*.$TYPE"
        else
            find "$recursive" $FIND_OPT
        fi > $TMPBASE.files

        EmacsCallList "$TMPBASE.files"

    else
        if [ ! "$1" ]; then
            Die "ERROR: missing argument. See --help"
        fi

        local file list space

        : > $TMPBASE.files

        for file in "$@"
        do
            if [ ! -f "$file" ]; then
                Warn "WARN: File does not exist: $file"
                continue;
            elif [ ! -r "$file" ]; then
                Warn "WARN: File not readable: $file"
                continue;
            fi

            echo "$file" >> $TMPBASE.files

            case "$file" in
                *\ *) space="$space"
                    ;;
            esac

            list="$list $file"
        done

        if [ "$space" ]; then
            EmacsCallList "$@"
        else
            EmacsCall "$@"

        fi
    fi
}

trap 'AtExit' 0 1 2 3 15

Main "$@"

# End of file
