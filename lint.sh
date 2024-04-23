#!/bin/bash
#
#  lint.sh -- A static analysis tool for programming languages
#
#  Copyright
#
#       Copyright (C) 2009-2024 Jari Aalto
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
#   Description
#
#       A static code analysis tool for use with programming
#       languages: Java, PHP and SQL. The purpose of the tool is to find
#       inadequate or incomplete coding conventions.
#
#   Install
#
#       install -m 755 lint.sh /usr/local/bin
#       install -m 644 lint-layout.el /usr/local/bin
#       cd /usr/local/bin
#
#       # make symlinks to use specific Lint features automatically:
#
#       ln -s lint.sh javalint
#       ln -s lint.sh sqllint
#       ln -s lint.sh phplint
#
#       export EMACS_LINT_LIBDIR=<directory location of lint-layout.el>
#
#   Call syntax
#
#       <program name> --help
#
#   Requires
#
#       Emacs
#       GNU binutils: grep, find etc.
#
#   Notes
#
#       This program is not the actual Lint. It only serves as a
#       front-end to collect list of files and to select specific
#       options for passing them to Emacs package lint-layout.el.
#       Emacs is the actual workhorse.
#
#       All the logic, checks and messages are printed from the Emacs
#       Lisp package lint-layout.el

AUTHOR="Jari Aalto <jari.aalto@cante.net>"
LICENCE="GPL-2+"

# Make sure this program is run under Bash because not all /bin/sh
# support $()

( eval "[[ 1 ]]" 2> /dev/null ) || exec /bin/bash "$0" "$@"

# Locations

PROGRAM_DIR=$(cd $(dirname $0); pwd)

EMACS_LINT_BIN=${EMACS_BIN:-emacs}
# EMACS_LINT_LIBDIR default is same as program location

# with *.el or *.elc
EMACS_LINT_PROGRAM=${EMACS_PROGRAM:-lint-layout}

# Batch option implied -q (--no-init-file)
EMACS_LINT_OPTIONS="--batch --no-site-file --no-site-lisp"

# System variables

PROGRAM=$(basename $0)
VERSION="2024.0423.1402"   # YYYY.MMDD.HHMM of last edit

# Run in clean environment

TZ=${TZ:-Europe/Helsinki}
LC_ALL=C
LANG=C
unset DEBUG VERBOSE WHITESPACE_OPT TYPE TEST

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
#   Utilities: Help and Definitions
#
#######################################################################

Help ()
{
    local file="file"
    local languages=${TYPE:-"Java, PHP, SQL and CSS"}

    local type="
    -t, --type TYPE
        Only used with --recursive option. Check certain type
        of file extensions. Allowed values for TYPE: java, php, css
        and sql.

"

    [ "$TYPE" ] && type=""

    local file

    case "$TYPE" in
        java) file=Program ;;
    esac

    local example=$file${TYPE:+.$TYPE}

    if [ ! "$TYPE" ]; then
        example="$file.{java,php,sql,css}"
    fi

    echo "\
SYNOPSIS
    $PROGRAM [options] FILE ...

DESCRIPTION
    A static style checking Lint tool for programming languages:
    $languages

OPTIONS
    --check-doc
        Enable oly docblock check. In Java, this is Javadoc.

    -D, --debug [LEVEL]
        Enable debug. If LEVEL is set, enable all (development option).

    -l, --libdir DIR
        Define code check library directory (development option).

    -r, --recursive DIR
        Run style checks recursively for all files under DIR and below (slow).

    -t, --test
        Run in test mode. Do not actually do anything (development option).

    -w, --whitespace
        Run whitespace checks only.

    -h, --help
        Display help.

    -V, --version
        Display version number.

EXAMPLES
    Check style of a single file:

        $PROGRAM $example

    Check directory recursively for files

        $PROGRAM -r directory/

ENVIRONMENT
    EMACS_LINT_BIN
        Emacs program to use. Defaults to 'emacs'.

    EMACS_LINT_LIBDIR
        Directory location of the Emacs file lint-layout.el. If not
        set, please use --libdir option to point to directory.

    EMACS_LINT_PROGRAM
        Lint library file name without the *.el extension in EMACS_LINT_LIBDIR.
        Defaults to 'lint-layout'.

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
    exit 0
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

OsType ()
{
    local str=$(uname -s)

    case "$str" in
        Darwin)
            echo "macos"
            ;;
        Linux)
            echo "linux"
            ;;
        *CYGWIN*)
            echo "cygwin"
            ;;
        *MINGW*)
            echo "mingw"
            ;;
        *) echo "$str"
           ;;
    esac
}

IsMacOs()
{
    [ "$(OsType)" = "macos" ]
}

ResolveSymlink ()
{
    local file=$1

    if type realpath > /dev/null; then
        realpath "$file"
    elif type readlink > /dev/null; then
        if IsMacOs ; then
            readlink -f "$file"
        else
            readlink "$file"
        fi
    else
        # Very simple way to read symlink
        ls -l "$file" | awk '{print $(NF)}'
    fi
}

#######################################################################
#
#   Emacs lint library
#
#######################################################################

EmacsLib ()
{
    local base1="$EMACS_LINT_LIBDIR/$EMACS_LINT_PROGRAM"
    local base2="$PROGRAM_DIR/$EMACS_LINT_PROGRAM"
    local basetmp="/tmp/$EMACS_LINT_PROGRAM"
    local base lib try ext

    local real=$(ResolveSymlink $(command -v $0))

    local base3

    if [ "$real" ] && [ ! "$real" = "$0" ] ; then
        base3="$(dirname $real)/$EMACS_LINT_PROGRAM"
    fi

    for base in "$base1" "$base2" $base3 "$basetmp"
    do
        for ext in elc el
        do
            try="$base.$ext"

            if [ -f "$try" ]; then
                lib="$try"
                break 2;
            fi
        done
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

    local lib=$EMACS_LIB

    if [ ! "$lib" ]; then
        lib=$(EmacsLib)

        [ "$lib" ] && EMACS_LIB=$lib   # save to global
    fi

    if [ ! "$lib" ]; then
        Die "Abort. Lint library not available. Define EMACS_LINT_LIBDIR or use option --libdir"
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

    if [ "$OPT_DOC" ]; then
        java="(progn (setq lint-layout-check-java-code-functions '(lint-layout-generic-class-check-variables lint-layout-generic-check-statement-end)) (setq lint-layout-check-java-generic-functions (append '(lint-layout-java-check-doc-missing lint-layout-generic-check-doc-main) lint-layout-check-java-code-functions)) (lint-layout-check-batch-generic-command-line))"
    fi

    # lint-layout-check-sql-functions
    local sql="(progn (lint-layout-check-batch-generic-command-line))"

    case "$TYPE" in
        *java*)
            eval="$java"
            ;;
        *sql*)
            eval="$sql"
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

    if [ "$TEST" ]; then
        head --verbose --lines=3000 "$@"

        echo $EMACS_LINT_BIN \
        $EMACS_LINT_OPTIONS \
        $opt \
        --eval "'$debug'" \
        -l "$lib" \
        --eval "'$eval'" \
        "$@"
    else
        $EMACS_LINT_BIN \
        $EMACS_LINT_OPTIONS \
        $opt \
        --eval "$debug" \
        -l "$lib" \
        --eval "$eval" \
        "$@" 2>&1 |
        grep --extended --invert 'byte-compiled|Loading'
    fi

    [ "$DEBUG" ] && set +x
}

EmacsCallList ()
{
    local input="$1"

    if [ ! "$input" ]; then
        return 1
    fi

    if [ "$DEBUG" ]; then
        echo "DEBUG: Check list of files one-by-one"
        sed 's,^,DEBUG: ,' "$input"
    fi

    EmacsCall "$input"
}

#######################################################################
#
#   Major functionality
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
    lib=${lib%c}  # no compiled files

    if [ ! "$lib" ]; then
        Die "Library version information not available"
    fi

    local prg=$(basename $0)
    local libver=$(awk '/def.*version-time/ {sub(/^[^0-9]+/,""); sub(/.$/,""); print; exit}' "$lib")

    echo "$prg $VERSION $EMACS_LINT_PROGRAM.el $libver"

    exit 0
}

Main ()
{
    # Turn on mode based on call name: javalint ...

    case "$0" in
        *java*) TYPE=java ;;
        *php*) TYPE=php ;;
        *css*) TYPE=css ;;
        *sql*) TYPE=sql ;;
    esac

    while :
    do
        case "$1" in
            --check-doc)
                shift
                OPT_DOC="opt-doc-check"
                ;;

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
            -l | --libdir)
                shift
                if [ ! "$1" ]; then
                    Die "Missing --libdir ARG"
                elif [ ! -d "$1" ]; then
                    Die "ERROR: Not a directory: $1"
                else
                    EMACS_LINT_LIBDIR="$1"
                fi
                shift
                ;;
            -r | --recursive)
                recursive=$2
                shift 2
                if [ ! "$recursive" ]; then
                    Die "ERROR: missing --recursive ARG. See --help."
                fi

                case "$resursive" in
                    -* ) Die "ERROR: Invalid --recursive option $recursive." \
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
            -T | --test)
                shift
                TEST=test
                ;;
            -w | --whitespace)
                shift
                WHITESPACE_OPT=whitespace
                ;;
            -v | --verbose)
                VERBOSE="verbose"
                shift
                ;;
            -h | --help)
                Help
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

        local opt
        local files="$TMPBASE.files.lst"

        if [ "$TYPE" ]; then
            find "$recursive" -iname "*.$TYPE"
        else
            find "$recursive" $FIND_OPT
        fi > "$files"

        if [ -s "$files" ]; then
            EmacsCallList "$files"
        else
            echo "No files found to check (recursive)"
        fi

    else
        if [ ! "$1" ]; then
            Die "ERROR: missing file argument. See --help"
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
            EmacsCallList "$TMPBASE.files"
        elif [ ! "$list" ]; then
            echo "Nothing to check."
        else
            EmacsCall $list

        fi
    fi
}

trap AtExit EXIT HUP INT QUIT TERM
Main "$@"

# End of file
