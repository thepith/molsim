#!/usr/bin/env bash
set -o nounset                              # Treat unset variables as an error
set -euo pipefail

# Filename
archFile="Src/make.arch"
fftwFile="Src/make.fftwpath"
conffile="version.conf"

print_usage() {
   printf "MOLSIM configure script

  Options:
   -h   show this help
   -n   non-interactive mode, it will use the default options
   -s   show the current configuration
   -d   delete the current configuration

The compiler which is to be used is written into $archFile.

The path of the fftw header files and the fftw libary are written into $fftwFile.

The name of the version will make the installed version of Molsim be called
\"mosim_ser.<version name>\". The version name is written to $conffile."
   exit 0
}

print_current() {
   if [[ -e "$archFile" ]]; then
      source <(sed 's/ //g' $archFile)
   else
      ARCH=""
      MPIFC=""
   fi
   if [[ -e "$fftwFile" ]]; then
      source <(sed 's/ //g' $fftwFile)
   else
      FFTW_PATH=""
      FFTWLIB=""
   fi
   if [[ -e "$conffile" ]]; then
      version="version name is: \"$(cat $conffile)\""
   else
      version="version name is not set"
   fi

   printf "MOLSIM configure script

Current configuration:

compiler:       $ARCH
mpi-compiler:   $MPIFC
fftw header at: $FFTW_PATH
fftw libary at: $FFTWLIB

$version"
   exit 0
}

delete_config() {
   printf "MOLSIM configure script

the current configuration will be deleted."
   rm $archFile
   rm $fftwFile
   rm $conffile
   exit 0
}

nonInteractive=false

while getopts ':hnsd' opt; do
  case "${opt}" in
    n) nonInteractive=true ;;
    h) print_usage ;;
    s) print_current ;;
    d) delete_config ;;
    \?) echo "Invalid option: -${OPTARG}" >&2 ; exit 1 ;;
  esac
done

echo -n "Checking ifort ..."
if command -v ifort >/dev/null 2>&1; then
   echo "yes"
   lifort=true
else
   echo "no"
   lifort=false
fi
echo -n "Checking gfortran ..."
if command -v gfortran >/dev/null 2>&1; then
   echo "yes"
   lgfortran=true
else
   echo "no"
   lgfortran=false
fi
echo -n "Checking mpiifort ..."
if command -v mpiifort >/dev/null 2>&1; then
   echo "yes"
   lintelmpi=true
   intelmpiFC=$( mpiifort -show | { read -a array ; echo ${array[0]} ; })
   echo "mpiifort uses $intelmpiFC"
else
   echo "no"
   lintelmpi=false
   intelmpiFC=""
fi
echo -n "Checking mpifort ..."
if command -v mpifort >/dev/null 2>&1; then
   echo "yes"
   lopenmpi=true
   openmpiFC=$(mpifort -show | { read -a array ; echo ${array[0]} ; })
   echo "mpifort uses $openmpiFC"
else
   echo "no"
   lopenmpi=false
   openmpiFC=""
fi

FC="none"
MPIFC="none"
if [ $lifort = true ] && [ "$intelmpiFC" = "ifort" ]; then
   echo "Found intelmpi and ifort"
   FC="ifort"
   MPIFC="mpiifort"
elif [ $lifort = true ] && [ "$openmpiFC" = "ifort" ]; then
   echo "Found openmpi and ifort"
   FC="ifort"
   MPIFC="mpifort"
elif [ $lgfortran = true ] && [ "$openmpiFC" = "gfortran" ]; then
   echo "Found openmpi and gfortran"
   FC="gfortran"
   MPIFC="mpifort"
elif [ $lgfortran = true ] && [ "$intelmpiFC" = "gfortran" ]; then
   echo "Found intelmpi and gfortran"
   FC="gfortran"
   MPIFC="mpiifort"
else
   echo "Warning: Could not detect a working mpi compiler combination. Unless the mpi compiler is adapted, the compilation of the parallel version will fail"
   if [ $lifort = true ]; then
      echo "Using ifort"
      FC="ifort"
   elif [ $lgfortran = true ]; then
      echo "Using gfortran"
      FC="gfortran"
   else
      echo "Warning: Automatic detection of the fortran compiler failed"
   fi
fi

if [ $nonInteractive = true ]; then
   setcomp=true
else
   setcomp=false
   while [ $setcomp = false ]; do
      read -e -p "Use $FC as a compiler and $MPIFC as mpi compiler? " -i "y" docomp
      case ${docomp:0:1} in
         y|Y )
            setcomp=true
            ;;
         * )
            read -e -p "Which compiler to use? " -i "$FC" FC
            read -e -p "Which mpi compiler to use? " -i "$MPIFC" MPIFC
      esac
   done
fi

if [ "$FC" = "gfortran" ]; then
   echo "ARCH = LOCAL_GFORTRAN" > $archFile
elif [ "$FC" = "ifort" ]; then
   echo "ARCH = LOCAL_INTEL" > $archFile
else
   echo "Compiler $FC is not supported by molsim"
   exit 1
fi
echo "MPIFC = $MPIFC" >> $archFile

fftwpath="other"
fftwlib="other"
fftwpaths=""
fftwlibs=""
echo "Search FFTW files"
if [ -x "$(command -v cpp)" ]; then
   echo "checking for FFTW3 header using gcc"
   if echo '#include <fftw3.f03>' | cpp -H -o /dev/null 2> /dev/null ; then
      paths=`echo -e '#include <fftw3.f03>' | cpp -H -o /dev/null 2>&1 | grep "\.\.* " | sed 's/^\.\.* //g'`
      fftwpaths+=" $(dirname $paths)"
      echo "|$fftwpaths|"
   fi
fi
if [ -x "$(command -v locate)" ]; then
   echo "checking for FFTW3 header using locate"
   if locate -l 1 -r "fftw3\.f03$" > /dev/null ; then
      fftwpaths+=" $(dirname $(locate -r "fftw3\.f03$") | uniq)"
   fi
fi
if [ -f "$HOME/.fftw/include/fftw3.f03" ]; then
   echo "found FFTW3 header in $HOME/.fftw"
   fftwpaths+=" $HOME/.fftw/include"
fi
fftwpaths=$(echo $fftwpaths | sed 's/^ //' | tr ' ' '\n' | uniq)

if [ -x /sbin/ldconfig ]; then
   echo "checking for FFTW3 libary using ldconfig"
   if /sbin/ldconfig -p | grep "libfftw3\(\.dll\)*\(\.a\|\.so\)$" > /dev/null; then
      paths=$(/sbin/ldconfig -p | grep "libfftw3\(\.dll\)*\(\.a\|\.so\)$" | sed 's/^.* => //')
      fftwlibs+=" $(dirname $paths)"
   fi
fi
if [ -x "$(command -v locate)" ]; then
   echo "checking for FFTW3 libary using locate"
   if locate -l 1 -r "libfftw3\(\.dll\)*\(\.a\|\.so\)$" > /dev/null ; then
      fftwlibs+=" $(dirname $(locate -r "libfftw3\(\.dll\)*\(\.a\|\.so\)$") | uniq)"
   fi
fi
if [ -f "$HOME/.fftw/lib/libfftw3.a" ]; then
   echo "found FFTW3 header in $HOME/.fftw"
   fftwlibs+=" $HOME/.fftw/lib"
fi
fftwlibs=$(echo $fftwpaths | sed 's/^ //' | tr ' ' '\n' | uniq)

if [ ! -z "$fftwpaths" ] && [ ! -z "$fftwlibs" ] ; then
   echo ""
   echo "Which FFTW3 version should be used?"
   if [ $nonInteractive = true ]; then
      fftwpatharray=($fftwpaths)
      fftwpath=${fftwpatharray[0]}
   else
      select d in $(echo $fftwpaths "other"); do
         if [ -n "$d" ]; then
            fftwpath=$d
            break
         fi
      done
   fi
   echo "$fftwpath selected"

   echo ""
   echo "Which FFTW3 lib should be used?"
   if [ $nonInteractive = true ]; then
      fftwlibarray=($fftwlibs)
      fftwlib=${fftwlibarray[0]}
   else
      select d in $(echo $fftwlibs "other"); do
         if [ -n "$d" ]; then
            fftwlib=$d
            break
         fi
      done
   fi
   echo "$fftwlib selected"
fi

if [ "$fftwlib" = "other" ] || [ "$fftwpath" = "other" ]; then
   echo ""
   echo "Automatic detection of FFTW3 failed."
   echo "You can either install it locally, or provide the path to the FFTW3 libary."
   if [ $nonInteractive = true ]; then
      exit 1
   fi
   read -e -p "Install under ~/.fftw? (y/n) " -i "n" dofftw
   case ${dofftw:0:1} in
      y|Y )
         FILE="fftw-3.3.4.tar.gz"
         dnfftw=""
         if [ ! -f $FILE ];
         then
            read -e -p "$FILE not found. Download from fftw.org? (requires wget) " -i "n" dnfftw
            case ${dnfftw:0:1} in
               y|Y )
                  command -v wget >/dev/null 2>&1 || { echo >&2 "I require wget but it's not installed.  Aborting."; exit 1; }
                  wget ftp://ftp.fftw.org/pub/fftw/fftw-3.3.4.tar.gz
                  ;;
               * )
                  read -e -p "Provide path to file: " FILE
            esac
         fi

         if [ ! -f $FILE ];
         then
            echo "ERROR: $FILE not found."
            exit 1
         fi

         echo "Using $FILE"
         echo "Installing in $HOME/.fftw"

         mkdir -p $HOME/.fftw
         curdir=$PWD
         tar xfv $FILE -C $HOME/.fftw
         if [ -f fftw-*.tar.gz ];
         then
            case ${dnfftw:0:1} in
               y|Y )
                  rm fftw-3.3.4.tar.gz
                  ;;
               * )
                  echo "not removing tar file"
            esac
         fi
         cd $HOME/.fftw/fftw*
         pwd
         ./configure --prefix="$HOME/.fftw"
         make
         make install
         cd $curdir
         echo ""
         echo "FFTW installed"
         echo ""
         fftwpath="$HOME/.fftw/include"
         fftwlib=`ls -d $HOME/.fftw/lib*`
         ;;
      * )
         echo "Please provide the path to the directory of the fftw3.f03 file:"
         read -e -i "/" fftwpath

         echo "Please provide the path to the directory of the FFTW3 libary (libfftw3):"
         read -e -i "/" fftwlib
   esac
fi
echo "FFTW_PATH = $fftwpath" > $fftwFile
echo "FFTWLIB = $fftwlib" >> $fftwFile
echo "yes"

echo -n "Checking ~/bin ..."
if [[ ! ":$PATH:" == *":$HOME/bin:"* ]]; then
   echo ""
   echo "Setting up ~/bin"
   mkdir -p $HOME/bin

   #Add ~/bin to PATH
   export PATH=".:$HOME/bin:$PATH"
   rc=${SHELL#*/bin/}rc
   echo 'PATH=".:$HOME/bin:$PATH"' >> "$HOME/.$rc"
   echo 'export PATH' >> "$HOME/.$rc"
fi
echo "yes"


#creating version.conf where user parameters are stored

if [[ -e "$conffile" ]]; then
   if [ $nonInteractive = true ]; then
      doconf="n"
   else
      read -e -p "$conffile exists. Overwrite? " -i "n" doconf
   fi
else
   doconf="y"
fi

case ${doconf:0:1} in
   y|Y )
      if [ $nonInteractive = true ]; then
         echo "version without a name"
         touch $conffile
      else
         read -e -p "Name of the version? " -i "" ver
         echo $ver > $conffile
      fi
      ;;
   * )
      echo "not changing $conffile"
      touch $conffile
      ;;
esac
