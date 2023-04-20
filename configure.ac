# configure.ac for Emacs AI Utilities

AC_INIT([Emacs AI Utilities], [1.0], [j@wal.sh])

# Installation directories
AC_PREFIX_DEFAULT(/usr/local)
AC_ARG_WITH([prefix],
            [AS_HELP_STRING([--with-prefix=DIR], [Installation prefix])],
            [prefix=$withval],
            [prefix=$prefix_default])
AC_SUBST([PREFIX], [$prefix])

LIB_DIR="${PREFIX}/lib"
BIN_DIR="${PREFIX}/bin"
AC_SUBST([LIB_DIR], [$LIB_DIR])
AC_SUBST([BIN_DIR], [$BIN_DIR])

# Generate config.mk file
AC_CONFIG_FILES([config.mk])

# Finish
AC_OUTPUT
