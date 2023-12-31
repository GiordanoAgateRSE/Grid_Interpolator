#-------------------------------------------------------------------------------
# "Grid_Interpolator v.3.0" (Grid manager tool)
# Copyright 2016-2022 (RSE SpA)
# "Grid_Interpolator v.3.0" authors and email contact are provided on 
# the documentation file.
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Description: Grid_Interpolator Makefile.
#-------------------------------------------------------------------------------

# Variables to be modified: start

#    CODE VERSION
#       Suggested values:
#          " vx_UserInitials_CommitData"
VERSION = v3_

#    COMPILER
#       Suggested values:
#          " ifort"
#          " gfortran"
COMPILER = gfortran

#    EXECUTION
#       Suggested values:
#          " optimized"
#          " debug"
EXECUTION = debug

#    PARALLELIZATION
#       Suggested values:
#          parallel simulations
#             " OMP"
#          sequential simulations
#             " NO"
PARALLELIZATION = OMP

# Variables to be modified: end

# IDs of the above choices reported in the name of the executable code
ifeq ($(COMPILER), ifort)
   ID_COMPILER = 1
   else ifeq ($(COMPILER), gfortran)
      ID_COMPILER = 2
endif
ifeq ($(EXECUTION), optimized)
   ID_EXECUTION = 1
   else ifeq ($(EXECUTION), debug)
      ID_EXECUTION = 2
endif
ifeq ($(PARALLELIZATION), NO)
   ID_PARALLELIZATION = 0
   else ifeq ($(PARALLELIZATION), OMP)
      ID_PARALLELIZATION = 1
endif

# COMPILATION_FLAGS
COMPILATION_FLAGS = -O1
ifeq ($(EXECUTION), debug)
   ifeq ($(COMPILER), gfortran)
      COMPILATION_FLAGS = -g -O0 -fbacktrace -C -Wall -fbounds-check
      else ifeq ($(COMPILER), ifort)
         COMPILATION_FLAGS = -g -O0 -traceback -C -check bounds -check noarg_temp_created -debug all -fpe0
   endif
endif

# OMP_FLAG
OMP_FLAG =  
ifeq ($(PARALLELIZATION), OMP)
   ifeq ($(COMPILER), ifort)
      OMP_FLAG = -qopenmp
      else ifeq ($(COMPILER), gfortran)
         OMP_FLAG = -fopenmp
   endif
endif

EXE_DIR = ../exe/

SOURCES = \
$(wildcard ./Modules/main_module.f90) \
$(wildcard ./Main_algorithm/interpolation.f90) \
$(wildcard ./Main_algorithm/output_grid_partition.f90) \
$(wildcard ./Memory_IO/*.f90) \
$(wildcard ./Pre_processing/*.f90) \
$(wildcard ./Post_processing/*.f90) \
$(wildcard ./Topography/*.f90)

MAIN_FILE_ROOT = ./Main_algorithm/Grid_Interpolator

EXE_CODE = \
$(EXE_DIR)Grid_Interpolator_$(VERSION)_$(ID_COMPILER)$(ID_EXECUTION)$(ID_PARALLELIZATION)

OBJECTS = $(patsubst %.f90,%.o,$(SOURCES))

# Targets: start
#    Correct sequence to compile+link from a Linux terminal: 
#       "make"

all: touch compile link clean 

compile: $(SOURCES)

$(SOURCES): %.f90: %.o
	$(COMPILER) $@ -o $< $(OMP_FLAG) $(COMPILATION_FLAGS) -c

link: $(OBJECTS)
	touch $(EXE_CODE).x
	$(COMPILER) $(MAIN_FILE_ROOT).f90 $^ -o $(EXE_CODE).x $(OMP_FLAG) $(COMPILATION_FLAGS)
	rm -f $(MAIN_FILE_ROOT).o

clean: $(OBJECTS)
	rm -f $^
	rm -f *.mod

touch:
	touch $(OBJECTS)

remove: 
	rm -f $(EXE_CODE).x

# Targets: end
