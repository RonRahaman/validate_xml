VPATH = %.inc xml-fortran
VPATH = %.mod xml-fortran
F90 = gfortran
F90FLAGS = -cpp -fbacktrace

xml_data_geometry_t.o: geometry_t.f90 xml-fortran/xmlreader
	$(F90) $(F90FLAGS) -I xml-fortran -c $< -o $@

geometry_t.f90: xml-fortran/xmlreader geometry_t.xml
	xml-fortran/xmlreader geometry_t

xml-fortran/xmlreader:
	cd xml-fortran; make F90=$(F90) F90FLAGS="$(F90FLAGS)"

clean:
	cd xml-fortran; make clean
	rm *.o *.mod geometry_t.f90 xmlreader.out
