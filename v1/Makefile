VPATH = %.inc xml-fortran
VPATH = %.mod xml-fortran
F90 = gfortran
F90FLAGS = -cpp -fbacktrace -g
xml_fortran_objects = xml-fortran/read_xml_primitives.o xml-fortran/write_xml_primitives.o \
  xml-fortran/xmlparse.o

read_write_driver:  $(xml_fortran_objects) xml_data_geometry_t.o read_write_driver.f90
	$(F90) $(F90FLAGS) -J xml-fortran -I xml-fortran $^ -o $@

xml_data_geometry_t.o: geometry_t.f90 xml-fortran/xmlreader
	$(F90) $(F90FLAGS) -I xml-fortran -c $< -o $@

geometry_t.f90: xml-fortran/xmlreader geometry_t.xml
	$< geometry_t

xml-fortran/xmlreader $(xml_fortran_objects): xml-fortran/Makefile
	cd xml-fortran; make F90=$(F90) F90FLAGS="$(F90FLAGS)"

clean:
	cd xml-fortran; make clean
	rm *.o *.mod geometry_t.f90 xmlreader.out read_write_driver geometry_driver_output.xml
