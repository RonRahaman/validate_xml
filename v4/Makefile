VPATH = %.inc xml-fortran
VPATH = %.mod xml-fortran
F90 = gfortran
F90FLAGS = -g 

xml_fortran_objects = xml-fortran/read_xml_primitives.o xml-fortran/write_xml_primitives.o \
  xml-fortran/xmlparse.o

xml_data_t_objects = xml_data_cross_sections_t.o xml_data_geometry_t.o xml_data_materials_t.o \
										 xml_data_plots_t.o xml_data_settings_t.o xml_data_tallies_t.o

read_write_driver:  $(xml_fortran_objects) $(xml_data_t_objects) read_write_driver.f90
	$(F90) $(F90FLAGS) -J xml-fortran -I xml-fortran $^ -o $@

xml_data_%_t.o: %_t.f90 xml-fortran/xmlreader
	$(F90) $(F90FLAGS) -I xml-fortran -c $< -o $@

%_t.f90: xml-fortran/xmlreader %_t.xml
	$< $(basename $@)

xml-fortran/xmlreader $(xml_fortran_objects): xml-fortran/Makefile
	cd xml-fortran; make F90=$(F90) F90FLAGS="$(F90FLAGS)"

clean:
	rm *.o *.mod *_t.f90 xmlreader.out read_write_driver *_validation.xml; cd xml-fortran; make clean
