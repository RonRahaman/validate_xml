VPATH = %.inc xml_fortran_v0
VPATH = %.mod xml_fortran_v0
F90 = gfortran
F90FLAGS = -cpp -fbacktrace

xml_data_geometry_t.o: geometry_t.f90
	$(F90) $(F90FLAGS) -I xml_fortran_v0 -c $< -o $@

geometry_t.f90: xml_fortran_v0/xmlreader geometry_t.xml
	xml_fortran_v0/xmlreader geometry_t

xml_fortran_v0/xmlreader:
	cd xml_fortran_v0; make F90=$(F90) F90FLAGS="$(F90FLAGS)"








