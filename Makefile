F90 = gfortran
F90FLAGS = -cpp -fbacktrace


geometry_t.f90: xml_fortran_v0/xmlreader geometry_t.xml
	xml_fortran_v0/xmlreader geometry_t

xml_fortran_v0/xmlreader:
	cd xml_fortran_v0; make F90=$(F90) F90FLAGS="$(F90FLAGS)"






