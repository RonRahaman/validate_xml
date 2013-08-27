program read_write_driver

  use read_xml_primitives
  use write_xml_primitives
  use xmlparse
  use xml_data_geometry_t

  implicit none

  character(len=50) :: mode

  call get_command_argument(1, mode)

  select case(trim(mode))
  case("geometry")
    print *, "*** calling read_write_geometry() ..."
    call read_write_geometry()
  case default
    print *, "Error from read_write_driver"
    print *, "Must provide command line argument for testing mode: ('geometry')"
    stop
  end select

  contains

    subroutine read_write_geometry()
      integer :: tstart, tend, trate

      call system_clock(tstart, trate)
      call read_xml_file_geometry_t('geometry.xml')
      call system_clock(tend)
      print *, 'read_xml_file_geometry_t completed in (seconds):', &
        dble(tend-tstart)/trate
      !call write_xml_file_geometry_t('geometry_driver_output.xml')

    end subroutine read_write_geometry

end program read_write_driver
