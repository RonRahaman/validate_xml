program read_write_driver

  use read_xml_primitives
  use write_xml_primitives
  use xmlparse
  use xml_data_materials_t
  use xml_data_tallies_t

  implicit none

  character(len=50) :: mode

  call get_command_argument(1, mode)

  select case(trim(mode))
  case("materials")
    print *, "*** calling read_write_materials() ..."
    call read_write_materials()
  case("tallies")
    print *, "*** calling read_write_tallies() ..."
    call read_write_tallies()
  case default
    print *, "Error from read_write_driver"
    print *, "Must provide command line argument for testing mode: ('geometry')"
    stop
  end select

  contains

    subroutine read_write_materials()
      integer :: tstart, tend, trate

      call system_clock(tstart, trate)
      call read_xml_file_materials_t('materials.xml')
      call system_clock(tend)
      print *, 'read_xml_file_materials_t completed in (seconds):', &
        dble(tend-tstart)/trate
      call write_xml_file_materials_t('materials_validation.xml')
    end subroutine read_write_materials

    subroutine read_write_tallies()
      integer :: tstart, tend, trate

      call system_clock(tstart, trate)
      call read_xml_file_tallies_t('materials.xml')
      call system_clock(tend)
      print *, 'read_xml_file_tallies_t completed in (seconds):', &
        dble(tend-tstart)/trate
      call write_xml_file_materials_t('tallies_validation.xml')
    end subroutine read_write_tallies
      

end program read_write_driver
