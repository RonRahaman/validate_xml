program read_write_driver

use read_xml_primitives
use write_xml_primitives
use xmlparse
use xml_data_cross_sections_t
use xml_data_geometry_t
use xml_data_materials_t
use xml_data_plots_t
use xml_data_settings_t
use xml_data_tallies_t

implicit none

character(len=50) :: mode

call get_command_argument(1, mode)

select case(trim(mode))
case('cross_sections')
  print *, '*** calling read_write_cross_sections() ...'
  call read_write_cross_sections()
case('geometry')
  print *, 'calling read_write_geometry() ...'
  call read_write_geometry()
case('materials')
  print *, '*** calling read_write_materials() ...'
  call read_write_materials()
case('plots')
  print *, '*** calling read_write_plots() ...'
  call read_write_plots()
case('settings')
  print *, '*** calling read_write_settings() ...'
  call read_write_settings()
case('tallies')
  print *, '*** calling read_write_tallies() ...'
  call read_write_tallies()
case default
  print *, 'Error from read_write_driver'
  print *, 'Must provide command line argument for testing mode: '// &
    '(cross_sections, geometry, materials, plots, settings, tallies)'
  stop
end select

contains

subroutine read_write_cross_sections()
  integer :: tstart, tend, trate
  call system_clock(tstart, trate)
  call read_xml_file_cross_sections_t('cross_sections.xml')
  call system_clock(tend)
  print *, 'read_xml_file_cross_sections_t completed in (seconds):', &
    dble(tend-tstart)/trate
  call write_xml_file_cross_sections_t('cross_sections_validation.xml')
end subroutine read_write_cross_sections

subroutine read_write_geometry()
  integer :: tstart, tend, trate
  call system_clock(tstart, trate)
  call read_xml_file_geometry_t('geometry.xml')
  call system_clock(tend)
  print *, 'read_xml_file_geometry_t completed in (seconds):', &
    dble(tend-tstart)/trate
  call write_xml_file_geometry_t('geometry_validation.xml')
end subroutine read_write_geometry

subroutine read_write_materials()
  integer :: tstart, tend, trate
  call system_clock(tstart, trate)
  call read_xml_file_materials_t('materials.xml')
  call system_clock(tend)
  print *, 'read_xml_file_materials_t completed in (seconds):', &
    dble(tend-tstart)/trate
  call write_xml_file_materials_t('materials_validation.xml')
end subroutine read_write_materials

subroutine read_write_plots()
  integer :: tstart, tend, trate
  call system_clock(tstart, trate)
  call read_xml_file_plots_t('plots.xml')
  call system_clock(tend)
  print *, 'read_xml_file_plots_t completed in (seconds):', &
    dble(tend-tstart)/trate
  call write_xml_file_plots_t('plots_validation.xml')
end subroutine read_write_plots

subroutine read_write_settings()
  integer :: tstart, tend, trate
  call system_clock(tstart, trate)
  call read_xml_file_settings_t('settings.xml')
  call system_clock(tend)
  print *, 'read_xml_file_settings_t completed in (seconds):', &
    dble(tend-tstart)/trate
  call write_xml_file_settings_t('settings_validation.xml')
end subroutine read_write_settings

subroutine read_write_tallies()
  integer :: tstart, tend, trate
  call system_clock(tstart, trate)
  call read_xml_file_tallies_t('tallies.xml')
  call system_clock(tend)
  print *, 'read_xml_file_tallies_t completed in (seconds):', &
    dble(tend-tstart)/trate
  call write_xml_file_tallies_t('tallies_validation.xml')
end subroutine read_write_tallies


end program read_write_driver
