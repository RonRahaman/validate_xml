! Part of XML-Fortran library:
!
! $Id: read_xml_array.inc,v 1.3 2007/02/26 20:33:38 arjenmarkus Exp $
!
type(XML_PARSE), intent(inout)                :: info
character(len=*), intent(in)                  :: tag
logical, intent(inout)                        :: endtag
character(len=*), dimension(:,:), intent(in)  :: attribs
integer, intent(in)                           :: noattribs
character(len=*), dimension(:), intent(in)    :: data
integer, intent(in)                           :: nodata
logical, intent(inout)                        :: has_var

character(len=len(attribs(1,1)))              :: buffer
integer                                       :: idx
integer                                       :: ierr

!
! The big trick:
! A string long enough to hold all data strings
!
character(len=:), pointer :: bufferd => null(), bufferd_new => null()
integer :: start
integer :: len_new



allocate(character(len=16) :: bufferd)

!
! The value can be stored in an attribute values="..." or in
! the data
!
has_var = .false.
idx = xml_find_attrib( attribs, noattribs, 'values', buffer )
if ( idx .gt. 0 ) then
  call read_from_buffer( buffer, var, ierr )
  if ( buffer .ne. ' ' ) then
    has_var = .true.
  endif
else
  bufferd = ' '
  start   = 1
  do idx = 1,nodata
    if ( data(idx) .ne. ' ' ) then

      if ( start+len(data(idx)) .gt. len(bufferd) ) then
        len_new = len(bufferd)*2
        allocate(character(len=len_new) :: bufferd_new)
        bufferd_new = bufferd
        deallocate(bufferd)
        bufferd => bufferd_new
        bufferd_new => null()
      endif

      bufferd(start:) = data(idx)
      start = start + len(data(idx)) + 1
    endif
  enddo
  call read_from_buffer( bufferd, var, ierr )
  if ( bufferd .ne. ' ' ) then
    has_var = .true.
  endif
endif

if ( ierr .ne. 0 ) then
  write(*,*) 'Error reading variable - tag = ', trim(tag)
  has_var = .false.
endif
