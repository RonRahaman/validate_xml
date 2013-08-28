!!!! WRITE LUDEF, main
!!!! WRITE LUSUBS, main
!!!! WRITE LUPROLOG, open_tmp_files
!!!! WRITE LUSTART, open_tmp_files
!!!! WRITE LULOOP, open_tmp_files
!!!! WRITE LUMAIN, main
!!!! WRITE LUEND, open_tmp_files
   if ( present(errout) ) errout = error
   call xml_close(info)
end subroutine

!!!! WRITE LUDEFLT, open_tmp_files
!!!! WRITE LUWRITE, open_tmp_files
!!!! WRITE LUWRITP, main
!!!! WRITE LUWRV, main
   write(info%lun,'(a)') '</materials_t>'
   call xml_close(info)
end subroutine

!!!! WRITE LUINIT, main

end subroutine

end module
