! RUN IN VERBOSE MODE: fpm test '*test_suite*' -- brief=F
program runtest
use, intrinsic :: iso_fortran_env, only: &
& stdin => input_unit,   &
& stdout => output_unit, &
& stderr => error_unit
use M_framework, only : unit_test_start, unit_test, unit_test_msg
use M_framework, only : unit_test_end, unit_test_stop, unit_test_mode
use M_framework, only : unit_test_level, unit_test_flags
!use M_xxxx
implicit none
logical, parameter :: T=.true., F=.false.
logical            :: matched
! optional call to change default modes
   call unit_test_mode(        &
       keep_going=T,           &
       flags=[0],              &
       luns=[stdout],          &
       command='',             &
       brief=T,                &
       match='',               &
       interactive=F,          &
       CMDLINE=T,              &
       debug=F)

   unit_test_level=0

   call test_suite_basename               ()
   call test_suite_canon_path             ()
   call test_suite_delete_file            ()
   call test_suite_dirname                ()
   call test_suite_execute_and_read_output()
   call test_suite_exists                 ()
   call test_suite_fileclose              ()
   call test_suite_fileopen               ()
   call test_suite_filewrite              ()
   call test_suite_get_dos_path           ()
   call test_suite_get_home               ()
   call test_suite_getline                ()
   call test_suite_get_local_prefix       ()
   call test_suite_get_temp_filename      ()
   call test_suite_is_absolute_path       ()
   call test_suite_is_dir                 ()
   call test_suite_is_hidden_file         ()
   call test_suite_join_path              ()
   call test_suite_LINE_BUFFER_LEN        ()
   call test_suite_list_files             ()
   call test_suite_mkdir                  ()
   call test_suite_number_of_rows         ()
   call test_suite_os_delete_dir          ()
   call test_suite_parent_dir             ()
   call test_suite_read_lines             ()
   call test_suite_read_lines_expanded    ()
   call test_suite_run                    ()
   call test_suite_unix_path              ()
   call test_suite_warnwrite              ()
   call test_suite_which                  ()
   call test_suite_windows_path           ()
   call unit_test_stop()

contains

subroutine test_suite_basename               ()
   call unit_test_start("basename               ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("basename               ", 0 .eq. 0, "checking",100)
   call unit_test_end("basename               ",msg="")
end subroutine test_suite_basename               

subroutine test_suite_canon_path             ()
   call unit_test_start("canon_path             ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("canon_path             ", 0 .eq. 0, "checking",100)
   call unit_test_end("canon_path             ",msg="")
end subroutine test_suite_canon_path             

subroutine test_suite_delete_file            ()
   call unit_test_start("delete_file            ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("delete_file            ", 0 .eq. 0, "checking",100)
   call unit_test_end("delete_file            ",msg="")
end subroutine test_suite_delete_file            

subroutine test_suite_dirname                ()
   call unit_test_start("dirname                ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("dirname                ", 0 .eq. 0, "checking",100)
   call unit_test_end("dirname                ",msg="")
end subroutine test_suite_dirname                

subroutine test_suite_execute_and_read_output()
   call unit_test_start("execute_and_read_output",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("execute_and_read_output", 0 .eq. 0, "checking",100)
   call unit_test_end("execute_and_read_output",msg="")
end subroutine test_suite_execute_and_read_output

subroutine test_suite_exists                 ()
   call unit_test_start("exists                 ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("exists                 ", 0 .eq. 0, "checking",100)
   call unit_test_end("exists                 ",msg="")
end subroutine test_suite_exists                 

subroutine test_suite_fileclose              ()
   call unit_test_start("fileclose              ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("fileclose              ", 0 .eq. 0, "checking",100)
   call unit_test_end("fileclose              ",msg="")
end subroutine test_suite_fileclose              

subroutine test_suite_fileopen               ()
   call unit_test_start("fileopen               ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("fileopen               ", 0 .eq. 0, "checking",100)
   call unit_test_end("fileopen               ",msg="")
end subroutine test_suite_fileopen               

subroutine test_suite_filewrite              ()
   call unit_test_start("filewrite              ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("filewrite              ", 0 .eq. 0, "checking",100)
   call unit_test_end("filewrite              ",msg="")
end subroutine test_suite_filewrite              

subroutine test_suite_get_dos_path           ()
   call unit_test_start("get_dos_path           ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("get_dos_path           ", 0 .eq. 0, "checking",100)
   call unit_test_end("get_dos_path           ",msg="")
end subroutine test_suite_get_dos_path           

subroutine test_suite_get_home               ()
   call unit_test_start("get_home               ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("get_home               ", 0 .eq. 0, "checking",100)
   call unit_test_end("get_home               ",msg="")
end subroutine test_suite_get_home               

subroutine test_suite_getline                ()
   call unit_test_start("getline                ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("getline                ", 0 .eq. 0, "checking",100)
   call unit_test_end("getline                ",msg="")
end subroutine test_suite_getline                

subroutine test_suite_get_local_prefix       ()
   call unit_test_start("get_local_prefix       ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("get_local_prefix       ", 0 .eq. 0, "checking",100)
   call unit_test_end("get_local_prefix       ",msg="")
end subroutine test_suite_get_local_prefix       

subroutine test_suite_get_temp_filename      ()
   call unit_test_start("get_temp_filename      ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("get_temp_filename      ", 0 .eq. 0, "checking",100)
   call unit_test_end("get_temp_filename      ",msg="")
end subroutine test_suite_get_temp_filename      

subroutine test_suite_is_absolute_path       ()
   call unit_test_start("is_absolute_path       ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("is_absolute_path       ", 0 .eq. 0, "checking",100)
   call unit_test_end("is_absolute_path       ",msg="")
end subroutine test_suite_is_absolute_path       

subroutine test_suite_is_dir                 ()
   call unit_test_start("is_dir                 ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("is_dir                 ", 0 .eq. 0, "checking",100)
   call unit_test_end("is_dir                 ",msg="")
end subroutine test_suite_is_dir                 

subroutine test_suite_is_hidden_file         ()
   call unit_test_start("is_hidden_file         ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("is_hidden_file         ", 0 .eq. 0, "checking",100)
   call unit_test_end("is_hidden_file         ",msg="")
end subroutine test_suite_is_hidden_file         

subroutine test_suite_join_path              ()
   call unit_test_start("join_path              ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("join_path              ", 0 .eq. 0, "checking",100)
   call unit_test_end("join_path              ",msg="")
end subroutine test_suite_join_path              

subroutine test_suite_LINE_BUFFER_LEN        ()
   call unit_test_start("LINE_BUFFER_LEN        ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("LINE_BUFFER_LEN        ", 0 .eq. 0, "checking",100)
   call unit_test_end("LINE_BUFFER_LEN        ",msg="")
end subroutine test_suite_LINE_BUFFER_LEN        

subroutine test_suite_list_files             ()
   call unit_test_start("list_files             ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("list_files             ", 0 .eq. 0, "checking",100)
   call unit_test_end("list_files             ",msg="")
end subroutine test_suite_list_files             

subroutine test_suite_mkdir                  ()
   call unit_test_start("mkdir                  ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("mkdir                  ", 0 .eq. 0, "checking",100)
   call unit_test_end("mkdir                  ",msg="")
end subroutine test_suite_mkdir                  

subroutine test_suite_number_of_rows         ()
   call unit_test_start("number_of_rows         ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("number_of_rows         ", 0 .eq. 0, "checking",100)
   call unit_test_end("number_of_rows         ",msg="")
end subroutine test_suite_number_of_rows         

subroutine test_suite_os_delete_dir          ()
   call unit_test_start("os_delete_dir          ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("os_delete_dir          ", 0 .eq. 0, "checking",100)
   call unit_test_end("os_delete_dir          ",msg="")
end subroutine test_suite_os_delete_dir          

subroutine test_suite_parent_dir             ()
   call unit_test_start("parent_dir             ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("parent_dir             ", 0 .eq. 0, "checking",100)
   call unit_test_end("parent_dir             ",msg="")
end subroutine test_suite_parent_dir             

subroutine test_suite_read_lines             ()
   call unit_test_start("read_lines             ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("read_lines             ", 0 .eq. 0, "checking",100)
   call unit_test_end("read_lines             ",msg="")
end subroutine test_suite_read_lines             

subroutine test_suite_read_lines_expanded    ()
   call unit_test_start("read_lines_expanded    ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("read_lines_expanded    ", 0 .eq. 0, "checking",100)
   call unit_test_end("read_lines_expanded    ",msg="")
end subroutine test_suite_read_lines_expanded    

subroutine test_suite_run                    ()
   call unit_test_start("run                    ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("run                    ", 0 .eq. 0, "checking",100)
   call unit_test_end("run                    ",msg="")
end subroutine test_suite_run                    

subroutine test_suite_unix_path              ()
   call unit_test_start("unix_path              ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("unix_path              ", 0 .eq. 0, "checking",100)
   call unit_test_end("unix_path              ",msg="")
end subroutine test_suite_unix_path              

subroutine test_suite_warnwrite              ()
   call unit_test_start("warnwrite              ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("warnwrite              ", 0 .eq. 0, "checking",100)
   call unit_test_end("warnwrite              ",msg="")
end subroutine test_suite_warnwrite              

subroutine test_suite_which                  ()
   call unit_test_start("which                  ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("which                  ", 0 .eq. 0, "checking",100)
   call unit_test_end("which                  ",msg="")
end subroutine test_suite_which                  

subroutine test_suite_windows_path           ()
   call unit_test_start("windows_path           ",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("windows_path           ", 0 .eq. 0, "checking",100)
   call unit_test_end("windows_path           ",msg="")
end subroutine test_suite_windows_path           

end program runtest
